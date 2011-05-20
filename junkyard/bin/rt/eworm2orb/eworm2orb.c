/*
 * eworm2orb.c:  Prototype module to take messages from an Earthworm shared-
 *  memory ring and transfer them to an orb-server.
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * November, 1996
 */

#ifdef _OS2
FAIL--not coded for OS2
#endif

#include <stdio.h>
#include <stdlib.h>
#include <thread.h>
#include <string.h>
#include <time.h>
#include <earthworm.h>
#include <transport.h>
#include <kom.h>
#include <trace_buf.h>
#include "stock.h"
#include "orb.h"
#include "defunctpkt.h"
#include "iceworm_extensions.h"

float htonf (float arg);

#define IW_ORB_TRACE_HEADER_SIZE 16
#define INT(x)  ((x)<0.0?((x)-0.5):((x)+0.5))
#define STREQ(a, b) (strcmp((a), (b)) == 0)


/*
 * Functions in this source file 
 */
void eworm2orb_config( char * );
void eworm2orb_lookup( void );
void eworm2orb_status( unsigned char, short, char * );
void eworm2orb_tracebuf( TracePacket *, long );
void terminate_if_requested( void );
void handle_getmsg_problem( int, MSG_LOGO *, long, char * );
void *Heartbeat( void * );

static SHM_INFO Region;		/* shared memory region to use for i/o    */

#define  MAXLOGO   2
MSG_LOGO GetLogo[MAXLOGO];	/* array for requesting
					 * module,type,instid */
short	nLogo;

#define MSG_SIZE 60000		/* define maximum size for an incoming msg */
#define MAX_LOGMSG_SIZE 256	/* maximum size of a log message */
#define NOMSG_SLEEPMSEC 3	/* milliseconds to sleep if we don't get a msg*/
#define NOORB_SLEEPMSEC 8000	/* milliseconds to sleep if we don't get a msg*/

int OrbFd;			/* File descriptor for orb connection */

/*
 * Things to read or derive from configuration file 
 */
static char	RingName[20];	/* name of transport ring for i/o    */
static char	MyModName[20];	/* speak as this module name/id      */
static int 	LogSwitch;	/* 0 if no logfile should be written */
static long	HeartBeatInterval;	/* seconds between heartbeats        */
static char	Orbname[STRSZ];		/* IP:Port of orbserver */
static int	Compress;	/* Whether to first difference/ generic compress 
				  data (with Harvey's routine) */
static char	Network[20] = "-";	/* Name of network to export */
static int	Timecorr;	/* Whether to correct time stamps per SITE_DB */

/*
 * Things to look up in the earthworm.h tables with getutil.c functions 
 */
static long	RingKey;	/* key of transport ring for i/o     */
static unsigned char InstId;	/* local installation id             */
static unsigned char MyModId;	/* Module Id for this program        */
static unsigned char TypeHeartBeat;
static unsigned char TypeError;
static unsigned char TypeTracebuf;

/*
 * Error messages used by eworm2orb 
 */
#define  ERR_MISSMSG       0	/* message missed in transport ring       */
#define  ERR_TOOBIG        1	/* retreived msg too large for buffer     */
#define  ERR_NOTRACK       2	/* msg retreived; tracking limit exceeded */

main( int argc, char **argv )
{
	thread_t tidHeartbeat;
	TracePacket tp; /* storage locatation for incoming trace packet */
	long	recsize;/* size of retrieved message     */
	MSG_LOGO reclogo;/* logo of retrieved message     */
	int	res;
	char 	*str;
	char	sitedb[FILENAME_MAX];

	if ( argc != 2 ) 
	{
		fprintf( stderr, "Usage: eworm2orb <configfile>\n" );
		exit( 0 );
	}

	chdir_ewparams( "eworm2orb" );

	eworm2orb_config( argv[1] );

	eworm2orb_lookup( );

	logit_init( "eworm2orb", (short) MyModId, MAX_LOGMSG_SIZE, LogSwitch );
	logit( "", "%s: Read command file <%s>\n", argv[0], argv[1] );

	while( ( OrbFd = orbopen( Orbname, "w&" ) ) == -1 ) {
		logit( "et", "Failed to connect to orb on port %d\n", OrbFd );
		sleep_ew( NOORB_SLEEPMSEC );
	}

	tport_attach( &Region, RingKey );
	logit( "", "eworm2orb: Attached to public memory region %s: %d\n",
	      RingName, RingKey );

	StartThread( Heartbeat, 0, &tidHeartbeat );

	if( ( ! STREQ( Network, "-" ) ) || Timecorr ) {

        	str = getenv( "SITE_DB" );
        	if ( str == NULL )
        	{
                	fprintf(stderr, "Environment variable SITE_DB not defined; ");
                	fprintf(stderr, "eworm2orb exiting.\n");
                	exit( -1 );
        	}
        	else
        	{
                	strcpy( sitedb, str );
	 
                	read_site_db( sitedb );

			if( ! STREQ( Network, "-" ) ) {

                       		lookup_network( sitedb, Network );
                       		if( subset_for_network() <= 0 )
                       		{
                               		fprintf(stderr,
                               		"eworm2orb: No stations in %s Network. Bye.\n",
                               		Network );
                               		exit( -1 );
                       		}
			}
        	}

	}

	for( ;; ) 
	{
		terminate_if_requested();

		res = tport_getmsg( &Region, GetLogo, nLogo,
		    &reclogo, &recsize, tp.msg, sizeof( tp.msg ) - 1 );

                switch( res )
		{
                       case GET_OK:
                               break;
                       case GET_NONE:
				sleep_ew( NOMSG_SLEEPMSEC );
                               continue;
                       case GET_TOOBIG:
                               handle_getmsg_problem( res,&reclogo,
							recsize,tp.msg );
                               continue;
                       case GET_MISS:
                       case GET_NOTRACK:
                               handle_getmsg_problem( res,&reclogo,
							recsize,tp.msg );
                               break;
                       default:
                               /* Shouldn't happen */
                               continue;
                       }

                       /* Fall through: Process the message */

		if ( reclogo.type == TypeTracebuf )
		{
			eworm2orb_tracebuf( &tp, recsize );
		}
	}
}

/******************************************************************************
 *  eworm2orb_config() processes command file(s) using kom.c functions;        *
 *                    exits if any errors are encountered.                    *
 ******************************************************************************/
void 
eworm2orb_config( char *configfile )
{
	int	ncommand; /* # of required commands you expect to process */
	char	init[10]; /* init flags, one byte for each required command */
	int	nmiss;	/* number of required commands that were missed */
	char	*com;
	char	*str;
	int	nfiles;
	int	success;
	int	i;

	/*
	 * Set to zero one init flag for each required command 
	 */
	ncommand = 8;
	for ( i = 0; i < ncommand; i++ )
		init[i] = 0;
	nLogo = 0;

	/*
	 * Open the main configuration file 
	 */
	nfiles = k_open( configfile );
	if ( nfiles == 0 ) 
	{
		fprintf( stderr,
		   "eworm2orb: Error opening command file <%s>; exiting!\n",
			configfile );
		exit( -1 );
	}
	/*
	 * Process all command files 
	 */
	while ( nfiles > 0 ) 
	{	/* While there are command files open */
		while ( k_rd() ) 
		{/* Read next line from active file  */
			com = k_str();	/* Get the first token from line */

			/*
			 * Ignore blank lines & comments 
			 */
			if ( !com )
				continue;
			if ( com[0] == '#' )
				continue;

			/*
			 * Open a nested configuration file 
			 */
			if ( com[0] == '@' ) 
			{
				success = nfiles + 1;
				nfiles = k_open( &com[1] );
				if ( nfiles != success ) 
				{
					fprintf( stderr,
					"eworm2orb: Error opening command " );
					fprintf( stderr,
					"file <%s>; exiting!\n", &com[1] );
					exit( -1 );
				}
				continue;
			}
			/*
			 * Process anything else as a command 
			 */
			 /* 0 */ if ( k_its( "LogFile" ) ) 
			 {
				LogSwitch = k_int();
				init[0] = 1;
			}
			 /* 1 */ 
			else if ( k_its( "MyModuleId" ) ) 
			{
				str = k_str();
				if ( str )
					strcpy( MyModName, str );
				init[1] = 1;
			}
			 /* 2 */ 
			else if ( k_its( "RingName" ) ) 
			{
				str = k_str();
				if ( str )
					strcpy( RingName, str );
				init[2] = 1;
			}
			 /* 3 */ 
			else if ( k_its( "HeartBeatInterval" ) ) 
			{
				HeartBeatInterval = k_long();
				init[3] = 1;
			}
			/*
			 * Enter installation & module to get event messages
			 * from 
			 */
                         /* 4 */
                        else if ( k_its( "GetTracesFrom" ) ) 
			{
                           if ( nLogo >= MAXLOGO ) 
			   {
                              fprintf( stderr,
                                "eworm2orb: Too many <GetTracesFrom> " );
                              fprintf( stderr,"commands in <%s>",
                                        configfile );
                              fprintf( stderr, "; max=%d; exiting!\n",
                                        (int) MAXLOGO / 2 );
                              exit( -1 );
                           }
                           if ( ( str = k_str() ) ) 
			   {
                                if ( GetInst( str, &GetLogo[nLogo].instid ) != 0 ) 
				{
                                   fprintf( stderr,
                                   "eworm2orb: Invalid installation name <%s>",
                                   str );
                                   fprintf( stderr,
                                   " in <GetTracesFrom> cmd; exiting!\n" );
                                   exit( -1 );
                                }
                           }
                           if ( ( str = k_str() ) ) 
			   {
                                if ( GetModId( str, &GetLogo[nLogo].mod ) != 0 ) 
				{
                                   fprintf( stderr,
                                   "eworm2orb: Invalid module name <%s>", str );
                                   fprintf( stderr,
                                   " in <GetTracesFrom> cmd; exiting!\n" );
                                   exit( -1 );
                                }
                           }
                           if ( ( str = k_str() ) ) 
			   {
                                if ( GetType( str, &GetLogo[nLogo].type ) != 0 ) 
				{
                                   fprintf( stderr,
                                      "eworm2orb: Invalid message type <%s>",
                                      str );
                                   fprintf( stderr, "; exiting!\n" );
                                   exit( -1 );
                                }
                           }
			   nLogo += 1;
			   init[4] = 1;
			}
			 /* 5 */ 
			else if ( k_its( "Orbname" ) ) 
			{
				str = k_str();
				if( str )
				strcpy( Orbname, str );
				init[5] = 1;
			}
			 /* 6 */
			else if ( k_its( "Compress" ) ) 
			 {
				Compress = k_int();
				init[6] = 1;
			}
			 /* 7 */
			else if ( k_its( "Timecorr" ) ) 
			 {
				Timecorr = k_int();
				init[7] = 1;
			}
			 /* optional */ 
			else if ( k_its( "Network" ) ) 
			{
				str = k_str();
				if ( str )
					strcpy( Network, str );
			}
			/*
			 * Unknown command 
			 */
			else 
			{
				fprintf( stderr,
				   "eworm2orb: <%s> Unknown command in <%s>.\n",
				   com, configfile );
				continue;
			}

			/*
			 * See if there were any errors processing the
			 * command 
			 */
			if ( k_err() ) 
			{
				fprintf( stderr,
				"eworm2orb: Bad <%s> command in <%s>; exiting!\n",
					com, configfile );
				exit( -1 );
			}
		}
		nfiles = k_close();
	}

	/*
	 * After all files are closed, check init flags for missed commands 
	 */
	nmiss = 0;
	for ( i = 0; i < ncommand; i++ )
		if ( !init[i] )
			nmiss++;
	if ( nmiss ) 
	{
		fprintf( stderr, "eworm2orb: ERROR, no " );
		if ( !init[0]) fprintf( stderr, "<LogFile> " );
		if ( !init[1] ) fprintf( stderr, "<MyModuleId> " );
		if ( !init[2] ) fprintf( stderr, "<RingName> " );
		if ( !init[3] ) fprintf( stderr, "<HeartBeatInterval> " );
		if ( !init[4] ) fprintf( stderr, "<GetTracesFrom> " );
		if ( !init[5] ) fprintf( stderr, "<OrbPort> " );
		if ( !init[6] ) fprintf( stderr, "<Compress> " );
		if ( !init[7] ) fprintf( stderr, "<Timecorr> " );
		fprintf( stderr, "command(s) in <%s>; exiting!\n", configfile );
		exit( -1 );
	}
	return;
}

/******************************************************************************
 *  eworm2orb_lookup( )   Look up important info from earthworm.h tables       *
 ******************************************************************************/
void 
eworm2orb_lookup( void )
{
	/*
	 * Look up keys to shared memory regions 
	 */
	if ( ( RingKey = GetKey( RingName ) ) == -1 ) 
	{
		fprintf( stderr,
		"eworm2orb:  Invalid ring name <%s>; exiting!\n", RingName );
		exit( -1 );
	}
	/*
	 * Look up installations of interest 
	 */
	if ( GetLocalInst( &InstId ) != 0 ) 
	{
		fprintf( stderr,
		"eworm2orb: error getting local installation id; exiting!\n" );
		exit( -1 );
	}
	/*
	 * Look up modules of interest 
	 */
	if ( GetModId( MyModName, &MyModId ) != 0 ) 
	{
		fprintf( stderr,
		"eworm2orb: Invalid module name <%s>; exiting!\n", MyModName );
		exit( -1 );
	}
	/*
	 * Look up message types of interest 
	 */
	if ( GetType( "TYPE_HEARTBEAT", &TypeHeartBeat ) != 0 ) 
	{
		fprintf( stderr,
		"eworm2orb: Invalid message type <TYPE_HEARTBEAT>; exiting!\n" );
		exit( -1 );
	}
	if ( GetType( "TYPE_ERROR", &TypeError ) != 0 ) 
	{
		fprintf( stderr,
		"eworm2orb: Invalid message type <TYPE_ERROR>; exiting!\n" );
		exit( -1 );
	}
	if ( GetType( "TYPE_TRACEBUF", &TypeTracebuf ) != 0 ) 
	{
		fprintf( stderr,
		"eworm2orb: Invalid message type <TYPE_TRACEBUF>; exiting!\n" );
		exit( -1 );
	}
	return;
}

/******************************************************************************
 * eworm2orb_status() builds a heartbeat or error message & puts it into       *
 *                   shared memory.  Writes errors to log file & screen.      *
 ******************************************************************************/
void 
eworm2orb_status( unsigned char type, short ierr, char *note )
{
	MSG_LOGO logo;
	char	msg[256];
	long	size;
	long	t;

	/*
	 * Build the message 
	 */
	logo.instid = InstId;
	logo.mod = MyModId;
	logo.type = type;

	time( &t );

	if ( type == TypeHeartBeat ) 
	{
		sprintf( msg, "%ld\n\0", t );
	} 
	else if ( type == TypeError ) 
	{
		sprintf( msg, "%ld %hd %s\n\0", t, ierr, note );
		logit( "et", "eworm2orb: %s\n", note );
	}
	size = strlen( msg );	/* don't include the null byte in the message */

	/*
	 * Write the message to shared memory 
	 */
	if ( tport_putmsg( &Region, &logo, size, msg ) != PUT_OK ) 
	{
		if ( type == TypeHeartBeat ) 
		{
			logit( "et", "eworm2orb:  Error sending heartbeat.\n" );
		} 
		else if ( type == TypeError ) 
		{
			logit( "et",
			 "eworm2orb:  Error sending error:%d.\n", ierr );
		}
	}
	return;
}

/********************************************************************/
/* terminate_if_requested() checks the ring for a TERMINATE request */
/* And shuts down eworm2orb if such a request is found             */
/********************************************************************/
void
terminate_if_requested( void  )
{
        if ( tport_getflag( &Region ) == TERMINATE )
        {
                tport_detach( &Region );
                logit( "t", "eworm2orb: Termination requested; exiting\n" );
                fflush( stdout );
                exit( 0 );
        } 
	else 
	{
                return;
        }
}
/******************************************************************
 *                             Heartbeat                          *
 *          Send a heartbeat to the transport ring buffer         *
 ******************************************************************/
void *Heartbeat( void *dummy )
{
        char     msg[20];
        unsigned short length;
        time_t   now;
        MSG_LOGO logo;
 
        logo.instid = InstId;
        logo.mod   = MyModId;
        logo.type  = TypeHeartBeat;
        for(;;)
        {
                sleep_ew( HeartBeatInterval*1000 );
                time( &now );
                sprintf( msg, "%d\n\0", (int) now );
                length = strlen( msg );
                if ( tport_putmsg( &Region, &logo, length, msg ) != PUT_OK )
                        logit("t", "eworm2orb: Error sending heartbeat\n");
        }
}

/***********************************************************************/
/* handle_getmsg_problem() squeals about all difficulties that occurred */
/*  when reading messages off the shared-memory ring                    */
/***********************************************************************/
void
handle_getmsg_problem( int problem, MSG_LOGO *reclogo, long recsize, char *msg )
{
        char    Text[150];
 
        switch( problem )
        {
        case GET_TOOBIG:
                sprintf( Text,
                        "Retrieved msg[%ld] (c%u m%u t%u) too big for msg[%d]",
                        recsize,
                        reclogo->instid,
                        reclogo->mod,
                        reclogo->type,
                        sizeof( msg ) - 1 );
                eworm2orb_status( TypeError, ERR_TOOBIG, Text );
                break;
        case GET_MISS:
                sprintf( Text, "Missed msg(s)  c%u m%u t%u  %s.",
                        reclogo->instid,
                        reclogo->mod,
                        reclogo->type,
                        RingName );
                eworm2orb_status( TypeError, ERR_MISSMSG, Text );
                break;
        case GET_NOTRACK:
                sprintf( Text,
                "Msg received (c%u m%u t%u); transport.h NTRACK_GET exceeded",
                          reclogo->instid, reclogo->mod, reclogo->type );
                eworm2orb_status( TypeError, ERR_NOTRACK, Text );
                break;
        default:
                /* Shouldn't happen */
                break;
        }
 
        return;
}
 
/*******************************************************************************
 *  eworm2orb_tracebuf( )  process a trace-packet message                     *
 *******************************************************************************/
void 
eworm2orb_tracebuf( TracePacket *tp, long size )
{
	STACHAN *stachan;
	char	srcid[STRSZ];
	char	orbpacket[MAX_TRACEBUF_SIZ+100];
	struct PreHdr prehdr;
	char	*ptr;
	int	rc;
	float	samprate;
	int	calib;
	unsigned short nsamp;
	unsigned short pinno;
	unsigned short quality;
	union {
		char *c;
		int  *i;
		short *s;
		float *f;
	} datap;
	unsigned char *buf=NULL;
	int	bsize;
	int	databuf_size;
	int	packetsize;
	float	convert;
	char	datatype[4];
	double	commdelay;
	double	starttime;
	int	*data;
	int	i;
	int	nout;

	WaveMsgMakeLocal( (TRACE_HEADER *) tp );

	stachan = lookup_stachan( tp->trh.sta, tp->trh.chan  );

	if( ! STREQ( Network, "-" ) && stachan == NULL ) 
	{
		return;
	} 
	else
	{
		prehdr.hdrtype = htons (IWH);
		prehdr.pkttype = htons (IWTB);
		prehdr.hdrsiz = htons( sizeof( struct PreHdr ) + IW_ORB_TRACE_HEADER_SIZE );

		samprate = tp->trh.samprate;
		/* samprate = htonl ( convert ); */
		convert = 0.0;
		calib = htonl( convert );					/* calib */
		pinno = htons( (unsigned short) tp->trh.pinno );
		nsamp = htons( (unsigned short) tp->trh.nsamp );
		memcpy( &quality, &(tp->trh.quality[0]), 2 );
		quality = htons( quality );

		datap.c = tp->msg + sizeof( TRACE_HEADER );

		if( Compress )
		{
			strcpy( datatype, "gc" );

			data = (int *) malloc (tp->trh.nsamp*sizeof(int));
			if (data == NULL) {
				elog_log(0, "eworm2orb_tracebuf: malloc() error.\n");
				return;
			}

			if( STREQ( tp->trh.datatype, "s2" ) )
			{
				data[0] = datap.s[0];
				for( i=1; i<tp->trh.nsamp; i++ )
				{
					data[i] = datap.s[i] - datap.s[i-1];
				}
			}
			else if( STREQ( tp->trh.datatype, "s4" ) )
			{
				data[0] = datap.i[0];
				for( i=1; i<tp->trh.nsamp; i++ )
				{
					data[i] = datap.i[i] - datap.i[i-1];
				}
			}
			else if( STREQ( tp->trh.datatype, "t4" ) )
			{
				data[0] = INT( datap.f[0] );
				for( i=1; i<tp->trh.nsamp; i++ )
				{
					data[i] = INT( datap.f[i] ) - INT( datap.f[i-1] );
				}
			}
			else
			{
				elog_log( 0, "eworm2orb_tracebuf: Datatype %s not understood\n",
						   tp->trh.datatype );
				return;
			}

			if( gencompress (&buf, &nout, &bsize, data, tp->trh.nsamp, 25) < 0)
			{
				elog_log(0, "eworm2orb_tracebuf: gencompress() error.\n");
				return;
			}

			free( data );

			databuf_size = nout;
		}
		else
		{
			strcpy( datatype, tp->trh.datatype );

			buf = (unsigned char *) datap.c;

			databuf_size = tp->trh.nsamp * datasize( datatype );
		}

		packetsize = databuf_size + sizeof( struct PreHdr ) + IW_ORB_TRACE_HEADER_SIZE;
		prehdr.pktsiz = htons( packetsize );

		ptr = &orbpacket[0];

		memcpy( ptr, &prehdr, sizeof( struct PreHdr ) );
		ptr += sizeof( struct PreHdr );
		memcpy( ptr, &samprate, sizeof(float));
		ptr += sizeof( float );
		memcpy( ptr, &calib, sizeof(float));
		ptr += sizeof( float );
		memcpy( ptr, &pinno, sizeof(unsigned short));
		ptr += sizeof( unsigned short );
		memcpy( ptr, &nsamp, sizeof(unsigned short));
		ptr += sizeof( unsigned short );
		memcpy( ptr, datatype, 2);
		ptr += 2;
		memcpy( ptr, &quality, sizeof(unsigned short));
		ptr += sizeof( unsigned short );

		memcpy( ptr, buf, databuf_size );

		if( Compress )
		{
			free( buf );
		}

		if( Timecorr == 0 || stachan == NULL ) {
			commdelay = 0;
		} else {
			commdelay = stachan->commdelay;
		}
		
		starttime = tp->trh.starttime - commdelay;

		sprintf( srcid, "%s_%s_%s", tp->trh.net,
					  tp->trh.sta,
					  tp->trh.chan );

		rc = orbput( OrbFd, srcid, starttime, &orbpacket[0], packetsize );

		if( rc != 0 ) logit( "et", 
			"orbput failed for packet source %s\n", srcid );
	}
}
