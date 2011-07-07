/*
 * orb2eworm.c:  Prototype Iceworm module to take messages from an orb server
 *  and put them on an Earthworm shared-memory ring
 *  for the moment assumes that it is dealing with trace-packets, and labels
 *  their mod-ids as the orb-import module
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
#include <kom.h>
#include <transport.h>
#include <trace_buf.h>
#include <iceworm_extensions.h>
#include "stock.h"
#include "orb.h"
#include "Pkt.h"


/*
 * Functions in this source file 
 */
void orb2eworm_config( char * );
void orb2eworm_lookup( void );
void orb2eworm_status( unsigned char, short, char * );
void terminate_if_requested( void );
int pktchan_to_tracebuf( PktChannel *, TracePacket *, double, int * );
void *Heartbeat( void * );

#define ENDTIME(TIME,SAMPRATE,NSAMP)   ((TIME) + ((NSAMP)-1)/(SAMPRATE))
#define STREQ(a, b) (strcmp((a), (b)) == 0)

static SHM_INFO Region;		/* shared memory region to use for i/o    */

#define  MAXLOGO   2
MSG_LOGO GetLogo[MAXLOGO];	/* array for requesting
					 * module,type,instid */
short	nLogo;

#define MSG_SIZE 60000		/* define maximum size for an event msg   */
#define MAX_LOGMSG_SIZE 256	/* Maximum size for a log message */

#define UNSTUFFPKT_DATATYPE "s4" 

/*
 * Things to read or derive from configuration file 
 */
static char	RingName[20];	/* name of transport ring for i/o    */
static char	MyModName[20];	/* speak as this module name/id      */
static int 	LogSwitch;	/* 0 if no logfile should be written */
static long	HeartBeatInterval;	/* seconds between heartbeats        */
static char	OrbName[STRSZ]; /* Name of orb to which to connect */
static char	ChannelSelect[STRSZ]; /* Regular expression to choose data
					streams to import */
static char	ChannelReject[STRSZ]; /* Regular expression to choose data
					streams not to import */
static int	OrbFd;		/* File Descriptor for talking to orb */

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
 * Error messages used by orb2eworm 
 */
#define  ERR_MISSMSG       0	/* message missed in transport ring       */
#define  ERR_TOOBIG        1	/* retreived msg too large for buffer     */
#define  ERR_NOTRACK       2	/* msg retreived; tracking limit exceeded */


main( int argc, char **argv )
{
	thread_t tidHeartbeat;
	int	nbytes_orb;	
	int	nbytes_tp;
	MSG_LOGO putlogo;	/* logo of message     */
	int	res;
	TracePacket tp;
	int     pktid;
	char    srcname[STRSZ];
	double  mytime;
	char    *rawpkt = NULL;
	struct Packet *Pkt = NULL;
	struct PktChannel *pktchan = NULL;
	int	ichan;
	int     bufsize = 0;
	char	*sitedb;
	int	rc;	

	if ( argc != 2 ) 
	{
		fprintf( stderr, "Usage: orb2eworm <configfile>\n" );
		exit( 0 );
	}

	chdir_ewparams( "orb2eworm" );

	elog_init( argc, argv );

	orb2eworm_config( argv[1] );

	logit_init( "orb2eworm", (short) MyModId, MAX_LOGMSG_SIZE, LogSwitch );
	logit( "", "%s: Read command file <%s>\n", argv[0], argv[1] );

	orb2eworm_lookup( );

	if( ( sitedb = getenv( "SITE_DB" ) ) == NULL )
	{
		fprintf(stderr, "Environment variable SITE_DB not defined; ");
		fprintf(stderr, "orb2eworm exiting.\n");
		exit( -1 );
	}

	read_site_db( sitedb );

	OrbFd = orbopen( OrbName, "r&" );
	if( OrbFd < 0 ) 
	{
		logit( "e", "Failed to connect to orb %s--Bye\n", OrbName );
		exit( -1 );
	}
	else 
	{
		logit( "", "Connected to orb %s\n", OrbName );
	}

	if( ! STREQ( ChannelSelect, "" ) )
	{
		orbselect( OrbFd, ChannelSelect );
	}

	if( ! STREQ( ChannelReject, "" ) )
	{
		orbreject( OrbFd, ChannelReject );
	}

	tport_attach( &Region, RingKey );
	logit( "", "orb2eworm: Attached to public memory region %s: %d\n",
	      RingName, RingKey );

	StartThread( Heartbeat, 0, &tidHeartbeat );

	putlogo.instid = InstId;
	putlogo.mod = MyModId;
	putlogo.type = TypeTracebuf;

	for( ;; ) 
	{
		terminate_if_requested();

		/* Accept that heartbeats will be delayed for 
		   now while orbreap blocks */

		rc = orbreap( OrbFd, &pktid, srcname, &mytime,
			&rawpkt, (int *)&nbytes_orb, &bufsize );
		if( rc < 0 )
		{
			elog_clear_register( 1 );
			continue;
		}

		rc = unstuffPkt( srcname, mytime, rawpkt, nbytes_orb, &Pkt );
		if( rc == 0 )
		{
			logit( "et", "Unstuff failure in orb2eworm for %s\n", srcname );
			elog_clear_register( 1 );
			continue;
		}

		for( ichan = 0; ichan < Pkt->nchannels; ichan++ )
		{
			pktchan = gettbl( Pkt->channels, ichan );

			if( pktchan_to_tracebuf( pktchan, &tp,
						 mytime, &nbytes_tp  ) )
					continue;

			res = tport_putmsg( &Region, &putlogo,
					    nbytes_tp, tp.msg );
		}
	}

}

/******************************************************************************
 *  orb2eworm_config() processes command file(s) using kom.c functions;        *
 *                    exits if any errors are encountered.                    *
 ******************************************************************************/
void 
orb2eworm_config( char *configfile )
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
	ncommand = 7;
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
		   "orb2eworm: Error opening command file <%s>; exiting!\n",
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
					"orb2eworm: Error opening command " );
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
			 /* 4 */ 
			else if ( k_its( "OrbName" ) ) 
			{
				str = k_str();
				if ( str )
					strcpy( OrbName, str );
				init[4] = 1;
			}
			 /* 5 */ 
			else if ( k_its( "ChannelSelect" ) ) 
			{
				str = k_str();
				if ( str )
					strcpy( ChannelSelect, str );
				init[5] = 1;
			}
			 /* 6 */ 
			else if ( k_its( "ChannelReject" ) ) 
			{
				str = k_str();
				if ( str )
					strcpy( ChannelReject, str );
				init[6] = 1;
			}
			/*
			 * Unknown command 
			 */
			else 
			{
				fprintf( stderr,
				   "orb2eworm: <%s> Unknown command in <%s>.\n",
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
				"orb2eworm: Bad <%s> command in <%s>; exiting!\n",
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
		fprintf( stderr, "orb2eworm: ERROR, no " );
		if ( !init[0] ) fprintf( stderr, "<LogFile> " );
		if ( !init[1] ) fprintf( stderr, "<MyModuleId> " );
		if ( !init[2] ) fprintf( stderr, "<RingName> " );
		if ( !init[3] ) fprintf( stderr, "<HeartBeatInterval> " );
		if ( !init[4] ) fprintf( stderr, "<OrbName> " );
		if ( !init[5] ) fprintf( stderr, "<ChannelSelect> " );
		if ( !init[6] ) fprintf( stderr, "<ChannelReject> " );
		fprintf( stderr, "command(s) in <%s>; exiting!\n", configfile );
		exit( -1 );
	}
	return;
}

/******************************************************************************
 *  orb2eworm_lookup( )   Look up important info from earthworm.h tables       *
 ******************************************************************************/
void 
orb2eworm_lookup( void )
{
	/*
	 * Look up keys to shared memory regions 
	 */
	if ( ( RingKey = GetKey( RingName ) ) == -1 ) 
	{
		fprintf( stderr,
		"orb2eworm:  Invalid ring name <%s>; exiting!\n", RingName );
		exit( -1 );
	}
	/*
	 * Look up installations of interest 
	 */
	if ( GetLocalInst( &InstId ) != 0 ) 
	{
		fprintf( stderr,
		"orb2eworm: error getting local installation id; exiting!\n" );
		exit( -1 );
	}
	/*
	 * Look up modules of interest 
	 */
	if ( GetModId( MyModName, &MyModId ) != 0 ) 
	{
		fprintf( stderr,
		"orb2eworm: Invalid module name <%s>; exiting!\n", MyModName );
		exit( -1 );
	}
	/*
	 * Look up message types of interest 
	 */
	if ( GetType( "TYPE_HEARTBEAT", &TypeHeartBeat ) != 0 ) 
	{
		fprintf( stderr,
		"orb2eworm: Invalid message type <TYPE_HEARTBEAT>; exiting!\n" );
		exit( -1 );
	}
	if ( GetType( "TYPE_ERROR", &TypeError ) != 0 ) 
	{
		fprintf( stderr,
		"orb2eworm: Invalid message type <TYPE_ERROR>; exiting!\n" );
		exit( -1 );
	}
	if ( GetType( "TYPE_TRACEBUF", &TypeTracebuf ) != 0 ) 
	{
		fprintf( stderr,
		"orb2eworm: Invalid message type <TYPE_TRACEBUF>; exiting!\n" );
		exit( -1 );
	}
	return;
}

/******************************************************************************
 * orb2eworm_status() builds a heartbeat or error message & puts it into       *
 *                   shared memory.  Writes errors to log file & screen.      *
 ******************************************************************************/
void 
orb2eworm_status( unsigned char type, short ierr, char *note )
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
		logit( "et", "orb2eworm: %s\n", note );
	}
	size = strlen( msg );	/* don't include the null byte in the message */

	/*
	 * Write the message to shared memory 
	 */
	if ( tport_putmsg( &Region, &logo, size, msg ) != PUT_OK ) 
	{
		if ( type == TypeHeartBeat ) 
		{
			logit( "et", "orb2eworm:  Error sending heartbeat.\n" );
		} 
		else if ( type == TypeError ) 
		{
			logit( "et",
			 "orb2eworm:  Error sending error:%d.\n", ierr );
		}
	}
	return;
}

/********************************************************************/
/* terminate_if_requested() checks the ring for a TERMINATE request */
/* And shuts down orb2eworm if such a request is found             */
/********************************************************************/
void
terminate_if_requested( void  )
{
        if ( tport_getflag( &Region ) == TERMINATE )
        {
                tport_detach( &Region );
                logit( "t", "orb2eworm: Termination requested; exiting\n" );
                fflush( stdout );
                exit( 0 );
        } 
	else 
	{
                return;
        }
}

/********************************************************************/
/* pktchan_to_tracebuf() Cast an orb PktChannel into an Iceworm     */
/*   tracebuf message						    */
/********************************************************************/
int
pktchan_to_tracebuf( PktChannel *pktchan,
		     TracePacket *tp,
		     double starttime, 
		     int *nbytes )
{
	char	*datap;
	int	dsize_bytes;
	STACHAN *stachan;

	strcpy( tp->trh.datatype, UNSTUFFPKT_DATATYPE );

	if( ( stachan = lookup_stachan( pktchan->sta, pktchan->chan ) ) == NULL )
	{
		logit( "et", "Couldn't find %s %s in station info\n",
			pktchan->sta, pktchan->chan );
		return -1;
	}
	else
	{
		tp->trh.pinno = stachan->pinno;
	}

	strcpy( tp->trh.sta, pktchan->sta );
	strcpy( tp->trh.chan, pktchan->chan );
	strcpy( tp->trh.net, pktchan->net );
	tp->trh.samprate = pktchan->samprate;
	tp->trh.nsamp = pktchan->nsamp;
	tp->trh.starttime = starttime;
	tp->trh.endtime = ENDTIME( starttime,
				   pktchan->samprate,
				   pktchan->nsamp );
	strcpy( tp->trh.quality, "" );
	strcpy( tp->trh.pad, "" );

	datap = &tp->msg[0] + sizeof( TRACE_HEADER );
	dsize_bytes = datasize( tp->trh.datatype ) * tp->trh.nsamp;

	*nbytes = dsize_bytes + sizeof( TRACE_HEADER );

	memcpy( datap, pktchan->data, dsize_bytes );

	return 0;
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
                        logit("t", "orb2eworm: Error sending heartbeat\n");
        }
}
