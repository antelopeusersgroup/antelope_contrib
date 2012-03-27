 /* guralp2orb
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1998 - 2002
 *
 */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <math.h>
#include <thread.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/uio.h>
#include <errno.h>

#ifdef sun
#include <sys/priocntl.h>
#include <sys/rtpriocntl.h>
#include <sys/tspriocntl.h>
#endif

#include "stock.h"
#include "orb.h"
#include "db.h"
#include "coords.h"
#include "pf.h"
#include "Pkt.h"
#include "brttutil.h"
#include "bns.h"

#define ORBGCF_VERSION 1
#define RAWGCF_PACKET_SIZE 1063
#define ORBGCF_HEADER_SIZE 12
#define ORBGCF_PACKET_SIZE ( ORBGCF_HEADER_SIZE + RAWGCF_PACKET_SIZE )
#define BLOCK_SEQUENCE_MAX 65536
#define GCF_MOTOROLA_BYTEORDER 1
#define GCF_INTEL_BYTEORDER 2
#define SECONDS_PER_DAY (24*60*60)
#define IDENTIFY_SERVER 252
#define REQUEST_OLDEST_SEQNO 254
#define PACKET_REQUEST 255
#define PACKET_NOT_AVAILABLE "\xFF\xFF\xFF\xFF"
#define SERVER_ID_STRING "GCFSERV"
#define REINITIATE_INTERVAL_SEC 60
#define PFWATCH_SLEEPTIME_SEC 1
#define DEFAULT_NET "-"
#define DEFAULT_SEGTYPE "-"
#define DEFAULT_CALIB 0
#define DEFAULT_CALPER -1
#define UDPLISTEN_RT_PRIORITY 10
#define PACKET_QUEUE_SIZE 50000
#define RECOVERY_QUEUE_SIZE 1000
#define PRTHROTTLE_TRIGGER 10000
#define PRTHROTTLE_RELEASE 2000

#ifdef WORDS_BIGENDIAN
static int Words_bigendian = 1;
#else
static int Words_bigendian = 0;
#endif

typedef struct g2orbpkt_ {
	char	packet[ORBGCF_PACKET_SIZE];
	int	len;
	int	tcprecovered;
	struct in_addr udpip;
	in_port_t udpport;
	char	udpsource[STRSZ];
	char	sysid[7];
	char	streamid[7];
	char	srcname[ORBSRCNAME_SIZE];
	double	samprate;
	int	byteorder;
	unsigned short blockseq;
	double	time;
} G2orbpkt;

typedef struct udpinitiater_ {
	mutex_t	statuslock;
	cond_t	up;
	char	screamip[STRSZ];
	in_port_t screamport;
	in_port_t udpreceive_port;
	thread_t initiater_thread;
	int 	failed;
} Udpinitiater;

typedef struct udplistener_ {
	mutex_t	socketlock;
	mutex_t	statuslock;
	cond_t	up;
	in_port_t port;
	int	so;
	struct sockaddr_in local;
	int	lenlocal;
	thread_t listener_thread;
	int	failed;
} Udplistener;

typedef struct recoverreq_ {
	char	udpsource[STRSZ];
	struct in_addr udpip;
	in_port_t udpport;
	int	first;
	int	last;
} Recoverreq;

typedef struct tabletrack_ {
	char	filename[FILENAME_MAX];
	int	use;
	struct stat *statbuf;
	double	last_mtime;
} Tabletrack;

typedef struct calibvals_ {
	double	calib;
	double 	calper;
	double 	validuntil;
} Calibvals;

static struct {
	char 	dbname[FILENAME_MAX];
	int	usedbcalib;
	int	usedbsegtype;
	Arr	*calibarr;	
	Arr	*segtypearr;
	Tabletrack *ctrk;	/* calibration table */
	Tabletrack *strk;	/* sensor table */
	Tabletrack *itrk;	/* instrument table */
} Calibinfo;

Arr *ul_arr; 
Arr *ui_arr; 
Pmtfifo *Packets_mtf;
Pmtfifo *Recover_mtf;

int maxpacketqueue = -1;
mutex_t mpq_mutex;

cond_t prthrottle;	/* Packet Recovery Throttle */
mutex_t prthrottle_mutex;

Arr *Lastpacket;
mutex_t lp_mutex; /* Last packet */

Arr *Recovery_failures;
mutex_t rf_mutex; /* Recovery failures */

static Pf *pf;
static char Pffile[STRSZ];
mutex_t pfparams_mutex;

static char *Default_net = 0;
static char *Default_segtype = 0;
static char LogpktLogfile[FILENAME_MAX];
static Morphtbl *srcname_morphmap = 0;
static int Orbfd = 0; 
static int Verbose = 0;
static int VeryVerbose = 0;
static int Reject_future_packets = 0;
static double Reject_future_packets_sec = 0;
static int Reject_past_packets = 0;
static double Reject_past_packets_sec = 0;
static int nrecovery_threads = 1;
static int max_recovery_failures = 0;
static int Buffer_tail_padding = 20;

/* Added by glp */
static Arr *Recovery_list;
static int Max_Packets_to_Recover;


static void
usage()
{
	elog_die( 1, "Usage: guralp2orb [-v] [-V] [-p pffile] [-d calibdb] [-l file_for_logpackets] orbname\n" );
	
}

static double 
datecode2epoch( int datecode )
{
	double 	epoch;
	int	days_since_Nov_17_1989;	
	int	seconds_since_midnight;

	epoch = 627264000.0; /* Seconds from Midnight 1/1/70 to Nov. 17, 1989 */

	days_since_Nov_17_1989 = datecode >> 17;
	seconds_since_midnight = datecode & 0x0001FFFF;

	epoch += days_since_Nov_17_1989 * SECONDS_PER_DAY;
	epoch += seconds_since_midnight;

	return epoch;
}

static char *
decodebase36( unsigned int v, char *s, int space ) {
	int 	imed;
	int	i;
	char	*c;
	int	len;

	memset( s, 0, space+1 );

	for ( i=space-1; i>=0; i-- ) {
		imed = v % 36;
		if ( imed <= 9 ) {
			s[i] = imed + '0';
		} else {
			s[i] = imed - 10 + 'A';
		}
		v /= 36;
	}
	
	c = &s[0];
	while( *c == '0' ) c++;
	len = strlen( s );
	strncpy( s, c, len );

	return( s );
}

static int 
previous_in_sequence( int current )
{
	return ( current + BLOCK_SEQUENCE_MAX - 1 ) % BLOCK_SEQUENCE_MAX;
}

static int 
next_in_sequence( int current )
{

	return ( current + 1 ) % BLOCK_SEQUENCE_MAX;
}

static void
retry_recovery( Recoverreq *rr )
{
	if( Verbose ) {
		fprintf( stderr, 
		"guralp2orb: still missing %d to %d from %s; retry recovery\n",
			rr->first, 
			rr->last,
			rr->udpsource);
	}

	pmtfifo_push( Recover_mtf, (void *) rr );
}

static void 
register_packet( G2orbpkt *gpkt )
{
	int	*blockseq;
	Recoverreq *rr;
	int number_to_recover;

	mutex_lock( &lp_mutex );

	blockseq = (int *) getarr( Lastpacket, gpkt->udpsource );

	if( blockseq == NULL ) {

		allot( int *, blockseq, 1 );
		setarr( Lastpacket, gpkt->udpsource, (void *) blockseq );

		*blockseq = gpkt->blockseq;

	} else {

		blockseq = getarr( Lastpacket, gpkt->udpsource );

		if( ( gpkt->blockseq != next_in_sequence( *blockseq ) ) &&
		    ( gpkt->blockseq != *blockseq ) ) {
		    if(Verbose)fprintf(stderr,
			"Recovery request for udp source ->%s<-\n",
			gpkt->udpsource);
		    if(getarr(Recovery_list,gpkt->udpsource)!=NULL)
		    {
			if(VeryVerbose)fprintf(stderr,
			  "Attempting recovery of data from %s\n",
				gpkt->udpsource);

			allot( Recoverreq *, rr, 1 );

			strcpy( rr->udpsource, gpkt->udpsource );
			rr->udpip = gpkt->udpip;
			rr->udpport = gpkt->udpport;
			rr->first = next_in_sequence( *blockseq );
			rr->last = previous_in_sequence( gpkt->blockseq );

			if( Verbose ) {
				fprintf( stderr, 
				"guralp2orb: missed %d to %d from %s\n",
					rr->first, 
					rr->last,
					rr->udpsource);
			}

			number_to_recover = (rr->last)-(rr->first)+1;
			if((number_to_recover>0) 
				&& (number_to_recover<Max_Packets_to_Recover)
				&& (nrecovery_threads > 0 ) ) {
				pmtfifo_push( Recover_mtf, (void *) rr );
			} else {
				if(number_to_recover>0)
					elog_notify(0,"Absurd recovery request for ip %s dropped\nRequested recovery of %d packets\n",
						rr->udpsource,
						number_to_recover);
				free( rr );
			}
		    }
		    else
		    {
			if(Verbose)
			  fprintf(stderr,"guralp2orb:  ignored recovery request for %s for packets %d to %d\n",
				gpkt->udpsource,next_in_sequence( *blockseq ),
				previous_in_sequence( gpkt->blockseq ));
		    }
		}

		*blockseq = gpkt->blockseq;
	}

	mutex_unlock( &lp_mutex );
}

static int
pmtfifo_getqueue( Pmtfifo *mtf )
{
	int	queue = 0;

	if( ! mtf ) return queue;

	mutex_lock( &(mtf->mutex) );
	queue = mtf->queue;
	mutex_unlock( &(mtf->mutex) );

	return queue;
}

static void
report_queuemax( void )
{
	FILE	*fp;
	int	queue;

	queue = pmtfifo_getqueue( Packets_mtf );

	mutex_lock( &mpq_mutex );

	if( queue > maxpacketqueue ) {

		maxpacketqueue = queue;

		if( Verbose ) {
			fprintf( stderr,
			    "MAXQUEUE up to %d\n", maxpacketqueue );

			fp = fopen( "guralp2orb_QUEUEMAX", "w" );
			fprintf( fp, "MAXQUEUE up to %d\n", maxpacketqueue );
			fclose( fp );
		}
	}

	if( VeryVerbose ) {

		fp = fopen( "guralp2orb_QUEUECURRENT", "w" );
		fprintf( fp, "CURRENTQUEUE is %d\n", queue );
		fclose( fp );
	}

	mutex_unlock( &mpq_mutex );
}

static void 
construct_srcname( char *srcname, char *sysid, char *streamid, int samprate ) 
{
	Srcname parts;
	char	starting_srcname[ORBSRCNAME_SIZE];
	int	n;

	memset( &parts, 0, sizeof( parts ) );

	mutex_lock( &pfparams_mutex );
	strcpy( parts.src_net, Default_net );
	mutex_unlock( &pfparams_mutex );

	strcpy( parts.src_sta, sysid );
	strcpy( parts.src_chan, streamid );

	if( samprate != 0 ) {
		strcpy( parts.src_suffix, "GCF" );
	} else {
		strcpy( parts.src_suffix, "GCFS" );
	}

	join_srcname( &parts, starting_srcname );

	mutex_lock( &pfparams_mutex );

	n = morphtbl( starting_srcname, srcname_morphmap, 
		  MORPH_ALL|MORPH_PARTIAL, srcname );

	mutex_unlock( &pfparams_mutex );
}

static struct stat *
table_check( char *table, Tabletrack **ttrk )
{
	Dbptr	db;
	char	*filename;
	int	nrecs;
	int 	ret;

	if( *ttrk == (Tabletrack *) NULL ) {

		allot( Tabletrack *, *ttrk, 1 );
		allot( struct stat *, (*ttrk)->statbuf, 1 );
		(*ttrk)->use = 1;

		ret = dbopen( Calibinfo.dbname, "r", &db );

		if( ret < 0 || db.database < 0 ) {

			elog_complain( 1, "Failed to open %s\n",
				 Calibinfo.dbname );
			
			(*ttrk)->use = 0;

			free( (*ttrk)->statbuf );
			(*ttrk)->statbuf = 0;

			return (*ttrk)->statbuf;
		}

		db = dblookup( db, 0, table, 0, 0 );

		if( db.table < 0 ) {

			dbclose( db );

			elog_complain( 1, "Failed to lookup %s.%s\n", 
		  	     	Calibinfo.dbname,
		  	     	table );

			(*ttrk)->use = 0;

			free( (*ttrk)->statbuf );
			(*ttrk)->statbuf = 0;

			return (*ttrk)->statbuf;
		} 

		dbquery( db, dbTABLE_FILENAME, (Dbvalue *) &filename );
		abspath( filename, (*ttrk)->filename );

		ret = stat( (*ttrk)->filename, (*ttrk)->statbuf );

		if( ret < 0 && errno == ENOENT ) {

			dbclose( db );

			elog_complain( 1, "%s does not exist\n", (*ttrk)->filename );

			(*ttrk)->use = 0;
			free( (*ttrk)->statbuf );
			(*ttrk)->statbuf = 0;

			return (*ttrk)->statbuf;

		} else if( ret < 0 ) {

			dbclose( db );

			elog_complain( 1, "Failed to stat %s\n", (*ttrk)->filename );

			(*ttrk)->use = 0;
			free( (*ttrk)->statbuf );
			(*ttrk)->statbuf = 0;

			return (*ttrk)->statbuf;
		} 

		dbquery( db, dbRECORD_COUNT, &nrecs );
		
		if( nrecs <= 0 ) {
			
			dbclose( db );

			elog_complain( 1, "No records in %s\n", (*ttrk)->filename );

			(*ttrk)->use = 0;
			free( (*ttrk)->statbuf );
			(*ttrk)->statbuf = 0;

			return (*ttrk)->statbuf;

		} else {

			dbclose( db );
			
			(*ttrk)->last_mtime = (double) (*ttrk)->statbuf->st_mtime;

			return (*ttrk)->statbuf;
		}

	} else if( (*ttrk)->use == 0 ) {

		return (struct stat *) NULL;

	} else {

		ret = stat( (*ttrk)->filename, (*ttrk)->statbuf );

		if( ret < 0 ) {
			elog_complain( 1, "Failed to stat %s\n", (*ttrk)->filename );

			(*ttrk)->use = 0;
			free( (*ttrk)->statbuf );
			(*ttrk)->statbuf = 0;

		} else {

			return (*ttrk)->statbuf;
		}
	}
}

static int
update_is_necessary( char *table, Tabletrack *ttrk )
{
	struct stat *statbuf;

	statbuf = table_check( table, &ttrk );

	if( (double) statbuf->st_mtime > ttrk->last_mtime ) {

		ttrk->last_mtime = (double) statbuf->st_mtime;

		return 1;

	} else {

		return 0;
	}
}

static char * 
add_segtype( char *sta, char *chan, double time, Dbptr *pdb )
{
	Dbptr	db, dbs, dbi;
	char	*segtype = 0;
	char	key[STRSZ];
	char	expr[STRSZ];
	int	nrecs = 0;
	int	ret;
	
	if( pdb == (Dbptr *) NULL ) {
		dbopen( Calibinfo.dbname, "r", &db );
	} else {
		db = *pdb;
	}

	dbs = dblookup( db, 0, "sensor", 0, 0 );
	dbi = dblookup( db, 0, "instrument", 0, 0 );
	db = dbjoin( dbs, dbi, 0, 0, 0, 0, 0 );

	sprintf( key, "%s:%s", sta, chan );
	
	if( ( segtype = getarr( Calibinfo.segtypearr, key ) ) == (char *) NULL ) {

		allot( char *, segtype, 3 );
		setarr( Calibinfo.segtypearr, key, segtype );
	}

	sprintf( expr,
		 "sta == \"%s\" && chan == \"%s\" && time <= %f && (endtime == NULL || endtime >= %f)",
		 sta, chan, time, time );
	db = dbsubset( db, expr, 0 );

	dbquery( db, dbRECORD_COUNT, &nrecs );

	if( nrecs > 0 ) {
		db.record = 0;
		ret = dbgetv( db, 0, "rsptype", segtype, 0 );
	}

	if( ret < 0 || nrecs <= 0 ) {
		elog_complain( 1, "Failed to get segtype from database for %s\n", key );
		mutex_lock( &pfparams_mutex );
		strcpy( segtype, Default_segtype );
		mutex_unlock( &pfparams_mutex );
	}

	if( ! strcmp( segtype, "-" ) ) {

		mutex_lock( &pfparams_mutex );
		strcpy( segtype, Default_segtype );
		mutex_unlock( &pfparams_mutex );

		if( VeryVerbose ) {
			fprintf( stderr, 
			  "Database has null segtype for %s; using default\n",
			  key );
		}
	}

	if( VeryVerbose ) {
		fprintf( stderr, "Using segtype %s for %s\n",
			 segtype, key );
	}

	if( pdb == (Dbptr *) NULL ) {
		dbclose( db );
	}

	return segtype;
}

static void
update_segtypevals( double time )
{
	Dbptr	db;
	Srcname	parts;
	Tbl	*keys;
	char	*key;
	char	*s;	
	Tbl	*ssplit;
	int 	i;

	if( VeryVerbose ) {
		fprintf( stderr, "Database sensor/instrument table changed; rereading segtype\n" );
	}

	dbopen( Calibinfo.dbname, "r", &db );

	keys = keysarr( Calibinfo.segtypearr );
	if( keys == (Tbl *) NULL ) {
		dbclose( db );
		return;
	}

	for( i=0; i<maxtbl( keys ); i++ ) {

		key = gettbl( keys, i );
		
		s = strdup( key );
		ssplit = split( s, ':' );

		add_segtype( gettbl( ssplit, 0 ), 
			     gettbl( ssplit, 1 ),
			     time, &db );

		free( s );
		freetbl( ssplit, 0 );
	}

	dbclose( db );
}

static Calibvals * 
add_current_calibvals( char *sta, char *chan, double time, Dbptr *pdb )
{
	Dbptr	db;
	Calibvals *cv;
	char	key[STRSZ];
	char	expr[STRSZ];
	int	nrecs = 0;
	int	ret;
	
	if( pdb == (Dbptr *) NULL ) {
		dbopen( Calibinfo.dbname, "r", &db );
	} else {
		db = *pdb;
	}

	db = dblookup( db, 0, "calibration", 0, 0 );

	sprintf( key, "%s:%s", sta, chan );
	
	if( ( cv = getarr( Calibinfo.calibarr, key ) ) == (Calibvals *) NULL ) {

		allot( Calibvals *, cv, 1 );
		setarr( Calibinfo.calibarr, key, cv );
	}

	sprintf( expr,
		 "sta == \"%s\" && chan == \"%s\" && time <= %f && (endtime == NULL || endtime >= %f)",
		 sta, chan, time, time );
	db = dbsubset( db, expr, 0 );

	dbquery( db, dbRECORD_COUNT, &nrecs );

	if( nrecs > 0 ) {
		db.record = 0;
		ret = dbgetv( db, 0, "calib", &(cv->calib), 
			       	     "calper", &(cv->calper),
			             "endtime", &(cv->validuntil), 0 );
	}

	if( ret < 0 || nrecs <= 0 ) {
		elog_complain( 1, "Failed to get calib and calper from database for %s\n", key );
		cv->calib = DEFAULT_CALIB;
		cv->calper = DEFAULT_CALPER;
		cv->validuntil = 9999999999.999;
	}

	if( VeryVerbose ) {
		fprintf( stderr, 
			 "Using calib %f, calper %f for %s\n",
			 cv->calib, cv->calper, key );
	}

	if( pdb == (Dbptr *) NULL ) {
		dbclose( db );
	}

	return cv;
}

static void
update_calibvals( double time )
{
	Dbptr	db;
	Srcname	parts;
	Tbl	*keys;
	char	*key;
	char	*s;	
	Tbl	*ssplit;
	int 	i;

	if( VeryVerbose ) {
		fprintf( stderr, 
		   "Database calibration table changed; rereading calib and calper\n" );
	}

	dbopen( Calibinfo.dbname, "r", &db );

	keys = keysarr( Calibinfo.calibarr );
	if( keys == (Tbl *) NULL ) {
		dbclose( db );
		return;
	}

	for( i=0; i<maxtbl( keys ); i++ ) {

		key = gettbl( keys, i );
		
		s = strdup( key );
		ssplit = split( s, ':' );

		add_current_calibvals( gettbl( ssplit, 0 ), 
				       gettbl( ssplit, 1 ),
				       time, &db );

		free( s );
		freetbl( ssplit, 0 );
	}

	dbclose( db );
}

static void
get_calibinfo( Srcname *parts, double time, char **segtype, double *calib, double *calper )
{
	char	key[STRSZ];
	Calibvals *cv;

	if( Calibinfo.usedbsegtype == 0 ) {

		mutex_lock( &pfparams_mutex );
		if( *segtype == NULL ) {
			allot( char *, *segtype, 3 );
		}
		strncpy( *segtype, Default_segtype, 2 );
		(*segtype)[1] = '\0';
		mutex_unlock( &pfparams_mutex );

	} else {

		if( update_is_necessary( "sensor", Calibinfo.strk ) ||
		    update_is_necessary( "instrument", Calibinfo.itrk ) ) {

			/* Use the current packet time to find valid rows: */
			update_segtypevals( time );
		}	

		sprintf( key, "%s:%s", parts->src_sta, parts->src_chan );
	
		*segtype = (char *) getarr( Calibinfo.segtypearr, key );

		if( *segtype == (char *) NULL ) {

			*segtype = add_segtype( parts->src_sta, parts->src_chan, time, 0 );
		}
	}

	if( Calibinfo.usedbcalib == 0 ) {

		*calib = DEFAULT_CALIB;
		*calper = DEFAULT_CALPER;

	} else {

		if( update_is_necessary( "calibration", Calibinfo.ctrk ) ) {
			
			update_calibvals( time );
		}

		sprintf( key, "%s:%s", parts->src_sta, parts->src_chan );

		cv = (Calibvals *) getarr( Calibinfo.calibarr, key );

		if( cv == (Calibvals *) NULL || 
		    ( cv->validuntil != 9999999999.999 && time > cv->validuntil ) ) {

			cv = add_current_calibvals( parts->src_sta, parts->src_chan, time, 0 );
		}

		*calib = cv->calib;
		*calper = cv->calper;
	}
	
	return;
}

static void  
insert_orbgcf_hdr( G2orbpkt *gpkt )
{
	Srcname parts;
	short	version = ORBGCF_VERSION;
	char	*segtype = 0;
	double	calib;
	double 	calper;

	split_srcname( gpkt->srcname, &parts );

	if( ! strcmp( parts.src_suffix, "GCFS" ) ) {

		memmove( &gpkt->packet[2], gpkt->packet, gpkt->len );
		version = htons( version );
		memcpy( gpkt->packet, &version, 2 );
		gpkt->len += 2;

	} else {

		memmove( &gpkt->packet[12], gpkt->packet, gpkt->len );
		version = htons( version );
		memcpy( gpkt->packet, &version, 2 );
		
		get_calibinfo( &parts, gpkt->time, &segtype, &calib, &calper );
		
		memcpy( &gpkt->packet[2], segtype, 1 );
		gpkt->packet[3] = 0;

		HD2NF( &gpkt->packet[4], &calib, 1 );
		HD2NF( &gpkt->packet[8], &calper, 1 );

		gpkt->len += 12;
	}

	return;
}

static void 
gcfpeek( G2orbpkt *gpkt )
{
	unsigned int sysid;
	unsigned int streamid;
	unsigned int datecode;

	memcpy( &sysid, &(gpkt->packet[0]), sizeof( unsigned int ) );
	memcpy( &streamid, &(gpkt->packet[4]), sizeof( unsigned int ) );
	memcpy( &datecode, &(gpkt->packet[8]), sizeof( unsigned int ) );
	memcpy( &(gpkt->blockseq), 
		&(gpkt->packet[1058]), 
		sizeof( unsigned short ) );

	gpkt->samprate = (unsigned char) gpkt->packet[13];
	gpkt->byteorder = (unsigned char) gpkt->packet[1060];

	if( ( gpkt->byteorder == GCF_MOTOROLA_BYTEORDER && ! Words_bigendian ) || 
	    ( gpkt->byteorder == GCF_INTEL_BYTEORDER && Words_bigendian ) ) {
		
		swap2( &(gpkt->blockseq), &(gpkt->blockseq), 1 );
		swap4( &sysid, &sysid, 1 );	
		swap4( &streamid, &streamid, 1 );	
		swap4( &datecode, &datecode, 1 );	
	}

	decodebase36( sysid, gpkt->sysid, 6 );
	decodebase36( streamid, gpkt->streamid, 6 );
	gpkt->time = datecode2epoch( datecode );

	construct_srcname( gpkt->srcname, 
			   gpkt->sysid,
			   gpkt->streamid,
			   gpkt->samprate );
}

static int 
healthy_packet( G2orbpkt *gpkt )
{
	Srcname	parts;
	struct timespec tp;
	double 	tdelta;
	char	*s;

	split_srcname( gpkt->srcname, &parts );

	if( ! strcmp( parts.src_suffix, "GCFS" ) ) {
		return 1; /* Let all status packets through */
	}

	if( ( gpkt->byteorder != GCF_MOTOROLA_BYTEORDER ) &&
	    ( gpkt->byteorder != GCF_INTEL_BYTEORDER ) ) {

		if( Verbose ) {
			fprintf( stderr, 
			"guralp2orb: Rejecting # %d (%s) from %s: bad byte order code %d\n",
			gpkt->blockseq,
			gpkt->srcname,
			gpkt->udpsource, 
			gpkt->byteorder );
		}

		return 0;
	}

#ifdef __APPLE__
	tdelta = gpkt->time - now() ;
#else
	clock_gettime( CLOCK_REALTIME, &tp );
	tdelta = gpkt->time - tp.tv_sec+tp.tv_nsec/1e9;
#endif

	if( Reject_future_packets &&
	    ( tdelta > Reject_future_packets_sec ) ) {

		if( Verbose ) {
			s = strtdelta( tdelta );
			strtrim( s );
			fprintf( stderr, 
			"guralp2orb: Rejecting # %d (%s) from %s: starts %s in the future\n",
			gpkt->blockseq,
			gpkt->srcname,
			gpkt->udpsource, 
			s );
			free( s );
		}

		return 0;
	}

	if( Reject_past_packets &&
	    ( tdelta < -1 * Reject_past_packets_sec ) ) {

		if( Verbose ) {
			s = strtdelta( tdelta );
			strtrim( s );
			fprintf( stderr, 
			"guralp2orb: Rejecting # %d (%s) from %s: starts %s in the past\n",
			gpkt->blockseq,
			gpkt->srcname,
			gpkt->udpsource, 
			s );
			free( s );
		}

		return 0;
	}
}

static G2orbpkt * 
recover_packet( Bns *bns, unsigned short requested )
{
	G2orbpkt *gpkt;
	char 	msg;
	int 	rc = 0;

	msg = PACKET_REQUEST;
	bnsput( bns, &msg, BYTES, 1 );
	bnsput( bns, &requested, TWO_BYTES, 1 );
	bnsflush( bns );

	allot( G2orbpkt *, gpkt, 1 );
	
	/* Fill these in for completeness */
	gpkt->len = RAWGCF_PACKET_SIZE;
	gpkt->tcprecovered = 1;

	rc = bnsget( bns, 
		     &(gpkt->packet), 
		     BYTES, 
		     4 );

	if( rc < 0 ) {

		free( gpkt );
		gpkt = (G2orbpkt *) NULL;
		return gpkt; 

	} else if( ! strncmp( gpkt->packet, PACKET_NOT_AVAILABLE, 4 ) ) {

		free( gpkt );
		gpkt = (G2orbpkt *) NULL;
		return (G2orbpkt *) 1; /* inelegant but effective */
	}

	rc = bnsget( bns, 
		     &(gpkt->packet[4]), 
		     BYTES, 
		     gpkt->len - 4 ); 

	if( rc < 0 ) {

		free( gpkt );
		gpkt = (G2orbpkt *) NULL;
		return gpkt; 

	} else {

		return gpkt; 
	}
}

static void
recovery_failed( char *udpsource )
{
	int	*recovery_failures; 

	if( max_recovery_failures <= 0 ) {
		return;
	}

	mutex_lock( &rf_mutex );

	recovery_failures = (int *) getarr( Recovery_failures, udpsource );

	if( recovery_failures == NULL ) {

		allot( int *, recovery_failures, 1 );
		*recovery_failures = 0;
		setarr( Recovery_failures, udpsource, recovery_failures );
	}
	
	*recovery_failures++;

	if( *recovery_failures > max_recovery_failures ) {
		fprintf( stderr,
		 "guralp2orb: recovery for %s failed %d times; disabling.\n",
		udpsource, *recovery_failures );
	}

	mutex_unlock( &rf_mutex );
}

static void 
recovery_succeeded( char *udpsource ) 
{
	int	*recovery_failures;

	if( max_recovery_failures <= 0 ) {
		return;
	}

	mutex_lock( &rf_mutex );

	recovery_failures = (int *) getarr( Recovery_failures, udpsource );
	
	if( recovery_failures == NULL ) {

		allot( int *, recovery_failures, 1 );
		*recovery_failures = 0;
		setarr( Recovery_failures, udpsource, recovery_failures );
	}
	
	if( *recovery_failures > 0 && Verbose ) {
		fprintf( stderr, 
		"guralp2orb: TCP recovery for %s succeeded, resetting failure count\n" ,udpsource);
	}

	*recovery_failures = 0;

	mutex_unlock( &rf_mutex );
}

static int
recovery_ok( char *udpsource ) 
{
	int	*recovery_failures; 
	int	retcode = 1;

	if( max_recovery_failures <= 0 ) {
		return 1;
	}

	mutex_lock( &rf_mutex );

	recovery_failures = (int *) getarr( Recovery_failures, udpsource );

	if( recovery_failures == NULL ) {

		allot( int *, recovery_failures, 1 );
		*recovery_failures = 0;
		retcode = 1;

	} else if( *recovery_failures > max_recovery_failures ) {

		retcode = 0;

	} else {

		retcode = 1;
	}

	mutex_unlock( &rf_mutex );

	return retcode;
}

static void 
trim_recovery_request( Recoverreq *rr, unsigned short oldest )
{
	int	first_available;
	int	oldest_reasonable;

	/* Assume the last missed packet is still available 
	   (the reason we know it was missed is that 
	   its immediate successor just came in) */

	first_available = rr->last;

	/* Avoid just-missed-it thrashing at the end of the buffer */

	oldest_reasonable = ( oldest + Buffer_tail_padding ) % BLOCK_SEQUENCE_MAX;

	while( first_available != rr->first &&
	       first_available != oldest_reasonable ) {

		first_available = previous_in_sequence( first_available );
	}

	if( ( first_available != rr->first ) && VeryVerbose ) {
		
		elog_complain( 1, 
		"TCP recovery: packets %d to %d no longer "
		"available from %s\n",
		rr->first,  previous_in_sequence( first_available ),
		rr->udpsource );
	}

	rr->first = first_available;

	return;
}

static void 
recover_packetsequence( Recoverreq *rr )
{
	G2orbpkt *gpkt;
	struct sockaddr_in sin;
	int 	so;
	Bns 	*bns;
	char 	msg;
	char	response[RAWGCF_PACKET_SIZE];
	unsigned short requested;
	unsigned short oldest;
	int	next;
	int	lastflag;

/*fprintf(stderr,"DEBUG:  attempting recover on %s\n",rr->udpsource);*/
	if( ! recovery_ok( rr->udpsource ) ) {
		free( rr );
		return;
	}

	so = socket( PF_INET, SOCK_STREAM, 0 );
	if( so < 0 ) {
		elog_complain( 1, 
		"Can't open tcp socket to %s for packet recovery\n", 
		rr->udpsource );
		recovery_failed( rr->udpsource );
		free( rr );
		return;
	}

	sin.sin_family = AF_INET;
	sin.sin_port = htons( 0 );  /* Any port */
	sin.sin_addr.s_addr = htonl( INADDR_ANY );
	
	if( bind( so, (struct sockaddr *) &sin, sizeof( sin ) ) ) {
		elog_complain( 1,
		"Couldn't bind packet recovery socket\n" );
		close( so );
		recovery_failed( rr->udpsource );
		free( rr );
		return;
	}
		
	sin.sin_port = htons( rr->udpport );
	sin.sin_addr = rr->udpip;
	
	if( connect( so, (struct sockaddr *) &sin, sizeof( sin ) ) ) {
		elog_complain( 1,
		"Couldn't connect packet recovery socket for %s\n", 
		rr->udpsource );
		close( so );
		recovery_failed( rr->udpsource );
		free( rr );
		return;
	}

	bns = bnsnew( so, 100 * RAWGCF_PACKET_SIZE );
	bnsuse_sockio( bns );

	msg = IDENTIFY_SERVER;
	bnsput( bns, &msg, BYTES, 1 );
	bnsflush( bns );

	bnsget( bns, &response[0], BYTES, 16 );

	if( strncmp( response, SERVER_ID_STRING, 
		sizeof( SERVER_ID_STRING ) - 1 ) ) {
		elog_complain( 1, 
		  "%s not a GCF server; TCP packet recovery failed. Server response was %s\n", 
		  rr->udpsource, response );
		bnsclose( bns );
		recovery_failed( rr->udpsource );
		free( rr );
		return;
	} else if( VeryVerbose ) {
		fprintf( stderr, 
			"Server id for %s is %s\n",
			rr->udpsource, response );
	}

	msg = REQUEST_OLDEST_SEQNO;
	bnsput( bns, &msg, BYTES, 1 );
	bnsflush( bns );

	bnsget( bns, &oldest, BYTES, 2 );
	oldest = ntohs( oldest );

	if( VeryVerbose ) {
		fprintf( stderr, "Oldest packet on %s is %d\n",
			rr->udpsource, oldest );
	}

	trim_recovery_request( rr, oldest );

	lastflag = 0;
	next = rr->first;

	while( ! lastflag ) {

		requested = htons( next );

		gpkt = recover_packet( bns, requested );
		
		if( gpkt == (G2orbpkt *) 1 ) {

			if( Verbose ) {
				elog_complain( 1, 
				"TCP recovery: packet %d no longer "
				"available from %s\n",
				next, 
				rr->udpsource );
			}

		} else if( gpkt == (G2orbpkt *) NULL ) {

			if( Verbose ) {
				elog_complain( 1, 
				"failed to get packet %d from %s via "
				"TCP, errno %d. Will retry block.\n", 
				next, 
				rr->udpsource, 
				bnserrno( bns ) );
			}
			bnsclose( bns );
			recovery_failed( rr->udpsource );
			rr->first = next;
			retry_recovery( rr );
			return;

		} else {

			gpkt->udpip = rr->udpip;
			gpkt->udpport = rr->udpport;
			strcpy( gpkt->udpsource, rr->udpsource );

			gcfpeek( gpkt );

			report_queuemax();

/*fprintf(stderr,"guralp2orb(DEBUG):  recovery ok, entering possible wait loop\n");*/
			mutex_lock( &prthrottle_mutex );

			while( pmtfifo_getqueue( Packets_mtf ) > PRTHROTTLE_TRIGGER )
				cond_wait( &prthrottle, &prthrottle_mutex );
			mutex_unlock( &prthrottle_mutex );
/*fprintf(stderr,"guralp2orb(DEBUG):  Ok, got past wait loop\n");*/

			pmtfifo_push( Packets_mtf, (void *) gpkt );
/*fprintf(stderr,"guralp2org(DEBUG):  Successful recovery from %s\n",rr->udpip);*/

			recovery_succeeded( rr->udpsource );
		}

		if( next == rr->last ) lastflag++;
		next = next_in_sequence( next );
	} 

	free( rr );
	bnsclose( bns );
	close( so );

	return;
}

static void * 
guralp2orb_packetrecover( void *arg )
{
	Recoverreq *rr;

	while( pmtfifo_pop( Recover_mtf, (void **) &rr ) != 0 ) { 

		recover_packetsequence( rr );
	}

	return( NULL );
}

static void * 
guralp2orb_packettrans( void *arg )
{
	G2orbpkt *gpkt;
	Srcname parts;
	Packet	*pkt = 0;
	int	rc = 0;
	char	*s;
	char 	*contents = "Null string";
	FILE	*fp;
	char	cmd[STRSZ];

	while( pmtfifo_pop( Packets_mtf, (void **) &gpkt ) != 0 ) { 

		split_srcname( gpkt->srcname, &parts );

		if( ! strcmp( parts.src_suffix, "GCFS" ) ) {

			rc = unstuffPkt( gpkt->srcname, 
					 gpkt->time, 
					 gpkt->packet,
				    	 gpkt->len,
					 &pkt );
			if( rc == -1 ) {
				elog_complain( 1, "Unrecognized packet type GCFS\n" );
				continue;
			} else if( rc <= 0 ) {
				elog_complain( 1, 
					  "Error %d unstuffing GCFS packet\n",
					  rc );
				continue;
			}

			if( pkt->string != NULL ) {
				contents = pkt->string;
			}

			if( strcmp( LogpktLogfile, "" ) ) {
				fp = fopen( LogpktLogfile, "a" );
				if( fp ) {
					fprintf( fp, 
			"\nStatus packet from %s:%s at %s:\n\n%s\n\n",
					parts.src_sta, parts.src_chan,
					s = strtime( gpkt->time ),
					contents );
					free( s );
					fclose( fp );
				} else {
					elog_complain( 1,
					"Couldn't open %s for %s packet\n",
						LogpktLogfile,
						gpkt->srcname );
				}
			}

			if( Verbose ) {
				printf( "\nStatus packet from %s:%s at %s:\n\n%s\n\n",
					parts.src_sta, parts.src_chan, 
					s = strtime( gpkt->time ),
					contents );

				free( s );
			}
		}

		if( VeryVerbose ) {
			char method[STRSZ];

			if( gpkt->tcprecovered ) {
				strcpy( method, "TCP Recovery" );
			} else {
				strcpy( method, "UDP" );
			}

			fprintf( stderr,
			"guralp2orb: received # %d from %s (%s:%s => %s) via %s\n", 
				gpkt->blockseq,
				gpkt->udpsource, 
				gpkt->sysid,
				gpkt->streamid,
				gpkt->srcname,
				method );
		}

		insert_orbgcf_hdr( gpkt );

		rc = orbput( Orbfd, 
			     gpkt->srcname, 
			     gpkt->time,
			     gpkt->packet, 
			     gpkt->len );

		if( rc != 0 ) elog_clear_register( 1 );

		free( gpkt );

		mutex_lock( &prthrottle_mutex );
		if( pmtfifo_getqueue( Packets_mtf ) <= PRTHROTTLE_RELEASE ) {
			cond_signal( &prthrottle );
		}
		mutex_unlock( &prthrottle_mutex );
	}

	return( NULL );
}

#ifdef sun

void set_udplisten_priority()
{
	uid_t	starting_uid;
	pcinfo_t pcinfo;
	pcparms_t pcparams;
	rtparms_t *rtparams;
	tsparms_t *tsparams;
	pri_t	oldpri;
	int	rc;
	char 	*s;

	starting_uid = getuid();

	rc = setuid( 0 );

	/* Try RT priority first */
	if( ! rc ) {
	
		strcpy( pcinfo.pc_clname, "RT" );
		rc = priocntl( 0, 0, PC_GETCID, (caddr_t) &pcinfo );

		pcparams.pc_cid = pcinfo.pc_cid;
		rtparams = (rtparms_t *) &pcparams.pc_clparms;
		rtparams->rt_pri = UDPLISTEN_RT_PRIORITY;
		rtparams->rt_tqnsecs = RT_TQDEF;

		rc = priocntl( P_LWPID, P_MYID, PC_SETPARMS, (caddr_t) &pcparams );

		if( ! rc ) {
			if( Verbose ) {
				fprintf( stderr,
		 		"Priority of udplisten thread set to Real-time %d\n",
		 		UDPLISTEN_RT_PRIORITY );
			}
		}

		setuid( starting_uid );

		if( ! rc ) {

			return;
		}
	}

	/* Try TS priority increase */

	strcpy( pcinfo.pc_clname, "TS" );
	rc = priocntl( 0, 0, PC_GETCID, (caddr_t) &pcinfo );

	pcparams.pc_cid = pcinfo.pc_cid;
	tsparams = (tsparms_t *) &pcparams.pc_clparms;

	rc = priocntl( P_LWPID, P_MYID, PC_GETPARMS, (caddr_t) &pcparams );

	oldpri = tsparams->ts_upri;

	tsparams->ts_upri = tsparams->ts_uprilim;

	rc = priocntl( P_LWPID, P_MYID, PC_SETPARMS, (caddr_t) &pcparams );

	if( ! rc && Verbose ) {
		fprintf( stderr, 
			"Priority of udplisten thread changed from Time-Share %d to %d\n",
			(int) oldpri, (int) tsparams->ts_uprilim );
	}

	return;
}

#endif

static void * 
guralp2orb_udplisten( void *arg )
{
	Udplistener *ul = (Udplistener *) arg;
	char	*inaddr = "0.0.0.0";
	struct sockaddr_in foreign;
	int	lenforeign = sizeof( foreign );
	G2orbpkt *gpkt;

	if( Verbose ) {
		fprintf( stderr, 
			"guralp2orb: udplisten: opening connection on port %d\n",
			 ul->port );
	}

	ul->so = socket( AF_INET, SOCK_DGRAM, 0 );
	if( ul->so < 0 ) {
		elog_complain( 1, "Can't open socket for port %d\n", ul->port );
		ul->failed = 1;
		mutex_lock( &(ul->statuslock) );
		cond_signal( &(ul->up) );
		mutex_unlock( &(ul->statuslock) );
		return( NULL );
	}

	setsockopt( ul->so, SOL_SOCKET, SO_REUSEADDR, (char *) 0, 0 );

	ul->lenlocal = sizeof( ul->local );
	memset( (char *) &(ul->local), '\0', ul->lenlocal );

	ul->local.sin_family = AF_INET;
	ul->local.sin_port = htons( (unsigned short) ul->port );
	ul->local.sin_addr.s_addr = inet_addr( inaddr );

	if( bind( ul->so, (struct sockaddr *) &(ul->local), ul->lenlocal ) < 0 ) {
		elog_complain( 1, "Can't bind address to socket\n" );
		ul->failed = 1;
		mutex_lock( &(ul->statuslock) );
		cond_signal( &(ul->up) );
		mutex_unlock( &(ul->statuslock) );
		return( NULL );
	}

	if( getsockname( ul->so, (struct sockaddr *) &(ul->local),
			 &ul->lenlocal ) < 0 ) {
		elog_complain( 1, "Error getting socket name\n" );
		ul->failed = 1;
		mutex_lock( &(ul->statuslock) );
		cond_signal( &(ul->up) );
		mutex_unlock( &(ul->statuslock) );
		return( NULL );
	}

	#ifdef sun
	set_udplisten_priority();
	#endif

	if( Verbose ) {
		fprintf( stderr, 
			"guralp2orb: udplisten: listening on port %d\n",
			 ul->port );
	}

	mutex_lock( &(ul->statuslock) );
	cond_signal( &(ul->up) );
	mutex_unlock( &(ul->statuslock) );

	for (;;) {
		
		allot( G2orbpkt *, gpkt, 1 );

		gpkt->tcprecovered = 0;

		mutex_lock( &(ul->socketlock) );
		gpkt->len = recvfrom( ul->so, gpkt->packet, RAWGCF_PACKET_SIZE, 0,
				(struct sockaddr *) &foreign, &lenforeign );
		mutex_unlock( &(ul->socketlock) );

		if ( gpkt->len == -1 ) {
			elog_complain( 1, 
				"Was not able to get udp packet; errno %d\n",
				errno );
			free( gpkt );
			continue;
		} 

		gpkt->udpip = foreign.sin_addr;
		gpkt->udpport = foreign.sin_port;

		sprintf( gpkt->udpsource, "%s:%d",
				inet_ntoa( foreign.sin_addr ),
				foreign.sin_port );

		if ( ! strncmp( gpkt->packet, "GCFACKN", 8 ) ) {
			if( VeryVerbose ) {
				fprintf( stderr, 
				"guralp2orb: received GCFACKN from %s\n",
					gpkt->udpsource );
			}
			free( gpkt );
			continue;
		}

		gcfpeek( gpkt );

		if( ! healthy_packet( gpkt ) ) {

			free( gpkt );
			continue;
		}

		register_packet( gpkt );

		report_queuemax();

		pmtfifo_push( Packets_mtf, (void *) gpkt );
	}

	return( NULL );
}

static Udplistener *  
launch_udplisten_thread( in_port_t port )
{
	Udplistener *ul;
	char	key[STRSZ];
	int	ret;

	sprintf( key, "%d", port );

	if( ( ul = (Udplistener *) getarr( ul_arr, key ) ) != NULL ) {

		if( Verbose ) {
			fprintf( stderr,
			"guralp2orb: udplisten:   ...already listening on port %s\n",
			key );
		}

		return ul;
	}

	allot( Udplistener *, ul, 1 );

	mutex_init( &(ul->socketlock), USYNC_THREAD, NULL );
	mutex_init( &(ul->statuslock), USYNC_THREAD, NULL );

	cond_init( &(ul->up), USYNC_THREAD, 0 );

	ul->failed = 0;
	ul->port = port;
	
	setarr( ul_arr, key, (void *) ul );

	ret = thr_create( NULL, 0, guralp2orb_udplisten, 
			  (void *) ul, THR_BOUND | THR_NEW_LWP,
			  &(ul->listener_thread) );

	mutex_lock( &(ul->statuslock) );
	cond_wait( &(ul->up), &(ul->statuslock) );
	mutex_unlock( &(ul->statuslock) );

	if( ul->failed ) {

		delarr( ul_arr, key );

		mutex_destroy( &(ul->statuslock) );
		mutex_destroy( &(ul->socketlock) );
		
		cond_destroy( &(ul->up) );
		
		free( ul );
		
		ul = (Udplistener *) NULL;
	}

	return ul;
}

static void * 
guralp2orb_udpinitiate( void *arg )
{
	Udpinitiater *ui = (Udpinitiater *) arg;
	Udplistener *ul;
	struct sockaddr_in remote;
	int	lenremote = sizeof( remote );
	int	first = 1;

	if( Verbose ) {
		fprintf( stderr, 
		"guralp2orb: udpinitiate: initiating connection with %s:%d into udp port %d\n", 
			ui->screamip, ui->screamport, ui->udpreceive_port );
		fprintf( stderr, 
		"guralp2orb: udpinitiate: ...creating udplisten thread for port %d\n", 
			ui->udpreceive_port );
	}
	
	ul = launch_udplisten_thread( ui->udpreceive_port );

	if( ul == (Udplistener *) NULL ) {

		ui->failed = 1;

		mutex_lock( &(ui->statuslock) );
		cond_signal( &(ui->up) );
		mutex_unlock( &(ui->statuslock) );

		return NULL;
	}

	remote.sin_family = AF_INET;
	remote.sin_port = htons( ui->screamport );
	remote.sin_addr.s_addr = inet_addr( ui->screamip );

	mutex_lock( &(ui->statuslock) );
	cond_signal( &(ui->up) );
	mutex_unlock( &(ui->statuslock) );

	for( ;; ) {
		if( first ) {
			first = 0;
		} else if( VeryVerbose ) {
			fprintf( stderr, 
			"guralp2orb: refreshing connection from %s:%d into port %d\n", 
			ui->screamip, ui->screamport, ui->udpreceive_port );
		}

		sendto( ul->so, "GCFSEND:B", 9, 0,
			(struct sockaddr *) &remote, lenremote );
		sleep( REINITIATE_INTERVAL_SEC );
	}

	return( NULL );

}

static Udpinitiater *
launch_udpinitiate_thread( char *screamip, in_port_t screamport, in_port_t udpreceive_port )
{
	Udpinitiater *ui;
	char	key[STRSZ];
	int	ret;

	sprintf( key, "%s:%d (udp %d)", screamip, screamport, udpreceive_port );

	if( ( ui = (Udpinitiater *) getarr( ui_arr, key ) ) != NULL ) {

		if( Verbose ) {
			fprintf( stderr,
			"guralp2orb: udpinitiate:   ...already connecting to %s\n",
			key );
		}

		return ui;
	}

	allot( Udpinitiater *, ui, 1 );

	mutex_init( &(ui->statuslock), USYNC_THREAD, NULL );

	cond_init( &(ui->up), USYNC_THREAD, 0 );

	ui->failed = 0;

	strcpy( ui->screamip, screamip );
	ui->screamport = screamport;
	ui->udpreceive_port = udpreceive_port;

	setarr( ui_arr, key, (void *) ui );

	ret = thr_create( NULL, 0, guralp2orb_udpinitiate,
		(void *) ui, 0,
		&(ui->initiater_thread) );

	mutex_lock( &(ui->statuslock) );
	cond_wait( &(ui->up), &(ui->statuslock) );
	mutex_unlock( &(ui->statuslock) );
	
	if( ui->failed ) {
		
		delarr( ui_arr, key );

		mutex_destroy( &(ui->statuslock) );

		cond_destroy( &(ui->up) );

		free( ui );

		ui = (Udpinitiater *) NULL;
	}

	return ui;
}

static int	
init_calibinfo( void )
{
	Dbptr 	db;
	int	ret;
	struct stat *statbuf;

	Calibinfo.usedbcalib = 1;
	Calibinfo.usedbsegtype = 1;
	Calibinfo.ctrk = 0;
	Calibinfo.strk = 0;
	Calibinfo.itrk = 0;
	Calibinfo.calibarr = newarr( 0 );
	Calibinfo.segtypearr = newarr( 0 );

	if( ! strcmp( Calibinfo.dbname, "" ) ) {
		Calibinfo.usedbcalib = 0;
		Calibinfo.usedbsegtype = 0;
		return 0;
	}

	ret = dbopen( Calibinfo.dbname, "r", &db );

	if( ret < 0 || db.database < 0 ) {

		elog_complain( 1, "%s %s; %s\n",
		  "Failed to open database",
		  Calibinfo.dbname,
  		  "calib, calper, and segtype will be default values" );

		Calibinfo.usedbcalib = 0;
		Calibinfo.usedbsegtype = 0;

		return -1;
	}

	dbclose( db );

	if( table_check( "calibration", &Calibinfo.ctrk ) == (struct stat *) NULL ) {

		elog_complain( 1, "Using default values for calib and calper.\n" );

		Calibinfo.usedbcalib = 0;
	}

	if( table_check( "sensor", &Calibinfo.strk ) == (struct stat *) NULL || 
	    table_check( "instrument", &Calibinfo.itrk ) == (struct stat *) NULL ) {

		elog_complain( 1, "Using default value for segtype.\n" );

		Calibinfo.usedbsegtype = 0;
	}
}

static void *
dup_element( void *element )
{
	return element;
}

static void
launch_udplisten_threads( void )
{
	Tbl	*udplisten;
	Tbl	*t;
	in_port_t udpreceive_port;
	char	*line;
	int 	nvals;

	t = pfget_tbl( pf, "udplisten" );

	if( t == (Tbl *) NULL ) {

		udplisten = (Tbl *) NULL; 

	} else {

		udplisten = duptbl( t, dup_element );
	}

	while( ( udplisten != (Tbl *) NULL ) && 
	       ( line = poptbl( udplisten ) ) != NULL ) {

		nvals = sscanf( line, "%hd\n", &udpreceive_port );

		if( nvals != 1 ) {
			elog_complain( 1, 
				"guralp2orb: line \"%s\" not understood in udplisten table of parameter file; skipping.\n", line );
			continue;
		}

		launch_udplisten_thread( udpreceive_port );
	}

	if( udplisten != (Tbl *) NULL ) {

		freetbl( udplisten, 0 );
	}

	return;
}

static void
launch_udpinitiate_threads( void ) 
{
	Tbl	*udpinitiate;
	Tbl	*t;
	char	myline[STRSZ];
	char	*line;
	char	screamip[STRSZ];
	in_port_t screamport;
	in_port_t udpreceive_port;
	int 	nvals;

	t = pfget_tbl( pf, "udpinitiate" );

	if( t == (Tbl *) NULL ) {

		udpinitiate = (Tbl *) NULL; 

	} else {

		udpinitiate = duptbl( t, dup_element );
	}

	while( ( udpinitiate != (Tbl *) NULL ) &&
	       ( line = poptbl( udpinitiate ) ) != NULL ) {

		strsub( line, ":", " ", myline );

		nvals = sscanf( myline, "%s %hd %hd\n", 
					screamip, 
					&screamport, 
					&udpreceive_port );

		if( nvals != 3 ) {
			elog_complain( 1, 
				"guralp2orb: line \"%s\" not understood in udpinitiate table of parameter file; skipping.\n", line );
			continue;
		}

		launch_udpinitiate_thread( screamip, screamport, udpreceive_port );
	}

	if( udpinitiate != (Tbl *) NULL ) {

		freetbl( udpinitiate, 0 );
	}
	
	return;
}

static Tbl *
healthy_morphlist( Tbl *morphlist ) 
{
	Tbl	*new_morphlist;
	Tbl	*indices;
	int	i;
	char	*cp;
	char	*entry;

	if( morphlist == (Tbl *) NULL ) {
		return morphlist;
	} 

	new_morphlist = duptbl( morphlist, (void *(*)()) strdup );

	indices = greptbl( "^[/\"\'].*", new_morphlist );
	
	if( maxtbl( indices ) <= 0 ) {

		return new_morphlist;

	} else {

		for( i=maxtbl(indices)-1; i>=0; i-- ) {

			entry = gettbl( new_morphlist, (int) gettbl( indices, i ) );

			memmove( entry, entry + 1, strlen( entry ) );
			cp = entry + strlen( entry );
			while( cp > entry ) {
				if( *(cp-1) != '\\' && 
				    (*cp == '/' || *cp == '"' || *cp == '\'') ) {
					*cp = ' ';
				}
				cp--;	
			}

			strtrim( entry );
		}	

		return new_morphlist;
	}
}

static void *
guralp2orb_pfwatch( void *arg )
{
	Tbl	*morphlist;
	int	new_nrecovery_threads;
	int	new_max_recovery_failures;
	int	new_reject_future_packets_sec;
	int	new_reject_past_packets_sec;
	int	new_buffer_tail_padding;
	int	ret;
	int	i;
	char	*s;

	/* parameter-file updates cannot be triggered by 
	   incoming packets, in case the current connections
	   are temporarily dead when a new connect thread is added 
	   (it won't get noticed if the program is waiting for another 
	   packet). Thus this must be a separate thread. */

	for( ;; ) {
		
		ret = pfupdate( Pffile, &pf );

		if( ret < 0 ) {

			elog_complain( 1, "pfupdate failed\n" );

		} else if( ret == 0 ) {

			; /* no reconfiguration necessary */

		} else if( ret == 1 ) {

			if( Verbose ) {
				fprintf( stderr, 	
				 "guralp2orb: parameter-file changed; rereading\n" );
			} 

			mutex_lock( &pfparams_mutex );

			free( Default_net );

			Default_net = pfget_string( pf, "default_net" );

			if( Default_net == (char *) NULL ) {
				Default_net = strdup( DEFAULT_NET );
			} else {
				Default_net = strdup( Default_net );
			}

			free( Default_segtype );

			Default_segtype = pfget_string( pf, "default_segtype" );

			if( Default_segtype == (char *) NULL ) {
				Default_segtype = strdup( DEFAULT_SEGTYPE );
			} else {
				Default_segtype = strdup( Default_segtype );
			}

			freemorphtbl( srcname_morphmap );

			morphlist = pfget_tbl( pf, "srcname_morph" );
			morphlist = healthy_morphlist( morphlist );

			newmorphtbl( morphlist, &srcname_morphmap ); 

			if( morphlist != (Tbl *) NULL ) {
				freetbl( morphlist, free );
			}

			new_reject_future_packets_sec = fabs( pfget_double( pf, "reject_future_packets_sec" ) );

			if( new_reject_future_packets_sec != Reject_future_packets_sec ) {

				Reject_future_packets_sec = new_reject_future_packets_sec;

				if( Reject_future_packets_sec == 0 ) {
					Reject_future_packets = 0;
				} else {
					Reject_future_packets = 1;
				}

				if( Verbose ) {
					if( Reject_future_packets ) {
						s = strtdelta( Reject_future_packets_sec );
						strtrim( s );

						fprintf( stderr, "%s%s%s\n",
			  				"guralp2orb: rejecting all packets that are ", s,
			  				" or more into the future" );
						free( s );
					} else {
						fprintf( stderr, 
			  			"guralp2orb: turning off future-packet rejection\n" );
					}
				}
			}
			/* Recovery list is cleared and reset.  Since we are mutex
			protected that should be ok.  This may be a small memory
			leak because only the keys are actually used in the arr.
			Hence, we should not have to free the contents.*/
			freearr(Recovery_list,0);
			Recovery_list = pfget_arr(pf,"recovery_host_list");
			Max_Packets_to_Recover = pfget_int(pf,"maximum_packets_to_recover");
			

			new_reject_past_packets_sec = fabs( pfget_double( pf, "reject_past_packets_sec" ) );

			if( new_reject_past_packets_sec != Reject_past_packets_sec ) {

				Reject_past_packets_sec = new_reject_past_packets_sec;

				if( Reject_past_packets_sec == 0 ) {
					Reject_past_packets = 0;
				} else {
					Reject_past_packets = 1;
				}

				if( Verbose ) {
					if( Reject_past_packets ) {
						s = strtdelta( Reject_past_packets_sec );
						strtrim( s );

						fprintf( stderr, "%s%s%s\n",
			  				"guralp2orb: rejecting all packets that are ", s,
			  				" or more in the past" );
						free( s );
					} else {
						fprintf( stderr, 
			  			"guralp2orb: turning off past-packet rejection\n" );
					}
				}
			}

			new_nrecovery_threads = pfget_int( pf, "nrecovery_threads" );
			new_max_recovery_failures = pfget_int( pf, "max_recovery_failures" );

			new_buffer_tail_padding = pfget_int( pf, "buffer_tail_padding" );

			if( new_buffer_tail_padding != Buffer_tail_padding ) {

				if( Verbose ) {
					fprintf( stderr, 
						"guralp2orb: buffer_tail_padding changed from "
						"%d to %d packets\n",
						Buffer_tail_padding, 
						new_buffer_tail_padding );
				}

				Buffer_tail_padding = new_buffer_tail_padding;
			}
			
			mutex_unlock( &pfparams_mutex );

			if( new_max_recovery_failures != max_recovery_failures ) {

				max_recovery_failures = new_max_recovery_failures;

				if( Verbose ) {
					fprintf( stderr, 
					"guralp2orb: max_recovery_failures changed to %d\n", 
					max_recovery_failures );
				}
			}
			
			if( new_nrecovery_threads > nrecovery_threads ) {

				if( Verbose ) {
						fprintf( stderr, 
					"guralp2orb: nrecovery_threads increased to %d, "
					"launching %d more TCP packet-recovery threads\n",
						new_nrecovery_threads,
						new_nrecovery_threads - nrecovery_threads );
				}

				for( i=nrecovery_threads; i < new_nrecovery_threads; i++ ) {
					ret = thr_create( NULL, 0,
				  			guralp2orb_packetrecover, 
				  			0, 0, 0 );
					if( ret != 0 ) {
						elog_complain( 1,
		 				"Failed to create packet-recovery thread\n" );
					}
				}

				nrecovery_threads = new_nrecovery_threads;
			}

			launch_udplisten_threads();

			launch_udpinitiate_threads();

		}

		sleep( PFWATCH_SLEEPTIME_SEC );
	}
}

int
main( int argc, char **argv )
{
	char	orbname[STRSZ];
	char	c;
	char 	*s;
	Tbl	*morphlist;
	char	disabled[STRSZ];
	int	ret;
	int	ithread;

	elog_init( argc, argv );

	strcpy( Pffile, "guralp2orb" );
	memset( LogpktLogfile, 0, FILENAME_MAX );
	memset( Calibinfo.dbname, 0, FILENAME_MAX );

	while ( ( c = getopt( argc, argv, "vVp:d:l:" ) ) != -1 ) {
		switch( c ) {
		case 'v':
			Verbose++;
			break;
		case 'V':
			Verbose++;
			VeryVerbose++;
			break;
		case 'p':
			strcpy( Pffile, optarg );
			break;
		case 'd':
			strcpy( Calibinfo.dbname, optarg );
			break;
		case 'l':
			strcpy( LogpktLogfile, optarg );
			break;
		default:
			usage();
			break;
		}
	}

	if( Verbose && strcmp( Calibinfo.dbname, "" ) ) {

		fprintf( stderr, 
			"guralp2orb: using database \"%s\" for calibration data\n", 
			Calibinfo.dbname );
	}

	if( argc - optind != 1 ) {
		usage();
	} else {
		strcpy( orbname, argv[argc-1] );
	}

	if( ( Orbfd = orbopen( orbname, "w&" ) ) < 0 ) {
		elog_die( 1, "Failed to open orb %s\n", orbname ); 
	}

	if( pfupdate( Pffile, &pf ) < 0 ) {
		elog_die( 1, "Couldn't read parameter file %s\n", Pffile );
	}

	mutex_init( &pfparams_mutex, USYNC_THREAD, NULL );

	init_calibinfo();

	Buffer_tail_padding = pfget_int( pf, "buffer_tail_padding" );

	Reject_future_packets_sec = fabs( pfget_double( pf, "reject_future_packets_sec" ) );

	if( Reject_future_packets_sec == 0 ) {
		Reject_future_packets = 0;
	} else {
		Reject_future_packets = 1;
	}

	if( Verbose && Reject_future_packets ) {

		s = strtdelta( Reject_future_packets_sec );
		strtrim( s );

		fprintf( stderr, "%s%s%s\n",
			  "guralp2orb: rejecting all packets that are ", s,
			  " or more into the future" );
		free( s );
	}

	Reject_past_packets_sec = fabs( pfget_double( pf, "reject_past_packets_sec" ) );

	if( Reject_past_packets_sec == 0 ) {
		Reject_past_packets = 0;
	} else {
		Reject_past_packets = 1;
	}

	if( Verbose && Reject_past_packets ) {

		s = strtdelta( Reject_past_packets_sec );
		strtrim( s );

		fprintf( stderr, "%s%s%s\n",
			  "guralp2orb: rejecting all packets that are ", s,
			  " or more in the past" );
		free( s );
	}

	Default_net = pfget_string( pf, "default_net" );

	if( Default_net == (char *) NULL ) {
		Default_net = strdup( DEFAULT_NET );
	} else {
		Default_net = strdup( Default_net );
	}

	Default_segtype = pfget_string( pf, "default_segtype" );

	if( Default_segtype == (char *) NULL ) {
		Default_segtype = strdup( DEFAULT_SEGTYPE );
	} else {
		Default_segtype = strdup( Default_segtype );
	}

	morphlist = pfget_tbl( pf, "srcname_morph" );
	morphlist = healthy_morphlist( morphlist );

	newmorphtbl( morphlist, &srcname_morphmap ); 

	if( morphlist != (Tbl *) NULL ) {
		freetbl( morphlist, free );
	}

	mutex_init( &mpq_mutex, USYNC_THREAD, NULL );

	mutex_init( &prthrottle_mutex, USYNC_THREAD, NULL );
	cond_init( &prthrottle, USYNC_THREAD, NULL );

	Lastpacket = newarr( 0 );
	mutex_init( &lp_mutex, USYNC_THREAD, NULL );

	Recovery_failures = newarr( 0 );
	mutex_init( &rf_mutex, USYNC_THREAD, NULL );

	Packets_mtf = pmtfifo_create( PACKET_QUEUE_SIZE, 1, 0 );
	Recover_mtf = pmtfifo_create( RECOVERY_QUEUE_SIZE, 1, 0 );

	ignoreSIGPIPE();

	ret = thr_create( NULL, 0, guralp2orb_packettrans, 0, 0, 0 );
	if( ret != 0 ) {
		elog_die( 1,
		 "Failed to create packet-translation thread\n" );
	}

	max_recovery_failures = pfget_int( pf, "max_recovery_failures" );
	if( Verbose ) {

		if( max_recovery_failures ) {
		fprintf( stderr, 
		"guralp2orb: limiting each instance to %d recovery failures\n",
			max_recovery_failures );
		}
	}

	nrecovery_threads = pfget_int( pf, "nrecovery_threads" );

	if( Verbose ) {
		if( nrecovery_threads <= 0 ) {
			strcpy( disabled, " (TCP-recovery disabled)" );	
		} else {
			strcpy( disabled, "" );
		}
		fprintf( stderr, 
		"guralp2orb: launching %d TCP packet-recovery threads%s\n",
			nrecovery_threads, disabled );
	}
	
	for( ithread = 0; ithread < nrecovery_threads; ithread++ ) {

		ret = thr_create( NULL, 0,
				  guralp2orb_packetrecover, 
				  0, 0, 0 );
		if( ret != 0 ) {
			elog_die( 1,
		 	"Failed to create packet-recovery thread\n" );
		}
	}
	/* Added March 2003 by G Pavlis.  Only attempt recovery on
	network addresses (ip:socket) listed in the parameter file.
	This was found necessary to screen problem stations that would
	cause guralp2orb to go catatonic.*/
	Recovery_list = pfget_arr(pf,"recovery_host_list");
	if(Recovery_list == NULL) 
	{
		elog_notify(0,"recovery_host_list is missing from the parameter file\nAssuming null list\n");
		elog_clear();
		Recovery_list = newarr(0);
	}
	Max_Packets_to_Recover = pfget_int(pf,"maximum_packets_to_recover");

	ul_arr = newarr( 0 );
	ui_arr = newarr( 0 );

	launch_udplisten_threads();

	launch_udpinitiate_threads();

	if( cntarr( ul_arr ) == 0 && cntarr( ui_arr ) == 0 ) {
		elog_die( 1, 
		  "no udplisten or udpinitiate threads. Nothing to do, bye.\n" );
	}
	
	ret = thr_create( NULL, 0, guralp2orb_pfwatch, 0, 0, 0 );
	if( ret != 0 ) {
		elog_die( 1,
		 "Failed to create parameter-file watch thread\n" );
	}

	while( thr_join( (thread_t) NULL, 
			 (thread_t *) NULL,
			 (void **) NULL ) == 0 );

	return 0;
}
