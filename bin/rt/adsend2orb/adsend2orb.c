/* adsend2orb 
 *
 * K. Lindquist
 * University of Alaska
 * Geophysical Institute
 * July, 1998 
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <thread.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/priocntl.h>
#include <sys/rtpriocntl.h>
#include "db.h"
#include "tr.h"
#include "stock.h"
#include "pkt.h"
#include "orb.h"
#include "pf.h"
#include "packet.h"
#include "data_buf.h"
#include "trace_buf.h"

#define DEMUX_THREAD_SLEEPTIME_NSEC 50000000	/* 50 milliseconds */
#define IW_ORB_TRACE_HEADER_SIZE 16
#define RUN_LENGTH_ENCODING_LENGTH 25

#define INT(x)  ((x)<0.0?((x)-0.5):((x)+0.5))
#define STREQ(a, b) (strcmp((a), (b)) == 0)

typedef struct {                   /******** description of message *********/
        unsigned char    type;     /* message is of this type               */
        unsigned char    mod;      /* was created by this module id         */
        unsigned char    instid;   /* at this installation                  */
} MSG_LOGO;                        /*****************************************/

typedef struct {
	char           *buffer;       /* Buffer start     */
	MSG_LOGO       logo;          /* msginstid, msgtype, modid   */
	unsigned char  cSeqNum;       /* Message sequence number on coax */
	unsigned short msgByteCnt;    /* Message length so far    */
	unsigned char  nxtFragNum;    /* Next expected packet fragment number */
} MSG_INFO;

typedef struct {
	PACKET 	p;
	int	length; 
} UDPMSG;

typedef struct {
	int	pinno;
	char	sta[7];
	char	chan[9];
	char	net[9];
	double	calib;
	double	commdelay;
} SINFO;

static Arr *station_info;
static Tbl *udpmsg_fifo;
static mutex_t message_mutex;
static Pf *pf;
static int ewADBUF_TYPE = -1;
static int ewTRACEBUF_TYPE = -1;
static int ewMOD = -1;
static int ewINST = -1;
static int swap_adbuf_bytes = 0;
static int Compress = 0;
static int pinno_as_adchan = 0;
static int rt_priority = -1;
static int reject_future_packets = 0;
static double reject_future_packets_sec = 0;

extern void SwapTraceBuf( char * );

void *read_udp_packets( void * );
void set_rt_priority( void );
MSG_INFO *accumulate_udpmsg( UDPMSG * );
void ew_adbuf2orb( MSG_INFO *, int );
void ew_tracebuf2orb( MSG_INFO *, int );
void update_station_info( void );
int cmp_string( void *, void *, void * );
void orbput_welltimed_packet( double, int, char *, double, char *, int );

main( int argc, char **argv )
{
	char	*orbname;
	int	orbfd;
	unsigned short adsend_port;
	UDPMSG	*udpmsg;
	MSG_INFO *ewmsg;
	Tbl	*keys;
	char	*key;
	char	dir[FILENAME_MAX];
	char	base[STRSZ];
	char	errmsg[STRSZ];
	char	*s;
	int	ns, ne;
	int	rc;
	int	nmessages;
	struct timespec ts;

	thread_t tid;

	dirbase( argv[0], dir, base );

	if( argc != 3 ) {
		die( 1, "Usage: %s orbname adsend_port\n", base );
	} else {
		orbname = argv[1];
		adsend_port = (unsigned short) atoi( argv[2] );
	}

	if( pfread( base, &pf ) != 0 ) {

		die( 1, "adsend2orb: missing or incorrect parameter file\n" );

	} else {

		keys = pfkeys( pf );
		allot( char *, key, STRSZ );

		sprintf( key, "%s", "swap_adbuf_bytes" );
		searchtbl( &key, keys, cmp_string, 0, &ns, &ne );
		if( ns <= ne ) swap_adbuf_bytes = 
			pfget_int( pf, "swap_adbuf_bytes" );

		sprintf( key, "%s", "compress" );
		searchtbl( &key, keys, cmp_string, 0, &ns, &ne );
		if( ns <= ne ) Compress = 
			pfget_int( pf, "compress" );

		sprintf( key, "%s", "ewADBUF_TYPE" );
		searchtbl( &key, keys, cmp_string, 0, &ns, &ne );
		if( ns <= ne ) ewADBUF_TYPE = pfget_int( pf, "ewADBUF_TYPE" );

		sprintf( key, "%s", "ewTRACEBUF_TYPE" );
		searchtbl( &key, keys, cmp_string, 0, &ns, &ne );
		if( ns <= ne ) ewTRACEBUF_TYPE =
				pfget_int( pf, "ewTRACEBUF_TYPE" );

		if( ewADBUF_TYPE == -1 && ewTRACEBUF_TYPE == -1 ) {
			sprintf( errmsg, "%s",
				"adsend2orb: ewADBUF_TYPE or ewTRACEBUF_TYPE" );
			strcat( errmsg, 
				"must be specified in parameter file\n" );
			die( 1, errmsg );
		}

		sprintf( key, "%s", "ewMOD" );
		searchtbl( &key, keys, cmp_string, 0, &ns, &ne );
		if( ns <= ne ) ewMOD = pfget_int( pf, "ewMOD" );

		sprintf( key, "%s", "ewINST" );
		searchtbl( &key, keys, cmp_string, 0, &ns, &ne );
		if( ns <= ne ) ewINST = pfget_int( pf, "ewINST" );

		sprintf( key, "%s", "pinno_as_adchan" );
		searchtbl( &key, keys, cmp_string, 0, &ns, &ne );
		if( ns <= ne ) pinno_as_adchan =
				pfget_boolean( pf, "pinno_as_adchan" );

		sprintf( key, "%s", "rt_priority" );
		searchtbl( &key, keys, cmp_string, 0, &ns, &ne );
		if( ns <= ne ) rt_priority = pfget_int( pf, "rt_priority" );

		sprintf( key, "%s", "reject_future_packets_sec" );
		searchtbl( &key, keys, cmp_string, 0, &ns, &ne );
		if( ns <= ne ) {
			reject_future_packets_sec = 
			pfget_double( pf, "reject_future_packets_sec" );
			
			reject_future_packets = 1;
		
			fprintf( stderr, "%s%s%s\n",
			 "adsend2orb: rejecting all packets that are ",
			 ( s = strtdelta( reject_future_packets_sec ) ),
			 " or more into the future" );
		}

		free( key );
	}

	while( ( orbfd = orbopen( orbname, "w&" ) ) < 0 ) {
		clear_register( 1 );
		sleep( 5 );
	}

	if( mutex_init( &message_mutex, USYNC_THREAD, (void *) 0 ) != 0 ) {
		
		die( 1, "adsend2orb: failed to create mutex.\n" );
	}

	udpmsg_fifo = newtbl( 0 );

	if( thr_create( (void *) 0, 0, read_udp_packets, (void *) &adsend_port,
			THR_BOUND, &tid ) != 0 ) {
		
		die( 1, "adsend2orb: Failed to create input thread\n" );
	}

	thr_yield();

	ts.tv_sec = 0;
	ts.tv_nsec = DEMUX_THREAD_SLEEPTIME_NSEC;

	for( ;; ) {

		do {
			mutex_lock( &message_mutex );

			nmessages = maxtbl( udpmsg_fifo );
			udpmsg = (UDPMSG *) shifttbl( udpmsg_fifo );

			mutex_unlock( &message_mutex );

			if( nmessages && udpmsg ) {

				ewmsg = accumulate_udpmsg( udpmsg );

				free( udpmsg );

				if( ewmsg != NULL ) {

				   if( ewmsg->logo.type == ewADBUF_TYPE ) {

					ew_adbuf2orb( ewmsg, orbfd );

				   } else if( ewmsg->logo.type == ewTRACEBUF_TYPE ) {

					ew_tracebuf2orb( ewmsg, orbfd );
				   } 

				   free( ewmsg->buffer );
				   free( ewmsg );

				}
			}

		} while( nmessages > 0 );

		nanosleep( &ts, 0 );
	}
}

void set_rt_priority()
{
	uid_t	starting_uid;
	pcinfo_t pcinfo;
	pcparms_t pcparams;
	rtparms_t *rtparams;
	int	rc;
	char 	*s;

	if( rt_priority < 0 )  return;

	starting_uid = getuid();

	rc = setuid( 0 );
	if( rc ) {
		complain( 1, 
			"adsend2orb: failed to set user ID to root: %s",
			strerror( errno ) );
		return;
	}
	
	strcpy( pcinfo.pc_clname, "RT" );
	rc = priocntl( 0, 0, PC_GETCID, (caddr_t) &pcinfo );

	pcparams.pc_cid = pcinfo.pc_cid;
	rtparams = (rtparms_t *) &pcparams.pc_clparms;
	rtparams->rt_pri = rt_priority;
	rtparams->rt_tqnsecs = RT_TQDEF;

	rc = priocntl( P_LWPID, P_MYID, PC_SETPARMS, (caddr_t) &pcparams );

	if( rc ) {
		complain( 1, 
	"adsend2orb: Failed to set real-time priority %d for udp thread: %s\n",
			rt_priority, strerror( errno ) );
	} else {
		fprintf( stderr,
		 "Real-time priority of UDP thread set to %d\n",
		 rt_priority );
	}

	setuid( starting_uid );
}

void
orbput_welltimed_packet( double endtime, int orb, char *srcname, 
			 double time, char *packet, int nbytes )
{
	int	rc;
	struct timespec tp;
	double 	tdelta;
	char	*s;

	clock_gettime( CLOCK_REALTIME, &tp );

	tdelta = endtime - tp.tv_sec+tp.tv_nsec/1e9;

	if( reject_future_packets && tdelta > reject_future_packets_sec ) {

		complain( 1, 
		"adsend2orb: rejecting packet from %s, which ends %s into future\n",
		srcname, ( s = strtdelta( tdelta ) ) );
		free( s );

	} else {

		rc = orbput( orb, srcname, time, packet, nbytes );

		if( rc ) complain( 1,
			"adsend2orb: orbput failed for %s\n", srcname );
	}
}

void
ew_tracebuf2orb( MSG_INFO *ewmsg, int orbfd )
{
	struct PreHdr prehdr;
	SINFO	*sinfo;
	char	*ptr;
	char    orbpacket[MAX_TRACEBUF_SIZ+100];
	int	packetsize;
	TracePacket *tp;
	float	samprate;
	float 	convert;
	float 	calib; 
	unsigned short pinno;
	unsigned short nsamp;
	unsigned short quality;
	char	adchan_key[STRSZ];
	char	net_sta_chan_key[STRSZ];
	char	srcid[STRSZ];
	char	*net;
	char	*sta;
	char	*chan;
	union {
		char *c;
		int  *i;
		short *s;
		float *f;
	} datap;
	unsigned char *buf=NULL;
	int     databuf_size;
	char    datatype[4];
	double	starttime;
	double	commdelay;
	double	endtime;
	int     *data;
	int	bsize = 0;
	int	nout;
	int	i;
	int	rc;

	update_station_info();

	tp = (TracePacket *) ewmsg->buffer;

	WaveMsgMakeLocal( (TRACE_HEADER *) tp );

	prehdr.hdrtype = htons (IWH);
	prehdr.pkttype = htons (IWTB);
	prehdr.hdrsiz = htons( sizeof( struct PreHdr ) + 
				IW_ORB_TRACE_HEADER_SIZE );

	samprate = tp->trh.samprate;
	nsamp = htons( (unsigned short) tp->trh.nsamp );
	memcpy( &quality, &(tp->trh.quality[0]), 2 );
	quality = htons( quality );

	if( pinno_as_adchan ) {
		sprintf( adchan_key, "%d", tp->trh.pinno );

		if( ( sinfo = getarr( station_info, adchan_key ) ) == NULL ) {
			return;
		}

		net = sinfo->net;
		sta = sinfo->sta;
		chan = sinfo->chan;

		commdelay = sinfo->commdelay;

		pinno = htons( (unsigned short) sinfo->pinno );

		convert = (float) sinfo->calib;
		calib = htonl( convert );

	} else {
		sprintf( net_sta_chan_key, "%s_%s_%s", 	
				tp->trh.net, tp->trh.sta, tp->trh.chan );

		if( ( sinfo = getarr( station_info, net_sta_chan_key ) ) == NULL ) {
			net = tp->trh.net;
			sta = tp->trh.sta;
			chan = tp->trh.chan;

			commdelay = 0.0;

			pinno = htons( (unsigned short) tp->trh.pinno );

			convert = 0.0;
			calib = htonl( convert );

		} else {
			net = sinfo->net;
			sta = sinfo->sta;
			chan = sinfo->chan;

			commdelay = sinfo->commdelay;

			pinno = htons( (unsigned short) sinfo->pinno );

			convert = (float) sinfo->calib;
			calib = htonl( convert );
		}
	}

	datap.c = tp->msg + sizeof( TRACE_HEADER );

        if( Compress )
        {
                strcpy( datatype, "gc" );

                data = (int *) malloc (tp->trh.nsamp*sizeof(int));
                if (data == NULL) {
                        register_error (0, "adsend2orb: malloc() error.\n");
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
                        register_error( 0, "adsend2orb: Datatype %s not understood\n",
                                           tp->trh.datatype );
                        return;
                }

                if( gencompress (&buf, &nout, &bsize, data, tp->trh.nsamp, 25) < 0)
                {
                        register_error (0, "adsend2orb: gencompress() error.\n");
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
        memcpy( ptr, &samprate, sizeof(float) );
        ptr += sizeof( float );
        memcpy( ptr, &calib, sizeof(float) );
        ptr += sizeof( float );
        memcpy( ptr, &pinno, sizeof(unsigned short) );
        ptr += sizeof( unsigned short );
        memcpy( ptr, &nsamp, sizeof(unsigned short) );
        ptr += sizeof( unsigned short );
        memcpy( ptr, datatype, 2);
        ptr += 2;
        memcpy( ptr, &quality, sizeof(unsigned short) );
        ptr += sizeof( unsigned short );

        memcpy( ptr, buf, databuf_size );

        if( Compress )
        {
                free( buf );
        }
		
	sprintf( srcid, "%s_%s_%s", net, sta, chan );

	starttime = tp->trh.starttime - commdelay; 
	endtime = ENDTIME( starttime, tp->trh.samprate, tp->trh.nsamp );

	orbput_welltimed_packet( endtime, orbfd, srcid, starttime,
				 orbpacket, packetsize );
}

void
ew_adbuf2orb( MSG_INFO *ewmsg, int orbfd )
{
	WF_HEADER *adbuf_head;
	SINFO	*sinfo;
	double	pkt_starttime;
	double	starttime;
	double	endtime;
	int	nsamp_int;
	float	samprate;
	char	adchan_key[STRSZ];
	int	chan_index;
	int	scan_index;
	short	*bufptr;
	short	*data;
	int	*datadiffs;
	unsigned char *buf=NULL;
	int	bsize = 0;
	int	nout;
	char	srcid[STRSZ];
	char	*orbpacket;
	char	*ptr;
	int	packetsize;
	int	databuf_size;
	struct PreHdr prehdr;
	float	calib;
	char 	datatype[3];
	unsigned short nsamp;
	unsigned short pinno;
	unsigned short quality = 0;	/* HARD-WIRE */
	int	rc;

	update_station_info();

	prehdr.hdrtype = htons (IWH);
	prehdr.pkttype = htons (IWTB);
	prehdr.hdrsiz = htons( sizeof( struct PreHdr ) + 
				IW_ORB_TRACE_HEADER_SIZE );

	if( swap_adbuf_bytes ) SwapTraceBuf( ewmsg->buffer );

	adbuf_head = (WF_HEADER *) ewmsg->buffer;

	pkt_starttime =(double) adbuf_head->tssec+( 1e-6 * adbuf_head->tsmic );
	nsamp_int = (int) adbuf_head->nsample;
	samprate = nsamp_int / adbuf_head->sample_dt;

	for( chan_index = 0; chan_index < adbuf_head->nchan; chan_index++ ) {

		sprintf( adchan_key, "%d", chan_index );
		if( ( sinfo = getarr( station_info, adchan_key ) ) == NULL ) {
			continue;
		}

		bufptr = (short *) ( ewmsg->buffer + sizeof(WF_HEADER) +
				     adbuf_head->nchan * sizeof(short) +
				     chan_index * sizeof(short) );
				     

		allot( short *, data, adbuf_head->nscan );

		for( scan_index = 0; scan_index < adbuf_head->nscan; scan_index++ ) {

			data[scan_index] = *bufptr;

			bufptr += adbuf_head->nchan;
		}

		if( Compress ) {

			allot( int *, datadiffs, adbuf_head->nscan );

			datadiffs[0] = data[0];
			for( scan_index = 1; 
				scan_index < adbuf_head->nscan;
					scan_index++ ) {

				datadiffs[scan_index] = 
					data[scan_index] - data[scan_index-1];
			} 

			if( gencompress( &buf, &nout, &bsize, 
					 datadiffs, adbuf_head->nscan, 
					 RUN_LENGTH_ENCODING_LENGTH 
					) < 0 ) {
				complain( 1, "adsend2orb: gencompress error\n" );
				free( datadiffs );
				free( data );
				continue;
			}

			free( datadiffs );

			strcpy( datatype, "gc" );

			databuf_size = nout;

		} else {

			strcpy( datatype, "s2" );

			databuf_size = adbuf_head->nscan * sizeof( short );

		}

		starttime = pkt_starttime - sinfo->commdelay;
		calib = sinfo->calib;
		quality = htons( quality );
		pinno = sinfo->pinno;
		pinno = htons( pinno );
		nsamp = nsamp_int;
		nsamp = htons( nsamp );

		endtime = ENDTIME( starttime, samprate, nsamp_int );

		packetsize = databuf_size +
				sizeof( struct PreHdr ) +
					IW_ORB_TRACE_HEADER_SIZE;
		prehdr.pktsiz = htons( packetsize );

		allot( char *, orbpacket, packetsize );

		ptr = orbpacket;

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

		if( Compress ) {
			memcpy( ptr, buf, databuf_size );
			free( buf );
			buf = NULL;
			bsize = 0;

		} else {
			memcpy( ptr, data, databuf_size );
		}

		sprintf( srcid, "%s_%s_%s", 
				sinfo->net,
				sinfo->sta,
				sinfo->chan );

		orbput_welltimed_packet( endtime, orbfd, srcid, starttime,
					 orbpacket, packetsize );

		free( orbpacket );
		free( data );
	}

	return;
}

MSG_INFO *
accumulate_udpmsg( UDPMSG *udpmsg )
{
	static Arr	*ewmessages = NULL;
	static Arr	*track = NULL;
	char	logo_key[STRSZ];
	MSG_INFO *ewmsg;
	int	datalen;
	int	*expected_seqnum;
	int	lost;

	if( ewmessages == NULL ) ewmessages = newarr( 0 );
	if( track == NULL ) track = newarr( 0 );

	if( ( udpmsg->p.msgType != ewTRACEBUF_TYPE ) &&
	    ( udpmsg->p.msgType != ewADBUF_TYPE ) )
						return (MSG_INFO *) NULL; 
	if( ( ewMOD != -1 ) && ( udpmsg->p.modId != ewMOD ) )
						return (MSG_INFO *) NULL; 
	if( ( ewINST != -1 ) && ( udpmsg->p.msgInst != ewINST ) )
						return (MSG_INFO *) NULL; 

	sprintf( logo_key, "%d_%d_%d", 
			   (int) udpmsg->p.msgInst,
			   (int) udpmsg->p.msgType,
			   (int) udpmsg->p.modId );
	
	ewmsg = getarr( ewmessages, logo_key ); 

	datalen = udpmsg->length - UDP_HDR;

	if( ewmsg == (MSG_INFO *) NULL ) {

		/* Wait for the start of a new message */
		if( udpmsg->p.fragNum != 0 ) {
			return (MSG_INFO *) NULL;
		}

		allot( MSG_INFO *, ewmsg, 1 );

		allot( char *, ewmsg->buffer, datalen );
		memcpy( ewmsg->buffer, udpmsg->p.text, datalen );

		ewmsg->logo.instid	= udpmsg->p.msgInst;
		ewmsg->logo.type	= udpmsg->p.msgType;
		ewmsg->logo.mod		= udpmsg->p.modId;
		ewmsg->cSeqNum		= udpmsg->p.msgSeqNum;
		ewmsg->msgByteCnt	= datalen;
		ewmsg->nxtFragNum	= 1;

		setarr( ewmessages, logo_key, (void *) ewmsg );

	} else {
		
		if( udpmsg->p.msgSeqNum != ewmsg->cSeqNum ||
		    udpmsg->p.fragNum   != ewmsg->nxtFragNum ) {

			delarr( ewmessages, logo_key );

			if( ewmsg->buffer ) free( ewmsg->buffer );
			free( ewmsg );

			return (MSG_INFO *) NULL;
		}

		reallot( char *, ewmsg->buffer, ewmsg->msgByteCnt + datalen );
		memcpy( ewmsg->buffer + ewmsg->msgByteCnt, udpmsg->p.text, datalen );

		ewmsg->msgByteCnt += datalen;
		ewmsg->nxtFragNum++;
		
	}

	if( udpmsg->p.lastOfMsg ) {

		expected_seqnum = getarr( track, logo_key );		

		if( expected_seqnum == NULL ) {

			allot( int *, expected_seqnum, 1 );

			*expected_seqnum = ( udpmsg->p.msgSeqNum + 1 ) % 256;
			
			setarr( track, logo_key, expected_seqnum );

		} else if( *expected_seqnum != udpmsg->p.msgSeqNum ) {

			lost = udpmsg->p.msgSeqNum - *expected_seqnum;
			if( lost < 0 ) lost += 256; 	/* Rolled over */

			complain( 1, 
	"adsend2orb: lost %d messages for inst_type_mod %s, got # %d expecting # %d\n",
				lost, logo_key,
				udpmsg->p.msgSeqNum, *expected_seqnum );

			*expected_seqnum = ( udpmsg->p.msgSeqNum + 1 ) % 256;
			

		} else {
			
			*expected_seqnum = ( *expected_seqnum + 1 ) % 256;

		}

		delarr( ewmessages, logo_key );

	} else {

		ewmsg = (MSG_INFO *) NULL; /* Only return complete Earthworm messages */
	}

	return ewmsg;
}

void *
read_udp_packets( void *portp )
{
	unsigned short adsend_port = *((unsigned short *) portp);

	int	mysocket;
	const int reuse = 1;
	struct sockaddr_in name;
	int	dummy;
	UDPMSG	*udpmsg;

	set_rt_priority(); 

	if( ( mysocket = socket( AF_INET, SOCK_DGRAM, 0 ) ) == -1 ) {

		die( 1, "adsend2orb: Can't open the socket.\n" );
	}

	if( setsockopt(
			mysocket, SOL_SOCKET, SO_REUSEADDR,
			(char *) &reuse, sizeof(int) )
		== -1 ) {

		die( 1, "adsend2orb: Error permitting socket address reuse.\n" );
	}
 
	memset( (char *) &name, '\0', sizeof(name) );
	name.sin_family      = AF_INET;
	name.sin_port        = htons( adsend_port );
	name.sin_addr.s_addr = htonl( INADDR_ANY );
 
	if( bind( mysocket, (struct sockaddr *) &name, sizeof(name) ) == -1 ) {

		die( 1, "adsend2orb: Can't bind address to socket.\n" );
	}
		
	for( ;; ) {
			       
		allot( UDPMSG *, udpmsg, 1 );

		udpmsg->length = recvfrom( mysocket, (char *) &udpmsg->p,
					   UDP_SIZ, 0, NULL, &dummy );

		mutex_lock( &message_mutex );
		pushtbl( udpmsg_fifo, udpmsg );
		mutex_unlock( &message_mutex );

	}

}

int
cmp_string( void *ap, void *bp, void *private )
{
	char *a = *((char **) ap);
	char *b = *((char **) bp);

	return strcmp( a, b );
}

void 
update_station_info( void )
{
	SINFO 	*sinfo;
	static int ignore_site_db = 0;
	static double pins_mtime;
	static double ewanalog_mtime;
	static double calibration_mtime;
	static double timecorr_mtime;
	double	test_mtime;
	static char *pins_path = NULL;
	static char *ewanalog_path = NULL;
	static char *calibration_path = NULL;
	static char *timecorr_path = NULL;
	struct stat stat_buf;
	int	present;
	char	*site_db;
	Dbptr	db;
	Dbptr	dbt;
	int	nrecs;
	int	refresh = 0;
	int	adchan;
	char	adchan_key[STRSZ];
	char	net_sta_chan_key[STRSZ];
	double	calib;
	double	commdelay;
	char 	errmsg[STRSZ];

	if( ignore_site_db ) return;

	if( ! pins_path ) {	/* Use as initializer */
		
		if( ( site_db = getenv( "SITE_DB" ) ) == NULL ) {

			if( pinno_as_adchan ) {
				sprintf( errmsg, "%s",
				"adsend2orb: pinno_as_adchan option " );
				strcat( errmsg, 
				"requires environment variable SITE_DB " );
				strcat( errmsg, "to be set.\n" );

				die( 1, errmsg );

			} else {
				sprintf( errmsg, "%s",
				"adsend2orb: no SITE_DB environment variable:");
				strcat( errmsg, 
					" relying on packet contents\n" );
				fprintf( stderr, "%s", errmsg );

				/* Support null return from all station-info
				   lookups */
				station_info = newarr( 0 );

				ignore_site_db = 1;
			}

			return;
		}

		dbopen( site_db, "r", &db );

		db = dblookup( db, 0, "pins", 0, 0 );
		dbquery( db, dbTABLE_PRESENT, &present );
		if( present ) {
			dbquery( db, dbTABLE_FILENAME, &pins_path );
			stat( pins_path, &stat_buf );
			pins_mtime = stat_buf.st_mtime;
		} else {
			die( 1, "adsend2orb: pins table not present in %s\n", site_db );
		}

		db = dblookup( db, 0, "ewanalog", 0, 0 );
		dbquery( db, dbTABLE_PRESENT, &present );
		if( present ) {
			dbquery( db, dbTABLE_FILENAME, &ewanalog_path );
			stat( ewanalog_path, &stat_buf );
			ewanalog_mtime = stat_buf.st_mtime;
		} else {
			die( 1, 
				"adsend2orb: ewanalog table not present in %s\n", 
				site_db );
		}

		refresh = 1;

	} else {
		stat( ewanalog_path, &stat_buf );
		test_mtime = stat_buf.st_mtime;

		if( ewanalog_mtime != test_mtime ) {
			ewanalog_mtime = test_mtime;
			refresh = 1;
		}

		stat( pins_path, &stat_buf );
		test_mtime = stat_buf.st_mtime;

		if( pins_mtime != test_mtime ) {
			pins_mtime = test_mtime;
			refresh = 1;
		}
	}

	if( ! calibration_path ) {
		db = dblookup( db, 0, "calibration", 0, 0 );
		dbquery( db, dbTABLE_PRESENT, &present );
		if( present ) {
			dbquery( db, dbTABLE_FILENAME, &calibration_path );

			stat( calibration_path, &stat_buf );
			calibration_mtime = stat_buf.st_mtime;

			refresh = 1;
		}
	} else {
		stat( calibration_path, &stat_buf );
		test_mtime = stat_buf.st_mtime;

		if( calibration_mtime != test_mtime ) {
			calibration_mtime = test_mtime;
			refresh = 1;
		}
	}

	if( ! timecorr_path ) {
		db = dblookup( db, 0, "timecorr", 0, 0 );
		dbquery( db, dbTABLE_PRESENT, &present );
		if( present ) {
			dbquery( db, dbTABLE_FILENAME, &timecorr_path );
			stat( timecorr_path, &stat_buf );
			timecorr_mtime = stat_buf.st_mtime;
			refresh = 1;
		}
	} else {
		
		stat( timecorr_path, &stat_buf );
		test_mtime = stat_buf.st_mtime;

		if( timecorr_mtime != test_mtime ) {
			timecorr_mtime = test_mtime;
			refresh = 1;
		}
	}

	if( refresh ) {
		
		if( station_info != NULL ) freearr( station_info, free );
		
		station_info = newarr( 0 );

		db = dblookup( db, 0, "pins", 0, 0 );
		dbt = dblookup( db, 0, "ewanalog", 0, 0 );

		db = dbtheta( db, dbt, 
			      "sta == ewanalog.sta && chan == ewanalog.chan",
			      0, 0 );
		db = dbsubset( db, "savechan == \"y\"", 0 );

		dbquery( db, dbRECORD_COUNT, &nrecs );
		for( db.record = 0; db.record < nrecs; db.record++ ) {
			allot( SINFO *, sinfo, 1 );

			dbgetv( db, 0, "net", sinfo->net,
				       "sta", sinfo->sta,
				       "chan", sinfo->chan,
				       "pinno", &sinfo->pinno,
				       "adchan", &adchan,
				       0 );
			sinfo->calib = 0.;
			sinfo->commdelay = 0.;

			sprintf( adchan_key, "%d", adchan );
			sprintf( net_sta_chan_key, "%s_%s_%s", 
					sinfo->net, sinfo->sta, sinfo->chan );

			setarr( station_info, adchan_key, (void *) sinfo );
			setarr( station_info, net_sta_chan_key, (void *) sinfo );
		}

		if( calibration_path ) {
			dbt = dblookup( db, 0, "calibration", 0, 0 );
			dbt = dbjoin( db, dbt, 0, 0, 0, 0, 0 );
			dbt = dbsubset( dbt, "calibration.endtime == NULL", 0 );
			dbquery( dbt, dbRECORD_COUNT, &nrecs );
			for( dbt.record = 0; dbt.record < nrecs; dbt.record++ ) {

				dbgetv( dbt, 0, "adchan", &adchan,
					       "calib", &calib, 
					       0 );

				sprintf( adchan_key, "%d", adchan );

				sinfo = getarr( station_info, adchan_key );

				sinfo->calib = calib;
			}
		}

		if( timecorr_path ) {
			dbt = dblookup( db, 0, "timecorr", 0, 0 );
			dbt = dbjoin( db, dbt, 0, 0, 0, 0, 0 );
			dbt = dbsubset( dbt, "timecorr.endtime == NULL", 0 );
			dbquery( dbt, dbRECORD_COUNT, &nrecs );
			for( dbt.record = 0; dbt.record < nrecs; dbt.record++ ) {

				dbgetv( dbt, 0, "adchan", &adchan,
					       "commdelay", &commdelay, 
					       0 );

				sprintf( adchan_key, "%d", adchan );

				sinfo = getarr( station_info, adchan_key );

				sinfo->commdelay = commdelay;
			}
		}
	}

}

