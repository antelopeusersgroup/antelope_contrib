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
#include <thread.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/uio.h>
#include <errno.h>

#include "stock.h"
#include "orb.h"
#include "coords.h"
#include "pf.h"
#include "Pkt.h"
#include "brttutil.h"
#include "bns.h"

#define PACKET_SIZE 1063
#define BLOCK_SEQUENCE_MAX 65536
#define GCF_MOTOROLA_BYTEORDER 1
#define GCF_INTEL_BYTEORDER 2
#define SECONDS_PER_DAY (24*60*60)
#define QUEUE_MAX_PACKETS 100
#define QUEUE_MAX_RECOVER 100
#define IDENTIFY_SERVER 252
#define REQUEST_OLDEST_SEQNO 254
#define PACKET_REQUEST 255
#define SERVER_ID_STRING "GCFSERV"
#define REINITIATE_INTERVAL 60

typedef struct g2orbpkt_ {
	char	packet[PACKET_SIZE];
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
	char	screamip[STRSZ];
	int	screamport;
	int	udpreceive_port;
	thread_t initiater_thread;
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
} Udplistener;

typedef struct recoverreq_ {
	char	udpsource[STRSZ];
	struct in_addr udpip;
	in_port_t udpport;
	int	first;
	int	last;
} Recoverreq;

Arr *ul_arr; 
Arr *ui_arr; 
Mtfifo *Packets_mtf;
Mtfifo *Recover_mtf;
Arr *Lastpacket;
mutex_t lp_mutex;

static Morphtbl *srcname_morphmap;
static char *Default_net;
static char StatuspktLogfile[FILENAME_MAX];
static int Orbfd = 0; 
static int Verbose = 0;
static int VeryVerbose = 0;
static int Reject_future_packets = 0;
static double Reject_future_packets_sec = 0;
static int nrecovery_threads = 1;

static void
usage()
{
	die( 1, "Usage: guralp2orb [-v] [-V] [-p pffile] [-l status_logfile] [-r sec] orbname\n" );
	
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
register_packet( G2orbpkt *gpkt )
{
	int	*blockseq;
	Recoverreq *rr;

	mutex_lock( &lp_mutex );

	blockseq = (int *) getarr( Lastpacket, gpkt->udpsource );

	if( blockseq == NULL ) {

		allot( int *, blockseq, 1 );
		setarr( Lastpacket, gpkt->udpsource, (void *) blockseq );

		*blockseq = gpkt->blockseq;

	} else {

		blockseq = getarr( Lastpacket, gpkt->udpsource );

		if( gpkt->blockseq != next_in_sequence( *blockseq ) ) {

			allot( Recoverreq *, rr, 1 );

			strcpy( rr->udpsource, gpkt->udpsource );
			rr->udpip = gpkt->udpip;
			rr->udpport = gpkt->udpport;
			rr->first = next_in_sequence( *blockseq );
			rr->last = previous_in_sequence( gpkt->blockseq );

			if( VeryVerbose ) {
				fprintf( stderr, 
				"guralp2orb: missed %d to %d from %s\n",
					rr->first, 
					rr->last,
					rr->udpsource);
			}

			if( nrecovery_threads > 0 ) {
				mtfifo_push( Recover_mtf, (void *) rr );
			} else {
				free( rr );
			}
		}

		*blockseq = gpkt->blockseq;
	}

	mutex_unlock( &lp_mutex );
}

static void 
construct_srcname( char *srcname, char *sysid, char *streamid, int samprate ) 
{
	Srcname parts;
	char	starting_srcname[ORBSRCNAME_SIZE];
	int	n;

	memset( &parts, 0, sizeof( parts ) );

	strcpy( parts.src_net, Default_net );
	strcpy( parts.src_sta, sysid );
	strcpy( parts.src_chan, streamid );

	if( samprate != 0 ) {
		strcpy( parts.src_suffix, "GCF" );
	} else {
		strcpy( parts.src_suffix, "GCFS" );
	}

	join_srcname( &parts, starting_srcname );

	n = morphtbl( starting_srcname, srcname_morphmap, 
		  MORPH_ALL|MORPH_PARTIAL, srcname );
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

	if( ( gpkt->byteorder == GCF_MOTOROLA_BYTEORDER && ! WORDS_BIGENDIAN ) || 
	    ( gpkt->byteorder == GCF_INTEL_BYTEORDER && WORDS_BIGENDIAN ) ) {
		
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

static void *
guralp2orb_packetrecover( void *arg )
{
	G2orbpkt *gpkt;
	struct sockaddr_in sin;
	int 	so;
	int	rc = 0;
	Recoverreq *rr;
	Bns 	*bns;
	char 	msg;
	char	response[PACKET_SIZE];
	unsigned short requested;
	int	next;
	int	lastflag;

	while( mtfifo_pop( Recover_mtf, (void **) &rr ) != 0 ) { 
		
		/* SCAFFOLD acquiesce to lost packets on socket failures */
		/* N.B. This could be handled differently with some expire 
		   mechanism that puts packets back on the recovery 
		   queue */

		so = socket( PF_INET, SOCK_STREAM, 0 );
		if( so < 0 ) {
			complain( 1, 
			"Can't open tcp socket to %s for packet recovery\n", 
			rr->udpsource );
			free( rr );
			continue;
		}

		sin.sin_family = AF_INET;
		sin.sin_port = htons( 0 );  /* Any port */
		sin.sin_addr.s_addr = htonl( INADDR_ANY );
	
		if( bind( so, (struct sockaddr *) &sin, sizeof( sin ) ) ) {
			complain( 1,
			"Couldn't bind packet recovery socket\n" );
			close( so );
			free( rr );
			continue;
		}
		
		sin.sin_port = htons( rr->udpport );
		sin.sin_addr = rr->udpip;
	
		if( connect( so, (struct sockaddr *) &sin, sizeof( sin ) ) ) {
			complain( 1,
			"Couldn't connect packet recovery socket\n" );
			close( so );
			free( rr );
			continue;
		}

		bns = bnsnew( so, 100 * PACKET_SIZE );
		bnsuse_sockio( bns );

		msg = IDENTIFY_SERVER;
		bnsput( bns, &msg, BYTES, 1 );
		bnsflush( bns );

		/* Question whether this is always 16 bytes */
		bnsget( bns, &response[0], BYTES, 16 );

		if( strncmp( response, SERVER_ID_STRING, 
			sizeof( SERVER_ID_STRING ) - 1 ) ) {
			complain( 1, "%s not a GCF server; tcp packet recovery failed\n", rr->udpsource );
			bnsclose( bns );
			free( rr );
			continue;
		}

		/* SCAFFOLD: assume the packets are available */

		lastflag = 0;
		next = rr->first;

		while( ! lastflag ) {

			requested = htons( next );

			if( next == rr->last ) lastflag++;
			next = next_in_sequence( next );

			msg = PACKET_REQUEST;
			bnsput( bns, &msg, BYTES, 1 );
			bnsput( bns, &requested, TWO_BYTES, 1 );
			bnsflush( bns );

			allot( G2orbpkt *, gpkt, 1 );
	
			/* SCAFFOLD again assume packets are available */
	
			/* Fill these in for completeness */
			gpkt->len = PACKET_SIZE;
			gpkt->tcprecovered = 1;
			gpkt->udpip = rr->udpip;
			gpkt->udpport = rr->udpport;
			strcpy( gpkt->udpsource, rr->udpsource );
	
			gpkt->len = PACKET_SIZE;

			rc = bnsget( bns, 
				     &(gpkt->packet), 
				     BYTES, 
				     gpkt->len );

			if( rc < 0 ) {
				complain( 1, 
	"failed to get packet %d from %s via TCP, errno %d\n", 
					requested, 
					gpkt->udpsource, 
					bnserrno( bns ) );
				free( rr );
				free( gpkt );
				bnsclose( bns );
				continue;
			}

			gcfpeek( gpkt );

			mtfifo_push( Packets_mtf, (void *) gpkt );

		} 

		free( rr );
		bnsclose( bns );
		close( so );

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
	struct timespec tp;
	double 	tdelta;
	FILE	*fp;
	char	cmd[STRSZ];

	while( mtfifo_pop( Packets_mtf, (void **) &gpkt ) != 0 ) { 

		split_srcname( gpkt->srcname, &parts );

		if( ! strcmp( parts.src_suffix, "GCFS" ) ) {

			rc = unstuffPkt( gpkt->srcname, gpkt->time, gpkt->packet,
				    gpkt->len, &pkt );

			if( strcmp( StatuspktLogfile, "" ) ) {
				fp = fopen( StatuspktLogfile, "a" );
				if( fp ) {
					fprintf( fp, 
			"\nStatus packet from %s:%s at %s:\n\n%s\n\n",
					parts.src_sta, parts.src_chan,
					s = strtime( gpkt->time ),
					pkt->string );
					free( s );
					fclose( fp );
					sprintf( cmd, "dos2unix %s %s",
						StatuspktLogfile,
						StatuspktLogfile );
					system( cmd );
				} else {
					complain( 1,
					"Couldn't open %s for %s packet\n",
						StatuspktLogfile,
						gpkt->srcname );
				}
			}

			if( Verbose ) {
				printf( "\nStatus packet from %s:%s at %s:\n\n%s\n\n",
					parts.src_sta, parts.src_chan, 
					s = strtime( gpkt->time ),
					pkt->string );

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

		clock_gettime( CLOCK_REALTIME, &tp );
		tdelta = gpkt->time - tp.tv_sec+tp.tv_nsec/1e9;

		if( Reject_future_packets &&
		    tdelta > Reject_future_packets_sec && 
		    strcmp( parts.src_suffix, "GCFS" ) ) {
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
			continue;
		}

		rc = orbput( Orbfd, 
			     gpkt->srcname, 
			     gpkt->time,
			     gpkt->packet, 
			     gpkt->len );

		if( rc != 0 ) clear_register( 1 );

		free( gpkt );
	}

	return( NULL );
}

static void *
guralp2orb_udplisten( void *arg )
{
	Udplistener *ul = (Udplistener *) arg;
	char	*inaddr = "0.0.0.0";
	struct sockaddr_in foreign;
	int	lenforeign = sizeof( foreign );
	G2orbpkt *gpkt;
	int	rc = 0;

	if( Verbose ) {
		fprintf( stderr, 
			"guralp2orb: listening on port %d\n", ul->port );
	}

	ul->so = socket( AF_INET, SOCK_DGRAM, 0 );
	if( ul->so < 0 ) {
		complain( 1, "Can't open socket for port %d\n", ul->port );
		rc = -1;
		return( NULL );
	}

	setsockopt( ul->so, SOL_SOCKET, SO_REUSEADDR, (char *) 0, 0 );

	ul->lenlocal = sizeof( ul->local );
	memset( (char *) &(ul->local), '\0', ul->lenlocal );

	ul->local.sin_family = AF_INET;
	ul->local.sin_port = htons( (unsigned short) ul->port );
	ul->local.sin_addr.s_addr = inet_addr( inaddr );

	if( bind( ul->so, (struct sockaddr *) &(ul->local), ul->lenlocal ) < 0 ) {
		complain( 1, "Can't bind address to socket\n" );
		rc = -1;
		return( NULL );
	}

	if( getsockname( ul->so, (struct sockaddr *) &(ul->local),
			 &ul->lenlocal ) < 0 ) {
		complain( 1, "Error getting socket name\n" );
		rc = -1;
		return( NULL );
	}

	mutex_lock( &(ul->statuslock) );
	cond_signal( &(ul->up) );
	mutex_unlock( &(ul->statuslock) );

	for (;;) {
		
		allot( G2orbpkt *, gpkt, 1 );

		gpkt->tcprecovered = 0;

		mutex_lock( &(ul->socketlock) );
		gpkt->len = recvfrom( ul->so, gpkt->packet, PACKET_SIZE, 0,
				(struct sockaddr *) &foreign, &lenforeign );
		mutex_unlock( &(ul->socketlock) );

		if ( gpkt->len == -1 ) {
			complain( 1, 
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
		register_packet( gpkt );

		mtfifo_push( Packets_mtf, (void *) gpkt );
	}

	return( NULL );
}

static Udplistener * 
launch_udplisten( char *port )
{
	Udplistener *ul;
	int	ret;

	if( ul_arr == (Arr *) NULL ) {
		ul_arr = newarr( 0 );
	}

	if( ( ul = (Udplistener *) getarr( ul_arr, port ) ) != NULL ) {

		if( Verbose ) {
			fprintf( stderr,
			"guralp2orb: already listening on port %s\n",
			port );
		}

		return ul;
	}

	allot( Udplistener *, ul, 1 );

	mutex_init( &(ul->socketlock), USYNC_THREAD, NULL );
	mutex_init( &(ul->statuslock), USYNC_THREAD, NULL );

	cond_init( &(ul->up), USYNC_THREAD, 0 );

	ul->port = atoi( port );
	
	setarr( ul_arr, port, (void *) ul );

	ret = thr_create( NULL, 0, guralp2orb_udplisten, 
			  (void *) ul, THR_BOUND | THR_NEW_LWP,
			  &(ul->listener_thread) );

	mutex_lock( &(ul->statuslock) );
	cond_wait( &(ul->up), &(ul->statuslock) );
	mutex_unlock( &(ul->statuslock) );

	return ul;
}

static void *
guralp2orb_udpinitiate( void *arg )
{
	Udpinitiater *ui = (Udpinitiater *) arg;
	Udplistener *ul;
	char	port[STRSZ];
	struct sockaddr_in remote;
	int	lenremote = sizeof( remote );
	int	first = 1;

	sprintf( port, "%d", ui->udpreceive_port );
	ul = launch_udplisten( port );

	remote.sin_family = AF_INET;
	remote.sin_port = htons( ui->screamport );
	remote.sin_addr.s_addr = inet_addr( ui->screamip );

	if( Verbose ) {
		fprintf( stderr, 
		"guralp2orb: initiating connection from %s:%d into port %d\n", 
		ui->screamip, ui->screamport, ui->udpreceive_port );
	}
	
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
		sleep( REINITIATE_INTERVAL );
	}

	return( NULL );

}

int
main( int argc, char **argv )
{
	Udpinitiater *ui;
	char	pffile[STRSZ];
	char	orbname[STRSZ];
	char	c;
	char 	*s;
	Pf	*pf;
	Tbl	*udplisten;
	Tbl	*udpinitiate;
	Tbl	*morphlist;
	char	udpreceive_port[STRSZ];
	char	*line;
	char	myline[STRSZ];
	int	ret;
	int	ithread;
	char	*sp;

	strcpy( pffile, "guralp2orb" );
	memset( StatuspktLogfile, 0, FILENAME_MAX );

	while ( ( c = getopt( argc, argv, "vVp:r:l:" ) ) != -1 ) {
		switch( c ) {
		case 'v':
			Verbose++;
			break;
		case 'V':
			Verbose++;
			VeryVerbose++;
			break;
		case 'p':
			strcpy( pffile, optarg );
			break;
		case 'r':
			Reject_future_packets++;
			Reject_future_packets_sec = atof( optarg );
			break;
		case 'l':
			strcpy( StatuspktLogfile, optarg );
			break;
		default:
			usage();
			break;
		}
	}

	if( Verbose && Reject_future_packets ) {

		s = strtdelta( Reject_future_packets_sec );
		strtrim( s );

		fprintf( stderr, "%s%s%s\n",
			  "guralp2orb: rejecting all packets that are ", s,
			  " or more into the future" );
		free( s );
	}

	if( argc - optind != 1 ) {
		usage();
	} else {
		strcpy( orbname, argv[argc-1] );
	}

	if( ( Orbfd = orbopen( orbname, "w&" ) ) < 0 ) {
		die( 1, "Failed to open orb %s\n", orbname ); 
	}

	if( pfread( pffile, &pf ) < 0 ) {
		die( 1, "Couldn't read parameter file %s\n", pffile );
	}

	Default_net = pfget_string( pf, "default_net" );

	morphlist = pfget_tbl( pf, "srcname_morph" );
	newmorphtbl( morphlist, &srcname_morphmap );

	Lastpacket = newarr( 0 );

	mutex_init( &lp_mutex, USYNC_THREAD, NULL );

	Packets_mtf = mtfifo_create( QUEUE_MAX_PACKETS, 1, 0 );
	Recover_mtf = mtfifo_create( QUEUE_MAX_RECOVER, 1, 0 );

	ret = thr_create( NULL, 0, guralp2orb_packettrans, 
			0, 0, 0 );
	if( ret != 0 ) {
		die( 1,
		 "guralp2orb: Failed to create packet-translation thread\n" );
	}

	nrecovery_threads = pfget_int( pf, "nrecovery_threads" );

	if( Verbose ) {
		fprintf( stderr, 
		"guralp2orb: launching %d TCP packet-recovery threads\n",
			nrecovery_threads );
	}
	
	for( ithread = 0; ithread < nrecovery_threads; ithread++ ) {

		ret = thr_create( NULL, 0,
				  guralp2orb_packetrecover, 
				  0, 0, 0 );
		if( ret != 0 ) {
			die( 1,
		 	"Failed to create packet-recovery thread\n" );
		}
	}

	udplisten = pfget_tbl( pf, "udplisten" );

	while( ( line = poptbl( udplisten ) ) != NULL ) {

		strcpy( udpreceive_port, line );

		launch_udplisten( udpreceive_port );

	}

	udpinitiate = pfget_tbl( pf, "udpinitiate" );
	ui_arr = newarr( 0 );

	while( ( line = poptbl( udpinitiate ) ) != NULL ) {

		allot( Udpinitiater *, ui, 1 );

		strcpy( myline, line );
		sp = myline;
		while( *sp != ':' ) { sp++; }
		sp[0] = ' ';

		sscanf( myline, "%s %d %d\n", 
			ui->screamip, 
			&(ui->screamport), 
			&(ui->udpreceive_port) );

		ret = thr_create( NULL, 0, guralp2orb_udpinitiate,
			(void *) ui, THR_BOUND | THR_NEW_LWP,
			&(ui->initiater_thread) );
	}

	while( thr_join( (thread_t) NULL, 
			 (thread_t *) NULL,
			 (void **) NULL ) == 0 );

	return 0;
}
