/*
 * ryo2orb.c
 *
 * Import RYO-format real-time GPS data into an orbserver
 *
 * Copyright (c) 2005 Lindquist Consulting, Inc.
 * All rights reserved. 
 *                                                                     
 * Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
 * 
 * This software may be used freely in any way as long as 
 * the copyright statement above is not removed. 
 *
 */

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <thread.h>
#include <synch.h>
#include <errno.h>
#include "stock.h"
#include "db.h"
#include "orb.h"
#include "Pkt.h"
#include "bns.h"
#include "swapbytes.h"
#include "brttutil.h"

#define DEFAULT_SEGTYPE "-"
#define DEFAULT_CALIB 0
#define DEFAULT_CALPER -1
#define NULL_SAMPRATE -1
#define CONNECT_FAILURE_SLEEPTIME_SEC 1
#define SERVER_RESET_ALLOWANCE_SEC 1
#define NCOMPLAIN_MAX 5
#define BNS_BUFFER_SIZE 128 * 1024
#define DEFAULT_BNS_TIMEOUT 60000
#define PACKET_QUEUE_SIZE 50000

#define THR_PRIORITY_IMPORT 0
#define THR_PRIORITY_CONVERT 1

#define APPEND_BUFFER(BUFFER,BUFSZ,NBYTES,C) \
	NBYTES++; \
	RESIZE_BUFFER( unsigned char *, (BUFFER), (BUFSZ), (NBYTES) ); \
	BUFFER[NBYTES-1] = C;

#define BAD_BYTE(WHERE,C) \
	elog_complain( 0, "Bad byte in parser at location '%s': ", WHERE ); \
	{ int bad = (C); hexdump( stderr, &bad, 1 ); } \
	fprintf( stderr, "\n" );

int 	Verbose = 0;
int 	VeryVerbose = 0;
int 	Orbfd = -1;
double 	GPS_epoch;
int 	GPS_leapseconds;
int 	Multiplex_stations;
int 	Max_nsamples_per_channel;
char	*Net;
char	*Segtype;
double	Samprate_tolerance;
double	Multiply_data_by;
double 	Calper = -1;
Arr 	*Channel_names = 0;
Arr 	*Track_packets = 0;
Arr 	*Track_packetchans = 0;
Pmtfifo	*RYO2orbPackets_mtf;

typedef struct ImportThread {

	/* Shared variables: */

	mutex_t	it_mutex;
	Pf	*pf;
	int	stop;
	Morphtbl *srcname_morphmap;

	/* Thread-only variables: */

	char	name[STRSZ];			
	char	server_ipaddress[STRSZ];
	in_port_t server_port;	
	char	default_segtype[2];
	int	so;
	struct sockaddr_in sin;
	Bns	*bnsin;			
	unsigned char *buf;	
	int 	bufsize;
	int	nbytes;
	short	intake_packet_bytecount;
	enum	{ STREAM_SEARCH, 
		  STREAM_CONFIRM, 
		  STREAM_ACCUMULATE } sync_state;
	int	connectfail;
	int	bindfail;

} ImportThread;

typedef struct SatelliteInfo {
	int	sv_prn;
	int	prn_flags;
	int	ephemeris_available;
	int	L1_track;
	int	L2_track;
	int	elevation;
	int	azimuth;
} SatelliteInfo;

typedef struct RYO2orbPacket {

	int	message_type;
	int	GPS_week;
	int	GPS_millisecond;
	int	site_index;
	int	site_count;
	char	site_id[7];
	double	XYZT[4];
	int	position_byte;

	enum 	{ RYO_SIGNAL_UNDETERMINED, 
		  RYO_SIGNAL_L1, 
		  RYO_SIGNAL_L1L2 } position_signal;

	enum 	{ RYO_METHOD_UNDETERMINED, 
		  RYO_METHOD_ABSCODE, 
		  RYO_METHOD_RELCODE, 
		  RYO_METHOD_PHASEPLUSCODE } position_method;

	int	flags_byte;
	int	xyz_cov_present;
	int	tropo_cov_present;
	int	sat_info_present;
	double	time;
	double	variance_scale;
	double	variance_X;
	double	variance_Y;
	double	variance_Z;
	double	covariance_YX;
	double	covariance_YZ;
	double	covariance_ZX;
	double	variance_T;
	double	covariance_TX;
	double	covariance_TY;
	double	covariance_TZ;
	int	satellite_count;
	double	pdop;
	Tbl	*satellites;

} RYO2orbPacket;

void
usage() 
{
	cbanner( "$Date$", 
		 "[-v] [-V] [-p pfname] rtd_ipaddress:port orb",
		 "Kent Lindquist", 
		 "Lindquist Consulting", 
		 "kent@lindquistconsulting.com" );
	
	return;
}

ImportThread *
new_ImportThread( char *name )
{
	ImportThread *it;

	allot( ImportThread *, it, 1 );

	strcpy( it->name, name );
	it->stop = 0;
	it->pf = pfnew( PFARR );
	it->bnsin = NULL;
	it->buf = 0;
	it->bufsize = 0;
	it->nbytes = 0;
	it->sync_state = STREAM_SEARCH;
	it->connectfail = 0;
	it->bindfail = 0;
	it->srcname_morphmap = NULL;

	mutex_init( &it->it_mutex, USYNC_THREAD, NULL );

	return it;
}

void
refresh_import_thread( ImportThread *it )
{
	while( it->bnsin == NULL ) {

		it->so = socket( PF_INET, SOCK_STREAM, 0 );
		if( it->so < 0 ) {

			elog_complain( 1,
		 	  "'%s': Failed to open socket to %s:%d\n", 
			  it->name, it->server_ipaddress, it->server_port );

			sleep( CONNECT_FAILURE_SLEEPTIME_SEC );

			continue;
		}
	
		it->sin.sin_family = AF_INET;
		it->sin.sin_port = htons( 0 ); /* Any port */
		it->sin.sin_addr.s_addr = htonl( INADDR_ANY );

		if( bind( it->so, (struct sockaddr *) &it->sin, 
		                                   sizeof( it->sin ) ) ) {
			if( it->bindfail < NCOMPLAIN_MAX ) {

				elog_complain( 1, "'%s': Couldn't bind socket\n",
					  it->name );
			}

			if( it->bindfail++ == NCOMPLAIN_MAX ) {
					
				elog_complain( 0, 
					"'%s': Last message repeated %d "
					"times; will keep retrying "
					"every %d sec\n",
					it->name,
					NCOMPLAIN_MAX,
					CONNECT_FAILURE_SLEEPTIME_SEC );
			}

			close( it->so );
				
			sleep( CONNECT_FAILURE_SLEEPTIME_SEC );

			continue;

		} else {

			it->bindfail = 0;
		}
	
		it->sin.sin_port = htons( it->server_port );
		it->sin.sin_addr.s_addr = inet_addr( it->server_ipaddress );

		if( Verbose ) {

			elog_notify( 0, 
				  "'%s': Attempting to connect "
				  "to remote export module at %s:%d\n",
				  it->name, it->server_ipaddress, 
				  it->server_port );
		}

		if( connect( it->so, (struct sockaddr *) &it->sin, sizeof( it->sin ) ) ) {
			if( it->connectfail < NCOMPLAIN_MAX ) {

				elog_complain( 1, 
					"'%s': Failed to connect socket for %s:%d\n",
					it->name,
					it->server_ipaddress, it->server_port );
			}

			if( it->connectfail++ == NCOMPLAIN_MAX ) {
					
				elog_complain( 0, 
					"'%s': Last message repeated %d "
					"times; will keep retrying "
					"every %d sec\n",
					it->name,
					NCOMPLAIN_MAX,
					CONNECT_FAILURE_SLEEPTIME_SEC );
			}

			close( it->so );
				
			sleep( CONNECT_FAILURE_SLEEPTIME_SEC );

			continue;

		} else {

			it->connectfail = 0;

			if( Verbose ) {

				elog_notify( 0, 
					  "'%s': import thread Connected "
					  "to remote export module\n",
					  it->name );
			}
		}

		it->bnsin = bnsnew( it->so, BNS_BUFFER_SIZE ); 

		bnsuse_sockio( it->bnsin );

		bnstimeout( it->bnsin, DEFAULT_BNS_TIMEOUT );
	}

	return;
}

void
close_import_connection( ImportThread *it )
{
	bnsclose( it->bnsin );

	it->bnsin = 0;

	if( it->stop == 0 ) {
		
		sleep( SERVER_RESET_ALLOWANCE_SEC );
	}

	return;
}


/*
 * The RYOChecksum() routine below was taken from 
 * "Appendix B: RYO Protocol Specification" (other details unknown),
 * received from Yehuda Bock 2005, written communication 
 *
 */

unsigned short 
RYOChecksum( const unsigned char *a_pbyBuffer, unsigned int a_nByteCount )
{
	unsigned short nChecksum = 0;

	const unsigned char *pB = a_pbyBuffer;

	for( unsigned int nByte = 0; nByte < a_nByteCount; nByte++ ) {

		nChecksum = (unsigned short) (nChecksum + *pB);

		pB++;
	}

	return nChecksum;
}

SatelliteInfo *
new_SatelliteInfo()
{
	SatelliteInfo *si;

	allot( SatelliteInfo *, si, 1 );

	memset( si, '\0', sizeof( si ) );

	return si;
}

void
free_SatelliteInfo( void *sip )
{
	SatelliteInfo *si = (SatelliteInfo *) sip;

	free( si );

	return;
}

RYO2orbPacket *
new_RYO2orbPacket()
{
	RYO2orbPacket *r2opkt;

	allot( RYO2orbPacket *, r2opkt, 1 );

	memset( r2opkt, '\0', sizeof( RYO2orbPacket ) );

	r2opkt->satellites = newtbl( 0 );

	return r2opkt;
}

void
free_RYO2orbPacket( RYO2orbPacket *r2opkt )
{
	freetbl( r2opkt->satellites, free_SatelliteInfo );

	free( r2opkt );

	return;
}

void
RYO2orbPacket_dump( FILE *fp, RYO2orbPacket *r2opkt )
{
	char	*s;
	char	enum_string[STRSZ];
	SatelliteInfo *si;
	int	isat;

	fprintf( fp, "Received RYO Packet of type %d timestamped '%s':\n",
			r2opkt->message_type, 
			s = strtime( r2opkt->time ) );

	free( s );
			
	fprintf( fp, "\t% 20s:\t%s\n", 
		     "site_id", r2opkt->site_id );

	fprintf( fp, "\t% 20s:\t%d\n", 
		     "site_index", r2opkt->site_index );

	fprintf( fp, "\t% 20s:\t%d\n", 
		     "site_count", r2opkt->site_count );

	fprintf( fp, "\t% 20s:\t%d\n", 
		     "GPS_week", r2opkt->GPS_week );

	fprintf( fp, "\t% 20s:\t%d\n", 
		     "GPS_millisecond", r2opkt->GPS_millisecond );

	fprintf( fp, "\t% 20s:\t%f\n", "ITRF X", r2opkt->XYZT[0] );
	fprintf( fp, "\t% 20s:\t%f\n", "ITRF Y", r2opkt->XYZT[1] );
	fprintf( fp, "\t% 20s:\t%f\n", "ITRF Z", r2opkt->XYZT[2] );
	fprintf( fp, "\t% 20s:\t%f\n", "ITRF T", r2opkt->XYZT[3] );

	fprintf( fp, "\t% 20s:\t%d\n", 
		     "position_byte", r2opkt->position_byte );

	fprintf( fp, "\t% 20s:\t%d\n", 
		     "flags_byte", r2opkt->flags_byte );

	switch( r2opkt->position_signal ) {
	case RYO_SIGNAL_UNDETERMINED:
		strcpy( enum_string, "Undetermined" );
		break;
	case RYO_SIGNAL_L1:
		strcpy( enum_string, "L1" );
		break;
	case RYO_SIGNAL_L1L2:
		strcpy( enum_string, "L1/L2" );
		break;
	}

	fprintf( fp, "\t% 20s:\t%s\n", "position_signal", enum_string );

	switch( r2opkt->position_method ) {
	case RYO_METHOD_UNDETERMINED:
		strcpy( enum_string, "Undetermined" );
		break;
	case RYO_METHOD_ABSCODE:
		strcpy( enum_string, "Absolute code" );
		break;
	case RYO_METHOD_RELCODE:
		strcpy( enum_string, "Relative code" );
		break;
	case RYO_METHOD_PHASEPLUSCODE:
		strcpy( enum_string, "Phase + code" );
		break;
	}

	fprintf( fp, "\t% 20s:\t%s\n", "position_method", enum_string );

	fprintf( fp, "\t% 20s:\t%d\n", 
		     "xyz_cov_present", r2opkt->xyz_cov_present );

	fprintf( fp, "\t% 20s:\t%d\n", 
		     "tropo_cov_present", r2opkt->tropo_cov_present );

	fprintf( fp, "\t% 20s:\t%d\n", 
		     "sat_info_present", r2opkt->sat_info_present );

	if( r2opkt->xyz_cov_present ) {

		fprintf( fp, "\t% 20s:\t%f\n", 
			     "variance_scale", r2opkt->variance_scale );

		fprintf( fp, "\t% 20s:\t%f\n", 
			     "variance_X", r2opkt->variance_X );

		fprintf( fp, "\t% 20s:\t%f\n", 
			     "variance_Y", r2opkt->variance_Y );

		fprintf( fp, "\t% 20s:\t%f\n", 
			     "variance_Z", r2opkt->variance_Z );

		fprintf( fp, "\t% 20s:\t%f\n", 
			     "covariance_YX", r2opkt->covariance_YX );

		fprintf( fp, "\t% 20s:\t%f\n", 
			     "covariance_YZ", r2opkt->covariance_YZ );

		fprintf( fp, "\t% 20s:\t%f\n", 
			     "covariance_ZX", r2opkt->covariance_ZX );
	}

	if( r2opkt->tropo_cov_present ) {

		fprintf( fp, "\t% 20s:\t%f\n", 
			     "variance_T", r2opkt->variance_T );

		fprintf( fp, "\t% 20s:\t%f\n", 
			     "covariance_TX", r2opkt->covariance_TX );

		fprintf( fp, "\t% 20s:\t%f\n", 
			     "covariance_TY", r2opkt->covariance_TY );

		fprintf( fp, "\t% 20s:\t%f\n", 
			     "covariance_TZ", r2opkt->covariance_TZ );
	}


	if( r2opkt->sat_info_present ) {
		
		fprintf( fp, "\t% 20s:\t%d\n", 
			     "satellite_count", r2opkt->satellite_count );
		
		fprintf( fp, "\t% 20s:\t%f\n", 
			     "pdop", r2opkt->pdop );

		fprintf( fp, "\t% 20s:\n", "satellites" );

		fprintf( fp, "\t          " );
		fprintf( fp, "% 6s  ", "SV PRN" );
		fprintf( fp, "% 5s  ", "Elev" );
		fprintf( fp, "% 5s  ", "Az" );
		fprintf( fp, "% 5s  ", "flags" );
		fprintf( fp, "% 9s ", "eph_avail" );
		fprintf( fp, "% 8s ", "L1_track" );
		fprintf( fp, "% 8s ", "L2_track" );
		fprintf( fp, "\n" );

		for( isat = 0; isat < r2opkt->satellite_count; isat++ ) {

			si = gettbl( r2opkt->satellites, isat );

			fprintf( fp, "\t          " );
			fprintf( fp, "% 6d  ", si->sv_prn );
			fprintf( fp, "% 5d  ", si->elevation );
			fprintf( fp, "% 5d  ", si->azimuth );
			fprintf( fp, "% 5d  ", si->prn_flags );
			fprintf( fp, "% 8d  ", si->ephemeris_available );
			fprintf( fp, "% 8d  ", si->L1_track );
			fprintf( fp, "% 8d  ", si->L2_track );
			fprintf( fp, "\n" );
		}
	}

	fprintf( fp, "\n" );

	return;
}

int
buf_intake( ImportThread *it ) 
{
	unsigned short received_checksum;
	unsigned short recomputed_checksum;
	unsigned char *cp;
	RYO2orbPacket *r2opkt;
	int	isat;
	SatelliteInfo *si;

	cp = it->buf + it->nbytes - 2;
	uvs2hs( &cp, &received_checksum, 1 );

	recomputed_checksum = RYOChecksum( it->buf, it->nbytes-2 );

	if( recomputed_checksum != received_checksum ) {
		
		elog_complain( 0, "Checksum mismatch! Skipping packet\n" );

		return -1;
	}

	r2opkt = new_RYO2orbPacket();

	cp = it->buf + 4;
	r2opkt->message_type = (int) *cp;
	cp++;

	if( r2opkt->message_type != 1 ) {

		elog_complain( 0, "Only messagees of type 1 (Message ID 0x01) "
			     "are currently supported by ryo2orb. " 
			     "Skipping packet\n" );

		free_RYO2orbPacket( r2opkt );

		return -1;
	}

	vs2hi( &cp, &r2opkt->GPS_week, 1 );

	vi2hi( &cp, &r2opkt->GPS_millisecond, 1 );

	r2opkt->time = GPS_epoch + r2opkt->GPS_week * 7 * 24 * 3600 + 
		       r2opkt->GPS_millisecond / 1000 - GPS_leapseconds;

	r2opkt->site_index = (int) *cp;
	cp++;

	r2opkt->site_count = (int) *cp;
	cp++;

	strncpy( r2opkt->site_id, (char *) cp, 6 );
	cp += 6;
	r2opkt->site_id[6] = '\0';

	vd2hd( &cp, &r2opkt->XYZT[0], 4 );

	r2opkt->position_byte = (int) *cp;
	cp++;

	switch( r2opkt->position_byte & 3 ) {
	case 0:
		r2opkt->position_signal = RYO_SIGNAL_UNDETERMINED;
		break;
	case 1:
		r2opkt->position_signal = RYO_SIGNAL_L1;
		break;
	case 2:
		r2opkt->position_signal = RYO_SIGNAL_L1L2;
		break;
	default:
		elog_complain( 0, "L1/L2 bits in position-quality field "
			     "not understood!\n" );
		break;
	}
	
	switch( ( r2opkt->position_byte >> 2 ) & 7 ) {
	case 1:
		r2opkt->position_method = RYO_METHOD_UNDETERMINED;
		break;
	case 2:
		r2opkt->position_method = RYO_METHOD_ABSCODE;
		break;
	case 3:
		r2opkt->position_method = RYO_METHOD_RELCODE;
		break;
	case 4:
		r2opkt->position_method = RYO_METHOD_PHASEPLUSCODE;
		break;
	default:
		elog_complain( 0, "calculation-method bits in position-quality "
			     "field not understood!\n" );
		break;
	}

	r2opkt->flags_byte = (int) *cp;
	cp++;

	if( r2opkt->flags_byte & 1 ) {
		r2opkt->xyz_cov_present = 1;
	}

	if( r2opkt->flags_byte & 2 ) {
		r2opkt->tropo_cov_present = 1;
	}

	if( r2opkt->flags_byte & 4 ) {
		r2opkt->sat_info_present = 1;
	}

	if( r2opkt->xyz_cov_present ) {

		vd2hd( &cp, &r2opkt->variance_scale, 1 );
		vd2hd( &cp, &r2opkt->variance_X, 1 );
		vd2hd( &cp, &r2opkt->variance_Y, 1 );
		vd2hd( &cp, &r2opkt->variance_Z, 1 );
		vd2hd( &cp, &r2opkt->covariance_YX, 1 );
		vd2hd( &cp, &r2opkt->covariance_YZ, 1 );
		vd2hd( &cp, &r2opkt->covariance_ZX, 1 );
	}

	if( r2opkt->tropo_cov_present ) {

		vd2hd( &cp, &r2opkt->variance_T, 1 );
		vd2hd( &cp, &r2opkt->covariance_TX, 1 );
		vd2hd( &cp, &r2opkt->covariance_TY, 1 );
		vd2hd( &cp, &r2opkt->covariance_TZ, 1 );
	}

	if( r2opkt->sat_info_present ) {

		r2opkt->satellite_count = (int) *cp;
		cp++;

		vd2hd( &cp, &r2opkt->pdop, 1 );

		for( isat = 0; isat < r2opkt->satellite_count; isat++ ) {

			si = new_SatelliteInfo();

			si->sv_prn = (int) *cp;
			cp++;

			si->prn_flags = (int) *cp;
			cp++;

			if( si->prn_flags & 1 ) {
				si->ephemeris_available = 1;
			}

			if( si->prn_flags & 2 ) {
				si->L1_track = 1;
			}

			if( si->prn_flags & 4 ) {
				si->L2_track = 1;
			}

			vs2hi( &cp, &si->elevation, 1 );
			vs2hi( &cp, &si->azimuth, 1 );

			settbl( r2opkt->satellites, isat, si );
		}
	}

	if( VeryVerbose ) {
		elog_notify( 0, "...appending healthy RYO packet to queue\n" );
	}

	pmtfifo_push( RYO2orbPackets_mtf, (void *) r2opkt );

	return 0;
}

int
more_packet_data( ImportThread *it )
{
	int	rc;
	unsigned char	c;
	int	bns_errno;
	unsigned char bytecount[2];
	unsigned char *bytecountp;
	int	retcode = 0;

	while( ( rc = bnsget( it->bnsin, &c, BYTES, 1 ) ) == 0 ) {

		switch( it->sync_state ) {

		case STREAM_SEARCH: 

			if( c == 0x9c ) {
				
				it->nbytes = 0;

				APPEND_BUFFER( it->buf, it->bufsize, it->nbytes, c );

				it->sync_state = STREAM_CONFIRM;

			} else {

				BAD_BYTE( "search", c );
			}

			break;

		case STREAM_CONFIRM: 

			if( c == 0xa5 ) {
				
				APPEND_BUFFER( it->buf, it->bufsize, it->nbytes, c );

				it->sync_state = STREAM_ACCUMULATE;

				bnsget( it->bnsin, bytecount, TWO_BYTES, 1 );

				APPEND_BUFFER( it->buf, it->bufsize, it->nbytes, bytecount[0] );
				APPEND_BUFFER( it->buf, it->bufsize, it->nbytes, bytecount[1] );

				bytecountp = &bytecount[0];
				vs2hs( &bytecountp, &it->intake_packet_bytecount, 1 );

				if( VeryVerbose ) {

					elog_notify( 0, 
					  "Receiving a %d-byte packet...\n",
					  it->intake_packet_bytecount );

				}

			} else {

				it->sync_state = STREAM_SEARCH;

				it->nbytes = 0;

				BAD_BYTE( "search", c );
			}

			break;
	
		case STREAM_ACCUMULATE:

			APPEND_BUFFER( it->buf, it->bufsize, it->nbytes, c );

			if( it->nbytes == it->intake_packet_bytecount ) {

				it->sync_state = STREAM_SEARCH;

				return 1;
			}

			break;

		default:

			BAD_BYTE( "default", c );

			it->sync_state = STREAM_SEARCH;

			break;
		}
	}

	if( bnserr( it->bnsin ) != 0 ) {
			
		bns_errno = bnserrno( it->bnsin );

		if( Verbose ) {

		  if( bns_errno == ECONNRESET ) {

			elog_complain( 1, 
				  "'%s': Connection reset by peer\n",
				  it->name );

		  } else {

			elog_complain( 1, 
				  "'%s': bns error %d\n", 
				  it->name, bns_errno );
		  }
		}

		retcode = -1;

	} else if( bnseof( it->bnsin ) ) {

		if( Verbose ) {

			elog_complain( 1, 
				  "'%s': Connection closed by remote "
				  "export module\n",
				  it->name );
		}

		retcode = -1;
	}

	return retcode;
}

void
flush_packet( Packet *pkt )
{
	PktChannel *pktchan;
	double	time;
	char	srcname[ORBSRCNAME_SIZE];
	static char *orbpkt = 0;
	static int  orbpktnbytes = 0;
	static int  orbpktsz = 0;
	int	ichan;
	char	key[STRSZ];
	char	*s;

	for( ichan = maxtbl( pkt->channels ) - 1; ichan >= 0; ichan-- ) {

		pktchan = gettbl( pkt->channels, ichan );

		if( pktchan->nsamp <= 0 ) {

			sprintf( key, "%s:%s", pktchan->sta, pktchan->chan );

			delarr( Track_packetchans, key );

			deltbl( pkt->channels, ichan );

			freePktChannel( pktchan );
		}
	}

	pkt->nchannels = maxtbl( pkt->channels );;

	pktchan = gettbl( pkt->channels, 0 );
	pkt->time = pktchan->time;

	for( ichan = 1; ichan < pkt->nchannels; ichan++ ) {
		
		pktchan = gettbl( pkt->channels, ichan );

		if( pktchan->time < pkt->time ) {

			pkt->time = pktchan->time;
		}
	}

	stuffPkt( pkt, srcname, &time, &orbpkt, &orbpktnbytes, &orbpktsz );

	if( Multiplex_stations ) {
		
		sprintf( srcname, "%s/MGENC", Net );
	}
		
	if( orbput( Orbfd, srcname, time, orbpkt, orbpktnbytes ) < 0 ) {
		
		elog_complain( 0, "Orbput failed for packet! Trying to retain data internally\n" );

		return;

	} else if( VeryVerbose ) {

		elog_notify( 0, "Wrote '%s' timestamped '%s' to orb\n", 
			srcname, s = strtime( time ) );

		free( s );
	}

	for( ichan = 0; ichan < pkt->nchannels; ichan++ ) {

		pktchan = gettbl( pkt->channels, ichan );

		pktchan->nsamp = 0;
	}

	return;
}

void 
enqueue_sample( Packet *pkt, RYO2orbPacket *r2opkt, char *channel_identifier, double data_value )
{
	PktChannel *pktchan;
	char	key[STRSZ];
	char	*chan;
	int	isample;
	double	new_samprate;

	if( ( chan = getarr( Channel_names, channel_identifier ) ) == NULL ) {

		elog_die( 0, "channel_identifier '%s' missing from channel_names "
			     "in parameter file. Bye.\n" );
	}

	sprintf( key, "%s:%s", r2opkt->site_id, chan );

	if( ( pktchan = getarr( Track_packetchans, key ) ) == (PktChannel *) NULL ) {

		pktchan = newPktChannel();

		pushtbl( pkt->channels, pktchan );

		setarr( Track_packetchans, key, pktchan );

		/* Allot one extra sample so samprate can 
		 * be set dynamically when Max_nsamples_per_channel is 1 */

		allot( int *, pktchan->data, 
			(Max_nsamples_per_channel+1) * sizeof( int ) );

		pktchan->datasz = Max_nsamples_per_channel;

		strcpy( pktchan->segtype, Segtype );

		strcpy( pktchan->net, pkt->parts.src_net );
		strcpy( pktchan->sta, r2opkt->site_id );
		strcpy( pktchan->chan, chan );
		strcpy( pktchan->loc, "" );

		pktchan->calib = 1./Multiply_data_by;
		pktchan->calper = Calper;

		pktchan->nsamp = 0;
		pktchan->samprate = NULL_SAMPRATE;

	} 
	
	if( pktchan->nsamp >= Max_nsamples_per_channel && 
	    pktchan->samprate != NULL_SAMPRATE ) {

		if( VeryVerbose ) {
			
			elog_notify( 0, "Flushing packet because "
			   "pktchan->nsamp (%d) >= "
			   "Max_nsamples_per_channel (%d)\n",
			   pktchan->nsamp, Max_nsamples_per_channel );
		}
		
		flush_packet( pkt );
	}

	isample = pktchan->nsamp;

	if( isample >= 2 ) {

		new_samprate = 1. / 
		   ( r2opkt->time - SAMP2TIME( pktchan->time, pktchan->samprate, pktchan->nsamp - 1 ) );

		if( abs( 1 - pktchan->samprate / new_samprate ) > Samprate_tolerance ) {

			if( Verbose ) {
				elog_notify( 0, "Sample rate change detected: "
					"was %f, now %f. Flushing packet.\n",
					pktchan->samprate, new_samprate );
			}

			flush_packet( pkt );

			isample = pktchan->nsamp;
		}

	} else if( isample == 1 ) {

		pktchan->samprate = 1. / ( r2opkt->time - pktchan->time );
	} 
	
	if( isample == 0 ) {

		pktchan->time = r2opkt->time;
	}

	pktchan->data[isample] = data_value * Multiply_data_by;

	pktchan->nsamp++;

	return;
}

int
packet_ready( Packet *pkt, RYO2orbPacket *r2opkt ) 
{
	PktChannel *pktchan;

	pktchan = gettbl( pkt->channels, 0 );

	if( Multiplex_stations ) {

		if( r2opkt->site_index == r2opkt->site_count && 
	    	    pktchan->nsamp == Max_nsamples_per_channel &&
		    pktchan->samprate != NULL_SAMPRATE ) {

			return 1;
		}

	} else {
	
		if( pktchan->nsamp >= Max_nsamples_per_channel && 
		    pktchan->samprate != NULL_SAMPRATE ) {

			return 1;
		}
	}

	return 0;
}

void
enqueue_ryopkt( RYO2orbPacket *r2opkt, char *net, char *sta )
{
	Packet	*pkt;
	char	*key;

	if( Track_packets == NULL ) {
		
		Track_packets = newarr( 0 );
	}

	if( Track_packetchans == NULL ) {
		
		Track_packetchans = newarr( 0 );
	}

	if( sta == NULL ) {

		key = net;

	} else {
		
		key = sta;
	}

	pkt = getarr( Track_packets, key );

	if( pkt == (Packet *) NULL ) {
		
		pkt = newPkt();

		pkt->pkttype = suffix2pkttype("MGENC");

		strcpy( pkt->parts.src_net, net );
		strcpy( pkt->parts.src_loc, "" );

		if( sta != NULL ) {
			strcpy( pkt->parts.src_sta, sta );
		}

		strcpy( pkt->parts.src_chan, "" );

		setarr( Track_packets, key, (void *) pkt );
	}

	enqueue_sample( pkt, r2opkt, "ITRF_X", r2opkt->XYZT[0] );
	enqueue_sample( pkt, r2opkt, "ITRF_Y", r2opkt->XYZT[1] );
	enqueue_sample( pkt, r2opkt, "ITRF_Z", r2opkt->XYZT[2] );
	enqueue_sample( pkt, r2opkt, "ITRF_T", r2opkt->XYZT[3] );

	if( r2opkt->xyz_cov_present ) {

		enqueue_sample( pkt, r2opkt, 
			"variance_scale", r2opkt->variance_X );

		enqueue_sample( pkt, r2opkt, 
			"variance_X", r2opkt->variance_X );

		enqueue_sample( pkt, r2opkt, 
			"variance_Y", r2opkt->variance_Y );

		enqueue_sample( pkt, r2opkt, 
			"variance_Z", r2opkt->variance_Z );

		enqueue_sample( pkt, r2opkt, 
			"covariance_YX", r2opkt->covariance_YX );

		enqueue_sample( pkt, r2opkt, 
			"covariance_YZ", r2opkt->covariance_YZ );

		enqueue_sample( pkt, r2opkt, 
			"covariance_ZX", r2opkt->covariance_ZX );
	}

	if( r2opkt->tropo_cov_present ) {

		enqueue_sample( pkt, r2opkt, 
			"variance_T", r2opkt->variance_T );

		enqueue_sample( pkt, r2opkt, 
			"covariance_TX", r2opkt->covariance_TX );

		enqueue_sample( pkt, r2opkt, 
			"covariance_TY", r2opkt->covariance_TY );

		enqueue_sample( pkt, r2opkt, 
			"covariance_TZ", r2opkt->covariance_TZ );
	}

	if( packet_ready( pkt, r2opkt ) ) {

		if( VeryVerbose ) {
			elog_notify( 0, "Flushing packet at normal point " 
				"as per multiplexing instructions\n" );
		}

		flush_packet( pkt );
	}

	return;
}

void *
ryo2orb_convert( void *arg ) 
{
	RYO2orbPacket *r2opkt;

	thr_setprio( thr_self(), THR_PRIORITY_CONVERT );


	while( pmtfifo_pop( RYO2orbPackets_mtf, (void **) &r2opkt ) != 0 ) {

		if( VeryVerbose ) {
		
			RYO2orbPacket_dump( stderr, r2opkt );
		}

		if( Multiplex_stations ) {

			enqueue_ryopkt( r2opkt, Net, 0 );

		} else {

			enqueue_ryopkt( r2opkt, Net, r2opkt->site_id );
		}

		free_RYO2orbPacket( r2opkt );
	}

	return NULL;
}

void *
rtd_import( void *arg )
{
	char	*rtd_import = (char *) arg;
	ImportThread *it;
	char	ipaddress[STRSZ];
	int 	port;	
	Tbl	*ssplit;
	char	*s;
	int	rc;
	int	true = 1;

	thr_setprio( thr_self(), THR_PRIORITY_IMPORT );

	it = new_ImportThread( "rtd_import" );

	s = strdup( rtd_import );
	ssplit = split( s, ':' );
	strcpy( it->server_ipaddress, gettbl( ssplit, 0 ) );
	it->server_port = atoi( gettbl( ssplit, 1 ) );
	free( s );
	freetbl( ssplit, 0 );

	if( Verbose ) {

		elog_notify( 0, "Importing from RTD server %s on port %d\n", 
				it->server_ipaddress, it->server_port );
	}

	while( true ) {

		refresh_import_thread( it );

		if( ( rc = more_packet_data( it ) ) > 0 ) {

			buf_intake( it );

		} else if( rc < 0 ) {

			close_import_connection( it );
		}
	}

	return( 0 );
}

int
main( int argc, char **argv )
{
	char	c;
	char	*orbname = 0;
	char	*rtd_server = 0;
	int	rc;
	thread_t rtd_import_tid;
	thread_t ryo_convert_tid;
	char	*pfname = "ryo2orb";
	Pf	*pf;

	elog_init( argc, argv );

	while( ( c = getopt( argc, argv, "vVp:" ) ) != -1 ) {

		switch( c ) {
		case 'v':
			Verbose++;
			break;

		case 'V':
			VeryVerbose++;
			Verbose++;
			break;

		case 'p':
			pfname = optarg;
			break;

		case '?':
		default:
			usage();
			die( 0, "option %c not understood\n", c );
		}
	}

	if( argc - optind != 2 ) {

		usage();
		exit( -1 );

	} else {
		
		rtd_server = argv[optind++];
		orbname = argv[optind++];
	}

	pfread( pfname, &pf );

	GPS_epoch = pfget_time( pf, "GPS_epoch" );
	GPS_leapseconds = pfget_int( pf, "GPS_leapseconds" );

	Multiplex_stations = pfget_int( pf, "multiplex_stations" );
	Max_nsamples_per_channel = pfget_int( pf, "max_nsamples_per_channel" );

	Net = pfget_string( pf, "net" );
	Channel_names = pfget_arr( pf, "channel_names" );

	Samprate_tolerance = pfget_double( pf, "samprate_tolerance" );
	Multiply_data_by = pfget_double( pf, "multiply_data_by" );
	Calper = pfget_double( pf, "calper" );
	Segtype = pfget_string( pf, "segtype" );

	RYO2orbPackets_mtf = pmtfifo_create( PACKET_QUEUE_SIZE, 1, 0 );

	if( Verbose ) {
		elog_notify( 0, 
			"Establishing orb connection to orb '%s'...", 
			orbname );
	}

	if( ( Orbfd = orbopen( orbname, "w&" ) ) < 0 ) {
		
		die( 0, "Failed to open orb '%s' for writing\n", orbname );

	} else {

		elog_notify( 0, "Orb connection established\n" );
	}

	rc = thr_create( NULL, 0, ryo2orb_convert, 0, 0, &ryo_convert_tid );

	if( rc != 0 ) {

		die( 1, "Failed to create ryo_convert thread, "
			"thr_create error %d\n", rc );
	}

	rc = thr_create( NULL, 0, rtd_import, rtd_server, 0, &rtd_import_tid );

	if( rc != 0 ) {

		die( 1, "Failed to create rtd_import thread, "
			"thr_create error %d\n", rc );
	}

	thr_join( rtd_import_tid, (thread_t *) NULL, (void **) NULL );

	if( Verbose ) {

		elog_notify( 0, "Program terminated\n" );
	}

	return( 0 );
}
