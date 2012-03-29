/*
 * ryo2orb.c
 *
 * Import RYO-format real-time GPS data into an Antelope orbserver
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
#include <float.h>
#include <limits.h>
#include <thread.h>
#include <synch.h>
#include <errno.h>
#include <string.h>
#include "stock.h"
#include "coords.h"
#include "db.h"
#include "orb.h"
#include "Pkt.h"
#include "bns.h"
#include "swapbytes.h"
#include "brttutil.h"
#include "xtra.h"

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
#define PI 3.1415926535897932384626433

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
int 	Status_interval_sec;
char	*Rtd_server = 0;
char	*Net = 0;
char	*Match_expr = 0;
double	Samprate_tolerance = 0;
double	Nominal_samprate = NULL_SAMPRATE;
double	Longitude_branchcut_deg;
double	ECEF_semimajor_axis;
double	ECEF_flattening;
Arr 	*Channel_names = 0;
Arr 	*Stachan_calibs = 0;
Arr 	*Sta_statuses = 0;
mutex_t Sta_statuses_mutex;
Arr 	*Track_packets = 0;
Arr 	*Track_packetchans = 0;
Pmtfifo	*RYO2orbPackets_mtf;

/* This is officially declared in ieeefp.h on Solaris
   but is in math.h on linux. Sidestep the compile issues
   by declaring it explicitly. */
extern int finite(double);

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

typedef struct StachanCalib {

	double	offset;
	double	multdataby;
	double	calper;
	char	segtype[2];
	int	announced;

} StachanCalib;

typedef struct StaStatus {

	char	con[10];
	double	pdop;
	int	satellite_count;
	char	psig[20];
	char	pmeth[20];
	char	log[STRSZ];

} StaStatus;

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
	double	lat_radians;
	double	lon_radians;
	double	lat_deg;
	double	lon_deg;
	double	height_m;
	int	satellite_count;
	double	pdop;
	Tbl	*satellites;

} RYO2orbPacket;

void
usage() 
{
	cbanner( "$Date$ $Revision$", 
		 "[-v] [-V] [-p pfname] rtd_ipaddress:port orb",
		 "Kent Lindquist", 
		 "Lindquist Consulting", 
		 "kent@lindquistconsulting.com" );
	
	return;
}

StaStatus *
new_StaStatus()
{
	StaStatus *ss;

	allot( StaStatus *, ss, 1 );

	ss->pdop = -1;
	ss->satellite_count = -1;

	strcpy( ss->con, "waiting" );
	strcpy( ss->psig, "-" );
	strcpy( ss->pmeth, "-" );

	strcpy( ss->log, "" );

	return ss;
}

StaStatus *
add_StaStatus( char *sta )
{
	StaStatus *ss;

	if( ! strcmp( sta, "default" ) ) {

		return (StaStatus *) NULL;
	}

	mutex_lock( &Sta_statuses_mutex );

	if( ( ss = (StaStatus *) getarr( Sta_statuses, sta ) ) == (StaStatus *) NULL ) {
		
		ss = new_StaStatus();

		setarr( Sta_statuses, sta, (void *) ss );
	}

	mutex_unlock( &Sta_statuses_mutex );

	return ss;
}

StaStatus *
get_StaStatus( char *sta ) 
{
	StaStatus *ss;

	mutex_lock( &Sta_statuses_mutex );

	ss = (StaStatus *) getarr( Sta_statuses, sta );

	mutex_unlock( &Sta_statuses_mutex );

	return ss;
}

void
update_StaStatus( RYO2orbPacket *r2opkt )
{
	StaStatus *ss;
	char	line[STRSZ];
	SatelliteInfo *si;
	int	isat;

	if( ( ss = get_StaStatus( r2opkt->site_id ) ) == (StaStatus *) NULL ) {
		
		ss = add_StaStatus( r2opkt->site_id );
	}

	strcpy( ss->con, "yes" );

	if( r2opkt->sat_info_present ) {

		ss->pdop = r2opkt->pdop;
		ss->satellite_count = r2opkt->satellite_count;

	} else {

		ss->pdop = -1;
		ss->satellite_count = -1;
	}

	switch( r2opkt->position_signal ) {
	case RYO_SIGNAL_UNDETERMINED:
		strcpy( ss->psig, "Undetermined" );
		break;
	case RYO_SIGNAL_L1:
		strcpy( ss->psig, "L1" );
		break;
	case RYO_SIGNAL_L1L2:
		strcpy( ss->psig, "L1/L2" );
		break;
	}

	switch( r2opkt->position_method ) {
	case RYO_METHOD_UNDETERMINED:
		strcpy( ss->pmeth, "Undetermined" );
		break;
	case RYO_METHOD_ABSCODE:
		strcpy( ss->pmeth, "Absolute code" );
		break;
	case RYO_METHOD_RELCODE:
		strcpy( ss->pmeth, "Relative code" );
		break;
	case RYO_METHOD_PHASEPLUSCODE:
		strcpy( ss->pmeth, "Phase + code" );
		break;
	}

	if( r2opkt->sat_info_present ) {
		
		sprintf( ss->log, "satellites for %s ", r2opkt->site_id );

		sprintf( line, "(%s;", "SV PRN" );
		strcat( ss->log, line );
		sprintf( line, "%s;", "Elev" );
		strcat( ss->log, line );
		sprintf( line, "%s;", "Az" );
		strcat( ss->log, line );
		sprintf( line, "%s;", "flags" );
		strcat( ss->log, line );
		sprintf( line, "%s;", "eph_avail" );
		strcat( ss->log, line );
		sprintf( line, "%s;", "L1_track" );
		strcat( ss->log, line );
		sprintf( line, "%s):", "L2_track" );
		strcat( ss->log, line );
		sprintf( line, "\n" );
		strcat( ss->log, line );

		for( isat = 0; isat < r2opkt->satellite_count; isat++ ) {

			si = gettbl( r2opkt->satellites, isat );

			sprintf( line, "%3d:", si->sv_prn );
			strcat( ss->log, line );
			sprintf( line, "%4d;", si->elevation );
			strcat( ss->log, line );
			sprintf( line, "%4d;", si->azimuth );
			strcat( ss->log, line );
			sprintf( line, "%2d;", si->prn_flags );
			strcat( ss->log, line );
			sprintf( line, "%2d;", si->ephemeris_available );
			strcat( ss->log, line );
			sprintf( line, "%2d;", si->L1_track );
			strcat( ss->log, line );
			sprintf( line, "%8d", si->L2_track );
			strcat( ss->log, line );
			sprintf( line, "\n" );
			strcat( ss->log, line );
		}

	} else {

		sprintf( ss->log, "" );
	}

	return;
}

void
free_StaStatus( void *ssp )
{
	StaStatus *ss = (StaStatus *) ssp;

	free( ss );

	return;
}

StachanCalib *
new_StachanCalib()
{
	StachanCalib *scc = 0;

	allot( StachanCalib *, scc, 1 );

	memset( scc, '\0', sizeof( *scc ) );

	return scc;
}

StachanCalib *
dup_StachanCalib( void *sccp )
{
	StachanCalib *scc = (StachanCalib *) sccp;
	StachanCalib *dup = 0;

	dup = new_StachanCalib();

	memcpy( dup, scc, sizeof( *scc ) );

	return dup;
}

StachanCalib *
get_StachanCalib( char *site_id, char *channel_identifier ) 
{
	char	key_specific[STRSZ];
	char	key_default[STRSZ];
	StachanCalib *scc;

	sprintf( key_specific, "%s:%s", site_id, channel_identifier );
	sprintf( key_default, "%s:%s", "default", channel_identifier );

	scc = (StachanCalib *) getarr( Stachan_calibs, key_specific );

	if( scc == (StachanCalib *) NULL ) {

		if( VeryVerbose ) {

			elog_notify( 0, "Couldn't find customized "
				  "offsets for %s:%s; "
				  "using defaults.\n", 
				  site_id, channel_identifier );
	 	}
		
		scc = (StachanCalib *) getarr( Stachan_calibs, key_default );

		scc = dup_StachanCalib( scc );

		setarr( Stachan_calibs, key_specific, (void *) scc );
	}

	return scc;
}

void
show_StachanCalib( FILE *fp, StachanCalib *scc )
{
	fprintf( fp, "offset %f\n", scc->offset );;
	fprintf( fp, "multdataby %f\n", scc->multdataby );
	fprintf( fp, "calper %f\n", scc->calper );
	fprintf( fp, "segtype %s\n", scc->segtype );
	fprintf( fp, "announced %d\n", scc->announced );


}

void
announce_StachanCalib( void *sccp, char *sta, char *chan ) 
{
	StachanCalib *scc = (StachanCalib *) sccp;
	static Dbptr db = { dbINVALID, dbINVALID, dbINVALID, dbINVALID };

	if( db.database == dbINVALID ) {

		db = dbtmp( "rt1.0" );

		db = dblookup( db, "", "wfoffset", "", "dbSCRATCH" );
	}

	if( abs( scc->offset ) < DBL_MIN ) {
		
		return;
	}

	dbputv( db, 0, "sta", sta, 
		       "chan", chan, 
		       "time", now(), 
		       "endtime", 9999999999.999,
		       "valoffset", scc->offset,
		       0 );

	if( db2orbpkt( db, Orbfd ) < 0 ) {
		
		elog_complain( 0, "Orbput failed for wfoffset packet!\n" );

	} else {
		
		scc->announced++;
	}

	return;
}

void
free_StachanCalib( void *sccp )
{
	StachanCalib *scc = (StachanCalib *) sccp;

	free( scc );

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
	int	val;

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

			val = 1;

			if( setsockopt( it->so, SOL_SOCKET, SO_KEEPALIVE, 
					&val, sizeof(int) ) ) {
	
				elog_die( 1, 
					"Failed to set KEEPALIVE for socket\n" );
			}

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
	unsigned int nByte;

	const unsigned char *pB = a_pbyBuffer;

	for( nByte = 0; nByte < a_nByteCount; nByte++ ) {

		nChecksum = (unsigned short) (nChecksum + *pB);

		pB++;
	}

	return nChecksum;
}


/* Translation from ECEF coords to geodetic lat,lon,height.
 * Modified from a routine from Yehuda Bock 2005 */

/* "a" and "f" are semi-major axis and flattening (for the ellipsoid model),
 * i.e. ECEF spheroid parameters */

void
ECEF_to_latlonheight_radians(
	double m_x, 
	double m_y, 
	double m_z,
	double a, 
	double f,
	double *lat, 
	double *lon, 
	double *hi )
{
	double rsq=0, r=0, e=0, rho=0, nlatr=0;
	double cphi=0, dr=0, dz=0, dht=0, dnlatr=0;
	double flatfn=0, funsq=0, sphi=0, g1=0, g2=0;
	double fX;
	int i=0;

	if(m_x==0 && m_y==0 && m_z==0) {
		*lat = 0;
		*lon = 0;
		*hi = 0;
		return;
	}

	flatfn = (2.0 - f)*f;
	funsq = (1.0 - f)*(1.0 - f);

	rsq = m_x*m_x + m_y*m_y;
	r = sqrt(rsq);
	fX = m_x;
	if(!(fX)) {
		fX += 0.0001;
	}
	e = atan2(m_y,fX);
	if(e<0.0) {
		e = e + 2 * PI;
	}
	rho = sqrt(m_z*m_z + rsq);
	sphi = m_z/rho;
	nlatr = asin(sphi);
	*hi = rho - a*(1.0 - f*sphi*sphi);

	for(i=0; i<10; i++) {
		sphi = sin(nlatr);
		cphi = cos(nlatr);
		g1 = a/sqrt(1.0 - flatfn*sphi*sphi);
		g2 = g1*funsq + *hi;
		g1 = g1 + *hi;
		dr = r - g1*cphi;
		dz = m_z - g2*sphi;
		dht = dr*cphi + dz*sphi;
		*hi = *hi + dht;
		dnlatr = (dz*cphi - dr*sphi)/(a + *hi);
		nlatr = nlatr + dnlatr;
		if((fabs(dnlatr)<=(1.0e-14)) &&
		   ((fabs(dht)/(a + *hi))<=(1.0e-14))) {
			break;
		}
	}

	*lat = nlatr;
	*lon = e;

	return;
}

double 
wrap_phase( double branchcut_deg, double angle_deg )
{
	while( angle_deg < branchcut_deg - 360 ) {

		angle_deg += 360;
	}

	while( angle_deg > branchcut_deg ) {

		angle_deg -= 360;
	}

	return angle_deg;
}

void 
add_geodetics( RYO2orbPacket *r2opkt  )
{

	ECEF_to_latlonheight_radians( r2opkt->XYZT[0], 
				      r2opkt->XYZT[1], 
				      r2opkt->XYZT[2], 
				      ECEF_semimajor_axis, 
				      ECEF_flattening,
				      &r2opkt->lat_radians, 
				      &r2opkt->lon_radians,
				      &r2opkt->height_m );
					
	r2opkt->lat_deg = deg( r2opkt->lat_radians );
	r2opkt->lon_deg = deg( r2opkt->lon_radians );

	r2opkt->lon_deg = wrap_phase( Longitude_branchcut_deg, r2opkt->lon_deg );

	return;
}


SatelliteInfo *
new_SatelliteInfo()
{
	SatelliteInfo *si;

	allot( SatelliteInfo *, si, 1 );

	memset( si, '\0', sizeof( *si ) );

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

	fprintf( fp, "\t% 20s:\t%f\n", "LAT(rad)", r2opkt->lat_radians );
	fprintf( fp, "\t% 20s:\t%f\n", "LON(rad)", r2opkt->lon_radians );
	fprintf( fp, "\t% 20s:\t%f\n", "LAT(deg)", r2opkt->lat_deg );
	fprintf( fp, "\t% 20s:\t%f\n", "LON(deg)", r2opkt->lon_deg );
	fprintf( fp, "\t% 20s:\t%f\n", "HEIGHT(m)", r2opkt->height_m );

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

				bnsget( it->bnsin, bytecount, BYTES, 1 );
				bnsget( it->bnsin, bytecount+1, BYTES, 1 );

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
send_log( char *log, char *srcname )
{
	static Packet *pkt = 0;
	static char *buf = 0;
	static int nbytes = 0;
	static int packetsz = 0;
	char	auto_srcname[STRSZ];
	double	time;

	if( ! strcmp( log, "" ) ) {
		
		return;
	}

	if( pkt == (Packet *) NULL ) {

		pkt = newPkt();
		pkt->pkttype = suffix2pkttype( "log" );
	}

	pkt->string = strdup( log );
	pkt->string_size = strlen(log) + 1;

	pkt->time = now();

	if( pkt->string_size > 512 ) {
		elog_complain( 0, "Warning: log message exceeds 512 bytes\n" );
	}

	if( buf == 0 ) {
		/* stuff_log appears not to allocate correctly? */
		allot( char *, buf, 1024 );
		packetsz = 1024;
	}

	if( stuffPkt( pkt, auto_srcname, &time, &buf, &nbytes, &packetsz ) < 0 ) {
		
		elog_complain( 0, "stuffPkt failed for log message!\n" );

		free( pkt->string );
		pkt->string = 0;
		pkt->string_size = 0;

		return;
	}

	if( orbput( Orbfd, srcname, now(), buf, nbytes ) < 0 ) {

		elog_complain( 0, "orbput failed for log message!\n" );
	}

	free( pkt->string );
	pkt->string = 0;
	pkt->string_size = 0;

	return;
}

void *
ryo2orb_status( void *arg )
{
	void	*vstk;
	char	*pf_string;
	char	srcname[ORBSRCNAME_SIZE];
	char	log_srcname[ORBSRCNAME_SIZE];
	char	line[STRSZ];
	char	hostname[STRSZ];
	Packet *pkt = 0;
	char *buf = 0;
	int nbytes = 0;
	int packetsz = 0;
	double	time;
	Tbl	*keys;
	int	ikey;
	char	*key;
	StaStatus *ss;
	int	true = 1;

	my_hostname( hostname );

	pkt = newPkt();
	pkt->pkttype = suffix2pkttype( "pf" );

	while( true ) {

		vstk = 0;

		pushstr( &vstk, "target ryo2orb\n" );
		pushstr( &vstk, "model ryo\n" );
		pushstr( &vstk, "type dl\n" );

		sprintf( line, "pid %d\n", (int) getpid() );
		pushstr( &vstk, line );

		sprintf( line, "itvl %d\n", Status_interval_sec ); 
		pushstr( &vstk, line );

		sprintf( line, "hostname %s\n", hostname );
		pushstr( &vstk, line );
	
		pushstr( &vstk, "dls &Arr{\n" );

		mutex_lock( &Sta_statuses_mutex );

		keys = keysarr( Sta_statuses );

		for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {

			key = gettbl( keys, ikey );

			ss = getarr( Sta_statuses, key );

			if( ss == (StaStatus *) NULL ) {

				elog_complain( 0, 
					"Unexpected null sta in status info!\n" );
				continue;
			}

			sprintf( line, "%s &Arr{\n", key );
			pushstr( &vstk, line );

			sprintf( line, "con %s\n", ss->con );
			pushstr( &vstk, line );

			sprintf( line, "inp tcp:%s\n", Rtd_server );
			pushstr( &vstk, line );

			sprintf( line, "pdop %f\n", ss->pdop );
			pushstr( &vstk, line );

			sprintf( line, "satcnt %d\n", ss->satellite_count );
			pushstr( &vstk, line );

			sprintf( line, "psig %s\n", ss->psig );
			pushstr( &vstk, line );

			sprintf( line, "pmeth %s\n", ss->pmeth );
			pushstr( &vstk, line );

			pushstr( &vstk, "}\n" );
		}	

		for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {

			key = gettbl( keys, ikey );

			ss = getarr( Sta_statuses, key );

			sprintf( log_srcname, "%s_%s/log", Net, key );

			send_log( ss->log, log_srcname );  
		}


		freetbl( keys, 0 );
	
		mutex_unlock( &Sta_statuses_mutex );

		pushstr( &vstk, "}\n" );
	
		pf_string = popstr( &vstk, 1 );

		pfcompile( pf_string, &pkt->pf );

		pkt->time = now();

		if( stuffPkt( pkt, srcname, &time, &buf, &nbytes, &packetsz ) < 0 ) {
			
			elog_complain( 0, "stuffPkt failed for status packet!\n" );
		}

		sprintf( srcname, "%s/pf/st", Net );

		if( orbput( Orbfd, srcname, time, buf, nbytes ) < 0 ) {
			
			elog_complain( 0, "Orbput failed for status packet!\n" );
		}

		free( pf_string );

		pffree( pkt->pf );
		pkt->pf = 0;

		sleep( Status_interval_sec );
	}

	return NULL;
}

void
flush_packet( Packet *pkt )
{
	PktChannel *pktchan;
	double	time = 0;
	char	srcname[ORBSRCNAME_SIZE] = "";
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

	pkt->nchannels = maxtbl( pkt->channels );

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
samprate_verify( Packet *pkt, RYO2orbPacket *r2opkt )
{
	PktChannel *pktchan;
	double  new_samprate;
	int	ichannel;

	for( ichannel = 0; ichannel < pkt->nchannels; ichannel++ ) {

		pktchan = gettbl( pkt->channels, ichannel );

		if( pktchan->nsamp >= 2 &&
		    ! strcmp( pktchan->sta, r2opkt->site_id ) ) {

			new_samprate = 1. / 
		   	( r2opkt->time - 
		     	SAMP2TIME( pktchan->time, 
				   pktchan->samprate, 
				   pktchan->nsamp - 1 ) );

			if( ! finite( new_samprate ) ||
			      abs( 1 - Nominal_samprate / new_samprate ) >
				Samprate_tolerance ) {

				if( Verbose ) {
				elog_notify( 0, "New sample rate would be %f, outside tolerance %f of nominal rate %f. "
					"Flushing packet.\n",
					new_samprate, Samprate_tolerance, Nominal_samprate );
				}

				flush_packet( pkt );

				break;;
			}
		}
	}

	return;
}

void 
enqueue_sample( Packet *pkt, 
		RYO2orbPacket *r2opkt, 
		char *channel_identifier, 
		double data_value )
{
	PktChannel *pktchan;
	char	key[STRSZ];
	char	*chan;
	double	new_samprate;
	StachanCalib *scc;
	char	*s;

	if( ( chan = getarr( Channel_names, channel_identifier ) ) == NULL ) {

		elog_die( 0, "channel_identifier '%s' missing from channel_names "
			     "in parameter file. Bye.\n", channel_identifier );
	}

	scc = get_StachanCalib( r2opkt->site_id, channel_identifier );

	if( scc == (StachanCalib *) NULL ) {

		elog_complain( 0, "Couldn't find configured or default "
				  "offsets for %s:%s: "
				  "Can't enqueue sample!!!\n", 
				  r2opkt->site_id, channel_identifier );

		return;
	}

	if( scc->announced == 0 ) {

		announce_StachanCalib( scc, r2opkt->site_id, chan );
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

		strcpy( pktchan->segtype, scc->segtype );

		strcpy( pktchan->net, pkt->parts.src_net );
		strcpy( pktchan->sta, r2opkt->site_id );
		strcpy( pktchan->chan, chan );
		strcpy( pktchan->loc, "" );

		pktchan->calib = 1./scc->multdataby;
		pktchan->calper = scc->calper;
		pktchan->duser1 = scc->offset;

		pktchan->nsamp = 0;
		pktchan->samprate = NULL_SAMPRATE;
	} 
	
	if( pktchan->nsamp == 0 ) {

		pktchan->time = r2opkt->time;
	}

	if( pktchan->nsamp == 1 ) {

		if( r2opkt->time != pktchan->time ) {

			pktchan->samprate = 1. / ( r2opkt->time - pktchan->time );
		} else {
			
			/* SCAFFOLD */	
			elog_complain( 0, "Unexpected error appending sample"
			  " for site_id '%s'--new packet time matches "
			  "previous. Duplicate input data?? Skipping! \n",
			  r2opkt->site_id, s = strtime( pktchan->time ) ); 
			return;
			   
		}
	} 

	pktchan->nsamp++;

	/* SCAFFOLD prevent memory overruns under current structure: */
	insist( pktchan->nsamp <= Max_nsamples_per_channel + 1 );

	pktchan->data[pktchan->nsamp-1] =
			( data_value - scc->offset ) * scc->multdataby;

	return;
}

int
packet_ready( Packet *pkt, RYO2orbPacket *r2opkt ) 
{
	PktChannel *pktchan;

	if( pkt->nchannels <= 0 ) {

		return 0;
	}

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

	samprate_verify( pkt, r2opkt );

	enqueue_sample( pkt, r2opkt, "ITRF_X", r2opkt->XYZT[0] );
	enqueue_sample( pkt, r2opkt, "ITRF_Y", r2opkt->XYZT[1] );
	enqueue_sample( pkt, r2opkt, "ITRF_Z", r2opkt->XYZT[2] );
	enqueue_sample( pkt, r2opkt, "ITRF_T", r2opkt->XYZT[3] );

	enqueue_sample( pkt, r2opkt, "LAT_RAD", r2opkt->lat_radians );
	enqueue_sample( pkt, r2opkt, "LON_RAD", r2opkt->lon_radians );

	enqueue_sample( pkt, r2opkt, "LAT_DEG", r2opkt->lat_deg );
	enqueue_sample( pkt, r2opkt, "LON_DEG", r2opkt->lon_deg );

	enqueue_sample( pkt, r2opkt, "HEIGHT_M", r2opkt->height_m );

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

	pkt->nchannels = maxtbl( pkt->channels );;

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
	int	rc;
	int	true = 1;

	thr_setprio( thr_self(), THR_PRIORITY_CONVERT );

	while( true ) {

		rc = pmtfifo_pop( RYO2orbPackets_mtf, (void **) &r2opkt );

		switch( rc ) {
		case PMTFIFO_ERROR:
			elog_complain( 0,
				"pmtfifo_pop returned PMTFIFO_ERROR!\n" );
			elog_clear_register( 1 );
			continue;
			break;
		case PMTFIFO_RELEASED:
			elog_complain( 0,
				"pmtfifo_pop returned PMTFIFO_RELEASED!\n" );
			continue;
			break;
		case PMTFIFO_TIMEOUT:
			elog_complain( 0,
				"pmtfifo_pop returned PMTFIFO_TIMEOUT!\n" );
			continue;
			break;
		case PMTFIFO_NODATA:
			/* DEBUG elog_complain( 0,
				"pmtfifo_pop returned PMTFIFO_NODATA!\n" ); */
			sleep( 1 ); /* DEBUG */
			continue;
			break;
		case PMTFIFO_OK:
			/* Fallthrough */
			break;
		default:
			elog_complain( 0,
				"pmtfifo_pop returned unknown code!\n" );
			continue;
			break;
		}

		if( r2opkt == NULL ) {

			elog_complain( 0,
			"unexpected null pointer in ryo2orb_convert!!\n" );

			continue;
		}

		if( Match_expr != NULL ) {
			
			if( ! strmatches( r2opkt->site_id, Match_expr, 0 ) ) {

				if( VeryVerbose ) {

					elog_notify( 0,
						"Skipping packet for '%s' "
					 	"(not in match expression)\n",
					 	r2opkt->site_id );
				}

				free_RYO2orbPacket( r2opkt );

				continue;
			}
		}

		add_geodetics( r2opkt );

		if( VeryVerbose ) {
		
			RYO2orbPacket_dump( stderr, r2opkt );
		}

		if( Multiplex_stations ) {

			enqueue_ryopkt( r2opkt, Net, 0 );

		} else {

			enqueue_ryopkt( r2opkt, Net, r2opkt->site_id );
		}

		update_StaStatus( r2opkt );

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

void
load_stachan_calibs( Pf *pf ) 
{
	Pf	*pfstachan_calibs;
	Tbl	*stas;
	Arr	*chans;
	Tbl	*chanids;
	char	*aline;
	char	*sta;
	char	*chanid;
	int	ista;
	int	ichanid;
	char	key[STRSZ];
	StachanCalib *scc;

	Stachan_calibs = newarr( 0 );
	Sta_statuses = newarr( 0 );

	pfget( pf, "stachan_calibs", (void **) &pfstachan_calibs );

	stas = pfkeys( pfstachan_calibs );

	for( ista = 0; ista < maxtbl( stas ); ista++ ) {

		sta = gettbl( stas, ista );

		add_StaStatus( sta );

		chans = pfget_arr( pfstachan_calibs, sta );

		chanids = keysarr( chans );

		for( ichanid = 0; ichanid < maxtbl( chanids ); ichanid++ ) {

			chanid = gettbl( chanids, ichanid );

			scc = new_StachanCalib(); 

			aline = (char *) getarr( chans, chanid );

			sscanf( aline, "%lg %lg %lg %s", 
					&scc->offset,
					&scc->multdataby, 
					&scc->calper,
					scc->segtype );

			if( VeryVerbose ) {

				elog_notify( 0, 
					"Values for %s:%s:\n"
					"\t\toffset\t\t%f\n\t\tdatamultby\t"
					"%g\n\t\tcalper\t\t%f\n\t\tsegtype\t\t%s\n",
					sta, chanid, 
					scc->offset, 
					scc->multdataby, 
					scc->calper, 
					scc->segtype );
			}

			sprintf( key, "%s:%s", sta, chanid );

			setarr( Stachan_calibs, key, (void *) scc );
		}

		freetbl( chanids, 0 );

		freearr( chans, 0 );
	}

	freetbl( stas, 0 );
}

int
main( int argc, char **argv )
{
	char	c;
	char	*orbname = 0;
	int	rc;
	thread_t rtd_import_tid;
	thread_t ryo_convert_tid;
	thread_t ryo_status_tid;
	char	*pfname = "ryo2orb";
	Pf	*pf;

	elog_init( argc, argv );

	while( ( c = getopt( argc, argv, "vVm:p:" ) ) != -1 ) {

		switch( c ) {
		case 'v':
			Verbose++;
			break;

		case 'V':
			VeryVerbose++;
			Verbose++;
			break;

		case 'm':
			Match_expr = optarg;
			break;

		case 'p':
			pfname = optarg;
			break;

		case '?':
		default:
			usage();
			elog_die( 0, "option %c not understood\n", c );
		}
	}

	if( argc - optind != 2 ) {

		usage();
		exit( -1 );

	} else {
		
		Rtd_server = argv[optind++];
		orbname = argv[optind++];
	}

	if( Verbose ) {
		
		elog_notify( 0, 
			"ryo2orb revision $Revision$ "
			"starting at %s UTC\n",
			strtime( now() ) );
	}

	pfread( pfname, &pf );

	GPS_epoch = pfget_time( pf, "GPS_epoch" );
	GPS_leapseconds = pfget_int( pf, "GPS_leapseconds" );

	Multiplex_stations = pfget_boolean( pf, "multiplex_stations" );

	if( Match_expr != NULL && Multiplex_stations != 0 ) {

		elog_complain( 0, "Multiplexing stations is not currently "
			  "supported when subsetting stations; turning " 
			  "multiplexing off.\n" );

		Multiplex_stations = 0;
	}

	Max_nsamples_per_channel = pfget_int( pf, "max_nsamples_per_channel" );
	Status_interval_sec = pfget_int( pf, "status_interval_sec" );

	Net = pfget_string( pf, "net" );

	Samprate_tolerance = pfget_double( pf, "samprate_tolerance" );
	Nominal_samprate = pfget_double( pf, "nominal_samprate" );
	Longitude_branchcut_deg = pfget_double( pf, "longitude_branchcut_deg" );
	ECEF_semimajor_axis = pfget_double( pf, "ECEF_semimajor_axis" );
	ECEF_flattening = pfget_double( pf, "ECEF_flattening" );

	Channel_names = pfget_arr( pf, "channel_names" );

	load_stachan_calibs( pf );

	RYO2orbPackets_mtf = pmtfifo_create( PACKET_QUEUE_SIZE, 1, 0 );

	if( RYO2orbPackets_mtf->block != 0 && 
	    RYO2orbPackets_mtf->block != 1 ) {
		
		elog_complain( 0, "DEBUG WARNING: pmt fifo block is corrupted! (debugging work in progress)\n" );
	} else {

		elog_notify( 0, "DEBUG NOTIFY: pmt fifo block OK.\n" );
	}

	sleep( 1 ); /* DEBUG */

	if( Verbose ) {
		elog_notify( 0, 
			"Establishing orb connection to orb '%s'...", 
			orbname );
	}

	mutex_init( &Sta_statuses_mutex, USYNC_THREAD, NULL );

	rc = thr_create( NULL, 0, ryo2orb_status, 0, 0, &ryo_status_tid );

	if( rc != 0 ) {

		elog_die( 1, "Failed to create ryo_status thread, "
			"thr_create error %d\n", rc );
	}

	if( ( Orbfd = orbopen( orbname, "w&" ) ) < 0 ) {
		
		elog_die( 0, "Failed to open orb '%s' for writing\n", orbname );

	} else {

		elog_notify( 0, "Orb connection established\n" );
	}

	if( Verbose ) {

		elog_notify( 0, "Using ECEF_semimajor_axis = %f\n", ECEF_semimajor_axis );
		elog_notify( 0, "Using ECEF_flattening = %.18f\n", ECEF_flattening );
	}

	rc = thr_create( NULL, 0, ryo2orb_convert, 0, 0, &ryo_convert_tid );

	if( rc != 0 ) {

		elog_die( 1, "Failed to create ryo_convert thread, "
			"thr_create error %d\n", rc );
	}

	rc = thr_create( NULL, 0, rtd_import, Rtd_server, 0, &rtd_import_tid );

	if( rc != 0 ) {

		elog_die( 1, "Failed to create rtd_import thread, "
			"thr_create error %d\n", rc );
	}

	thr_join( rtd_import_tid, (thread_t *) NULL, (void **) NULL );

	if( Verbose ) {

		elog_notify( 0, "Program terminated\n" );
	}

	return( 0 );
}
