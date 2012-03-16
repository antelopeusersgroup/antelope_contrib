/* orb_quake_email
 *
 * K. Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1998
 */

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include "db.h"
#include "stock.h"
#include "coords.h"
#include "orb.h"
#include "xtra.h"
#include "Pkt.h"
#include "pf.h"

static char *Placedb;
static char *Mailx;
static double Dist_cutoff_km = 0;

char *compass_from_azimuth( double );

int dborigin_to_message( Dbptr, char *, char *, char **, int *, char * );
void distance_report( double, double, char * );
char *filter_recipients( Dbptr, Arr * );
void send_messages( char *, char *, char * );

void 
distance_report( lat, lon, report )
double	lat;
double	lon;
char	*report;
{
	Dbptr	db;
	char	expr[STRSZ];
	char	place[STRSZ];
	Tbl	*expr_tbl;
	Expression *dist_expr;
	Expression *az_expr;
	int	nrecs;
	double	dist_km, dist_mi;
	double	azimuth;
	char	*compass;

	if( dbopen( Placedb, "r", &db ) != 0 ) {

		elog_complain( 0, "Unable to open place database %s\n", Placedb );
		return;
	}

	db = dblookup( db, 0, "places", 0, 0 );

	dbquery( db, dbRECORD_COUNT, &nrecs );

	sprintf( expr, "azimuth(lat,lon,%f,%f)", lat, lon );
	dbex_compile( db, expr, &az_expr, 0 );

	sprintf( expr, "distance(lat,lon,%f,%f)*111.195", lat, lon );
	dbex_compile( db, expr, &dist_expr, 0 );

	expr_tbl = strtbl( expr, 0 );

	db = dbsort( db, expr_tbl, 0, 0 );

	freetbl( expr_tbl, 0 );

	strcpy( report, "This earthquake was:\n\n" );

	for( db.record = 0; db.record < nrecs; db.record++ ) {
		dbex_eval( db, dist_expr, 0, &dist_km );	

		if( Dist_cutoff_km > 0 && dist_km > Dist_cutoff_km ) {
			continue;
		}

		dist_mi = dist_km / 1.609;

		dbex_eval( db, az_expr, 0, &azimuth );
		compass = compass_from_azimuth( azimuth );
		
		dbgetv( db, 0, "place", place, 0 );
		sprintf( report, "%s\t%6.0f miles (%6.0f km) %3s of %s\n",
				 report, dist_mi, dist_km, compass, place );
	}
	
	dbex_free( dist_expr );
	dbex_free( az_expr );

	dbclose( db );
}

int
dborigin_to_message( db, header, footer, message, msglen, subject )
Dbptr	db;
char	*header;
char	*footer;
char	**message;
int	*msglen;
char	*subject;
{
	double	lat, lon, depth, time, ml;
	char	quake[10000];
	char	lines[STRSZ];
	char	distances[10000];
	int	needed;
	int	ndef;
        char    author[STRSZ];
	int	evid;
	int 	rc = 0;

	rc = dbgetv( db, 0, 
		       "lat", &lat,
	  	       "lon", &lon,
		       "depth", &depth,
		       "time", &time, 
		       "ml", &ml,
		       "ndef", &ndef,
		       "evid", &evid,
		       "auth", author,
		       0 );

	if( rc ) {
		elog_clear_register( 1 );
		elog_complain( 1, "Failed to get info from database pointer\n" );
		return -1;
	}

	if( ml == -999 ) {
		sprintf( subject, "Earthquake at %5.2f, %7.2f", lat, lon );
	} else {
		sprintf( subject, "Ml %3.1f Earthquake at %5.2f, %7.2f",
				  ml, lat, lon );
	}

	sprintf( quake, "\tLat:   %5.2f\n", lat );
	sprintf( lines, "\tLon:   %7.2f\n", lon );
	strcat( quake, lines );
	sprintf( lines, "\tDepth: %3.0f km\n", depth );
	strcat( quake, lines );
	sprintf( lines, "\tTime:  %s GMT\n\n", strtime( time ) );
	strcat( quake, lines );
	if( ml == -999 ) {
		sprintf( lines, 
			"\tMl: insufficient data for automatic solution\n\n" );
		strcat( quake, lines );
	} else {
		sprintf( lines, "\tMl: %3.1f\n\n", ml );
		strcat( quake, lines );
	}

	sprintf( lines, "\t%d phases used in solution\n", ndef );
	strcat( quake, lines );

	distance_report( lat, lon, distances );

	needed = strlen( header ) + strlen( quake ) +
		 strlen( distances ) + strlen( footer ) + 20;

	if( *msglen < needed ) {
		reallot( char *, *message, needed );
		*msglen = needed;
	}

	sprintf( *message, "\n%s\n%s\n%s\n%s\n",
			header, quake, distances, footer );

	return 0;
}

char *
filter_recipients( db, recipients )
Dbptr	db;
Arr	*recipients;
{
	Dbptr	dbfiltered;
	Tbl	*targets;
	Tbl	*interested;
	char	*addresses_string;
	char	*party;
	char	*condition;
	int	nrecs;
	int	i;

	db.record = dbadd( db, 0 );

	interested = keysarr( recipients );

	targets = newtbl( 0 );

	for( i = 0; i < maxtbl( interested ); i++ ) {

		party = gettbl( interested, i );	
		condition = getarr( recipients, party );

		if( ! strcmp( condition, "" ) ) {

			pushtbl( targets, party );

		} else {

			dbfiltered = dbsubset( db, condition, 0 );
			dbquery( dbfiltered, dbRECORD_COUNT, &nrecs );

			if( nrecs == 1 ) {
				pushtbl( targets, party );
			}
		}
	}

	addresses_string = jointbl( targets, "," );

	freetbl( interested, 0 );
	freetbl( targets, 0 );

	dbdelete( db );

	return addresses_string;
}

void
send_messages( message, subject, addresses_string )
char	*message;
char	*subject;
char	*addresses_string;
{
	char	*cmd;

	if( addresses_string == NULL || *addresses_string == '\0' ) {

		elog_complain( 0, "Unable to send -- no addresses.\n" );
		return;
	}

	cmd = malloc( ( strlen( message ) + strlen( subject ) +
		        strlen( addresses_string ) + STRSZ ) * sizeof( char ) );
	
	sprintf( cmd, "echo \"%s\" | %s -s \"%s\" %s\n",
		      message, Mailx, subject, addresses_string );

	system( cmd );

	free( cmd );
}

int
main( int argc, char **argv )
{
	Pf	*pf = NULL;
	char	pffile[FILENAME_MAX];
	char	dir[FILENAME_MAX];
	static char base[FILENAME_MAX];
	char	start[STRSZ];
	Dbptr	db;
	Packet	*unstuffed = 0;
	char	orbname[STRSZ];
	int	orbfd;
	char	srcname[STRSZ];
	char	*s;
	int	pktid;
	double	time;
	int	nbytes = 0;
	int	nmaxpkts = -1;
	int 	npkts = 0;
	int	optind;
	char	*packet = NULL;
	int	bufsize = 0;
	char	*message = 0;
	int	msglen = 0;
	char	subject[STRSZ];
	char	*header;
	char	*footer;
	char	*addresses_string;
	Arr	*recipients = 0;
	double	starttime;
	int	rc;
	int	orid;
	int	evid;
	int	type;

	elog_init( argc, argv );

	dirbase( argv[0], dir, base );
	Program_Name = base;
	strcpy( pffile, Program_Name );
	strcpy( start, "" );
	strcpy( srcname, "" );

	for( optind = 1; optind < argc; optind++ ) {

		if ( *(argv[optind]) != '-' || strcmp(argv[optind], "-") == 0 )
		break;

		if (strcmp(argv[optind], "-pf") == 0) {
			strcpy( pffile, argv[++optind] );
		}

		if (strcmp(argv[optind], "-start") == 0) {
			strcpy( start, argv[++optind] );
		}

		if (strcmp(argv[optind], "-n") == 0) {
			nmaxpkts = atoi( argv[++optind] );
		}
	}

	if( argc - optind != 1 ) {
		elog_die( 1, "Usage: %s [-pf pffile] [-n npkts] [-start {OLDEST|NEWEST|time}] orbname\n", Program_Name );
	} else {
		strcpy( orbname, argv[optind++] );
	}

	if( pfupdate( pffile, &pf ) < 0 ) {
		elog_die( 1, "%s: no parameter file %s", Program_Name, pffile );
	}

	orbfd = orbopen( orbname, "r&" );

	if( ! strcmp( start, "OLDEST" ) ) {
		
		orbseek( orbfd, ORBOLDEST );

	} else if( ! strcmp( start, "NEWEST" ) ) {

		orbseek( orbfd, ORBNEWEST );

	} else if( strcmp( start, "" ) ) {

		starttime = str2epoch( start );

		orbafter( orbfd, starttime );
	}
	
	orbselect( orbfd, "/db/origin" );

	for( ;; ) {

		if( nmaxpkts > 0 && npkts > nmaxpkts ) {

			exit( 0 );
		}

		orbreap( orbfd, &pktid, srcname, &time, 
			 &packet, &nbytes, &bufsize );
    
                type = unstuffPkt (srcname, time, packet, nbytes, &unstuffed);

                if (type != Pkt_db) continue;
	
		npkts++;

                db = unstuffed->db;

	        dbgetv( db, 0, "orid", &orid, "evid", &evid, 0);

		elog_clear_register( 1 );

		pfupdate( pffile, &pf );
		header = pfget_string( pf, "header" );
		footer = pfget_string( pf, "footer" );
		if( recipients ) {
			freearr( recipients, 0 );
		}
		recipients = pfget_arr( pf, "recipients" );
		Placedb = pfget_string( pf, "placedb" );
		Mailx = pfget_string( pf, "mailx" );
		s = pfget_string( pf, "dist_cutoff_km" );
		if( s != NULL && strcmp( s, "" ) ) {
			Dist_cutoff_km = atof( s );
		} else {
			Dist_cutoff_km = 0;
		}

		rc = dborigin_to_message( db, header, footer,
				     &message, &msglen,
				     subject );

		if( rc ) {
			elog_complain( 0,
			    "Skipping message due to conversion failure\n" );
			continue;
		}

		fprintf( stderr,"Sending message for orid, evid = %d %d\n%s\n",orid,evid,message);

		addresses_string = filter_recipients( db, recipients );

		send_messages( message, subject, addresses_string );

		if( addresses_string != NULL ) free( addresses_string );
	}
}
