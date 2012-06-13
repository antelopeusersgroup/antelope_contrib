/*
 *   Copyright (c) 2006 Lindquist Consulting, Inc.
 *   All rights reserved. 
 *                                                                     
 *   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
 *
 *   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
 *   KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 *   WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
 *   PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
 *   OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
 *   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 *   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
 *   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * 
 *   This software may be used freely in any way as long as 
 *   the copyright statement above is not removed. 
 *
 */

#include <stdio.h>
#include <strings.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <pthread.h>
#include <pthread.h>
#include <errno.h>

#include "Pkt.h"
#include "orb.h"
#include "forb.h"
#include "coords.h"
#include "stock.h"
#include "db.h"
#include "cgeom.h"

#define VERY_LARGE_DOUBLE   1e36
#define STAFF_NOTIFY_SLEEPTIME_SEC 1
#define DATABASE_WATCH_SLEEPTIME_SEC 1

extern int is_changed( char *path ); /* missing in Antelope 4.7 stock.h */
void process_db( Dbptr db );
void process_pf( char *srcname, Pf *pf );

typedef struct Alarm {
	long	alarmid;
	char	*alarmkey;
	char	*alarmclass;
	char	*alarmname;
	char	raw_dbrow[STRSZ];
	char	*subject;
	char	*body;
	char	*ack_subject_template;
	char	*ack_body_template;
	double	time;
	long	evid;
	long	orid;
	int	wait_ack;
	Tbl	*unsent_callblocks;
	Tbl	*sent_callblocks;
	Arr	*message_expressions;
} Alarm;

typedef struct Callblock {
	double	delay_sec;
	int	sent;
	double	time;
	char	*to; 
} Callblock;

Arr	*Alarms;
pthread_mutex_t	Alarms_mutex; 

Arr	*Registered;

Arr	*Threadnames;
pthread_mutex_t	Threadnames_mutex;

int	Verbose = 0;
int	VeryVerbose = 0;

char	*Dbname = 0;
Dbptr	Db;
pthread_mutex_t Db_mutex;

Dbptr	Dbplaces;
char	*Places_dbname = 0;
Arr	*Regions = 0;
double	Regions_branchcut;

char	*Orbname;
char	*Match = "(/pf/orb2dbt|/db/origin)";
char	*Reject = 0;
char	*Statefile = 0;
int	Specified_after = 0;
double	After = 0.0;
double	Until = VERY_LARGE_DOUBLE;
long	Maxpkts = LONG_MAX;
double	Max_ack_wait_sec = 7200;
int	Done = 0;

char	*Pfname = "orb_quake_alarm";
Pf	*pf;

void
usage()
{
    	cbanner( "$Date$", 
	      "[-p pfname] [-m match] [-r reject] [-S statefile] "
	      "[-n maxpkts] [-d dbname] [-v] [-V] "
	      "orbname [start-time [period|end-time]]\n",
	      "Kent Lindquist", 
	      "Lindquist Consulting, Inc.", 
	      "kent@lindquistconsulting.com" ) ;

    	exit(1);
}

void
register_threadname( pthread_t tid, char *threadname ) 
{
	char	key[STRSZ];

	sprintf( key, "%ld", (long) tid );

	pthread_mutex_lock(&Threadnames_mutex );

	if( Threadnames == (Arr *) NULL ) {

		Threadnames = newarr( 0 );	
	}

	setarr( Threadnames, key, strdup( threadname ) );

	pthread_mutex_unlock(&Threadnames_mutex );

	return;
}

char *
get_threadname()
{
	char	key[STRSZ];
	char	*threadname;
	pthread_t tid;

	tid = pthread_self();

	sprintf( key, "%ld", (long) tid );

	pthread_mutex_lock(&Threadnames_mutex );

	if( Threadnames == (Arr *) NULL || 
	    ( threadname = getarr( Threadnames, key ) ) == NULL ) {

		threadname = strdup( "unregistered thread" );
	}

	pthread_mutex_unlock(&Threadnames_mutex );

	return threadname;
}

double
wrap_phase( double in, double branchcut )
{
	double 	out = in;

	while( out < branchcut - 360 ) {
		
		out += 360;
	}

	while( out > branchcut ) {
		
		out -= 360;
	}

	return out;
}

int
is_inside( Dbptr db, char *function_string )
{
	CGPolygon *cgpy = 0;
	char	region[STRSZ];
	int	inside = 1;
	double	lon;
	double	lat;

	if( Regions == 0 ) {

		elog_complain( 0, 
			"[%s]: Places database not usable; expression '%s' "
			"defaults to true\n",
			get_threadname(), 
			function_string );

		return inside;
	}

	strcpy( region, function_string + 3 );

	region[strlen(region)-1] = 0;

	if( ( cgpy = getarr( Regions, region ) ) == (CGPolygon *) NULL ) {
		
		elog_complain( 0, 
			"[%s]: No region '%s' found in places database; "
			"expression '%s' defaults to true\n",
			get_threadname(), 
			region, 
			function_string );

		return inside;
	}

	dbgetv( db, 0, "lon", &lon, "lat", &lat, NULL );

	lon = wrap_phase( lon, Regions_branchcut );

	inside = xy_in_cgpolygon( cgpy, lon, lat );

	if( VeryVerbose ) {
		
		elog_notify( 0, 
			"[%s]: Expression '%s' returns '%s'\n", 
			get_threadname(),
			function_string,
			inside == 0 ? "no" : "yes" );
	}

	return inside;
}

char *
compass_from_azimuth( double azimuth )
{
	while( azimuth < 0. ) azimuth += 360.;
	while( azimuth > 360. ) azimuth -= 360.;

	if( azimuth >= 348.75 || azimuth < 11.25 ) {

		return "N";		/*  0.00 */

	} else if( azimuth >= 11.25 && azimuth < 33.75 ) {

		return "NNE";		/*  22.50 */

	} else if( azimuth >= 33.75 && azimuth < 56.25 ) {

		return "NE";		/*  45.00	 */

	} else if( azimuth >= 56.25 && azimuth < 78.75 ) {

		return "ENE";		/*  67.50	 */

	} else if( azimuth >= 78.75 && azimuth < 101.25 ) {

		return "E";		/*  90.00	 */

	} else if( azimuth >= 101.25 && azimuth < 123.75 ) {

		return "ESE";		/*  112.50	 */

	} else if( azimuth >= 123.75 && azimuth < 146.25 ) {

		return "SE";		/*  135.00	 */

	} else if( azimuth >= 146.25 && azimuth < 168.75 ) {

		return "SSE";		/*  157.50	 */

	} else if( azimuth >= 168.75 && azimuth < 191.25 ) {

		return "S";		/*  180.00	 */

	} else if( azimuth >= 191.25 && azimuth < 213.75 ) {

		return "SSW";		/*  202.50	 */

	} else if( azimuth >= 213.75 && azimuth < 236.25 ) {

		return "SW";		/*  225.00 	 */

	} else if( azimuth >= 236.25 && azimuth < 258.75 ) {

		return "WSW";		/*  247.50	 */

	} else if( azimuth >= 258.75 && azimuth < 281.25 ) {

		return "W";		/*  270.00	 */

	} else if( azimuth >= 281.25 && azimuth < 303.75 ) {

		return "WNW";		/*  292.50	 */

	} else if( azimuth >= 303.75 && azimuth < 326.25 ) {

		return "NW";		/*  315.00	 */

	} else if( azimuth >= 326.25 && azimuth < 348.75 ) {

		return "NNW";		/*  337.50	 */
	} 

	return "Compass error";
}

void
add_nearest( void *vstack, Dbptr db, char *function_string )
{
	Dbptr	dbplaces;
	char	args[STRSZ];
	char	expr[STRSZ];
	char	place[STRSZ];
	double	lat;
	double	lon;
	long	nseq = 0;
	long	nrecs = 0;
	Expression *dist_expr;
	Expression *az_expr;
	Tbl     *expr_tbl;
	char	*compass;
	double	dist_km;
	double	azimuth;

	if( Places_dbname == 0 ) {

		elog_complain( 0, 
			"[%s]: No places database available! "
			"Not filling in value for %%{%s}\n",
			get_threadname(),
			function_string );

		return;
	} 

	dbgetv( db, 0, "lat", &lat, "lon", &lon, NULL );

	dbplaces = dblookup( Dbplaces, 0, "places", 0, 0 );

	dbquery( dbplaces, dbRECORD_COUNT, &nrecs );

	strcpy( args, function_string + 8 );

	args[strlen(args)-1] = 0;

	nseq = atol( args );
	nseq -= 1;

	if( nseq >= nrecs ) {

		elog_complain( 0, 
			"[%s]: Not enough rows in places database; "
			"Not filling in value for %%{%s}\n",
			get_threadname(),
			function_string );

		return;
	}

	sprintf( expr, "azimuth(lat,lon,%f,%f)", lat, lon );
	dbex_compile( dbplaces, expr, &az_expr, 0 );

	sprintf( expr, "distance(lat,lon,%f,%f)*111.195", lat, lon );
	dbex_compile( dbplaces, expr, &dist_expr, 0 );

	expr_tbl = strtbl( expr, NULL );
	dbplaces = dbsort( dbplaces, expr_tbl, 0, 0 );
	freetbl( expr_tbl, 0 );

	dbplaces.record = nseq;

	dbex_eval( dbplaces, dist_expr, 0, &dist_km );

	dbex_eval( dbplaces, az_expr, 0, &azimuth );
	compass = compass_from_azimuth( azimuth );

	dbgetv( dbplaces, 0, "place", place, NULL );

	sprintf( expr, "%.0f km %3s of %s", 
		   dist_km, compass, place );

	if( VeryVerbose ) {

		elog_notify( 0, 
			"[%s]: Translating %%{%s} expression to '%s' \n",
			get_threadname(),
			function_string,
			expr );
	}

	pushstr( &vstack, expr );

	dbex_free( dist_expr );
	dbex_free( az_expr );

	return;
}

char *
fieldsub( Dbptr db, char *template, Arr *expressions )
{
	Dbptr	dbfield;
	void	*vstack = 0;
	char	*result;
	int	free_vstack = 1;
	char	*left;
	char	*right;
	char	*template_last_appended;
	char	*expr_string;
	char	*expr_value;
	char	field[STRSZ];
	char	value[STRSZ];
	char	clean_value[STRSZ];
	char	segment[STRSZ];
	int	c;
	int	n;

	if( template == 0 || ! strcmp( template, "" ) ) {
		
		return strdup( "" );

	} else {

		left = template;
		template_last_appended = template;
	}

	while( ( c = *left++ ) != 0 ) {

		if( c == '%' && *left == '{' ) {

			n = left - 1 - template_last_appended;

			if( n > 0 ) {

				strncpy( segment, template_last_appended, n );
				segment[n] = 0;

				pushstr( &vstack, segment );

				template_last_appended += n;
			}

			left++;

			right = strchr( left, '}' );

			if( right == 0 ) {
				
				elog_complain( 0,
					"[%s]: Parse error for template '%s'\n", 
					get_threadname(),
					template );

				if( vstack != 0 ) {

					free( vstack );
				}

				return 0;
			}

			n = right - left;
			strncpy( field, left, n );
			left += n + 1;
			field[n] = 0;

			dbfield = dblookup( db, 0, 0, field, 0 );

			if( dbfield.field >= 0 ) {

				dbget( dbfield, value );

				copystrip( clean_value, 
					   value, 
					   strlen( value ) );

				pushstr( &vstack, clean_value );

			} else if( ( expressions != (Arr *) NULL ) &&
			          ( expr_string = 
				    getarr( expressions, field ) ) != NULL ) { 

				dbex_evalstr( db, 
					      expr_string, 
					      dbSTRING, 
					      &expr_value );

				pushstr( &vstack, expr_value );

				free( expr_value );

			} else if( strncmp( field, "in(", 3 ) == 0 &&
				   field[strlen(field)-1] == ')' ) {

				if( is_inside( db, field ) ) {

					pushstr( &vstack, "1" ); 

				} else {

					pushstr( &vstack, "0" ); 
				}

			} else if( strncmp( field, "nearest(", 8 ) == 0 &&
				   field[strlen(field)-1] == ')' ) {

				add_nearest( vstack, db, field );

			} else {

				elog_complain( 0, 
					"[%s]: Escape field '%%{%s}' is not a "
					"database field or a pre-defined "
					"pattern; ignoring\n",
					get_threadname(),
					field );
			}

			template_last_appended = right + 1;
		}
	}

	n = left - 1 - template_last_appended;

	if( n > 0 ) {

		strncpy( segment, template_last_appended, n + 1 );
		segment[n+1] = 0;
		pushstr( &vstack, segment );
	}

	result = popstr( &vstack, free_vstack );

	return result;
}

void
rtmail_send( char *subject, char *body, char *to )
{
	char	cmd[STRSZ];
	FILE	*rtmail;

	sprintf( cmd, "rtmail -s '%s' %s", subject, to );

	rtmail = popen( cmd, "w" );

	fwrite( body, strlen( body ) + 1, 1, rtmail );

	pclose( rtmail );

	return;
}

void
send_message( Alarm *alarm, Callblock *callblock )
{
	if( VeryVerbose ) {

		elog_notify( 0, 
			"[%s]: Using rtmail to send message to '%s' with "
			"subject+body length %ld bytes. (This is "
			"at delay %.1lf seconds):\n"
			"\tsubject: %s\n"
			"\tbody: {\n%s\n\t}\n",
			get_threadname(),
			callblock->to, 
			strlen( alarm->subject ) + strlen( alarm->body ), 
			callblock->delay_sec,
			alarm->subject, 
			alarm->body );
	}

	rtmail_send( alarm->subject, alarm->body, callblock->to );

	return;
}

Callblock *
new_callblock() 
{
	Callblock *callblock;
	
	allot( Callblock *, callblock, 1 );

	callblock->delay_sec = -1;
	callblock->sent = 0;
	callblock->time = 0;
	callblock->to = 0;

	return callblock;
}

void free_callblock( Callblock *callblock )
{
	if( callblock->to != NULL ) {
		
		free( callblock->to );
	}

	free( callblock );

	return;
}

Alarm *
new_alarm()
{
	Alarm *alarm;
	static long fake_alarmid = 1;

	allot( Alarm *, alarm, 1 );

	pthread_mutex_lock(&Db_mutex );

	if( Dbname != NULL ) {

		alarm->alarmid = dbnextid( Db, "alarmid" );

	} else {

		alarm->alarmid = fake_alarmid++;
	}

	alarm->alarmkey = 0;
	alarm->alarmclass = 0;
	alarm->alarmname = 0;
	alarm->subject = 0;
	alarm->body = 0;
	alarm->ack_subject_template = 0;
	alarm->ack_body_template = 0;
	alarm->time = 0;
	alarm->evid = -1;
	alarm->orid = -1;
	alarm->wait_ack = 0;

	alarm->unsent_callblocks = newtbl( 0 );

	alarm->sent_callblocks = newtbl( 0 );

	alarm->message_expressions = 0;

	pthread_mutex_unlock(&Db_mutex );

	return alarm;
}

void
free_expressions( Arr *expressions )
{
	Tbl	*keys;
	char	*key;
	char	*val;
	int	ikey;
	int	nkeys;

	keys = keysarr( expressions );

	nkeys = maxtbl( keys );

	for( ikey = 0; ikey < nkeys; ikey++ ) {
	
		key = gettbl( keys, ikey );
	
		val = delarr( expressions, key );

		free( val );
	}

	freetbl( keys, 0 );

	freearr( expressions, 0 );

	return;
}

Arr *
dup_expressions( Arr *expressions )
{
	Arr 	*new; 
	Tbl	*keys;
	char	*key;
	char	*val;
	int	ikey;
	int	nkeys;

	new = newarr( 0 );

	keys = keysarr( expressions );

	nkeys = maxtbl( keys );

	for( ikey = 0; ikey < nkeys; ikey++ ) {
	
		key = gettbl( keys, ikey );
	
		val = getarr( expressions, key );

		setarr( new, key, strdup( val ) );
	}

	freetbl( keys, 0 );

	return new; 	
}

void 
free_alarm( Alarm **alarm )
{
	if( (*alarm)->alarmkey != NULL ) {
		
		free( (*alarm)->alarmkey );
	}

	if( (*alarm)->alarmclass != NULL ) {
		
		free( (*alarm)->alarmclass );
	}

	if( (*alarm)->alarmname != NULL ) {
		
		free( (*alarm)->alarmname );
	}

	if( (*alarm)->subject != NULL ) {
		
		free( (*alarm)->subject );
	}

	if( (*alarm)->body != NULL ) {
		
		free( (*alarm)->body );
	}

	if( (*alarm)->ack_subject_template != NULL ) {
		
		free( (*alarm)->ack_subject_template );
	}

	if( (*alarm)->ack_body_template != NULL ) {
		
		free( (*alarm)->ack_body_template );
	}

	if( (*alarm)->message_expressions != NULL ) {
		
		freearr( (*alarm)->message_expressions, free );
	}

	freetbl( (*alarm)->unsent_callblocks, (void (*)(void*)) free_callblock );

	freetbl( (*alarm)->sent_callblocks, (void (*)(void*)) free_callblock );

	free( *alarm );

	*alarm = 0;

	return;
}

void
dbarchive_notification( Alarm *alarm, Callblock *callblock )
{
	Dbptr	dbalarmcomm;
	int	rc;

	if( Dbname == 0 ) {
		
		return;
	}

	pthread_mutex_lock(&Db_mutex );

	dbalarmcomm = dblookup( Db, 0, "alarmcomm", 0, 0 );

	dbalarmcomm.record = dbaddnull( dbalarmcomm );

	if( dbalarmcomm.record == dbINVALID ) {
		
		elog_complain( 1, 
			"[%s]: Failed to add null row to alarmcomm table "
			"for alarmid %ld!\n",
			get_threadname(), alarm->alarmid );
		
		return;
	}

	rc = dbputv( dbalarmcomm, 0, 
			"alarmid", alarm->alarmid,
			"time", callblock->time,
			"recipient", callblock->to, 
			"delaysec", callblock->delay_sec,
			NULL );

	if( rc != 0 ) {

		elog_complain( 1, 
			"[%s]: Failed to fill in alarmcomm table row "
			"for alarmid %ld, recipient %s, delay %f!\n",
			get_threadname(), alarm->alarmid,
			callblock->to, callblock->delay_sec );
	}

	pthread_mutex_unlock(&Db_mutex );

	return;
}

void
dbarchive_alarm( Alarm *alarm )
{
	Dbptr	dbalarms;
	char	*alarm_dbfilenames = 0;	
	char	*alarm_filename = 0;
	FILE	*fp;
	int	rc;

	if( Dbname == 0 ) {
		
		return;
	}

	alarm_dbfilenames = pfget_string( pf, "alarm_dbfilenames" );

	pthread_mutex_lock(&Db_mutex );

	dbalarms = dblookup( Db, 0, "alarms", 0, 0 );

	dbalarms.record = dbaddnull( dbalarms );

	if( dbalarms.record == dbINVALID ) {
		
		elog_complain( 1, 
			"[%s]: Failed to add null row to alarms table "
			"for alarmid %ld!\n",
			get_threadname(), alarm->alarmid );
		
		return;
	}

	rc = dbputv( dbalarms, 0, 
			"alarmid", alarm->alarmid,
			"alarmkey", alarm->alarmkey,
			"alarmclass", alarm->alarmclass,
			"alarmname", alarm->alarmname,
			"time", alarm->time,
			"subject", alarm->subject,
			NULL );

	if( rc != 0 ) {

		elog_complain( 1, 
			"[%s]: Failed to fill in alarms table row "
			"for alarmid %ld!\n",
			get_threadname(), alarm->alarmid );
	}
	
	if( alarm->evid != -1 ) {

		rc = dbputv( dbalarms, 0, "evid", alarm->evid, NULL );

		if( rc != 0 ) {

			elog_complain( 1, 
				"[%s]: Failed to fill in alarms table evid "
				"%ld for alarmid %ld!\n",
				get_threadname(), alarm->evid,
				alarm->alarmid );
		}
	}

	if( alarm->orid != -1 ) {

		rc = dbputv( dbalarms, 0, "orid", alarm->orid, NULL );

		if( rc != 0 ) {

			elog_complain( 1, 
				"[%s]: Failed to fill in alarms table orid "
				"%ld for alarmid %ld!\n",
				get_threadname(), alarm->orid,
				alarm->alarmid );
		}
	}

	rc = trwfname( dbalarms, alarm_dbfilenames, &alarm_filename );

	if( rc == -1 ) {

		elog_complain( 1, 
			"[%s]: Failed to create filename to save body "
			"of alarm for alarmid %ld!\n",
			get_threadname(), alarm->alarmid );

	} else if( rc == -2 ) {

		elog_complain( 1, 
			"[%s]: Filename for body "
			"of alarm for alarmid %ld is too long for database "
			"(Database error!)\n",
			get_threadname(), alarm->alarmid );

	} else {
		
		if( VeryVerbose ) {
			
			elog_notify( 0, 
				"[%s]: Saving body of alarm for alarmid %ld"
				" to file '%s'\n",
				get_threadname(), 
				alarm->alarmid,
				alarm_filename );
		}

		fp = fopen( alarm_filename, "w" );

		if( fp == (FILE *) NULL ) {

			elog_complain( 1, 
				"[%s]: Failed to open file '%s' to save "
				"body of alarmid %ld!\n", 
				get_threadname(), 
				alarm_filename, 
				alarm->alarmid );

		} else {

			fprintf( fp, "%s\n", alarm->body );

			fclose( fp );
		}
	}

	if( alarm_filename != (char *) NULL ) {
		
		free( alarm_filename );
	}

	pthread_mutex_unlock(&Db_mutex );

	return;
}

void
register_alarm( Alarm *alarm )
{
	char	alarmid[STRSZ];

	alarm->time = now();

	sprintf( alarmid, "%ld", alarm->alarmid );

	pthread_mutex_lock(&Alarms_mutex );

	setarr( Alarms, alarmid, (void *) alarm );

	if( Verbose ) {

		elog_notify( 0, 
			     "[%s]: Registered new alarm with alarmid %s\n",
			     get_threadname(),
			     alarmid );
	}

	dbarchive_alarm( alarm );

	pthread_mutex_unlock(&Alarms_mutex );

	return;
}

void
deregister_alarm_nolocks( char *alarmid, char *reason ) 
{
	Alarm	*alarm;
	char	alarmid_copy[STRSZ];

	/* The actual alarmid string is a key which is freed upon delarr(): */
	strcpy( alarmid_copy, alarmid );

	alarm = delarr( Alarms, alarmid );

	if( alarm == (Alarm *) NULL ) {
		
		elog_complain( 0, 
			       "[%s]: Failed to de-register alarm with "
			       "alarmid %s\n", 
			       get_threadname(),
			       alarmid_copy );

	} else {

		free_alarm( &alarm );

		if( Verbose ) {

			elog_notify( 0, 
				     "[%s]: De-registered alarm with "
				     "alarmid %s because %s\n",
				     get_threadname(),
				     alarmid_copy,
				     reason );
		}
	}

	return;
}

int
alarm_count()
{
	long	nalarms;
	Tbl	*keys;

	pthread_mutex_lock(&Alarms_mutex );

	keys = keysarr( Alarms );

	nalarms = maxtbl( keys );

	freetbl( keys, 0 );

	pthread_mutex_unlock(&Alarms_mutex );

	return nalarms;
}

int
by_delay( Callblock **a, Callblock **b, void *private )
{
	if( (*a)->delay_sec < (*b)->delay_sec ) {
		
		return -1;

	} else if( (*a)->delay_sec > (*b)->delay_sec ) {

		return 1;

	} else {

		return 0;
	}
}

int
verify_unique( char *alarmname, char *alarmkey )
{
	char	key[STRSZ];
	long	val;
	long	one = 1;

	if( Registered == 0 ) {

		Registered = newarr( 0 );
	}

	sprintf( key, "%s:%s", alarmname, alarmkey );

	if( ( val = (long) getarr( Registered, key ) ) == 0 ) {

		setarr( Registered, key, (void *) one );

		return 1;

	} else {

		return 0;
	}
}

void
process_origin( Dbptr db ) 
{
	Alarm	*alarm;
	Pf	*pfcandidates;
	Pf	*pfcandidate;
	Tbl	*alarmnames;
	char	*alarmname;
	int	ialarmname;
	char	alarmkey[STRSZ];
	char	alarmid[STRSZ];
	char	*alarmclass = "hypocenter";
	Arr	*message_expressions_in = 0;
	char	literal[STRSZ];
	char	*trigger_condition_template;
	char	*trigger_condition;
	long	triggered;
	char	*subject_template;
	char	*body_template;
	Arr	*recipients;
	Tbl	*recipient_keys;
	char	*recipient;
	int	irecipient;
	long	evid;
	long	orid;
	int	rc;

	Callblock *callblock;

	pfupdate( Pfname, &pf );

	message_expressions_in = pfget_arr( pf, "message_expressions" );

	rc = pfresolve( pf, "alarms{hypocenter}", 0, &pfcandidates );

	elog_clear_register( 1 );

	alarmnames = pfkeys( pfcandidates );

	for( ialarmname = 0; ialarmname < maxtbl( alarmnames ); ialarmname++ ) {
		
		alarmname = gettbl( alarmnames, ialarmname );

		if( VeryVerbose ) {
			
			elog_notify( 0, 
				"[%s]: Processing alarmname '%s'\n",
				get_threadname(),
				alarmname );
		}

		pfget( pfcandidates, alarmname, (void **) &pfcandidate );

		trigger_condition_template = 
			pfget_string( pfcandidate, "trigger_condition" );
		
		if( trigger_condition_template == (char *) NULL ) {

			elog_complain( 0, 
				"[%s]: Unexpected error: alarm criterion is "
				"null! Skipping evaluation of this packet "
				"against this trigger_condition\n",
				get_threadname() );

			continue;
		}

		trigger_condition = fieldsub( db, 
			    	 trigger_condition_template, 
			    	 message_expressions_in );

		strtr( trigger_condition, "\n", " " );

		rc = dbex_evalstr( db, trigger_condition, dbBOOLEAN, &triggered );

		free( trigger_condition );

		if( rc < 0 ) {

			elog_complain( 1, 
				"[%s]: Skipping evaluation of this incoming "
				"packet against this trigger_condition!: Failed to "
				"compile expression: << %s >>\n",
				get_threadname(),
				trigger_condition );

			elog_clear_register( 1 );

			continue;
		}

		if( ! triggered ) {

			if( VeryVerbose ) {

				elog_notify( 0, 
					"[%s]: Not triggering '%s' alarm since "
					"hypocenter does not match "
					"trigger_condition: << %s >>\n",
					get_threadname(),
					alarmname, 
					trigger_condition_template );
			}

			continue;

		} else {

			if( VeryVerbose ) {

				elog_notify( 0, 
					"[%s]: Triggering '%s' alarm since "
					"hypocenter matches trigger_condition: "
					"<< %s >>\n",
					get_threadname(),
					alarmname, 
					trigger_condition_template );
			}

		}

		dbgetv( db, 0, "evid", &evid, 
			       "orid", &orid, NULL );

		sprintf( alarmkey, "evid%ld", evid );

		if( ! verify_unique( alarmname, alarmkey ) ) {

			if( Verbose ) {

				elog_notify( 0, 
					"[%s]: Not registering the new alarm "
					"for alarmname %s, alarmkey %s since "
					"a matching alarm has been previously "
					"registered. No communications will "
					"be issued for this alarm.\n",
					get_threadname(),
					alarmname, 
					alarmkey );
			}

			continue;
		}

		alarm = new_alarm();

		dbget( db, alarm->raw_dbrow );

		alarm->evid = evid;
		alarm->orid = orid;

		alarm->alarmkey = strdup( alarmkey );

		alarm->alarmclass = strdup( alarmclass );

		alarm->alarmname = strdup( alarmname );

		sprintf( alarmid, "%ld", alarm->alarmid );

		alarm->wait_ack = pfget_boolean( pfcandidate, "wait_ack" );

		alarm->ack_subject_template = 
			strdup( pfget_string( pfcandidate, "ack_subject" ) );

		alarm->ack_body_template = 
			strdup( pfget_string( pfcandidate, "ack_body" ) );

		if( Dbname == 0 ) {

			elog_complain( 0, 
				"[%s]: Turning off wait_ack for alarm %ld "
				"because database is not specified! "
				"(use the -d option to fix this, or turn "
				"off wait_ack in the parameter file)\n",
				get_threadname(),
				alarm->alarmid );

			alarm->wait_ack = 0;
		}
		
		alarm->message_expressions = duparr( message_expressions_in, 
						(void *(*)(void *)) strdup );

		sprintf( literal, "\"%s\"", alarmid );
		setarr( alarm->message_expressions, 
				"alarmid", 
				strdup( literal ) );

		sprintf( literal, "\"%s\"", alarmkey );
		setarr( alarm->message_expressions, 
				"alarmkey", 
				strdup( literal ) );

		sprintf( literal, "\"%s\"", alarmname );
		setarr( alarm->message_expressions, 
				"alarmname", 
				strdup( literal ) );

		sprintf( literal, "\"%s\"", alarmclass );
		setarr( alarm->message_expressions, 
				"alarmclass", 
				strdup( literal ) );

		subject_template = pfget_string( pfcandidate, "subject" );
		body_template = pfget_string( pfcandidate, "body" );

		alarm->subject = fieldsub( db, 
			    		subject_template, 
			    		alarm->message_expressions );

		alarm->body = fieldsub( db, 
			 		body_template, 
			 		alarm->message_expressions );

		if( alarm->subject == 0 || alarm->body == 0 ) {
		
			elog_complain( 0, 
				"[%s]: Failed to fill subject and/or body "
				"templates for alarm '%s'; Skipping "
				"notification!!\n",
				get_threadname(),
				alarmname );

			continue;
		}

		recipients = pfget_arr( pfcandidate, "recipients" );

		recipient_keys = keysarr( recipients );

		for( irecipient = 0; 
			irecipient < maxtbl( recipient_keys );
			   irecipient++ ) {
			
			recipient = gettbl( recipient_keys, irecipient );
				
			callblock = new_callblock();

			callblock->to = strdup( recipient );

			callblock->delay_sec = 
				atof( getarr( recipients, recipient ) );

			pushtbl( alarm->unsent_callblocks, callblock );
		}
		
		freetbl( recipient_keys, 0 );

		freearr( recipients, 0 );

		sorttbl( alarm->unsent_callblocks, 
			(int (*)(char *, char *, void *)) by_delay, 0 );
		
		register_alarm( alarm );
	}

	freearr( message_expressions_in, 0 );

	freetbl( alarmnames, 0 );

	return;
}

void 
process_pf( char *srcname, Pf *pf )
{
	char	*origin;
	Dbptr	dbtemp;

	if( ! strcmp( srcname, "/pf/orb2dbt" ) ) {
		
		origin = pfget_string( pf, "origin" );

		if( origin == (char *) NULL ) {

			elog_complain( 0, "[%s]: Couldn't find origin row in "
					"input parameter-file object '%s'\n",
					get_threadname(), 
					srcname );

			return;						
		}

		dbtemp = dbtmp( NULL );

		dbtemp = dblookup( dbtemp, "", "origin", "", "dbSCRATCH" );

		dbput( dbtemp, origin );

		process_db( dbtemp );

		dbclose( dbtemp );

	} else if( VeryVerbose ) {

		elog_complain( 0, "[%s]: No handler for parameter-file packet "
				  "'%s'\n", 
				  get_threadname(),
				  srcname );
	}

	return;
}

void
process_db( Dbptr db ) 
{
	char	*table_name;

	dbquery( db, dbTABLE_NAME, &table_name );

	if( ! strcmp( table_name, "origin" ) ) {

		process_origin( db );

	} else if( VeryVerbose ) {

		elog_complain( 0, "[%s]: No handler for database row from "
				  "'%s' table\n", 
				  get_threadname(),
				  table_name );
	}

	return;
}

int	
database_changed()
{
	Dbptr	db;
	char	*tablepath;
	int	retcode;

	pthread_mutex_lock(&Db_mutex );

	db = dblookup( Db, "", "alarms", "", "" );

	dbquery( db, dbTABLE_FILENAME, &tablepath );
	
	retcode = is_changed( tablepath );

	pthread_mutex_unlock(&Db_mutex );

	return retcode;
}

void
send_cancellation_ack( Alarm *alarm, double acktime, char *ackauth )
{
	char	*acksubject;
	char	*ackmsg;
	Dbptr	db;
	Callblock *callblock;
	int	nsent;
	int	isent;

	db = dblookup( Db, "", "origin", "", "dbSCRATCH" );

	dbput( db, alarm->raw_dbrow );

	acksubject = fieldsub( db, 
			       alarm->ack_subject_template, 
			       alarm->message_expressions );

	ackmsg = fieldsub( db, 
			       alarm->ack_body_template, 
			       alarm->message_expressions );

	nsent = maxtbl( alarm->sent_callblocks );

	for( isent = 0; isent < nsent; isent++ ) {
		
		callblock = gettbl( alarm->sent_callblocks, isent );
			
		if( VeryVerbose ) {
		elog_notify( 0, 
			"[%s]: Using rtmail to send acknowledgment message "
			"to '%s' with subject+body length %ld bytes:\n"
			"\tsubject: %s\n"
			"\tbody: {\n%s\n\t}\n",
			get_threadname(),
			callblock->to, 
			strlen( acksubject ) + strlen( ackmsg ), 
			acksubject, 
			ackmsg );
		}

		rtmail_send( acksubject, ackmsg, callblock->to );
	}

	free( acksubject );

	free( ackmsg );

	return;
}

static void *
database_watch( void *arg )
{
	Dbptr	db;
	Alarm	*alarm;
	char	*alarmid;
	int	true = 1;
	long	nalarms;
	long	ialarm;
	Tbl	*keys;
	char	expr[STRSZ];
	long	nrecs;
	char	acknowledged[2];
	char	ackauth[STRSZ];
	char	tempstr[STRSZ];
	double	acktime;
	char	*s = 0;

	register_threadname( pthread_self(), "database_watch" );

	if( Dbname == 0 ) {
		
		if( Verbose ) {

			elog_notify( 0, 
				"[%s]: thread exiting, nothing to do (no "
				"database specified with -d option)\n",
				get_threadname() );
		}

		return NULL;
	}

	while( true ) {

		if( ! database_changed() ) {
			
			sleep( DATABASE_WATCH_SLEEPTIME_SEC );

			continue;
		}

		if( VeryVerbose ) {

			elog_notify( 0, 
				"[%s]: alarms table change detected\n",
				get_threadname() );
		}

		pthread_mutex_lock(&Alarms_mutex );

		pthread_mutex_lock(&Db_mutex );

		db = dblookup( Db, "", "alarms", "", "" );

		dbquery( db, dbRECORD_COUNT, &nrecs );

		keys = keysarr( Alarms );

		nalarms = maxtbl( keys );

		if( VeryVerbose ) {

			elog_notify( 0, 
				"[%s]: Searching %ld alarm rows for matching entries\n",
				get_threadname(), nalarms );
		}

		for( ialarm = 0; ialarm < nalarms; ialarm++ ) {

			alarmid = gettbl( keys, ialarm );

			alarm = getarr( Alarms, alarmid );

			if( alarm->wait_ack == 0 ) {
				
				continue;
			}

			sprintf( expr, "alarmname == \"%s\" && alarmkey == \"%s\"",
					alarm->alarmname, 
					alarm->alarmkey );

			db.record = nrecs;
			
			db.record = dbfind( db, expr, 1, 0 );

			if( db.record >= 0 ) {

				dbgetv( db, 0, "acknowledged", &acknowledged,
					       "acktime", &acktime, 
					       "ackauth", &ackauth,
					       NULL );

				if( strcmp( acknowledged, "y" ) != 0 ) {

					continue;
				}

				chomp( ackauth );
				strtrim( ackauth );

				if( strcmp( ackauth, "-" ) == 0 ) {
					
					setarr( alarm->message_expressions, 
						"ackauth", 
						strdup( "\"unknown\"" ) );

				} else {

					strcpy( tempstr, ackauth );
					sprintf( ackauth, "\"%s\"", tempstr );
					
					setarr( alarm->message_expressions, 
						"ackauth", 
						strdup( ackauth ) );
				}

				if( acktime == -9999999999.999 ) {
					
					setarr( alarm->message_expressions, 
						"acktime", 
						strdup( "\"unknown\"" ) );

				} else {

					sprintf( tempstr, "\"%s\"", 
						s = epoch2str( 
							acktime,
							"%D %H:%M %Z" ) );
					free( s );

					setarr( alarm->message_expressions, 
						"acktime", 
						strdup( tempstr ) );
				}

				if( Verbose ) {

					elog_notify( 0, 
				 		"[%s]: Found acknowledgment for "
				 		"alarmname '%s', alarmkey '%s' "
				 		"in database, acknowledged by "
				 		"'%s' at time '%s'\n", 
				 		get_threadname(), 
				 		alarm->alarmname, 
				 		alarm->alarmkey, 
				 		ackauth, 
				 		acktime != -9999999999.999 ? 
							s = strtime( acktime ) : 
							"unknown" );
				}

				if( s != 0 ) {
						
					free( s );
				}

				send_cancellation_ack( alarm, 
						       acktime, 
						       ackauth );

				deregister_alarm_nolocks( alarmid, 
					   "alarm was acknowledged in "
					   "database" );
			}
		}

		if( VeryVerbose ) {

			elog_notify( 0, 
				"[%s]: Done searching alarm rows (all relevant matches "
				"were announced)\n",
				get_threadname() );
		}

		freetbl( keys, 0 );
		
		pthread_mutex_unlock(&Db_mutex );
		pthread_mutex_unlock(&Alarms_mutex );
	}

	return NULL;
}

static void *
staff_notify( void *arg )
{
	Tbl	*keys; 
	Alarm	*alarm;
	int	ikey;
	char	*alarmid;
	Callblock *callblock;
	char	expr[STRSZ];
	char	*s;

	register_threadname( pthread_self(), "staff_notify" );

	while( alarm_count() > 0 || ! Done ) {

		pthread_mutex_lock(&Alarms_mutex );

		keys = keysarr( Alarms );

		for( ikey = 0; ikey < maxtbl( keys ); ikey++ ) {

			alarmid = gettbl( keys, ikey );

			alarm = (Alarm *) getarr( Alarms, alarmid );

			while( ( callblock = 
				 gettbl( alarm->unsent_callblocks, 0 ) )
				 != (Callblock *) NULL &&
			        alarm->time + callblock->delay_sec < now() ) {

				callblock->time = now();
				callblock->sent = 1;

				send_message( alarm, callblock );

				dbarchive_notification( alarm, callblock );

				shifttbl( alarm->unsent_callblocks );

				pushtbl( alarm->sent_callblocks, callblock );
			}

			if( ! alarm->wait_ack ) {

				deregister_alarm_nolocks( alarmid,
					"wait_ack is 0" );

			} else if( alarm->time + Max_ack_wait_sec < now() ) {

				sprintf( expr, "maximum time of %s to wait for "
					"acknowledgment has passed",
					s = strtdelta( Max_ack_wait_sec ) );
				free( s );

				deregister_alarm_nolocks( alarmid, expr );
			}
		}

		freetbl( keys, 0 );

		pthread_mutex_unlock(&Alarms_mutex );

		sleep( STAFF_NOTIFY_SLEEPTIME_SEC );
	}

	return NULL;
}

static void *
network_watch( void *arg )
{
	int	orbfd;
	int	nmatch;
	double	decent_interval = 300.0;
	int	mode = PKT_NOSAMPLES;
	double	delta_t;
	long	totpkts = 0;
	double	totbytes = 0;
	static int last_pktid = -1;
	static double last_pkttime = 0.0;
	double	last_burial = 0.0;
	double	start_time;
	double	end_time;
	int	quit = 0;
	int	pktid;
	int	pkttype;
	char	srcname[ORBSRCNAME_SIZE];
	double	pkttime = 0.0 ;
	int	nbytes;
	char	*packet = 0;
	int	packetsz = 0;
	Packet	*unstuffed = 0;
	char	*s;
	int	rc;

	register_threadname( pthread_self(), "network_watch" );

	if( ( orbfd = orbopen( Orbname, "r&" ) ) < 0) {

		elog_die( 0, "[%s]: Can't open input orb '%s'\n", 
			     get_threadname(),
			     Orbname );
	}

	if( Statefile != 0 ) {

		if( exhume( Statefile, &quit, RT_MAX_DIE_SECS, 0 ) != 0 ) {

			if( Verbose ) {

				elog_notify (0, 
					"[%s]: read old state file\n", 
					get_threadname() );
			}
		}

		if( orbresurrect( orbfd, &last_pktid, &last_pkttime ) == 0 ) {

			if( Verbose ) {

				elog_notify (0, 
				"[%s]: resurrection successful: repositioned to "
				"pktid #%d @ %s\n",
				get_threadname(),
			 	last_pktid, s = strtime( last_pkttime ) );

				free( s );
			}

		} else {

			elog_complain (0, "[%s]: resurrection for orb read " 
					  "repositioning was unsuccessful\n",
					  get_threadname() );
		}
	}

	if( Match ) {

		nmatch = orbselect( orbfd, Match );

		if( nmatch < 0 ) {

			elog_complain( 1, 
				"[%s]: select '%s' returned %d sources\n",
				get_threadname(), 
				Match, nmatch );

		} else if( VeryVerbose ) {

			elog_notify( 1, 
				"[%s]: select '%s' returned %d sources\n",
				get_threadname(), 
				Match, nmatch );
		}
	}

	if( Reject ) {

		nmatch = orbreject( orbfd, Reject );

		if( nmatch < 0 ) {

			elog_complain(1,
				"[%s]: reject '%s' returned %d sources\n",
				get_threadname(),
				Reject, nmatch);

		} else if( VeryVerbose ) {

			elog_notify( 1, 
				"[%s]: reject '%s' returned %d sources\n",
				get_threadname(), 
				Reject, nmatch );
		}
	}

	if( nmatch > 0 && Verbose ) {

		elog_notify( 0, "[%s]: %d sources selected\n", 
				get_threadname(),
				nmatch);
	}

	if( Specified_after ) {

		pktid = orbafter( orbfd, After );

		if( pktid < 0 ) {

	    		elog_complain (1, 
				"[%s]: seek to %s failed\n", 
				get_threadname(),
				s = strtime( After ) );
	    		free( s );

	    		pktid = forbtell( orbfd );

			if( Verbose ) {

	    			elog_notify( 0,
					"[%s]: pktid is still #%d\n", 
					get_threadname(),
					pktid );
			}

		} else {

			if( Verbose ) {

				elog_notify( 0,
					"[%s]: new starting pktid is #%d\n", 
					get_threadname(),
					pktid );
			}
		}
	}

	start_time = now();

	while( ! quit && pkttime < Until && totpkts < Maxpkts ) {

		rc = orbreap (orbfd, &pktid, srcname, &pkttime, 
				     &packet, &nbytes, &packetsz);

		if( pkttime > Until ) {
			
			continue;
		}

		switch( rc ) {
		case 0:

			totpkts++;
			totbytes += nbytes;

			if( VeryVerbose ) {

				showPkt( pktid, srcname, pkttime, 
					 packet, nbytes, stdout, mode );
			}

			if( Statefile != 0 && 
			    last_pkttime - last_burial > decent_interval ) {

				bury ();
				last_burial = pkttime;
			}

			pkttype = unstuffPkt( srcname, pkttime, packet, 
					      nbytes, &unstuffed );

	    		last_pktid = pktid;
	    		last_pkttime = pkttime;

			switch( pkttype ) {
			case Pkt_wf:
			case Pkt_ch:
			default:
				continue;

			case Pkt_db:
				process_db( unstuffed->db );
				break;

			case Pkt_pf:
				process_pf( srcname, unstuffed->pf );
				break;
			}

			break;
		}

		elog_clear_register( 1 );

		if( unstuffed ) { 
		
			freePkt( unstuffed );
			unstuffed = 0;
		}
	}

	if( Statefile != 0 ) {

		bury();
	}

	end_time = now();

	delta_t = end_time - start_time;

	if( Verbose ) {

		if( totpkts > 0 ) {

			elog_notify( 0, 
				"[%s]: Done monitoring. Summary statistics:\n"
				"\t%ld %.2f byte packets "
				"(%.1f kbytes) in "
				"%.3f seconds\n\t%10.3f kbytes/s\n\t%10.3f "
				"kbaud\n\t%10.3f pkts/s\n",
				get_threadname(),
				totpkts, totbytes / totpkts, totbytes / 1024,
				delta_t,
				totbytes / delta_t / 1024,
				totbytes / delta_t / 1024 * 8,
				totpkts / delta_t );

		} else {

			elog_notify( 0, "[%s]: \nno packets copied\n",
					get_threadname() );
		}
	}

	if( orbclose( orbfd ) ) {

		elog_complain (1, 
			"[%s]: error closing input orbserver connection\n",
			get_threadname() );

	}

	Done = 1;

	return NULL;
}

void
cgpolygon_wrapphase( CGPolygon *cgpy )
{
	CGPoint	*cgpt;
	int	nvertices;
	int	ivertex;

	nvertices = cgpolygon_cnt( cgpy );

	for( ivertex = 0; ivertex < nvertices; ivertex++ ) {

		cgpt = cgpolygon_getvertex( cgpy, ivertex );

		cgpt->x = wrap_phase( cgpt->x, Regions_branchcut );
	}

	return;
}

void
fill_regions() 
{
	CGPolygon *cgpy = 0;
	Dbptr	dbregions;
	Dbptr	dbpolygon;
	Tbl	*cmds;
	long	nrecs;
	char	region[STRSZ];
	char	expr[STRSZ];

	Regions = newarr( 0 );

	Places_dbname = pfget_string( pf, "placedb" );
	Regions_branchcut = pfget_double( pf, "placedb_branchcut_deg" );

	if( Places_dbname == (char *) NULL ) {

		elog_complain( 0, 
			"[%s]: No placedb parameter defined: all region "
			"containment tests will default to true\n",
			get_threadname() );

		return;

	} else if( Verbose ) {

		elog_notify( 0, 
			"[%s]: Reading regions database '%s'...\n", 
			get_threadname(),
			Places_dbname );
	}
	
	dbopen( Places_dbname, "r", &Dbplaces );

	if( Dbplaces.database < 0 ) {

		elog_complain( 0, 
			"[%s]: Failed to open placedb '%s': all region "
			"containment tests will default to true\n", 
			get_threadname(), 
			Places_dbname );

		Places_dbname = 0;

		return;
	}

	dbregions = dblookup( Dbplaces, "", "regions", "", "" );

	if( dbregions.table < 0 ) {

		elog_complain( 0, 
			"[%s]: Failed to open regions table of '%s': "
			"all region containment tests will default "
			"to true\n", 
			get_threadname(), 
			Places_dbname );
	
		dbclose( dbregions );

		return;
	}

	cmds = strtbl( "dbopen regions", 
		       "dbsort regname vertex", 
		       "dbgroup regname", 
		       NULL );

	dbregions = dbprocess( dbregions, cmds, 0 );

	freetbl( cmds, 0 );

	dbquery( dbregions, dbRECORD_COUNT, &nrecs );

	for( dbregions.record = 0;
	       dbregions.record < nrecs; 
		 dbregions.record++ ) {

		dbgetv( dbregions, 0, "regname", &region, NULL );

		sprintf( expr, "dbsubset regname == \"%s\"", region );

		cmds = strtbl( expr, "dbungroup", NULL );

		dbpolygon = dbprocess( dbregions, cmds, 0 );

		freetbl( cmds, 0 );

		cgpy = db2cgpolygon( dbpolygon, "lon", "lat", 0, 0, 0, 0, 0 );

		cgpolygon_wrapphase( cgpy );

		setarr( Regions, region, (void *) cgpy );
	}

	elog_notify( 0, 
		"[%s]: Done reading regions database.\n",
		get_threadname() );

	return;
}

int
main( int argc, char **argv )
{
	int	c;
	int	errflag = 0;
	int	rc;
	char 	*rtmail_path = 0;
	pthread_t network_watch_tid;
	pthread_t staff_notify_tid;
	char	*s;

	elog_init( argc, argv );

	pthread_mutex_init(&Threadnames_mutex,0);

	pthread_mutex_init(&Db_mutex,0);

	pthread_mutex_init(&Alarms_mutex,0);

	register_threadname( pthread_self(), "main" );

	elog_notify(0, 
		"[%s]: %s $Revision$ $Date$\n",
		get_threadname(),
		Program_Name);

	while ((c = getopt (argc, argv, "m:n:d:p:r:S:vV")) != -1) {

		switch (c) {
	  	case 'm':
	    		Match = optarg;
	    		break;

	  	case 'n':
	    		Maxpkts = atol( optarg );
	    		break;

		case 'd':
			Dbname = optarg;
			break;

		case 'p':
			Pfname = optarg;
			break;

	  	case 'r':
			elog_complain( 0, 
				"Warning: the '-r' option is "
				"reserved for future capabilities "
				"and disabled in the current release\n" );
	    		/* Reject = optarg; */
	    		break;

	  	case 'S':
	    		Statefile = optarg;
	    		break;

	  	case 'v':
	    		Verbose++;
	    		break;

	  	case 'V':
			Verbose++;
			VeryVerbose++;
	    		break;

	  	case '?':
	    		errflag++;
			break;
		}
	}

    	if( errflag || argc - optind < 1 || argc - optind > 3 ) {

		usage ();
	}

	Orbname = argv[optind++];

	if( argc > optind ) {

		After = str2epoch( argv[optind++] );
		
		Specified_after = 1;

		if( argc > optind ) {

			Until = str2epoch( argv[optind++] );

			if( Until < After ) {

				Until += After ;
			}
		}
	}

	if( Verbose ) {

		elog_notify( 0, "[%s]: %s starting at %s UTC\n",
				get_threadname(),
				Program_Name, 
				s = strtime( now() ) );
		free( s );
	}

	rtmail_path = datafile( "PATH", "rtmail" );

	if( rtmail_path == 0 ||
	    ! strcmp( rtmail_path, "" ) ) {

		elog_die( 1, 
			"[%s]: Failed to find rtmail executable on " 
			"path. Bye.\n",
			get_threadname() );
	}

	if( Verbose ) {
		
		elog_notify( 0, "[%s]: Using '%s' for mailing\n", 
				get_threadname(),
				rtmail_path );
	}

	free( rtmail_path );

	if( Dbname != NULL ) {
		
		if( dbopen( Dbname, "r+", &Db ) < 0 ) {

			elog_die( 0, "[%s]: Failed to open '%s' for writing\n", 
				  get_threadname(),
				  Dbname );
		}

		Db = dblookup( Db, 0, "alarms", 0, 0 );

		if( Db.table < 0 ) {

			elog_die( 0, "[%s]: alarms table not accessible for "
				  "database '%s'\n",
				  get_threadname(),
				  Dbname );
		}

		Db = dblookup( Db, 0, "alarmcomm", 0, 0 );

		if( Db.table < 0 ) {

			elog_die( 0, "[%s]: alarmcomm table not accessible for "
				  "database '%s'\n",
				  get_threadname(),
				  Dbname );
		}

		Db.table = dbALL;
	}

	Alarms = newarr( 0 );

	if( pfread( Pfname, &pf ) < 0 ) {
		
		elog_clear_register( 1 );
		elog_die( 1, "[%s]: Problem reading parameter file '%s'\n", 
			get_threadname(),
			Pfname );
	}

	fill_regions();

	Max_ack_wait_sec = pfget_double( pf, "max_ack_wait_sec" );

	rc = pthread_create(&network_watch_tid ,0, network_watch, 0);

	rc = pthread_create(&staff_notify_tid ,0, database_watch, 0);

	rc = pthread_create(&staff_notify_tid ,0, staff_notify, 0);

	pthread_join(staff_notify_tid, (void **) NULL );

	if( Verbose ) {

		elog_notify( 0, "[%s]: %s stopping at %s UTC\n",
				get_threadname(),
				Program_Name, 
				s = strtime( now() ) );
		free( s );
	}

	return 0;
}
