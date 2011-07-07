/*
 * ew2orb.c
 *
 * Copyright (c) 2003-2005 Lindquist Consulting, Inc.
 * All rights reserved. 
 *                                                                     
 * Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
 * 
 * This software may be used freely in any way as long as 
 * the copyright statement above is not removed. 
 *
 */

#include <stdlib.h>

#include "orbew.h"

#define SERVER_RESET_ALLOWANCE_SEC 1
#define PACKET_QUEUE_SIZE 50000
#define DEFAULT_SEGTYPE "-"
#define DEFAULT_CALIB 0
#define DEFAULT_CALPER -1

#define DEFAULT_BANNER_PATH "/usr/bin/banner"

#define THR_PRIORITY_IMPORT 0
#define THR_PRIORITY_CONVERT 1
#define THR_PRIORITY_PFWATCH 2

#define APPEND_BUFFER(BUFFER,BUFSZ,NBYTES,C) \
	NBYTES++; \
	RESIZE_BUFFER( char *, BUFFER, BUFSZ, NBYTES ); \
	BUFFER[NBYTES-1] = C;
	
#define BAD_BYTE(WHERE,C) \
	elog_complain( 0, "Bad byte in parser at location '%s': ", WHERE ); \
	{ int bad = (C); hexdump( stderr, &bad, 1 ); } \
	fprintf( stderr, "\n" );

typedef struct ImportThread {

	/* Shared variables: */

	mutex_t	it_mutex;
	thread_t thread_id;
	Pf	*pf;
	int	update;
	int	new;
	int	stop;
	int	npacketrefs;
	Morphtbl *srcname_morphmap;
	Arr	*timecorr;

	/* Thread-only variables: */

	char	name[STRSZ];			
	char	server_ipaddress[STRSZ];
	in_port_t server_port;	
	char	expect_heartbeat_string[STRSZ];	
	Hook	*expect_heartbeat_hook;
	char	send_heartbeat_string[STRSZ];	
	int	send_heartbeat_sec;	
	double	last_heartbeat_sent;
	double	last_heartbeat_received;
	char	my_inst_str[STRSZ];
	char	my_mod_str[STRSZ];
	char	select[STRSZ];
	char	reject[STRSZ];
	char	default_segtype[2];
	Hook	*select_hook;
	Hook	*reject_hook;
	int	my_inst;	
	int	my_mod;	
	int	my_type_heartbeat;		
	int	so;
	struct sockaddr_in sin;
	Bns	*bnsin;			
	Bns	*bnsout;			
	char	*buf;	
	int 	bufsize;
	int	nbytes;
	enum	{ STREAM_SEARCH, STREAM_ACCUMULATE, STREAM_ESCAPE } sync_state;
	enum Loglevel loglevel;
	int	connectfail;
	int	bindfail;

} ImportThread;

typedef struct Ew2orbPacket {
	char	*buf;
	int	bufsize;
	int	nbytes;
	char	itname[STRSZ];
	ImportThread *it;
	int	inst;
	int	mod;
	int	type;
	char	inststr[STRSZ];
	char	modstr[STRSZ];
	char	typestr[STRSZ];
	TRACE_HEADER *th;
	TRACE2_HEADER *th2;
	Packet	*pkt;
	char	*orbpkt;
	int	orbpktsz;
	int	orbpktnbytes;
	double	time;
	char	srcname[STRSZ];
	enum Loglevel loglevel;
} Ew2orbPacket;

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

Arr	*Import_Threads;
rwlock_t Import_Threads_rwlock;
Pmtfifo	*E2oPackets_mtf;
int	Orbfd = -1;
char	*Pfname = "ew2orb";

Ew2orbPacket *
new_Ew2orbPacket()
{
	Ew2orbPacket *e2opkt;
	
	allot( Ew2orbPacket *, e2opkt, 1 );

	e2opkt->buf = 0;
	e2opkt->bufsize = 0;
	e2opkt->nbytes = 0;
	e2opkt->inst = 0;
	e2opkt->mod = 0;
	e2opkt->type = 0;
	e2opkt->loglevel = QUIET;
	strcpy( e2opkt->inststr, "INST_???" );
	strcpy( e2opkt->modstr, "MOD_???" );
	strcpy( e2opkt->typestr, "TYPE_???" );

	e2opkt->th = NULL;
	e2opkt->th2 = NULL;
	e2opkt->pkt = NULL;
	e2opkt->orbpkt = NULL;
	e2opkt->orbpktsz = 0;
	e2opkt->orbpktnbytes = 0;

	return e2opkt;
}

static void
free_Ew2orbPacket( Ew2orbPacket *e2opkt )
{
	if( e2opkt == NULL ) {

		return;
	}

	if( e2opkt->buf != NULL ) {
		
		free( e2opkt->buf );
	}

	if( e2opkt->th != NULL ) {
		
		free( e2opkt->th );
	}

	if( e2opkt->th2 != NULL ) {
		
		free( e2opkt->th2 );
	}

	if( e2opkt->pkt != NULL ) {
		
		freePkt( e2opkt->pkt );
	}

	if( e2opkt->orbpkt != NULL ) {
		
		free( e2opkt->orbpkt );
	}

	free( e2opkt );

	return;
}

static void
describe_packet( Ew2orbPacket *e2opkt )
{
	char	heartbeat[STRSZ];
	char	version[STRSZ];
	char	datatype[STRSZ];
	char	quality[STRSZ];

	memset( heartbeat, 0, STRSZ );
	memset( version, 0, STRSZ );
	memset( datatype, 0, STRSZ );
	memset( quality, 0, STRSZ );

	elog_notify( 0, "'%s': Received <%s> <%s> <%s>\n",
		  e2opkt->itname,
		  e2opkt->inststr, 
		  e2opkt->modstr, 
		  e2opkt->typestr );

	if( STREQ( e2opkt->typestr, Default_TYPE_HEARTBEAT ) ) { 

		strncpy( heartbeat, e2opkt->buf, strlen( e2opkt->buf ) );

		elog_notify( 0, "'%s':\tunpacked Heartbeat: \"%s\"\n",
	  		e2opkt->itname, 
	  		heartbeat );

	} else if( STREQ( e2opkt->typestr, Default_TYPE_TRACEBUF ) ) { 

		if( e2opkt->th ) {

			strncpy( datatype, e2opkt->th->datatype, 2 );
			strncpy( quality, e2opkt->th->quality, 2 );

			elog_notify( 0, 
				  "'%s': unpacked TRACEBUF Trace-data:\n\t"
				  "%s %s %s pinno %d "
				  "datatype %s quality '%s'\n\t"
				  "nsamp %d samprate %f\n\t"
				  "starttime %f endtime %f\n",
				  e2opkt->itname,
				  e2opkt->th->net, 
				  e2opkt->th->sta, 
				  e2opkt->th->chan,
				  e2opkt->th->pinno, 
				  datatype, 
				  quality, 
				  e2opkt->th->nsamp, 
				  e2opkt->th->samprate, 
				  e2opkt->th->starttime, 
				  e2opkt->th->endtime );
		} else {

			elog_notify( 0, "'%s':\tunpacked TRACEBUF Trace data\n", 
			  	e2opkt->itname  );
		}

	} else if( STREQ( e2opkt->typestr, Default_TYPE_TRACEBUF2 ) ) { 

		if( e2opkt->th2 ) {

			strncpy( version, e2opkt->th2->version, 2 );
			strncpy( datatype, e2opkt->th2->datatype, 2 );
			strncpy( quality, e2opkt->th2->quality, 2 );

			elog_notify( 0, 
				  "'%s': unpacked TRACEBUF2 Trace-data:\n\t"
				  "%s %s %s %s pinno %d "
				  "version %s datatype %s quality '%s'\n\t"
				  "nsamp %d samprate %f\n\t"
				  "starttime %f endtime %f\n",
				  e2opkt->itname,
				  e2opkt->th2->net, 
				  e2opkt->th2->sta, 
				  e2opkt->th2->chan,
				  e2opkt->th2->loc,
				  e2opkt->th2->pinno, 
				  version, 
				  datatype, 
				  quality, 
				  e2opkt->th2->nsamp, 
				  e2opkt->th2->samprate, 
				  e2opkt->th2->starttime, 
				  e2opkt->th2->endtime );
		} else {

			elog_notify( 0, "'%s':\tunpacked TRACEBUF2 Trace data\n", 
			  	e2opkt->itname  );
		}

	} else if( STREQ( e2opkt->typestr, Default_TYPE_TRACE_COMP_UA ) ) {

		if( e2opkt->th ) {

			strncpy( datatype, e2opkt->th->datatype, 2 );
			strncpy( quality, e2opkt->th->quality, 2 );

			elog_notify( 0, 
				  "'%s': unpacked Compressed trace-data:\n\t"
				  "%s %s %s pinno %d "
				  "datatype %s quality '%s'\n\t"
				  "nsamp %d samprate %f\n\t"
				  "starttime %f endtime %f\n" ,
				  e2opkt->itname,
				  e2opkt->th->net, 
				  e2opkt->th->sta, 
				  e2opkt->th->chan,
				  e2opkt->th->pinno, 
				  datatype,
				  quality,
				  e2opkt->th->nsamp, 
				  e2opkt->th->samprate, 
				  e2opkt->th->starttime, 
				  e2opkt->th->endtime );
		} else {

			elog_notify( 0, "'%s':\tunpacked Compressed trace data\n", 
			  	e2opkt->itname  );
		}

	} else {

		elog_complain( 0, "'%s':\tunpacked Unknown data\n", e2opkt->itname  );
	}

	return;
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

	/* Unused fallthrough to silence compiler complaint: */

	return (struct stat *) NULL;
}

static int	
init_calibinfo( void )
{
	Dbptr 	db;
	int	ret;

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

	return 0;
}

void
set_calibration_database( Pf *pf )
{
	char	*dbname;
	static int first = 1;

	if( ! first ) {

		return;

	} else {

		first = 0;
	}

	if( ( dbname = pfget_string( pf, "calibration_database" ) ) == NULL ) {

		strcpy( Calibinfo.dbname, "" );

		elog_complain( 0,
		     "WARNING: no calibration_database specified "
		     "in parameter file!!\n" );

	} else {
		
		strcpy( Calibinfo.dbname, dbname );

		if( translate_loglevel( Program_loglevel ) >= VERBOSE ) {

			elog_notify( 0, 
				"using database \"%s\" for calibration data\n", 
				Calibinfo.dbname );
		}
	}

	init_calibinfo();
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
add_segtype( char *sta, char *chan, double time, char *default_segtype, 
	     enum Loglevel loglevel, Dbptr *pdb )
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
		strcpy( segtype, default_segtype );
	}

	if( ! strcmp( segtype, "-" ) ) {

		strcpy( segtype, default_segtype );

		if( loglevel == VERYVERBOSE ) {
			fprintf( stderr, 
			  "Database has null segtype for %s; using default\n",
			  key );
		}
	}

	if( loglevel == VERYVERBOSE ) {
		fprintf( stderr, "Using segtype %s for %s\n",
			 segtype, key );
	}

	if( pdb == (Dbptr *) NULL ) {
		dbclose( db );
	}

	return segtype;
}

static void
update_segtypevals( double time, char *default_segtype, enum Loglevel loglevel )
{
	Dbptr	db;
	Tbl	*keys;
	char	*key;
	char	*s;	
	Tbl	*ssplit;
	int 	i;

	if( loglevel == VERYVERBOSE ) {
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
			     time, default_segtype, loglevel, &db );

		free( s );
		freetbl( ssplit, 0 );
	}

	dbclose( db );
}

static Calibvals * 
add_current_calibvals( char *sta, char *chan, double time, 
		       enum Loglevel loglevel, Dbptr *pdb )
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

	if( loglevel == VERYVERBOSE ) {
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
update_calibvals( double time, enum Loglevel loglevel )
{
	Dbptr	db;
	Tbl	*keys;
	char	*key;
	char	*s;	
	Tbl	*ssplit;
	int 	i;

	if( loglevel == VERYVERBOSE ) {
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
				       time, loglevel, &db );

		free( s );
		freetbl( ssplit, 0 );
	}

	dbclose( db );
}

static void
get_calibinfo( Srcname *parts, double time, char **segtype, 
	       double *calib, double *calper, 
	       char *default_segtype, enum Loglevel loglevel )
{
	char	key[STRSZ];
	Calibvals *cv;

	if( Calibinfo.usedbsegtype == 0 ) {

		if( *segtype == NULL ) {
			allot( char *, *segtype, 3 );
		}
		strncpy( *segtype, default_segtype, 2 );
		(*segtype)[1] = '\0';

	} else {

		if( update_is_necessary( "sensor", Calibinfo.strk ) ||
		    update_is_necessary( "instrument", Calibinfo.itrk ) ) {

			/* Use the current packet time to find valid rows: */
			update_segtypevals( time, default_segtype, loglevel );
		}	

		sprintf( key, "%s:%s", parts->src_sta, parts->src_chan );
	
		*segtype = (char *) getarr( Calibinfo.segtypearr, key );

		if( *segtype == (char *) NULL ) {

			*segtype = add_segtype( parts->src_sta, parts->src_chan,
						time, default_segtype,
						loglevel, 0 );
		}
	}

	if( Calibinfo.usedbcalib == 0 ) {

		*calib = DEFAULT_CALIB;
		*calper = DEFAULT_CALPER;

	} else {

		if( update_is_necessary( "calibration", Calibinfo.ctrk ) ) {
			
			update_calibvals( time, loglevel );
		}

		sprintf( key, "%s:%s", parts->src_sta, parts->src_chan );

		cv = (Calibvals *) getarr( Calibinfo.calibarr, key );

		if( cv == (Calibvals *) NULL || 
		    ( cv->validuntil != 9999999999.999 && time > cv->validuntil ) ) {

			cv = add_current_calibvals( parts->src_sta, 
						    parts->src_chan, 
						    time, loglevel, 0 );
		}

		*calib = cv->calib;
		*calper = cv->calper;
	}
	
	return;
}

int
buf_intake( ImportThread *it )
{
	Ew2orbPacket *e2opkt;
	char	*cp;
	char	astring[STRSZ];
	char	*s;

	e2opkt = new_Ew2orbPacket();

	e2opkt->it = it;

	strcpy( e2opkt->itname, it->name );

	cp = it->buf;

	memcpy( astring, cp, 3 );
	cp += 3;
	astring[3] = 0;
	e2opkt->inst = atoi( astring );

	memcpy( astring, cp, 3 );
	cp += 3;
	astring[3] = 0;
	e2opkt->mod = atoi( astring );

	memcpy( astring, cp, 3 );
	cp += 3;
	astring[3] = 0;
	e2opkt->type = atoi( astring );

	e2opkt->loglevel = it->loglevel;

	RESIZE_BUFFER( char *, e2opkt->buf, 
		       e2opkt->bufsize, it->nbytes - EWLOGO_SIZE + 1 );

	memset( e2opkt->buf, 0, e2opkt->bufsize + 1 );
	memcpy( e2opkt->buf, cp, it->nbytes - EWLOGO_SIZE );
	e2opkt->nbytes = it->nbytes - EWLOGO_SIZE;

	ewlogo_tostrings( e2opkt->inst, e2opkt->mod, e2opkt->type, 
			  e2opkt->inststr, e2opkt->modstr, e2opkt->typestr );

	if( STREQ( e2opkt->typestr, Default_TYPE_HEARTBEAT ) ) {

		if( it->loglevel == VERYVERBOSE ) {

			describe_packet( e2opkt );
		}

		if( ! strmatches( cp, 
				  it->expect_heartbeat_string, 
				  &it->expect_heartbeat_hook ) ) {

			elog_complain( 0, 
				"'%s': Warning: Received heartbeat '%s' doesn't "
				"match expected pattern '%s'\n",
				it->name, cp, it->expect_heartbeat_string );
		} else {
			
			if( it->loglevel == VERYVERBOSE ) {
			  if( it->last_heartbeat_received == 0 ) {

				elog_notify( 0, "'%s': First heartbeat received\n",
					     it->name );
			  } else {

				s = strtdelta( now() - 
					       it->last_heartbeat_received );

				elog_notify( 0, 
					  "'%s': Received-heartbeat latency %s\n",
					  it->name, s );

				free( s );
			  }
			}

			it->last_heartbeat_received = now();
		}

		free_Ew2orbPacket( e2opkt );
			
	} else {

		e2opkt->it->npacketrefs++;

		pmtfifo_push( E2oPackets_mtf, (void *) e2opkt ); 
	}

	return 0;
}

static void
usage() 
{
	cbanner( "$Date$", 
		 "[-p pfname] orb",
		 "Kent Lindquist", 
		 "Lindquist Consulting", 
		 "kent@lindquistconsulting.com" );
	
	return;
}

static void
free_ImportThread( ImportThread **it )
{

	mutex_destroy( &(*it)->it_mutex );

	if( (*it)->pf ) {

		pffree( (*it)->pf );
	}

	if( (*it)->buf ) {

		free( (*it)->buf );
	}

	if( (*it)->srcname_morphmap ) {

		freemorphtbl( (*it)->srcname_morphmap );
	}

	if( (*it)->timecorr ) {

		freearr( (*it)->timecorr, 0 );
	}

	if( (*it)->reject_hook ) {

		free_hook( &((*it)->reject_hook) );
	}

	if( (*it)->select_hook ) {

		free_hook( &((*it)->select_hook) );
	}

	free( *it );

	*it = 0;

	return;
}

ImportThread *
find_import_thread_byname( char *name )
{
	ImportThread *it;

	rw_rdlock( &Import_Threads_rwlock );

	it = getarr( Import_Threads, name ); 

	rw_unlock( &Import_Threads_rwlock );

	return it;
}

static void
stop_import_thread( char *name )
{
	ImportThread *it;

	if( ( it = find_import_thread_byname( name ) ) == 0 ) { 

		elog_complain( 1, "stop_import_thread: "
			  "Couldn't find import thread '%s' in registry\n",
			  name );
		return;
	}

	if( Flags.VeryVerbose ) {

		elog_notify( 0, "'%s': Request sent to stop import thread, "
			     "thread-id %ld\n", name, (long) it->thread_id );
	}

	mutex_lock( &it->it_mutex );

	it->stop = 1;

	mutex_unlock( &it->it_mutex );

	return;
}

static Tbl *
import_thread_names() 
{
	Tbl	*keys;
	Tbl	*dup;

	rw_rdlock( &Import_Threads_rwlock );

	keys = keysarr( Import_Threads );

	dup = duptbl( keys, (void *(*)()) strdup );

	freetbl( keys, 0 );

	rw_unlock( &Import_Threads_rwlock );

	return dup;
}

static int
num_import_threads()
{
	int	nthreads;

	rw_rdlock( &Import_Threads_rwlock );

	nthreads = cntarr( Import_Threads );

	rw_unlock( &Import_Threads_rwlock );

	return nthreads;
}

static void 
stop_all_import_threads()
{
	int	i;
	Tbl	*keys;

	keys = import_thread_names();

	for( i = 0; i < maxtbl( keys ); i++ ) {

		stop_import_thread( gettbl( keys, i ) );
	}

	freetbl( keys, (void (*)(void *)) free );

	return;
}

ImportThread *
find_import_thread_byid( thread_t tid )
{
	ImportThread *it;
	ImportThread *found = (ImportThread *) NULL;
	Tbl	*keys;
	char	*key;
	int 	i;

	rw_rdlock( &Import_Threads_rwlock );

	keys = keysarr( Import_Threads );

	for( i = 0; i < maxtbl( keys ); i++ ) {

		key = gettbl( keys, i );

		it = (ImportThread *) getarr( Import_Threads, key );

		if( it->thread_id == tid ) {

			found = it;

			break;
		}
	}

	freetbl( keys, 0 );

	rw_unlock( &Import_Threads_rwlock );

	return found;
}


static void
add_import_thread( char *name, ImportThread *it ) 
{
	rw_wrlock( &Import_Threads_rwlock );

	setarr( Import_Threads, name, (void *) it );

	rw_unlock( &Import_Threads_rwlock );

	return;
}

static void
delete_import_thread( ImportThread *it ) 
{
	if( it != (ImportThread *) NULL ) {

		mutex_trylock( &it->it_mutex );
		mutex_unlock( &it->it_mutex );

		rw_wrlock( &Import_Threads_rwlock );

		delarr( Import_Threads, it->name );

		if( it->npacketrefs <= 0 ) {

			free_ImportThread( &it );
		}

		rw_unlock( &Import_Threads_rwlock );
	}

	return;
}

static void
close_import_connection( ImportThread *it )
{
	char	eof = 004;

	bnsput( it->bnsout, &eof, BYTES, 1 );
	bnsflush( it->bnsout );

	bnsfree( it->bnsout );

	/* Keep export_generic from interpreting flushed data
	   as a malformed heartbeat: */

	bnsclr( it->bnsin );

	bnsclose( it->bnsin );

	it->bnsin = 0;

	if( it->stop == 0 ) {
		
		sleep( SERVER_RESET_ALLOWANCE_SEC );
	}

	return;
}

static void
ew2orb_import_shutdown()
{
	int	status = 0;
	ImportThread *it;
	char	name[STRSZ] = "Unknown";

	if( ( it = find_import_thread_byid( thr_self() ) ) == NULL ) {

		elog_complain( 0, "Couldn't find thread %ld in registry!\n",
			  	  (long) thr_self() );

	} else {
	
		if( it->bnsin != NULL ) {

			close_import_connection( it );
		}

		if( it->name != NULL ) {

			strcpy( name, it->name );
		}
	}

	if( Flags.verbose ) {

		elog_notify( 0, 
		"'%s': Thread (thread-id %ld) stopping at user request\n",
		  name, (long) thr_self() );
	}

	delete_import_thread( it );

	thr_exit( (void *) &status );

	return;
}

static void
reconfig_import_thread( ImportThread *it )
{
	char	*loglevel;
	Tbl	*morphlist;
	Tbl	*new_morphlist;
	Arr	*timecorr;

	mutex_lock( &it->it_mutex );

	if( it->stop == 1 ) {

		ew2orb_import_shutdown();
	}

	if( it->update == 1 ) {

		if( it->loglevel >= VERBOSE ) {

			if( it->new ) {
					
				elog_notify( 0, 
			  	"'%s': Configuring thread with: ", 
			  	it->name );

			} else {

				elog_notify( 0, 
			  	"'%s': Reconfiguring thread with: ", 
			  	it->name );

			}

			pfout( stderr, it->pf );
		}
			
		if( it->bnsin != NULL ) {
			
			close_import_connection( it );

			it->last_heartbeat_sent = 0;
		}

		strcpy( it->server_ipaddress,
			pfget_string( it->pf, "server_ipaddress" ) );

		it->server_port = 
			(in_port_t) pfget_int( it->pf, "server_port" );

		strcpy( it->expect_heartbeat_string,
			pfget_string( it->pf, "expect_heartbeat_string" ) );

		if( it->expect_heartbeat_hook ) {

			free_hook( &it->expect_heartbeat_hook );
		}

		strcpy( it->send_heartbeat_string,
			pfget_string( it->pf, "send_heartbeat_string" ) );

		it->send_heartbeat_sec = 
			pfget_int( it->pf, "send_heartbeat_sec" );

		strcpy( it->my_inst_str,
			pfget_string( it->pf, "my_inst" ) );

		strcpy( it->my_mod_str,
			pfget_string( it->pf, "my_mod" ) );

		ewlogo_tologo( it->my_inst_str, 
			       it->my_mod_str, 
			       Default_TYPE_HEARTBEAT, 
			       &it->my_inst,
			       &it->my_mod,
			       &it->my_type_heartbeat );

		strcpy( it->select, pfget_string( it->pf, "select" ) );
		strcpy( it->reject, pfget_string( it->pf, "reject" ) );

		strcpy( it->default_segtype, 
			pfget_string( it->pf, "default_segtype" ) );

		if( it->select_hook ) {

			free_hook( &it->select_hook );
		} 

		if( it->reject_hook ) {

			free_hook( &it->reject_hook );
		} 

		loglevel = pfget_string( it->pf, "loglevel" );

		it->loglevel = translate_loglevel( loglevel );
		
		if( it->srcname_morphmap != (Morphtbl *) NULL ) {
			
			freemorphtbl( it->srcname_morphmap );
		}

		morphlist = pfget_tbl( it->pf, "srcname_morph" );

		new_morphlist = healthy_morphlist( morphlist );

		freetbl( morphlist, 0 );

		newmorphtbl( new_morphlist, &it->srcname_morphmap );

		if( new_morphlist != (Tbl *) NULL ) {
			
			freetbl( new_morphlist, (void (*)(void *)) free );
		}

		if( it->timecorr != (Arr *) NULL ) {
			
			freearr( it->timecorr, free );
		}

		timecorr = pfget_arr( it->pf, "timecorr" );

		if( timecorr != (Arr *) NULL ) {

			it->timecorr = 
			    duparr( timecorr, ( void *(*)(void *) ) strdup );

			freearr( timecorr, 0 );
		}

		it->update = 0;
		it->new = 0;
		it->connectfail = 0;
		it->bindfail = 0;
	}

	mutex_unlock( &it->it_mutex );

	return;
}

static void
refresh_import_thread( ImportThread *it )
{
	reconfig_import_thread( it );

	while( it->bnsin == NULL ) {

		reconfig_import_thread( it );

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

		if( ( it->loglevel >= VERBOSE ) || Flags.verbose ) {

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

			if( ( it->loglevel >= VERBOSE ) || Flags.verbose ) {

				elog_notify( 0, 
					  "'%s': import thread Connected "
					  "to remote export module\n",
					  it->name );
			}
		}

		it->bnsin = bnsnew( it->so, BNS_BUFFER_SIZE ); 
		it->bnsout = bnsnew( it->so, BNS_BUFFER_SIZE ); 

		bnsuse_sockio( it->bnsin );
		bnsuse_sockio( it->bnsout );

		if( it->send_heartbeat_sec*1000 < DEFAULT_BNS_TIMEOUT ) {

			bnstimeout( it->bnsin, 
				it->send_heartbeat_sec * 1000 );

		} else {

			bnstimeout( it->bnsin, DEFAULT_BNS_TIMEOUT );
		}
	}

	return;
}

static void
send_heartbeat( ImportThread *it )
{
	char 	msg[STRSZ]; 
	char	stx = STX;
	char	etx = ETX;
	char	nul = 0;
	int	msglen;

	if( now() - it->last_heartbeat_sent < it->send_heartbeat_sec ) {
		
		return;
	}

	sprintf( msg, "%c%3d%3d%3d%s%c%c",
		      stx,
		      it->my_inst,
		      it->my_mod, 
		      it->my_type_heartbeat,
		      it->send_heartbeat_string,
		      nul,
		      etx );

	msglen = 12 + strlen( it->send_heartbeat_string );

	if( it->loglevel == VERYVERBOSE ) {

		elog_notify( 0, "'%s': Sending heartbeat: \"%s\"\n",
			  it->name, it->send_heartbeat_string );
	}

	bnsput( it->bnsout, msg, BYTES, msglen );

	bnsflush( it->bnsout );

	it->last_heartbeat_sent = now();

	return;
}

static void
display_banner( char *msg )
{
	char	bannercmd[STRSZ];

	if( Flags.have_banner == 0 ) {

		return;
	}

	sprintf( bannercmd, "%s %s", DEFAULT_BANNER_PATH, msg );

	system( bannercmd );

	return;
}

static int
more_packet_data( ImportThread *it )
{
	int	rc;
	char	c;
	char	previous = 0;
	int	bns_errno;

	while( ( rc = bnsget( it->bnsin, &c, BYTES, 1 ) ) == 0 ) {
			
		switch( it->sync_state ) {

		case STREAM_SEARCH: 

			if( c == STX && previous != ESC ) {

				it->sync_state = STREAM_ACCUMULATE;
				it->nbytes = 0;

			} else {

				BAD_BYTE( "search", c );
			}

			break;

		case STREAM_ACCUMULATE:

			if( c == ESC ) { 

				it->sync_state = STREAM_ESCAPE;

			} else if( c == ETX ) {

				/* Guarantee null-termination of 
				   string messages */

				APPEND_BUFFER( it->buf, it->bufsize, 
					       it->nbytes, '\0' );
				it->nbytes--;

				it->sync_state = STREAM_SEARCH;

				return 1;

			} else {

				APPEND_BUFFER( it->buf, it->bufsize, 
					       it->nbytes, c );
			}

			break;

		case STREAM_ESCAPE:
			
			if( c == ETX || c == STX || c == ESC ) {

				APPEND_BUFFER( it->buf, it->bufsize, 
					       it->nbytes, c );

				it->sync_state = STREAM_ACCUMULATE;

			} else {
	
				APPEND_BUFFER( it->buf, it->bufsize, 
					       it->nbytes, previous );
				APPEND_BUFFER( it->buf, it->bufsize, 
					       it->nbytes, c );

				it->sync_state = STREAM_ACCUMULATE;
			}

			break;

		default: 

			BAD_BYTE( "default", c );

			it->sync_state = STREAM_SEARCH;

			break;
		}

		previous = c;
	}

	if( bnserr( it->bnsin ) != 0 ) {
			
		bns_errno = bnserrno( it->bnsin );

		if( ( it->loglevel >= VERBOSE ) || Flags.verbose ) {

		  if( bns_errno == ECONNRESET ) {

			elog_complain( 1, 
				  "'%s': Connection reset by peer\n",
				  it->name );

			if( it->loglevel == VERYVERBOSE ) {

				display_banner( "ECONNRESET" ); 
			}

		  } else {

			elog_complain( 1, 
				  "'%s': bns error %d\n", 
				  it->name, bns_errno );
		  }
		}

		return -1;

	} else if( bnseof( it->bnsin ) ) {

		if( ( it->loglevel >= VERBOSE ) || Flags.verbose ) {

			elog_complain( 1, 
				  "'%s': Connection closed by remote "
				  "export module\n",
				  it->name );

			if( it->loglevel == VERYVERBOSE ) {

				display_banner( "CLOSED" ); 
			}
		}

		return -1;

	} else {

		return 0;
	}
}

static void *
ew2orb_import( void *arg )
{
	char	*name = (char *) arg;
	ImportThread *it;
	int	status = 0;
	int	rc;

	thr_setprio( thr_self(), THR_PRIORITY_IMPORT );

	if( Flags.verbose ) {

		elog_notify( 0,
			  "'%s':...ew2orb_import thread started\n",
			  name );
	}

	if( ( it = find_import_thread_byid( thr_self() ) ) == NULL ) {

		elog_complain( 1, 
			"Couldn't find thread id %ld in registry!\n",
			 (long) thr_self() );

		status = -1;

		thr_exit( (void *) &status );
	} 

	for( ;; ) {

		refresh_import_thread( it );

		send_heartbeat( it );

		if( ( rc = more_packet_data( it ) ) > 0 ) {

			buf_intake( it );

		} else if( rc < 0 ) {

			close_import_connection( it );
		}
	}
}

static ImportThread *
new_ImportThread( char *name )
{
	ImportThread *it;

	allot( ImportThread *, it, 1 );

	strcpy( it->name, name );
	it->update = 1;
	it->new = 1;
	it->stop = 0;
	it->npacketrefs = 0;
	it->pf = pfnew( PFARR );
	it->bnsin = NULL;
	it->bnsout = NULL;
	it->buf = 0;
	it->bufsize = 0;
	it->nbytes = 0;
	it->sync_state = STREAM_SEARCH;
	it->loglevel = QUIET;
	it->thread_id = (pthread_t) -1;
	it->last_heartbeat_sent = 0;
	it->last_heartbeat_received = 0;
	it->expect_heartbeat_hook = NULL;
	it->connectfail = 0;
	it->bindfail = 0;
	it->srcname_morphmap = NULL;
	it->timecorr = NULL;
	it->select_hook = NULL;
	it->reject_hook = NULL;

	mutex_init( &it->it_mutex, USYNC_THREAD, NULL );

	return it;
}

static void
update_import_thread( char *name, Pf *pf )
{
	ImportThread *it;
	Pf	*oldpf;
	char	key[STRSZ];
	int	ret;

	if( ( it = find_import_thread_byname( name ) ) == 0 ) {

		it = new_ImportThread( name );

		pfput_int( it->pf, "server_port", DEFAULT_SERVER_PORT );

		pfput_string( it->pf, "server_ipaddress", "" );

		pfput_int( it->pf, 
			   "send_heartbeat_sec", 
			   DEFAULT_SEND_HEARTBEAT_SEC );

		pfput_int( it->pf, 
			   "expect_heartbeat_sec", 
			   DEFAULT_EXPECT_HEARTBEAT_SEC );

		pfput_string( it->pf, 
			      "send_heartbeat_string", 
			      DEFAULT_SEND_HEARTBEAT_STRING );

		pfput_string( it->pf, 
			      "expect_heartbeat_string", 
			      DEFAULT_EXPECT_HEARTBEAT_STRING );

		pfput_string( it->pf, 
			      "select", 
			      DEFAULT_SELECT );

		pfput_string( it->pf, 
			      "reject", 
			      DEFAULT_REJECT );

		pfput_string( it->pf, 
			      "default_segtype", 
			      DEFAULT_SEGTYPE );

		pfput_string( it->pf, 
			      "loglevel", 
			      Program_loglevel );

		pfput_string( it->pf, 
			      "my_inst", 
			      DEFAULT_INST );

		pfput_string( it->pf, 
			      "my_mod", 
			      DEFAULT_MOD );
	} 

	mutex_lock( &it->it_mutex );

	oldpf = pfdup( it->pf );

	pfput_string( it->pf, "loglevel", Program_loglevel );

	pfreplace( pf, it->pf, "defaults{server_port}",
			       "server_port", "int" );

	pfreplace( pf, it->pf, "defaults{send_heartbeat_sec}",
			       "send_heartbeat_sec", "int" );

	pfreplace( pf, it->pf, "defaults{send_heartbeat_string}",
			       "send_heartbeat_string", "string" );

	pfreplace( pf, it->pf, "defaults{expect_heartbeat_sec}",
			       "expect_heartbeat_sec", "int" );

	pfreplace( pf, it->pf, "defaults{expect_heartbeat_string}",
			       "expect_heartbeat_string", "string" );

	pfreplace( pf, it->pf, "defaults{select}",
			       "select", "string" );

	pfreplace( pf, it->pf, "defaults{reject}",
			       "reject", "string" );

	pfreplace( pf, it->pf, "defaults{default_segtype}",
			       "default_segtype", "string" );

	pfreplace( pf, it->pf, "defaults{loglevel}",
			       "loglevel", "string" );

	pfreplace( pf, it->pf, "defaults{my_inst}", "my_inst", "string" );

	pfreplace( pf, it->pf, "defaults{my_mod}", "my_mod", "string" );

	pfreplace( pf, it->pf, "defaults{srcname_morph}", 
			       "srcname_morph", "tbl" );

	pfreplace( pf, it->pf, "defaults{timecorr}", 
			       "timecorr", "arr" );

	sprintf( key, "import_from{%s}{server_port}", name );
	pfreplace( pf, it->pf, key, "server_port", "int" );

	sprintf( key, "import_from{%s}{server_ipaddress}", name );
	pfreplace( pf, it->pf, key, "server_ipaddress", "string" );

	sprintf( key, "import_from{%s}{send_heartbeat_sec}", name );
	pfreplace( pf, it->pf, key, "send_heartbeat_sec", "int" );

	sprintf( key, "import_from{%s}{send_heartbeat_string}", name );
	pfreplace( pf, it->pf, key, "send_heartbeat_string", "string" );

	sprintf( key, "import_from{%s}{expect_heartbeat_sec}", name );
	pfreplace( pf, it->pf, key, "expect_heartbeat_sec", "int" );

	sprintf( key, "import_from{%s}{expect_heartbeat_string}", name );
	pfreplace( pf, it->pf, key, "expect_heartbeat_string", "string" );

	sprintf( key, "import_from{%s}{select}", name );
	pfreplace( pf, it->pf, key, "select", "string" );

	sprintf( key, "import_from{%s}{reject}", name );
	pfreplace( pf, it->pf, key, "reject", "string" );

	sprintf( key, "import_from{%s}{default_segtype}", name );
	pfreplace( pf, it->pf, key, "default_segtype", "string" );

	sprintf( key, "import_from{%s}{loglevel}", name );
	pfreplace( pf, it->pf, key, "loglevel", "string" );

	sprintf( key, "import_from{%s}{my_inst}", name );
	pfreplace( pf, it->pf, key, "my_inst", "string" );

	sprintf( key, "import_from{%s}{my_mod}", name );
	pfreplace( pf, it->pf, key, "my_mod", "string" );

	sprintf( key, "import_from{%s}{srcname_morph}", name );
	pfreplace( pf, it->pf, key, "srcname_morph", "tbl" );

	sprintf( key, "import_from{%s}{timecorr}", name );
	pfreplace( pf, it->pf, key, "timecorr", "arr" );

	if( it->new ) {

		add_import_thread( name, it );

		ret = thr_create( NULL, 0, ew2orb_import, 
				  (void *) name, 
				  THR_DETACHED,
				  &it->thread_id );

		if( ret != 0 ) {

			elog_complain( 1,
			    "'%s': Failed to create import thread: "
			    "thr_create error %d\n", name, ret );
			
			delete_import_thread( it );

			return;
		}

	} else if( pfcmp( oldpf, it->pf ) ) {

		it->update = 1;

	} else {

		it->update = 0;
	}

	if( oldpf ) {

		pffree( oldpf );
	}

	if( Flags.VeryVerbose ) {

		if( it->new ) {

			elog_notify( 0,
				"'%s': Started import thread as thread-id %ld\n", 
				it->name, (long) it->thread_id );

		} else if( it->update ) {

			elog_notify( 0,
				"'%s': Posted updates for import thread "
				"(thread-id %ld)\n", 
				it->name, (long) it->thread_id );

		} else {

			elog_notify( 0,
				"'%s': Import thread (thread-id %ld) unchanged\n",
				it->name, (long) it->thread_id );
		}
	}

	mutex_unlock( &it->it_mutex );

	return;
}

static void
reconfigure_import_threads( Pf *pf )
{
	Pf	*pfimport_from;
	Tbl	*new_keys;
	Tbl	*existing_keys;
	Arr	*anarr;
	char	*akey;
	int	i;

	if( pfget( pf, "import_from", (void **) &pfimport_from ) != PFARR ) {

		elog_complain( 1, 
		   "parameter 'import_from' not present or not an array\n" );

		stop_all_import_threads();
		
		if( Flags.verbose && num_import_threads() <= 0 ) {

			elog_complain( 0, 
			"Warning: no import threads defined; nothing to do\n" );
		}

		return;
	} 

	new_keys = pfkeys( pfimport_from );

	if( maxtbl( new_keys ) <= 0 ) {

		stop_all_import_threads();
		
		if( Flags.verbose && num_import_threads() <= 0 ) {

			elog_complain( 0, 
			"Warning: no import threads defined; nothing to do\n" );
		}

		freetbl( new_keys, 0 );

		return;
	} 

	existing_keys = import_thread_names();

	for( i = 0; i < maxtbl( existing_keys ); i++ ) {
		
		akey = gettbl( existing_keys, i );

		if( ( anarr = pfget_arr( pfimport_from, akey ) ) == NULL ) {

			stop_import_thread( akey ); 

		} else {

			update_import_thread( akey, pf );

			freearr( anarr, 0 );
		}
	}

	freetbl( existing_keys, (void (*)(void *)) free );
		
	for( i = 0; i < maxtbl( new_keys ); i++ ) {

		akey = gettbl( new_keys, i );

		if( find_import_thread_byname( akey ) != 0 ) {

			continue;
		}

		update_import_thread( akey, pf );
	}

	if( Flags.verbose && num_import_threads() <= 0 ) {

		elog_complain( 0, 
		"Warning: no import threads defined; nothing to do\n" );
	}

	freetbl( new_keys, 0 );

	return;
}

static int
crack_packet( Ew2orbPacket *e2opkt )
{
	int	retcode = 0;
	PktChannel *pktchan;
	char	*dp;
	char	*sp;
	char	old_srcname[ORBSRCNAME_SIZE];
	char	new_srcname[ORBSRCNAME_SIZE];
	char	*timeshift_string = 0;
	double	timeshift = 0;
	static char *temp_segtype = 0;
	int	n;

	if( ! STREQ( e2opkt->typestr, Default_TYPE_TRACEBUF ) &&
	    ! STREQ( e2opkt->typestr, Default_TYPE_TRACEBUF2 ) &&
	    ! STREQ( e2opkt->typestr, Default_TYPE_TRACE_COMP_UA ) ) {

		elog_complain( 0, 
			"convert: Don't know how to crack type <%s>\n",
			e2opkt->typestr );

		return -1;
	}

	e2opkt->pkt = newPkt();
	e2opkt->pkt->pkttype = suffix2pkttype("MGENC");

	e2opkt->pkt->nchannels = 1;

	pktchan = newPktChannel();

	pushtbl( e2opkt->pkt->channels, pktchan );

	/* Acknowledge the Earthworm packing strategy of 
	   memory-copying the entire trace-header structure. 
	   This was an unwise protocol design since the 
	   field order in a structure, officially, is at
	   the whim of the compiler implementation. In an 
	   attempt to sidestep difficulties, unpack the expected 
	   structure contents one item at a time in hopes they packed
	   "correctly" (i.e. without misalignments from compiler-dependent
	   structure padding) on the Earthworm side: */

	if( STREQ( e2opkt->typestr, Default_TYPE_TRACEBUF2 ) ) {

		allot( TRACE2_HEADER *, e2opkt->th2, 1 );

		dp = e2opkt->buf;
		memcpy( &e2opkt->th2->pinno, dp, sizeof( int ) );
		dp += sizeof( int );

		memcpy( &e2opkt->th2->nsamp, dp, sizeof( int ) );
		dp += sizeof( int );

		memcpy( &e2opkt->th2->starttime, dp, sizeof( double ) );
		dp += sizeof( double );

		memcpy( &e2opkt->th2->endtime, dp, sizeof( double ) );
		dp += sizeof( double );

		memcpy( &e2opkt->th2->samprate, dp, sizeof( double ) );
		dp += sizeof( double );

		memcpy( e2opkt->th2->sta, dp, TRACE2_STA_LEN );
		dp += TRACE2_STA_LEN;

		memcpy( e2opkt->th2->net, dp, TRACE2_NET_LEN );
		dp += TRACE2_NET_LEN;

		memcpy( e2opkt->th2->chan, dp, TRACE2_CHAN_LEN );
		dp += TRACE2_CHAN_LEN;

		memcpy( e2opkt->th2->loc, dp, TRACE2_LOC_LEN );
		dp += TRACE2_LOC_LEN;

		memcpy( e2opkt->th2->version, dp, 2 );
		dp += 2;

		memcpy( e2opkt->th2->datatype, dp, 3 );
		e2opkt->th2->datatype[2] = 0;
		dp += 3;

		memcpy( e2opkt->th2->quality, dp, 2 );
		dp += 2;

		memcpy( e2opkt->th2->pad, dp, 2 );
		dp += 2;

		strcpy( e2opkt->pkt->parts.src_net, e2opkt->th2->net );
		strcpy( e2opkt->pkt->parts.src_sta, e2opkt->th2->sta );
		strcpy( e2opkt->pkt->parts.src_chan, e2opkt->th2->chan );

		if( STREQ( e2opkt->th2->loc, LOC_NULL_STRING ) ) {

			strcpy( e2opkt->pkt->parts.src_loc, "" );

		} else {

			strcpy( e2opkt->pkt->parts.src_loc, e2opkt->th2->loc );
		}

	} else {

		allot( TRACE_HEADER *, e2opkt->th, 1 );

		dp = e2opkt->buf;
		memcpy( &e2opkt->th->pinno, dp, sizeof( int ) );
		dp += sizeof( int );

		memcpy( &e2opkt->th->nsamp, dp, sizeof( int ) );
		dp += sizeof( int );

		memcpy( &e2opkt->th->starttime, dp, sizeof( double ) );
		dp += sizeof( double );

		memcpy( &e2opkt->th->endtime, dp, sizeof( double ) );
		dp += sizeof( double );

		memcpy( &e2opkt->th->samprate, dp, sizeof( double ) );
		dp += sizeof( double );

		memcpy( e2opkt->th->sta, dp, TRACE_STA_LEN );
		dp += TRACE_STA_LEN;

		memcpy( e2opkt->th->net, dp, TRACE_NET_LEN );
		dp += TRACE_NET_LEN;

		memcpy( e2opkt->th->chan, dp, TRACE_CHAN_LEN );
		dp += TRACE_CHAN_LEN;

		memcpy( e2opkt->th->datatype, dp, 3 );
		e2opkt->th->datatype[2] = 0;
		dp += 3;

		memcpy( e2opkt->th->quality, dp, 2 );
		dp += 2;

		memcpy( e2opkt->th->pad, dp, 2 );
		dp += 2;

		strcpy( e2opkt->pkt->parts.src_net, e2opkt->th->net );
		strcpy( e2opkt->pkt->parts.src_sta, e2opkt->th->sta );
		strcpy( e2opkt->pkt->parts.src_chan, e2opkt->th->chan );
		strcpy( e2opkt->pkt->parts.src_loc, "" );
	}

	join_srcname( &e2opkt->pkt->parts, old_srcname );

	mutex_lock( &e2opkt->it->it_mutex );
		
	if( ! STREQ( e2opkt->it->reject, "" ) ) {

	    	n = strmatches( old_srcname, 
				e2opkt->it->reject,
				&e2opkt->it->reject_hook );

		if( n < 0 ) { 

			elog_complain( 0, 
			"Couldn't compile reject expression '%s'\n",
			e2opkt->it->reject );

		} else if( n > 0 ) {

			if( e2opkt->it->loglevel == VERYVERBOSE ) {

				elog_notify( 0, "'%s': Rejecting %s\n",
					     e2opkt->itname,
					     old_srcname );
			}

			mutex_unlock( &e2opkt->it->it_mutex );

			return -1;
		}
	}

	if( ! STREQ( e2opkt->it->select, "" ) ) {

	    	n = strmatches( old_srcname, 
				e2opkt->it->select,
				&e2opkt->it->select_hook );

		if( n < 0 ) { 

			elog_complain( 0, 
			"Couldn't compile select expression '%s'\n",
			e2opkt->it->select );

		} else if( n == 0 ) {

			if( e2opkt->it->loglevel == VERYVERBOSE ) {

				elog_notify( 0, 
				"'%s': %s doesn't match select expression\n",
					     e2opkt->itname,
					     old_srcname );
			}

			mutex_unlock( &e2opkt->it->it_mutex );

			return -1;
		}
	}

	if( e2opkt->it->timecorr != (Arr *) NULL ) {

		timeshift_string = getarr( e2opkt->it->timecorr, old_srcname );

		if( timeshift_string != (char *) NULL ) {

			timeshift = atof( timeshift_string );

			if( e2opkt->it->loglevel == VERYVERBOSE ) {

				elog_notify( 0, "'%s': adding %f seconds to "
				"timestamp value for incoming packet %s\n", 
				e2opkt->itname, timeshift, old_srcname );
						
			}

		} else {
			
			timeshift = 0.0;
		}

	} else {

		timeshift = 0.0;
	}

	n = morphtbl( old_srcname, e2opkt->it->srcname_morphmap, 
		      MORPH_ALL|MORPH_PARTIAL, new_srcname );

	mutex_unlock( &e2opkt->it->it_mutex );

	split_srcname( new_srcname, &e2opkt->pkt->parts );

	if( e2opkt->it->loglevel == VERYVERBOSE && 
	    ( n != 0 || strcmp( old_srcname, new_srcname ) ) ) {

		elog_notify( 0, "'%s': mapped %s to %s (%d "
				"transformation%s\n", 
				e2opkt->itname, old_srcname, 
				new_srcname, n,
				n == 1 ? ")" : "s)" );
	}

	strcpy( pktchan->net, e2opkt->pkt->parts.src_net );
	strcpy( pktchan->sta, e2opkt->pkt->parts.src_sta );
	strcpy( pktchan->chan, e2opkt->pkt->parts.src_chan );
	strcpy( pktchan->loc, e2opkt->pkt->parts.src_loc );

	if( STREQ( e2opkt->typestr, Default_TYPE_TRACEBUF2 ) ) {

		if( STREQ( e2opkt->th2->datatype, "s4" ) ) {

			sp = (char *) &e2opkt->th2->pinno;
			mi2hi( &sp, &e2opkt->th2->pinno, 1 );

			sp = (char *) &e2opkt->th2->nsamp;
			mi2hi( &sp, &e2opkt->th2->nsamp, 1 );

			sp = (char *) &e2opkt->th2->samprate;
			md2hd( &sp, &e2opkt->th2->samprate, 1 );

			sp = (char *) &e2opkt->th2->starttime;
			md2hd( &sp, &e2opkt->th2->starttime, 1 );

			sp = (char *) &e2opkt->th2->endtime;
			md2hd( &sp, &e2opkt->th2->endtime, 1 );

		} else if( STREQ( e2opkt->th2->datatype, "s2" ) ) {

			sp = (char *) &e2opkt->th2->pinno;
			mi2hi( &sp, &e2opkt->th2->pinno, 1 );

			sp = (char *) &e2opkt->th2->nsamp;
			mi2hi( &sp, &e2opkt->th2->nsamp, 1 );

			sp = (char *) &e2opkt->th2->samprate;
			md2hd( &sp, &e2opkt->th2->samprate, 1 );

			sp = (char *) &e2opkt->th2->starttime;
			md2hd( &sp, &e2opkt->th2->starttime, 1 );

			sp = (char *) &e2opkt->th2->endtime;
			md2hd( &sp, &e2opkt->th2->endtime, 1 );

		} else if( STREQ( e2opkt->th2->datatype, "i4" ) ) {

			sp = (char *) &e2opkt->th2->pinno;
			vi2hi( &sp, &e2opkt->th2->pinno, 1 );

			sp = (char *) &e2opkt->th2->nsamp;
			vi2hi( &sp, &e2opkt->th2->nsamp, 1 );
				
			sp = (char *) &e2opkt->th2->samprate;
			vd2hd( &sp, &e2opkt->th2->samprate, 1 );

			sp = (char *) &e2opkt->th2->starttime;
			vd2hd( &sp, &e2opkt->th2->starttime, 1 );

			sp = (char *) &e2opkt->th2->endtime;
			vd2hd( &sp, &e2opkt->th2->endtime, 1 );

		} else if( STREQ( e2opkt->th2->datatype, "i2" ) ) {

			sp = (char *) &e2opkt->th2->pinno;
			vi2hi( &sp, &e2opkt->th2->pinno, 1 );

			sp = (char *) &e2opkt->th2->nsamp;
			vi2hi( &sp, &e2opkt->th2->nsamp, 1 );

			sp = (char *) &e2opkt->th2->samprate;
			vd2hd( &sp, &e2opkt->th2->samprate, 1 );

			sp = (char *) &e2opkt->th2->starttime;
			vd2hd( &sp, &e2opkt->th2->starttime, 1 );

			sp = (char *) &e2opkt->th2->endtime;
			vd2hd( &sp, &e2opkt->th2->endtime, 1 );

		} else {

			elog_complain( 0, 
				"convert: Don't know how to translate datatype <%s>\n",
				e2opkt->th2->datatype );
			
			return -1;
		}

		pktchan->datasz = e2opkt->th2->nsamp;

		pktchan->nsamp = e2opkt->th2->nsamp;
		pktchan->samprate = e2opkt->th2->samprate;
		pktchan->time = e2opkt->th2->starttime + timeshift;

	} else {

		if( STREQ( e2opkt->th->datatype, "s4" ) ) {

			sp = (char *) &e2opkt->th->pinno;
			mi2hi( &sp, &e2opkt->th->pinno, 1 );

			sp = (char *) &e2opkt->th->nsamp;
			mi2hi( &sp, &e2opkt->th->nsamp, 1 );

			sp = (char *) &e2opkt->th->samprate;
			md2hd( &sp, &e2opkt->th->samprate, 1 );

			sp = (char *) &e2opkt->th->starttime;
			md2hd( &sp, &e2opkt->th->starttime, 1 );

			sp = (char *) &e2opkt->th->endtime;
			md2hd( &sp, &e2opkt->th->endtime, 1 );

		} else if( STREQ( e2opkt->th->datatype, "s2" ) ) {

			sp = (char *) &e2opkt->th->pinno;
			mi2hi( &sp, &e2opkt->th->pinno, 1 );

			sp = (char *) &e2opkt->th->nsamp;
			mi2hi( &sp, &e2opkt->th->nsamp, 1 );

			sp = (char *) &e2opkt->th->samprate;
			md2hd( &sp, &e2opkt->th->samprate, 1 );

			sp = (char *) &e2opkt->th->starttime;
			md2hd( &sp, &e2opkt->th->starttime, 1 );

			sp = (char *) &e2opkt->th->endtime;
			md2hd( &sp, &e2opkt->th->endtime, 1 );

		} else if( STREQ( e2opkt->th->datatype, "i4" ) ) {

			sp = (char *) &e2opkt->th->pinno;
			vi2hi( &sp, &e2opkt->th->pinno, 1 );

			sp = (char *) &e2opkt->th->nsamp;
			vi2hi( &sp, &e2opkt->th->nsamp, 1 );
				
			sp = (char *) &e2opkt->th->samprate;
			vd2hd( &sp, &e2opkt->th->samprate, 1 );

			sp = (char *) &e2opkt->th->starttime;
			vd2hd( &sp, &e2opkt->th->starttime, 1 );

			sp = (char *) &e2opkt->th->endtime;
			vd2hd( &sp, &e2opkt->th->endtime, 1 );

		} else if( STREQ( e2opkt->th->datatype, "i2" ) ) {

			sp = (char *) &e2opkt->th->pinno;
			vi2hi( &sp, &e2opkt->th->pinno, 1 );

			sp = (char *) &e2opkt->th->nsamp;
			vi2hi( &sp, &e2opkt->th->nsamp, 1 );

			sp = (char *) &e2opkt->th->samprate;
			vd2hd( &sp, &e2opkt->th->samprate, 1 );

			sp = (char *) &e2opkt->th->starttime;
			vd2hd( &sp, &e2opkt->th->starttime, 1 );

			sp = (char *) &e2opkt->th->endtime;
			vd2hd( &sp, &e2opkt->th->endtime, 1 );

		} else {

			elog_complain( 0, 
				"convert: Don't know how to translate datatype <%s>\n",
				e2opkt->th->datatype );

			return -1;
		}

		pktchan->datasz = e2opkt->th->nsamp;

		pktchan->nsamp = e2opkt->th->nsamp;
		pktchan->samprate = e2opkt->th->samprate;
		pktchan->time = e2opkt->th->starttime + timeshift;
	}

	get_calibinfo( &e2opkt->pkt->parts, pktchan->time, 
		       &temp_segtype, &pktchan->calib, &pktchan->calper,
		       e2opkt->it->default_segtype, 
		       e2opkt->it->loglevel );

	strcpy( pktchan->segtype, temp_segtype );

	if( STREQ( e2opkt->typestr, Default_TYPE_TRACEBUF ) ) {

		if( STREQ( e2opkt->th->datatype, "s4" ) ) {

			allot( int *, pktchan->data, 
			       e2opkt->th->nsamp * sizeof( int ) );

			mi2hi( &dp, pktchan->data, e2opkt->th->nsamp );

		} else if( STREQ( e2opkt->th->datatype, "s2" ) ) {

			allot( int *, pktchan->data, 
			       e2opkt->th->nsamp * sizeof( int ) );

			ms2hi( &dp, pktchan->data, e2opkt->th->nsamp );

		} else if( STREQ( e2opkt->th->datatype, "i4" ) ) {

			allot( int *, pktchan->data, 
			       e2opkt->th->nsamp * sizeof( int ) );

			vi2hi( &dp, pktchan->data, e2opkt->th->nsamp );

		} else if( STREQ( e2opkt->th->datatype, "i2" ) ) {

			allot( int *, pktchan->data, 
			       e2opkt->th->nsamp * sizeof( int ) );

			vs2hi( &dp, pktchan->data, e2opkt->th->nsamp );
		}
		
	} else if( STREQ( e2opkt->typestr, Default_TYPE_TRACEBUF2 ) ) {

		if( STREQ( e2opkt->th2->datatype, "s4" ) ) {

			allot( int *, pktchan->data, 
			       e2opkt->th2->nsamp * sizeof( int ) );

			mi2hi( &dp, pktchan->data, e2opkt->th2->nsamp );

		} else if( STREQ( e2opkt->th2->datatype, "s2" ) ) {

			allot( int *, pktchan->data, 
			       e2opkt->th2->nsamp * sizeof( int ) );

			ms2hi( &dp, pktchan->data, e2opkt->th2->nsamp );

		} else if( STREQ( e2opkt->th2->datatype, "i4" ) ) {

			allot( int *, pktchan->data, 
			       e2opkt->th2->nsamp * sizeof( int ) );

			vi2hi( &dp, pktchan->data, e2opkt->th2->nsamp );

		} else if( STREQ( e2opkt->th2->datatype, "i2" ) ) {

			allot( int *, pktchan->data, 
			       e2opkt->th2->nsamp * sizeof( int ) );

			vs2hi( &dp, pktchan->data, e2opkt->th2->nsamp );
		}
		
	} else if( STREQ( e2opkt->typestr, Default_TYPE_TRACE_COMP_UA ) ) {

		pktchan->datasz *= sizeof(int);

		retcode = genuncompress( &pktchan->data, &pktchan->nsamp, &pktchan->datasz, 
			       (unsigned char *) dp, e2opkt->nbytes - sizeof( TRACE_HEADER ) );

		pktchan->datasz /= sizeof(int);
	}

	stuffPkt( e2opkt->pkt, 
		  e2opkt->srcname, 
		  &e2opkt->time,
		  &e2opkt->orbpkt, 
		  &e2opkt->orbpktnbytes, 
		  &e2opkt->orbpktsz );

	return retcode;
}

static void *
ew2orb_convert( void *arg )
{
	Ew2orbPacket *e2opkt;

	thr_setprio( thr_self(), THR_PRIORITY_CONVERT );

	while( pmtfifo_pop( E2oPackets_mtf, (void **) &e2opkt ) != 0 ) {

		if( crack_packet( e2opkt ) < 0 ) {
			
			free_Ew2orbPacket( e2opkt );

			continue;
		}

		if( e2opkt->loglevel == VERYVERBOSE ) {

			describe_packet( e2opkt );
		}

		if( e2opkt->orbpkt != NULL && e2opkt->orbpktnbytes != 0 ) {

			orbput( Orbfd, e2opkt->srcname, 
			       	       e2opkt->time,
				       e2opkt->orbpkt, 
				       e2opkt->orbpktnbytes );
		}

		e2opkt->it->npacketrefs--;

		if( e2opkt->it->stop == 1 &&
		    e2opkt->it->npacketrefs <= 0 ) {

			free_ImportThread( &e2opkt->it );
		}

		free_Ew2orbPacket( e2opkt );
	}

	return NULL;
}

static void *
ew2orb_pfwatch( void *arg )
{
	Pf	*pf = 0;
	int	rc;

	thr_setprio( thr_self(), THR_PRIORITY_PFWATCH );

	rwlock_init( &Import_Threads_rwlock, USYNC_THREAD, NULL );

	Import_Threads = newarr( 0 );

	memset( &Ewinfo, '\0', sizeof( Earthworm_Info ) );

	mutex_init( &Ewinfo.ew_mutex, USYNC_THREAD, NULL );

	strcpy( Ewinfo.pfname, DEFAULT_EARTHWORM_PFNAME );

	for( ;; ) {

		refresh_earthworm_info();

		rc = pfupdate( Pfname, &pf );

		if( rc < 0 ) {
			
			elog_complain( 1, "pfupdate pf parameter file '%s' failed\n",
				  Pfname );

		} else if( rc == 0 ) {
			
			; /* No reconfiguration necessary */

		} else if( rc == 1 ) {

			set_program_loglevel( pf );

			set_calibration_database( pf );

			if( Flags.verbose ) {
				
				elog_notify( 0, 
				"Reconfiguring ew2orb from parameter file\n" );
			}

			reconfigure_import_threads( pf );
		}

		sleep( PFWATCH_SLEEPTIME_SEC );
	}
}

static int
have_banner()
{
	struct stat buf;

	if( ! is_file( DEFAULT_BANNER_PATH ) ) {

		return 0;
	}

	if( stat( DEFAULT_BANNER_PATH, &buf ) == 0 && (S_IXOTH & buf.st_mode) ) {

		return 1;

	} else {

		return 0;
	}
}

int
main( int argc, char **argv )
{
	char	c;
	char	*orbname = 0;
	int	rc;
	thread_t pfwatch_tid;

	elog_init( argc, argv );

	while( ( c = getopt( argc, argv, "p:" ) ) != -1 ) {

		switch( c ) {
		case 'p':
			Pfname = optarg;
			break;

		case '?':
		default:
			usage();
			elog_die( 0, "option not understood\n" );
		}
	}

	if( argc - optind != 1 ) {

		usage();
		elog_die( 0, "Must specify an output orb name\n" );

	} else {
		
		orbname = argv[optind++];
	}

	if( ( Orbfd = orbopen( orbname, "w&" ) ) < 0 ) {
		
		elog_die( 0, "Failed to open orb '%s' for writing\n", orbname );
	}

	Flags.have_banner = have_banner();

	sigignore( SIGPIPE );

	E2oPackets_mtf = pmtfifo_create( PACKET_QUEUE_SIZE, 1, 0 );

	rc = thr_create( NULL, 0, ew2orb_pfwatch, 0, 0, &pfwatch_tid );

	if( rc != 0 ) {

		elog_die( 1, "Failed to create parameter-file watch thread, "
			"thr_create error %d\n", rc );
	}

	rc = thr_create( NULL, 0, ew2orb_convert, 0, 0, 0 );

	if( rc != 0 ) {

		elog_die( 1, "Failed to create packet-conversion thread, "
			"thr_create error %d\n", rc );
	}

	thr_join( pfwatch_tid, (thread_t *) NULL, (void **) NULL );

	if( Flags.verbose ) {

		elog_notify( 0, "Program terminated\n" );
	}

	return( 0 );
}
