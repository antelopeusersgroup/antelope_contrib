#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include <thread.h>
#include <synch.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <netdb.h>
#include "db.h"
#include "tr.h"
#include "coords.h"
#include "stock.h"
#include "bns.h"

#define BNS_BUFFER_SIZE 8192
#define RETURN_DATATYPE "s4"
#define STREQ( a, b ) ( ! strcmp( (a), (b) ) )

static int Verbose = 0;
static int timeout_msec = 60000; /* Default one minute */
static Hook *Dbhandle;
static double Fill_value = 0;

typedef struct hook_cluster {
	Dbptr	db;
	mutex_t	db_mutex;
	int	wfdisc_nrecs;
	Expression *min_time;
	Expression *max_endtime;
	Tbl	*groupfields;
	Tbl	*transient_tables;
} Hook_cluster;

typedef struct server_args {
	int	fd;
} Server_args;

static void
usage ()
{
    fprintf (stderr, "Usage: %s [-vV] [-p pfname] dbname \n", Program_Name);
    banner (Program_Name, "$Revision$ $Date$\n");
    exit (1);
}

static void
free_hook_cluster( void **hook_clusterp )
{
	Hook_cluster *hook_cluster = (Hook_cluster *) *hook_clusterp;

	dbex_free( hook_cluster->min_time );
	dbex_free( hook_cluster->max_endtime );

	dbclose( hook_cluster->db );
	mutex_destroy( &hook_cluster->db_mutex );

	freetbl( hook_cluster->groupfields, 0 );

	free( hook_cluster );
}

static int 
init_database( Hook **dbhandle, char *dbname  )
{
	Hook_cluster *hook_cluster;
	Dbptr	db;
	int	nrecs;

	*dbhandle = new_hook( free_hook_cluster );

	allot( Hook_cluster *, hook_cluster, 1 );

	(*dbhandle)->p = hook_cluster;

	if( dbopen( dbname, "r", &(hook_cluster->db) ) < 0 ) {
		die( 1, "Failed to open %s\n", dbname );
	} 
	db = hook_cluster->db;

	db = dblookup( db, 0, "wfdisc", 0, 0 );
	if( db.database < 0 || db.table < 0 ) {
		die( 1, "Failed to open %s.wfdisc\n", dbname );
	}
	dbquery( db, dbRECORD_COUNT, &nrecs );
	if( nrecs <= 0 ) {
		die( 1, "No records in %s.wfdisc\n", dbname );
	}
	db = dblookup( db, 0, "pins", 0, 0 );
	dbquery( db, dbRECORD_COUNT, &nrecs );
	if( nrecs <= 0 ) {
		die( 1, "No records in %s.pins\n", dbname );
	}

	mutex_init( &(hook_cluster->db_mutex), USYNC_THREAD, NULL );

	putenv( "DBLOCKS=1" );

	/* Finish initializing hook_cluster: */

	hook_cluster->wfdisc_nrecs = 0; 

	hook_cluster->min_time = NULL;
	hook_cluster->max_endtime = NULL;

	/* Though pinno and sta:chan are redundant this sets up the 
	data extraction: */

	hook_cluster->groupfields =
		strtbl( "pinno", "sta", "chan", "net", 0 );

	hook_cluster->transient_tables = newtbl( 0 );

	return 1;
}

static void
lock_database( Hook *dbhandle )
{
	Hook_cluster *hook_cluster = (Hook_cluster *) dbhandle->p;

	mutex_lock( &hook_cluster->db_mutex );
}

static void
unlock_database( Hook *dbhandle )
{
	Hook_cluster *hook_cluster = (Hook_cluster *) dbhandle->p;

	mutex_unlock( &hook_cluster->db_mutex );
}

static void
free_transients( Dbptr db, Tbl *transients )
{
	db.record = db.field = dbALL;
	while( db.table = (int) poptbl( transients ) ) {
		dbfree( db );
	}
}
static void
get_timerange( Hook *dbhandle,  Dbptr db, double *time, double *endtime ) 
{
	Hook_cluster *hook_cluster = (Hook_cluster *) dbhandle->p;

	dbex_eval( db, hook_cluster->min_time, 0, time );
	dbex_eval( db, hook_cluster->max_endtime, 0, endtime );
}

static Dbptr
refresh_dbview( Hook *dbhandle )
{
	Hook_cluster *hook_cluster = (Hook_cluster *) dbhandle->p;
	Dbptr	db;
	Dbptr	dbt;

	db = hook_cluster->db;

	free_transients( db, hook_cluster->transient_tables );

	db = dblookup( db, 0, "wfdisc", 0, 0 );
	dbt = dblookup( db, 0, "pins", 0, 0 );
	db = dbjoin( db, dbt, 0, 0, 0, 0, 0 );

	pushtbl( hook_cluster->transient_tables, (char *) db.table );

	db = dbsort( db, hook_cluster->groupfields, 0, 0 );
	pushtbl( hook_cluster->transient_tables, (char *) db.table );

	db = dbgroup( db, hook_cluster->groupfields, 0, 0 );
	pushtbl( hook_cluster->transient_tables, (char *) db.table );

	if( ! hook_cluster->min_time ) {
		dbex_compile( db, "min(time)",
			      &hook_cluster->min_time, dbTIME );
	}
	if( ! hook_cluster->max_endtime ) {
		dbex_compile( db, "max(endtime)",
			      &hook_cluster->max_endtime, dbTIME );
	}

	hook_cluster->db = db;

	return db;
}

static Dbptr 
get_dbview( Hook *dbhandle )
{
	Hook_cluster *hook_cluster = (Hook_cluster *) dbhandle->p;
	Dbptr	db;
	int	nrecs;

	db = hook_cluster->db;

	db = dblookup( db, 0, "wfdisc", 0, 0 );
	dbquery( db, dbRECORD_COUNT, &nrecs );

	if( hook_cluster->wfdisc_nrecs != nrecs )  {

		hook_cluster->wfdisc_nrecs = nrecs;

		return refresh_dbview( Dbhandle );

	} else {

		return hook_cluster->db;
	}
}

static void
dbgrouprow2menu_entry( Hook *dbhandle, Dbptr db, char *entry )
{
	Hook_cluster *hook_cluster = (Hook_cluster *) dbhandle->p;
	int	pinno;
	char	sta[10],chan[10],net[10];
	double	time, endtime;

	dbgetv( db, 0, "pinno", &pinno,
		       "sta", sta, 
		       "chan", chan,
		       "net", net, 
		       0 );
	get_timerange( Dbhandle, db, &time, &endtime );

	sprintf( entry, "%d %s %s %s %f %f %s\n",
			pinno, sta, chan, net, 
			time, endtime, RETURN_DATATYPE );
}

static void
get_my_IP_address( char *address )
{
	struct in_addr in;
	struct hostent host;
	char buffer[1024];
	int buflen = 1024;
	int h_errnop;

	gethostbyname_r( "loghost", &host, buffer, buflen, &h_errnop );

	(void) memcpy(&in.s_addr, *host.h_addr_list, sizeof(in.s_addr));
	(void) strcpy( address, inet_ntoa( in ) );

	return;
}

static int
SocketInit( int portnumber, char *hostip )
{
	int 	on = 1;
	struct hostent *hp;
	struct sockaddr_in name;
	u_long 	addr;
	char	buffer[1024];
	int	buflen = 1024;
	int	h_errnop;
	int	skt;

	/* Open a socket
	   *************/
	skt = socket( AF_INET, SOCK_STREAM, 0 );
	if( skt < 0 )
	{
		die( 1, "Can't open the socket.\n" );
	}

	/* Allows the server to be stopped and restarted
	   *********************************************/
	setsockopt( skt, SOL_SOCKET, SO_REUSEADDR, 
			(char *) &on, sizeof( char * ) );

	/* Set up address for binding
	   **************************/
	if( (int) ( addr = inet_addr( hostip ) ) == -1 )
	{
		die( 1, 
		   "db2ew_wave_server: IP-address must be of the form a.b.c.d\n" );
	}

	hp = (struct hostent *) malloc( sizeof( struct hostent ) );

	gethostbyaddr_r( (char *) &addr, sizeof( addr ), AF_INET,
			hp, buffer, buflen, &h_errnop );

	if( hp == (struct hostent *) NULL )
	{
		die( 1,
		   "db2ew_wave_server: host info for %s not found\n", hostip );
	}

	(void) memset( (char *) &name, '\0', sizeof(name) );
	name.sin_family      = AF_INET;
	name.sin_port        = htons( (unsigned short) portnumber );
	(void) memcpy( (char *) &name.sin_addr, hp->h_addr_list[0], 
					sizeof( name.sin_addr ) );

	/* Bind socket to a name
	   *********************/
	if ( bind( skt, (struct sockaddr *) &name, sizeof(name) ) )
	{
		close( skt );
		die( 1,
		"db2ew_wave_server/SocketInit: Can't bind socket to the port\n" );
	}

	/* Prepare to queue up to five connect requests
	   ********************************************/
	if ( listen( skt, 5 ) )
	{
		close( skt );
		die( 1, 
		"db2ew_wave_server/SocketInit: socket listen error.\n" );
	}

	free( hp );

	return skt;
}

int
get_request( Bns *bns, char **request, int *requestlen )
{
	char	*c;
	int	rc;
	int	offset;

	if( ! *request ) {
		*requestlen = STRSZ;
		allot( char *, *request, *requestlen );
	}

	c = *request;

	while( ! ( rc = bnsget( bns, c++, BYTES, 1 ) ) ) {

		if( *(c-1) == '\n' ) {

			*(c-1) = '\0';
			return 1;

		} else if( c - *request + 10 > *requestlen ) {

			offset = c - *request; 

			*requestlen += STRSZ;
			reallot( char *, *request, *requestlen );

			c = *request + offset; 
		}
	}

	if( bnserr( bns ) ) {

		perror( "" );
		complain( 1, "Thread %d: bns socket error %d\n",
			thr_self(), bnserrno( bns ) );

	} else if( bnseof( bns ) ) {

		complain( 1, "Thread %d: socket closed by client\n", 
				thr_self() );

	} else {

		complain( 1, "Thread %d: connection timed out\n",
				thr_self() );
	}

	return 0;
}

int	
find_scn( char *sta, char *chan, char *net, Dbptr *db )
{
	Dbptr	dbk;
	Tbl	*pattern;
	Tbl	*matches = 0;
	Hook	*hook = 0;
	int	retcode;

	dbk = dblookup( *db, 0, "pins", 0, "dbSCRATCH" );
	dbputv( dbk, 0, "sta", sta,
			"chan", chan,
			"net", net, 
			0 );
	pattern = strtbl( "sta", "chan", "net", 0 );

	retcode = dbmatches( dbk, *db, &pattern, &pattern, &hook, &matches );

	free_hook( &hook );
	freetbl( matches, 0 );
	freetbl( pattern, 0 );

	if( retcode > 0 ) {
		(*db).record = (int) poptbl( matches );
		return 1;
	} else {
		return 0;
	}
}
int	
find_pin( int request_pin, Dbptr *db )
{
	Dbptr	dbk;
	Tbl	*pattern;
	Tbl	*matches = 0;
	Hook	*hook = 0;
	int	retcode;

	dbk = dblookup( *db, 0, "pins", 0, "dbSCRATCH" );
	dbputv( dbk, 0, "pinno", request_pin, 0 );
	pattern = strtbl( "pinno", 0 );

	retcode = dbmatches( dbk, *db, &pattern, &pattern, &hook, &matches );

	free_hook( &hook );
	freetbl( matches, 0 );
	freetbl( pattern, 0 );

	if( retcode > 0 ) {
		(*db).record = (int) poptbl( matches );
		return 1;
	} else {
		return 0;
	}
}

static int	
fill( Dbptr *dbp, Trsample *data, int *i0p, int *i1p, int *imaxp )
{
	int	i;
	for( i = *i0p; i < *i1p; i++ ) {
		data[i] = (Trsample) Fill_value;
	}

	return 0;
}

static int
send_data( Hook *dbhandle, Dbptr db, Bns *bns, 
	   double reqs, double reqe, double fillval )
{
	Hook_cluster *hook_cluster = (Hook_cluster *) dbhandle->p;
	int	pinno;
	char	sta[10], chan[10], net[10];
	char	st[STRSZ], et[STRSZ];
	double	min_time,max_endtime;
	double	samprate = 0;
	double	starttime;
	char	message[STRSZ];
	Dbptr	dbbundle;
	Dbptr	tr;
	int	nrecs;
	Trsample *data;
	int 	nsamp;
	int	i;

	dbgetv( db, 0, "pinno", &pinno,
			"sta", sta,
			"chan", chan,
			"net", net,
			"bundle", &dbbundle,
			0 );

	get_timerange( Dbhandle, db, &min_time, &max_endtime );

	if( reqe < min_time ) {

		sprintf( message, "%d %s %s %s FL %s %f %f\n", 
				pinno, sta, chan, net,
				RETURN_DATATYPE, min_time, samprate );
		bnsput( bns, message, BYTES, strlen( message ) );
		return 1;

	} else if( reqs > max_endtime ) {

		sprintf( message, "%d %s %s %s FR %s %f %f\n", 
				pinno, sta, chan, net,
				RETURN_DATATYPE, max_endtime, samprate );
		bnsput( bns, message, BYTES, strlen( message ) );
		return 1;

	} else {
		tr = dbinvalid();
		sprintf( st, "%f", reqs );
		sprintf( et, "%f", reqe );
		trload_cssgrp( dbbundle, st, et, &tr, 0, 0 );
		clear_register( 1 );
		tr = dblookup( tr, 0, "trace", 0, 0 );

		Fill_value = fillval; /* Rely on db_mutex lock */
		/*trsplice( tr, trTOLERANCE, fill, 0  );*/

		dbquery( tr, dbRECORD_COUNT, &nrecs );
		if( nrecs <= 0 ) {
			complain( 1, "Data access failure on thread %d\n", 
				thr_self() );
			return 1;
		} else if( nrecs > 1 ) {
			complain( 1, "Splice failed on thread %d\n",
				thr_self() );
		}

		tr.record = 0;
		dbgetv( tr, 0, "samprate", &samprate,
			       "time", &starttime,
			       "data", &data,
			       "nsamp", &nsamp,
			       0 );
		clear_register( 1 );

		sprintf( message, "%d %s %s %s F %s %f %f ", 
				pinno, sta, chan, net,
				RETURN_DATATYPE, starttime, samprate );

		bnsput( bns, message, BYTES, strlen( message ) );
		
		for( i = 0; i < nsamp; i++ ) {
			sprintf( message, "%d ", (int) data[i] );
			bnsput( bns, message, BYTES, strlen( message ) );
		}

		bnsput( bns, "\n", BYTES, 1 );

		tr.table = tr.field = tr.record = dbALL;
		trdestroy( &tr );
	}
}

void
handle_inq( Bns *bns, char *command, char *params )
{
	fprintf( stderr, "SCAFFOLD: %s %s\n", command, params );
}

void
handle_pinreq( Bns *bns, char *command, char *params )
{
	fprintf( stderr, "SCAFFOLD: %s %s\n", command, params );
}

void
handle_scnreq( Bns *bns, char *command, char *params )
{
	fprintf( stderr, "SCAFFOLD: %s %s\n", command, params );
}

void
handle_menu( Bns *bns, char *command, char *params )
{
	Dbptr	db;
	char	request_id[STRSZ];
	char	menu_entry[STRSZ];
	int	nrecs;
	int	i;

	if( strlen( params ) >= STRSZ ) {
		complain( 1, "MENU: command too long\n" );
		return;
	} else {
		copystrip( request_id, params, strlen( params ) );
	}
	sprintf( request_id, "%-12s", request_id );
	bnsput( bns, request_id, BYTES, strlen( request_id ) );
	bnsput( bns, "\n", BYTES, 1 );

	lock_database( Dbhandle );

	db = get_dbview( Dbhandle );
	dbquery( db, dbRECORD_COUNT, &nrecs );

	for( db.record = 0; db.record < nrecs; db.record++ ) {

		dbgrouprow2menu_entry( Dbhandle, db, menu_entry );
		bnsput( bns, menu_entry, BYTES, strlen(menu_entry) );
	}

	unlock_database( Dbhandle );

	bnsput( bns, "\n", BYTES, 1 );
	bnsflush( bns );

	return;
}

void
handle_menuscn( Bns *bns, char *command, char *params )
{
	Dbptr	db;
	char	request_id[STRSZ];
	char	menu_entry[STRSZ];
	char	request_sta[10], request_chan[10], request_net[10];

	if( strlen( params ) >= STRSZ ) {
		complain( 1, "MENUSCN: command too long\n" );
		return;
	} else {
		if( sscanf( params, "%s %s %s %s",
		    request_id, request_sta,
		    request_chan, request_net ) != 4 ) {
			complain( 1, "MENUSCN: incorrect format: %s\n",
				params );
		}
	}

	sprintf( request_id, "%-12s", request_id );
	bnsput( bns, request_id, BYTES, strlen( request_id ) );
	bnsput( bns, "\n", BYTES, 1 );

	lock_database( Dbhandle );

	db = get_dbview( Dbhandle );

	if( find_scn( request_sta, request_chan, request_net, &db ) ) {
		dbgrouprow2menu_entry( Dbhandle, db, menu_entry );
		bnsput( bns, menu_entry, BYTES, strlen(menu_entry) );
	}

	unlock_database( Dbhandle );

	bnsput( bns, "\n", BYTES, 1 );
	bnsflush( bns );

	return;
}

void
handle_menupin( Bns *bns, char *command, char *params )
{
	Dbptr	db;
	char	request_id[STRSZ];
	char	menu_entry[STRSZ];
	int	request_pin;

	if( strlen( params ) >= STRSZ ) {
		complain( 1, "MENUPIN: command too long\n" );
		return;
	} else {
		if( sscanf( params, "%s %d", request_id, &request_pin ) != 2 ) {
			complain( 1, "MENUPIN: incorrect format: %s\n",
				params );
		}
	}

	sprintf( request_id, "%-12s", request_id );
	bnsput( bns, request_id, BYTES, strlen( request_id ) );
	bnsput( bns, "\n", BYTES, 1 );

	lock_database( Dbhandle );

	db = get_dbview( Dbhandle );

	if( find_pin( request_pin, &db ) ) {
		dbgrouprow2menu_entry( Dbhandle, db, menu_entry );
		bnsput( bns, menu_entry, BYTES, strlen(menu_entry) );
	}

	unlock_database( Dbhandle );

	bnsput( bns, "\n", BYTES, 1 );
	bnsflush( bns );

	return;
}

void
handle_getpin( Bns *bns, char *command, char *params )
{
	Dbptr	db;
	char	request_id[STRSZ];
	int	request_pin;
	double	reqtime, reqendtime;
	double	fill_value;

	if( strlen( params ) >= STRSZ ) {
		complain( 1, "GETPIN: command too long\n" );
		return;
	} else {
		if( sscanf( params, "%s %d %lf %lf %lf",
		    request_id, &request_pin,
		    &reqtime, &reqendtime, &fill_value ) != 5 ) {
			complain( 1, "GETPIN: incorrect format: %s\n",
				params );
		}
	}

	sprintf( request_id, "%-12s", request_id );
	bnsput( bns, request_id, BYTES, strlen( request_id ) );
	bnsput( bns, " ", BYTES, 1 );

	lock_database( Dbhandle );

	db = get_dbview( Dbhandle );

	if( find_pin( request_pin, &db ) ) {
		send_data( Dbhandle, db, bns, reqtime, reqendtime, fill_value );
	}

	unlock_database( Dbhandle );

	bnsput( bns, "\n", BYTES, 1 );
	bnsflush( bns );

	return;
}

void
handle_getscn( Bns *bns, char *command, char *params )
{
	Dbptr	db;
	char	request_id[STRSZ];
	char	request_sta[10], request_chan[10], request_net[10];
	double	reqtime, reqendtime;
	double	fill_value;

	if( strlen( params ) >= STRSZ ) {
		complain( 1, "GETSCN: command too long\n" );
		return;
	} else {
		if( sscanf( params, "%s %s %s %s %lf %lf %lf",
		    request_id, request_sta,
		    request_chan, request_net, 
		    &reqtime, &reqendtime, &fill_value ) != 7 ) {
			complain( 1, "GETSCN: incorrect format: %s\n",
				params );
		}
	}

	sprintf( request_id, "%-12s", request_id );
	bnsput( bns, request_id, BYTES, strlen( request_id ) );
	bnsput( bns, " ", BYTES, 1 );

	lock_database( Dbhandle );

	db = get_dbview( Dbhandle );

	if( find_scn( request_sta, request_chan, request_net, &db ) ) {
		send_data( Dbhandle, db, bns, reqtime, reqendtime, fill_value );
	}

	unlock_database( Dbhandle );

	bnsput( bns, "\n", BYTES, 1 );
	bnsflush( bns );

	return;
}

void *
server_thread( void *arg )
{
	Server_args *args = (Server_args *) arg;
	Bns	*in;
	Bns	*out;
	char	*request = 0;
	char	*s;
	char	*cmd, *params;
	int	reqlen;

	if( Verbose ) {
		complain( 1,
			"Starting server thread %d for socket %d\n",
			thr_self(), args->fd );
	}

	in = bnsnew( args->fd, BNS_BUFFER_SIZE );
	out = bnsnew( args->fd, BNS_BUFFER_SIZE );

	bnsuse_sockio( in );
	bnsuse_sockio( out );

	while( get_request( in, &request, &reqlen ) ) {
		if( Verbose ) {
			fprintf( stderr, 
				"Received request %s on thread %d\n",
				request, thr_self() );
		}

		s = strdup( request );
		strtok_r( s, ":", &params );
		cmd = s;

		if( STREQ( cmd, "INQ" ) ) {	       /* Iceworm wave_server */
			
			handle_inq( out, cmd, params );

		} else if( STREQ( cmd, "PINREQ" ) ) {  /* Iceworm wave_server */
			
			handle_pinreq( out, cmd, params );

		} else if( STREQ( cmd, "SCNREQ" ) ) {  /* Iceworm wave_server */
			
			handle_scnreq( out, cmd, params );

		} else if( STREQ( cmd, "MENU" ) ) {	/* wave_serverV */
			
			handle_menu( out, cmd, params );

		} else if( STREQ( cmd, "MENUPIN" ) ) {	/* wave_serverV */
			
			handle_menupin( out, cmd, params );

		} else if( STREQ( cmd, "MENUSCN" ) ) {	/* wave_serverV */
			
			handle_menuscn( out, cmd, params );

		} else if( STREQ( cmd, "GETSCN" ) ) {	/* wave_serverV */
			
			handle_getscn( out, cmd, params );

		} else if( STREQ( cmd, "GETPIN" ) ) {	/* wave_serverV */
			
			handle_getpin( out, cmd, params );

		} else {
			complain( 1, 
				"Command on thread %d not understood: %s\n",
				thr_self(), s );
		}
		free( s );
	}

	bnsclose( in );

	if( request ) free( request );

	if( Verbose ) {
		complain( 1, "Thread %d terminating\n", thr_self() );
	}

	thr_exit( (void *) 0 );
}

int
main (int argc, char **argv)
{
	int	c;
	int	errflg = 0;
	int	result;
	int	server_port;
	int	skt;
	int	newsocket;
	struct sockaddr_in client_addr;
	int	client_addrlen = sizeof( client_addr );
	char	myIPaddress[16];
	char	*dbname;
	char	*pfname; 
	Pf	*pf;
	Server_args *args;
	thread_t tid;

	elog_init ( argc, argv ) ; 

	pfname = Program_Name;

	while ((c = getopt (argc, argv, "vVp:")) != -1) {
		switch (c) {
		case 'v':
	    		Verbose++ ;
	    		break;
		case 'V':
	    		banner (Program_Name,
			   "$Revision$ $Date$\n");
	    		exit (0);
		case 'p':
			pfname = optarg;
			break;
		default:
	    		errflg++;
	    		break ;
		}
	}

	if ( errflg || argc - optind != 1 ) usage ();

	if( pfread( pfname, &pf ) != 0 ) {
		die( 1, "Can't read parameter file %s\n", pfname );
	} else {
		server_port = pfget_int( pf, "port_number" );
		timeout_msec = 1000 * pfget_int( pf, "timeout_sec" );
	}

	dbname = argv[optind++];

	init_database( &Dbhandle, dbname );

	get_my_IP_address( myIPaddress );

	if( Verbose ) {
		fprintf( stderr,
			"Expecting connections to %s:%d\n",
			myIPaddress, server_port );
	}

	thr_setconcurrency( 5 );

	skt = SocketInit( server_port, myIPaddress );

	sigignore( SIGPIPE );

	for( ;; ) {
		newsocket = accept( skt, 
				(struct sockaddr *) &client_addr,
				&client_addrlen );
		if( newsocket < 0 ) {
			complain( 1, "Failed to connect: accept error\n" );
		} else { 
			if( Verbose ) {
				fprintf( stderr, "\n" );
				complain( 1, "Accepted new socket #%d\n", 
					newsocket );
			}

			allot( Server_args *, args, 1 );
			args->fd = newsocket;

			if( result = thr_create( NULL, 0, server_thread,
					(void *) args, THR_DETACHED, &tid ) ) {
				complain( 1, 
					"Failed to create server thread\n" );
			}
		}
	}

	return 0;
}

