#include <stdlib.h>
#include "stock.h"
#include "db.h"
 
usage() {
	elog_die( 0, "Usage: dbassoc_arrival_fold [-p pdelta] [-s sdelta] arrival_db origin_db\n" );
}

main( int argc, char **argv )
{
	int	c;
	char	cmd[STRSZ];
	char	expr[STRSZ];
	char	arow[STRSZ];
	char	*pdelta = 0;
	char	*sdelta = 0;
	char	*arrival_dbname = 0;
	char	*origin_dbname = 0;
	Dbptr	dbar;
	Dbptr	dbar_assoc;
	Dbptr	dbar_origin;
	Dbptr	dbar_arrival;
	Dbptr	dbor;
	Dbptr	dbor_origin;
	Dbptr	dbor_assoc;
	Dbptr	dbor_arrival;
	Dbptr	dbt;
	Dbptr	dbnewars;
	int	nrows;
	int	norigins;
	int	nass;
	int	is_present;
	double	lat;
	double	lon;
	double	depth;
	double	time;
	int	ardb_orid;
	int	ordb_orid;
	int	arid;
	Hook	*hook = 0;

	elog_init( argc, argv );

	while( ( c = getopt( argc, argv, "p:s:" ) ) != -1 ) {
		switch( c ) {
		case 'p':
			allot( char *, pdelta, STRSZ );
			sprintf( pdelta, " -p %s", optarg );
			break;
		case 's':
			allot( char *, sdelta, STRSZ );
			sprintf( sdelta, " -s %s", optarg );
			break;
		default:
			usage();
		}
	}

	if( argc - optind != 2 ) {
		usage();
	} else {
		arrival_dbname = strdup( argv[optind++] );
		origin_dbname = strdup( argv[optind++] );
	}

	dbopen( origin_dbname, "r+", &dbor );
	dbor_origin = dblookup( dbor, "", "origin", "", "" );
	dbor_assoc = dblookup( dbor, "", "assoc", "", "" );
	dbor_arrival = dblookup( dbor, "", "arrival", "", "" );

	dbopen( arrival_dbname, "r", &dbar );

	/* We'll use arrival table origin and assoc as scratch space. Therefore 
	make sure they're empty first: */

	dbar_assoc = dblookup( dbar, "", "assoc", "", "" );
	dbquery( dbar_assoc, dbTABLE_PRESENT, &is_present );
	if( is_present ) {
		elog_die( 0, "Cannot proceed: %s.assoc is already present\n", arrival_dbname );
	}

	dbar_origin = dblookup( dbar, "", "origin", "", "" );
	dbquery( dbar_origin, dbTABLE_PRESENT, &is_present );
	if( is_present ) {
		elog_die( 0, "Cannot proceed: %s.origin is already present\n", arrival_dbname );
	}

	sprintf( cmd, "dbassoc_arrival" );
	if( pdelta ) {
		strcat( cmd, pdelta );
	}
	if( sdelta ) {
		strcat( cmd, sdelta );
	}
	sprintf( cmd, "%s %s %s", cmd, arrival_dbname, origin_dbname );

	printf( "Executing: %s\n", cmd );
	system( cmd );

	dbquery( dbar_origin, dbRECORD_COUNT, &norigins );
	if( norigins <= 0 ) {
		elog_die( 0, "No origins in %s assoc'd with picks in %s\n", 
			origin_dbname, arrival_dbname );
	}

	for( dbar_origin.record = 0; dbar_origin.record < norigins; dbar_origin.record++ ) {

		dbgetv( dbar_origin, 0, "lat", &lat, 
				     "lon", &lon,
				     "depth", &depth,
				     "time", &time, 
				     "orid", &ardb_orid, 0 );
		dbt = dbor_origin;
		dbt.record = -1;
		sprintf( expr, 
			"lat == %f && lon == %f && depth == %f && time == %f", 
			 lat, lon, depth, time );
		dbt.record = dbfind( dbt, expr, 0, &hook );
		dbgetv( dbt, 0, "orid", &ordb_orid, 0 );
			 
		dbar_arrival = dblookup( dbar, "", "arrival", "", "" );
		dbnewars = dbjoin( dbar_arrival, dbar_assoc, 0, 0, 0, 0, 0 );
		sprintf( expr, "orid == %d", ardb_orid );
		dbnewars = dbsubset( dbnewars, expr, 0 );
		dbquery( dbnewars, dbRECORD_COUNT, &nrows );

		fprintf( stderr, "Adding %d origin/assoc rows for orid %d\n", nrows, ordb_orid );
		for( dbnewars.record = 0; dbnewars.record < nrows; dbnewars.record++ ) {

			dbgetv( dbnewars, 0, "arid", &arid, 0 );
			sprintf( expr, "%d", arid );

			dbt = dblookup( dbar_arrival, "", "arrival", "arid", expr );
			dbt.field = dbALL;
			dbget( dbt, arow );
			dbor_arrival.record = dbaddnull( dbor_arrival );
			dbput( dbor_arrival, arow );
			arid = dbnextid( dbor_arrival, "arid" );
			dbputv( dbor_arrival, 0, "arid", arid, 0 );

			dbt = dblookup( dbar_assoc, "", "assoc", "arid", expr );
			dbt.field = dbALL;
			dbget( dbt, arow );
			dbor_assoc.record = dbaddnull( dbor_assoc );
			dbput( dbor_assoc, arow );
			dbputv( dbor_assoc, 0, "arid", arid, "orid", ordb_orid, 0 );

			sprintf( expr, "%d", ordb_orid );
			dbt = dblookup( dbor_origin, "", "origin", "orid", expr );
			dbgetv( dbt, 0, "nass", &nass, 0 );
			dbputv( dbt, 0, "nass", nass + 1, 0 );
		}
	}

	sprintf( expr, "%s.assoc", arrival_dbname );
	unlink( expr );
	sprintf( expr, "%s.origin", arrival_dbname );
	unlink( expr );
}
