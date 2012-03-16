#include <stdlib.h>
#include "db.h"
#include "stock.h"
#include "dbmon.h"
#include "db2sql.h"

void newrow( Dbptr db, char *table, long irecord, char *sync, void *pvt );
void delrow( Dbptr db, char *table, char *sync, void *pvt );
Tbl *querysyncs( Dbptr db, char *table, void *pvt );

void
newrow( Dbptr db, char *table, long irecord, char *sync, void *pvt )
{ 
	char	row[10*STRSZ];
	FILE	*fp = (FILE *) pvt;

	dbget( db, row );
	
	fprintf( fp, "New Row %ld in '%s' [sync '%s']: %s\n", irecord, table, sync, row );

	return;
}

void
delrow( Dbptr db, char *table, char *sync, void *pvt )
{ 
	FILE	*fp = (FILE *) pvt;

	fprintf( fp, "Deleted row from '%s' with sync '%s'\n", table, sync );

	return;
}

Tbl *
querysyncs( Dbptr db, char *table, void *pvt )
{
	Tbl	*syncs = NULL;
	char	*sync = NULL;
	long	nrecs = 0L;

	syncs = newtbl( 0 );

	/* Pretend to already know about first record in each table, 
	   if the table exists */

	if( dbquery( db, dbRECORD_COUNT, &nrecs ) > 0 ) {

		db.record = 0;
	
		sync = dbmon_compute_row_sync( db );

		pushtbl( syncs, (void *) sync );
	} 

	return syncs;
}

int
main(int argc, char **argv )
{
	Dbptr	db;
	char	dbname[FILENAME_MAX];
	char	command[STRSZ];
	Hook	*dbmon_hook = NULL;
	Tbl	*tables = (Tbl *) NULL; 

	if( argc != 2 ) {

		elog_die( 0, "Usage: %s dbname\n", argv[0] );

	} else {
		
		strcpy( dbname, argv[1] );
	}

	dbopen_database( dbname, "r", &db );

	dbmon_hook = dbmon_init( db, tables, newrow, delrow, querysyncs, 0 );

	fprintf( stdout, "Resyncronizing with alleged previous results:\n" );

	dbmon_resync( dbmon_hook, (void *) stdout );

	dbmon_update( dbmon_hook, (void *) stdout );

	fprintf( stdout, "Adding new arrival table:\n" );

	sprintf( command, "cp data/mod.new.demo.arrival results/dbmon/demo.arrival" );
	system( command );

	sleep( 1 );

	dbmon_update( dbmon_hook, (void *) stdout );

	fprintf( stdout, "Shortening arrival table:\n" );

	sprintf( command, "cp data/mod.shorter.demo.arrival results/dbmon/demo.arrival" );
	system( command );

	sleep( 1 );

	dbmon_update( dbmon_hook, (void *) stdout );

	fprintf( stdout, "Lengthening arrival table:\n" );

	sprintf( command, "cp data/mod.longer.demo.arrival results/dbmon/demo.arrival" );
	system( command );

	sleep( 1 );

	dbmon_update( dbmon_hook, (void *) stdout );

	fprintf( stdout, "Chimney-collapsing arrival table:\n" );

	sprintf( command, "cp data/mod.chimney.demo.arrival results/dbmon/demo.arrival" );
	system( command );

	sleep( 1 );

	dbmon_update( dbmon_hook, (void *) stdout );

	dbmon_status( stdout, dbmon_hook );

	elog_clear_register( 1 );

	free_hook( &dbmon_hook );

	dbclose( db );

	exit( 0 );
}
