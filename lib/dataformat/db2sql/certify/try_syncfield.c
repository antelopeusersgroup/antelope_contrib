#include <stdlib.h>
#include "db.h"
#include "stock.h"
#include "db2sql.h"

char *private_compute_row_sync( Dbptr db );

int
main( int argc, char **argv )
{
	char	*dbname = NULL;
	Dbptr	db;
	Tbl	*sqlcommands = (Tbl *) NULL;
	char	*sync = NULL;
	char	*checkname = NULL;
	int	flags = DB2SQL_USE_DATASCOPE_NULLS;
	
	Program_Name = argv[0];

	if( argc != 2 ) {
		
		elog_die( 0, "Usage: %s dbname\n", Program_Name );

	} else {

		dbname = argv[1];
	}

	checkname = db2sql_get_syncfield_name();

	fprintf( stdout, "Check of sync-field name before modification: %s\n", checkname );

	free( checkname );

	db2sql_set_syncfield_name( "mytestsyncfield" );

	checkname = db2sql_get_syncfield_name();

	fprintf( stdout, "Check of sync-field name after modification: %s\n", checkname );

	free( checkname );

	dbopen( dbname, "r", &db );

	db = dblookup( db, "", "lastid", "", "" );

	sqlcommands = dbschema2sqlcreate( db, flags );

	db.record = 0;

	db2sqlinsert( db, &sqlcommands, private_compute_row_sync, flags );

	sync = private_compute_row_sync( db );

	db2sqldelete( db, sync, &sqlcommands, flags );		

	free( sync );

	debugtbl( stdout, "Conversion results:\n", sqlcommands );

	freetbl( sqlcommands, free );

	return 0;
}

