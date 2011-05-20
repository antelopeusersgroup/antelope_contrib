#include <stdlib.h>
#include "db.h"
#include "stock.h"
#include "db2sql.h"

char *private_compute_row_sync( Dbptr db );

int
main( int argc, char **argv )
{
	char	*dbname;
	Dbptr	db;
	Tbl	*sqlcommands = (Tbl *) NULL;
	char	*sync;
	int	flags = DB2SQL_USE_DATASCOPE_NULLS;
	
	Program_Name = argv[0];

	if( argc != 2 ) {
		
		elog_die( 0, "Usage: %s dbname\n", Program_Name );

	} else {

		dbname = argv[1];
	}

	dbopen( dbname, "r", &db );

	db = dblookup( db, "", "origin", "", "" );

	db.record = 0;

	db2sqlinsert( db, &sqlcommands, private_compute_row_sync, flags );

	sync = private_compute_row_sync( db );

	db2sqldelete( db, sync, &sqlcommands, flags );		

	free( sync );

	debugtbl( stdout, "Conversion results:\n", sqlcommands );

	freetbl( sqlcommands, free );

	return 0;
}

