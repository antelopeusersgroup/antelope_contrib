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
	Tbl	*sqlinsert = (Tbl *) NULL;
	int	flags = 0;
	
	Program_Name = argv[0];

	if( argc != 2 ) {
		
		die( 0, "Usage: %s dbname\n", Program_Name );

	} else {

		dbname = argv[1];
	}

	dbopen( dbname, "r", &db );

	db2sqlinsert( db, &sqlinsert, private_compute_row_sync, flags );

	debugtbl( stdout, "Conversion results:\n", sqlinsert );

	freetbl( sqlinsert, free );

	return 0;
}

