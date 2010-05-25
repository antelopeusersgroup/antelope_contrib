#include <stdlib.h>
#include "db.h"
#include "stock.h"
#include "db2sql.h"

int
main( int argc, char **argv )
{
	char	*dbname;
	Dbptr	db;
	Tbl	*sqlcreate = (Tbl *) NULL;
	int	flags = 0;
	
	Program_Name = argv[0];

	if( argc != 2 ) {
		
		die( 0, "Usage: %s dbname\n", Program_Name );

	} else {

		dbname = argv[1];
	}

	dbopen( dbname, "r", &db );

	sqlcreate = dbschema2sqlcreate( db, flags );

	debugtbl( stdout, "Conversion results:\n", sqlcreate );

	freetbl( sqlcreate, free );

	return 0;
}

