#include <stdlib.h>
#include "db.h"
#include "stock.h"
#include "db2sql.h"

int
main( int argc, char **argv )
{
	char	*dbname = "/opt/antelope/data/db/demo/demo";
	Dbptr	db;
	Tbl	*sqlcreate = (Tbl *) NULL;
	int	flags = 0;
	
	Program_Name = argv[0];

	dbopen( dbname, "r", &db );

	sqlcreate = dbschema2sqlcreate( db, flags );

	debugtbl( stdout, "Conversion results:\n", sqlcreate );

	freetbl( sqlcreate, free );

	return 0;
}

