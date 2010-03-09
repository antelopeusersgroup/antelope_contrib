#include <stdlib.h>
#include "db.h"
#include "stock.h"
#include "db2sql.h"

int
main( int argc, char **argv )
{
	char	*dbname = "/opt/antelope/data/db/demo/demo";
	Dbptr	db;
	Tbl	*sql = (Tbl *) NULL;
	int	flags = 0;
	
	Program_Name = argv[0];

	dbopen( dbname, "r", &db );

	sql = dbschema2sqlcreate( db, flags );

	db2sqlinsert( db, &sql, flags );

	debugtbl( stdout, "Conversion results:\n", sql );

	freetbl( sql, free );

	return 0;
}

