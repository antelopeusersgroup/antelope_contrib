#include <stdlib.h>
#include "db.h"
#include "stock.h"
#include "db2sql.h"

int
main( int argc, char **argv )
{
	char	*dbname = "/opt/antelope/data/db/demo/demo";
	Dbptr	db;
	Tbl	*sqlinsert = (Tbl *) NULL;
	int	flags = 0;
	
	Program_Name = argv[0];

	dbopen( dbname, "r", &db );

	db2sqlinsert( db, &sqlinsert, db2sql_compute_row_sync, flags );

	debugtbl( stdout, "Conversion results:\n", sqlinsert );

	freetbl( sqlinsert, free );

	return 0;
}

