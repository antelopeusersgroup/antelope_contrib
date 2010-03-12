#include <stdlib.h>
#include "db.h"
#include "stock.h"
#include "dbmon.h"
#include "db2sql.h"

void newrow( Dbptr db, char *table, char *sync, void *private );
void changerow( char *oldsync, Dbptr db, char *table, char *sync, void *private );
void delrow( Dbptr db, char *table, char *sync, void *private );

void
newrow( Dbptr db, char *table, char *sync, void *private )
{ 
	char	row[10*STRSZ];
	FILE	*fp = (FILE *) private;

	dbget( db, row );
	
	fprintf( fp, "New Row %ld in '%s' [sync '%s']: %s\n", db.record, table, sync, row );

	return;
}

void
changerow( char *oldsync, Dbptr db, char *table, char *sync, void *private )
{ 
	char	row[10*STRSZ];
	FILE	*fp = (FILE *) private;

	dbget( db, row );
	
	fprintf( fp, "Changed Row %ld in '%s' [Old sync '%s', New sync '%s']: %s\n", 
		 db.record, table, oldsync, sync, row );

	return;
}

void
delrow( Dbptr db, char *table, char *sync, void *private )
{ 
	FILE	*fp = (FILE *) private;

	fprintf( fp, "Deleted row from '%s' with sync '%s'\n", table, sync );

	return;
}

int
main(int argc, char **argv )
{
	Dbptr	db;
	char	*dbname = "/opt/antelope/data/db/demo/demo";
	Hook	*dbmon_hook = NULL;
	Tbl	*tables; 

	tables = (Tbl *) NULL;

	dbopen_database( dbname, "r", &db );

	dbmon_hook = dbmon_init( db, tables, newrow, changerow, delrow, 0 );

	dbmon_update( dbmon_hook, (void *) stdout );

	dbmon_status( stdout, dbmon_hook );
}
