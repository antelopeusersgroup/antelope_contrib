
#ifndef __db2sql__
#define __db2sql__

#define DB2SQL_SQL_MYSQL 1		# POSTGRES, ORACLE, ANSI, SQL92
#define DB2SQL_SQL_POSTGRES 2
#define DB2SQL_SQL_ORACLE 4
#define DB2SQL_SQL_ANSI 8
#define DB2SQL_SQL_SQL92 16

#define DB2SQL_USE_DATASCOPE_NULLS 32768
#define DB2SQL_OMIT_SYNC 65536

#define DB2SQL_SQL_DEFAULT DB2SQL_SQL_MYSQL
#define DB2SQL_SYNCFIELD_NAME_DEFAULT "syncstring"
#define DB2SQL_SYNCFIELD_SPEC "CHAR(40) DEFAULT '-'"
#define DB2SQL_SYNCFIELD_NULL "-"

#ifdef	__cplusplus
extern "C" {
#endif

Tbl *dbschema2sqlcreate( Dbptr db, long flags );
long db2sqlinsert( Dbptr db, Tbl **tbl, char *(*createsync)(Dbptr db), long flags );
int db2sqldelete( Dbptr db, char *sync, Tbl **tbl, long flags );
char *db2sql_compute_row_sync( Dbptr db );
char *db2sql_get_syncfield_name( void );
void db2sql_set_syncfield_name( char *name );

char *Db2sql_syncfield_name = DB2SQL_SYNCFIELD_NAME_DEFAULT;

#ifdef	__cplusplus
}
#endif

#endif

