
#ifndef __dbmon__
#define __dbmon__

#ifdef	__cplusplus
extern "C" {
#endif

Hook *dbmon_init( Dbptr db, Tbl *table_subset, 
		  void (*newrow)(Dbptr db, char *table, long irecord, char *sync, void *pvt), 
		  void (*delrow)(Dbptr db, char *table, char *sync, void *pvt), 
		  Tbl *(*querysyncs)(Dbptr db, char *table, void *pvt),
		  int flags );
int dbmon_resync( Hook *dbmon_hook, void *pvt );
int dbmon_update( Hook *dbmon_hook, void *pvt );
void dbmon_status( FILE *fp, Hook *dbmon_hook );
void dbmon_close( Hook **dbmon_hook );
char *dbmon_compute_row_sync( Dbptr db );

#ifdef	__cplusplus
}
#endif

#endif

