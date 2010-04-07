
#ifndef __dbmon__
#define __dbmon__

#ifdef	__cplusplus
extern "C" {
#endif

Hook *dbmon_init( Dbptr db, Tbl *table_subset, void (*newrow)(Dbptr db, char *table, char *sync, void *private), void (*changerow)(char *oldsync, Dbptr db, char *table, char *sync, void *private), void (*delrow)(Dbptr db, char *table, char *sync, void *private), int flags );
int dbmon_update( Hook *dbmon_hook, void *private );
void dbmon_status( FILE *fp, Hook *dbmon_hook );
void dbmon_close( Hook **dbmon_hook );
char *dbmon_compute_row_sync( Dbptr db );

#ifdef	__cplusplus
}
#endif

#endif

