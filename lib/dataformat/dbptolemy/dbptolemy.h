
#ifndef PTOLEMYDB
#define PTOLEMYDB

#define DBPTOLEMY_BNS 1

#ifdef  __cplusplus
extern "C" {
#endif

extern int db2ptolemy( Dbptr db, Tbl *fields, Tbl *expressions, void **xml, int flags );

#ifdef  __cplusplus
}
#endif

#endif
