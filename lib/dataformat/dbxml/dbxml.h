
#ifndef XMLDB
#define XMLDB

#ifdef  __cplusplus
extern "C" {
#endif

extern int db2xml( Dbptr db, char *rootnode, char *rownode, Tbl *fields, Tbl *expressions, void **xml, int flags );

#ifdef  __cplusplus
}
#endif

#endif
