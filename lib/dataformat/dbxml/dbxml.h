
#ifndef XMLDB
#define XMLDB

#define DBXML_BNS 1
#define DBXML_PRIMARY 2

#ifdef  __cplusplus
extern "C" {
#endif

extern int db2xml( Dbptr db, char *rootnode, char *rownode, Tbl *fields, Tbl *expressions, void **xml, int flags );
extern int xml2db( Dbptr db, char *xml );

#ifdef  __cplusplus
}
#endif

#endif
