
/* This file declares all the constants and routines needed for the public
 * interface to the db package */

#ifndef DB
#define DB

#include "stock.h"
#include <regex.h>

#define dbINVALID		    -102
#define INVALID_DBPTR 	{dbINVALID, dbINVALID, dbINVALID, dbINVALID} 

#define dbCOUNT -301
#define dbDATABASE_COUNT -302	       
#define dbTABLE_COUNT -303	      
				        
#define dbFIELD_COUNT -304	       
#define dbRECORD_COUNT -305	      
#define dbDESCRIPTION -306
#define dbSCHEMA_DESCRIPTION -307    
#define dbDATABASE_DESCRIPTION -308 
#define dbTABLE_DESCRIPTION -309   
#define dbFIELD_DESCRIPTION -310  
#define dbDETAIL -311
#define dbSCHEMA_DETAIL -312	       
#define dbDATABASE_DETAIL -313	      
#define dbTABLE_DETAIL -314	     
#define dbFIELD_DETAIL -315	    
#define dbNAME -316
#define dbSCHEMA_NAME -317	       
#define dbDATABASE_NAME -318	      
#define dbTABLE_NAME -319	     
#define dbFIELD_NAME -320	    
#define dbTABLE_PRESENT -321
#define dbSIZE -322
#define dbTABLE_SIZE -323	   
#define dbFIELD_SIZE -324	  
#define dbTYPE -325
#define dbFORMAT -326
#define dbFIELD_UNITS -327
#define dbFIELD_TYPE -328	  
#define dbTABLE_FILENAME   -329
#define dbDBPATH -330
#define dbTABLE_DIRNAME -331
#define dbPRIMARY_KEY -332
#define dbALTERNATE_KEY -333
#define dbFOREIGN_KEYS -334
#define dbUNIQUE_ID_NAME -336
#define dbSCHEMA_DEFAULT -338

#define dbFIELD_RANGE -341
#define dbVIEW_TABLE_COUNT -342
#define dbRECORD_SIZE -343
#define dbFIELD_FORMAT -344
#define dbFIELD_INDEX -345
#define dbTABLE_ADDRESS -346

#define dbTABLE_FIELDS -347
#define dbFIELD_TABLES -348
#define dbVIEW_TABLES -349
#define dbLINK_FIELDS -350
#define dbSCHEMA_FIELDS -351
#define dbTABLE_IS_WRITEABLE -352
#define dbTABLE_IS_VIEW -353
#define dbFIELD_BASE_TABLE -354
#define dbTABLE_IS_TRANSIENT -355
#define dbTIMEDATE_NAME -356
#define dbDATABASE_FILENAME -357
#define dbSCHEMA_TABLES -358
#define dbDATABASE_IS_WRITEABLE -359
#define dbLASTIDS -360


#define dbDATABASE		-401
#define dbVIEW			-402
#define dbTABLE			-403
#define dbFIELD			-404
#define dbRECORD		-405
#define dbMERGE			-406

#define dbALL			-501
#define dbPRIMARY		-502
#define dbALTERNATE		-503
#define dbSCRATCH		-504
#define dbNULL			-505

#define dbBOOLEAN	1
#define dbINTEGER		2
#define dbREAL 		3
#define dbTIME		4
#define dbYEARDAY		5
#define dbSTRING		6
#define dbLINK		8

#define dbWAVEFORM	136
#define dbRESPONSE	137

#define dbBFLOAT        138
#define dbBDOUBLE       139
#define dbBSHORT        140
#define dbBINT          141

#define dbDBPTR		142

#define dbUNIQUE  1
#define dbOUTER_JOIN 2

#define dbSORT_UNIQUE  2
#define dbSORT_REVERSE 1

typedef struct Dbptr
{
    int             database,
                    table,
                    field,
                    record;
}               Dbptr;

typedef union Dbvalue
{
    char           *t;
    char            s[STRSZ];
    int             i;
    double          d;
    Dbptr	    db ;
    Tbl		   *tbl ; 
    Arr		   *arr ; 
}               Dbvalue;

typedef union Exvalue {
    double d ;
    int i ;
    int pipe[2] ; 
    char *s ;
    struct re_pattern_buffer *p ;
    regex_t	*regexp ;
    int  (*fi)() ;
    double (*fd)() ;
    char *(*fs)() ;
    } Exvalue ;

typedef struct Expression {
    struct Expression *left, *right, *x3, *x4 ; 
    Tbl *expr_tbl ; 
    int type ; 
    int kind ;
    Exvalue value ;
    } Expression ; 
 

extern Xlat       Dbxlat[] ;
extern int	  NDbxlat ; 

extern void dbadd2db ( Dbptr dbin, Arr *records, int flags, Dbptr dbout, Dbptr dbconflict );
extern int dbcopy ( Dbptr dbinput, Dbptr dboutput, Arr *expressions );
extern Dbptr dbinvalid (void) ;
extern Dbptr dbgroup ( Dbptr db, Tbl *groupfields, char *name, int type );
extern Dbptr dbjoin ( Dbptr db1, Dbptr db2, Tbl **keys1p, Tbl **keys2p, int outer, Tbl **nojoin, char *name );
extern Dbptr dblist2subset ( Dbptr db, Tbl *list );
extern Dbptr dblookup ( Dbptr db, char *database_name, char *table_name, char *field_name, char *record_name );
extern Dbptr dbnojoin ( Dbptr db1, Dbptr db2, Tbl **keys1p, Tbl **keys2p, char *view_name );
extern Dbptr dbprocess ( Dbptr db, Tbl *list, Dbptr (*unknown)() );
extern Dbptr dbsort ( Dbptr db, Tbl *tbl, int flag, char *name );
extern Dbptr dbsubset ( Dbptr db, char *s, char *name );
extern Dbptr dbsever ( Dbptr db, char *tablename, char *name );
extern int dbuntangle ( Dbptr db, Arr **table_records );
extern Dbptr dbtheta ( Dbptr db1, Dbptr db2, char *ex_str, int joinflag, char *name );
extern Tbl * dbtables ( Dbptr db, char *fieldname );
extern int dbadd ( Dbptr db, char *record );
extern int dbadd_remark ( Dbptr db, char *remark );
extern int dbaddnull ( Dbptr db );
extern int dbaddv ( Dbptr db, char *tablename, ... );
extern int dbcompile ( Dbptr db, char *s );
extern int dbclose ( Dbptr db );
extern int dbcreate ( char *filename, char *schema, char *name, char *description, char *detail );
extern Dbptr dbtmp ( char *schema );
extern int dbcrunch ( Dbptr db );
extern int dbdelete ( Dbptr db );
extern int dbdestroy ( Dbptr db );
extern int dbex_compile ( Dbptr db, char *s, Expression **n, int type );
extern int dbex_eval ( Dbptr db, Expression *ex, int setflag, void *vresult );
extern int dbex_evalstr ( Dbptr db, char *s, int type, void *value );
extern int dbex_free ( Expression *tree );
extern int dbextfile ( Dbptr db, char *tablename, char *filename );
extern int dbfilename ( Dbptr db, char *filename );
extern int dbfind_join_keys ( Dbptr db1, Dbptr db2, Tbl **keys1p, Tbl **keys2p );
extern int dbflush_indexes ( Dbptr db );
extern int dbfree ( Dbptr db );
extern int dbget ( Dbptr db, char *s );
extern int dbget_remark ( Dbptr db, char **remark );
extern int dbgetv ( Dbptr db, char *tablename, ... );
extern int dbis_expression ( char *s );
extern int dbmark ( Dbptr db );
extern int dbmatches ( Dbptr dbk, Dbptr dbt, Tbl **kpatternp, Tbl **tpatternp, Hook **hookp, Tbl **tblp );
extern int dbmatch_range ( Dbptr dbk, Dbptr dbt, Tbl **kpatternp, Tbl **tpatternp, void **hookp, void **indexp, int *ns, int *ne );
extern int dbnextid ( Dbptr db, char *name );
extern int dbopen ( char *path, char *opentype, Dbptr *db );
extern int dbopen_database ( char *name, char *permissions, Dbptr *dbp );
extern int dbopen_table ( char *name, char *permissions, Dbptr *dbp );
extern int dbput ( Dbptr db, char *s );
extern int dbputv ( Dbptr db, char *tablename, ... );
extern int dbputx ( Dbptr db, void *value );
extern int dbquery ( Dbptr db, int code, void *vvalue );
extern int dbread_view ( FILE *file, Dbptr *dbr, char *view_name );
extern int dbsave_view ( Dbptr db );
extern int dbselect ( Dbptr db, Tbl *fields, FILE *file );
extern Dbptr dbseparate ( Dbptr db, char *tablename );
extern int dbset ( Dbptr db, int code, void *value );
extern int dbtruncate ( Dbptr db, int n );
extern Dbptr dbungroup ( Dbptr db, char *name );
extern int dbunjoin ( Dbptr db, char *database_name, int rewrite );
extern int dbwrite_view ( Dbptr db, FILE *file );
extern void dbget_range ( Dbptr db, int *s, int *e );
extern char * expand_env ( char *s );

#endif

/* $Id$ */
