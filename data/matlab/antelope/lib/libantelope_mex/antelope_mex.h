/*
 * Matlab interface to Datascope package
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#include <mex.h>
#include "db.h"
#include "tr.h"
#include "stock.h"
#include "coords.h"
#include "elog.h"
#include "response.h"
#include "pf.h"

#if __STDC__
#define PL_(x) x
#else
#define PL_(x) ( )
#endif /* __STDC__ */

#define STREQ(a, b) (strcmp((a), (b)) == 0)

extern int mxArrayToInt32 PL_(( mxArray *array_ptr ));
extern unsigned int mxArrayToUint32 PL_(( mxArray *array_ptr ));
extern mxArray *CreateDouble PL_(( double input_double ));
extern mxArray *Int32ToMxArray PL_(( int input_int ));
extern mxArray *Uint32ToMxArray PL_(( unsigned int input_uint ));
extern mxArray *CreateNullDbptrStruct PL_(( void ));
extern mxArray *CreateDbptrStructFromDbptr PL_(( Dbptr db ));
extern Dbptr CastDbptrStructToDbptr PL_(( const mxArray *DbptrStruct ));
extern int AssertIsDbptrStruct PL_(( const mxArray *teststruct ));
extern void get_malloced_string PL_(( const mxArray *array_ptr, char **buf ));
extern void antelope_mexUsageMsgTxt PL_(( char * ));
extern mxArray *stringtbl2cellstr PL_(( Tbl * ));
extern Tbl *cellstr2stringtbl PL_(( mxArray * ));
extern mxArray *pftbl2cellarr PL_(( Pf *, int ));
extern mxArray *pfarr2struct PL_(( Pf *, int ));
extern Pf *mxArray2Pf PL_(( mxArray * ));
extern mxArray *Pf2mxArray PL_(( Pf *, char * ));
extern Response *mxArray2Response PL_(( mxArray * ));
extern mxArray *Response2mxArray PL_(( Response * ));
extern mxArray *mxPfprompt PL_(( char * ));
extern mxArray *mxPfprompt_string PL_(( char * ));
extern mxArray *mxTranslate_Boolean PL_(( mxArray * ));
extern int pfput_mxArray PL_(( Pf *, char *, mxArray * ));
extern int pfpeek PL_(( Pf *, char *, Pf ** ));
extern int get_dbptr PL_(( mxArray *, Dbptr * ));
extern int get_string PL_(( mxArray *, char ** ));
extern int get_pf PL_(( mxArray *, Pf ** ));
extern int get_response PL_(( mxArray *, Response ** ));
extern int get_string PL_(( mxArray *, char ** ));
extern int get_scalar PL_(( mxArray *, double * ));
extern void antelope_mex_clear_register PL_(( int ));
extern mxArray *dbfield2mxArray PL_(( Dbptr ));
extern mxArray *dbcolumn2mxArray PL_(( Dbptr ));
extern Dbvalue *mxArray2dbvalue PL_(( mxArray * ));
extern void SCAFFOLD_fix_tr_endtime PL_(( Dbptr ));
extern mxArray *orbpkt2mxArray PL_(( char *, double, char *, int, char * ));


