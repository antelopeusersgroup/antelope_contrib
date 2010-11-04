/*
 * Matlab interface to Datascope package
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#include <sys/types.h>
#include <time.h>
#include "mex.h"
/* Matlab R2010b defines printf as mexPrintf: */
#undef printf
#include "db.h"
#include "tr.h"
#include "stock.h"
#include "coords.h"
#include "elog.h"
#include "complex.h"
#include "response.h"
#ifdef HAVE_CGEOM
#include "cgeom.h"
#endif
#include "pf.h"

#if __STDC__
#define PL_(x) x
#else
#define PL_(x) ( )
#endif /* __STDC__ */

#define STREQ(a, b) (strcmp((a), (b)) == 0)

extern long mxArrayToLong PL_(( mxArray *array_ptr ));
extern unsigned long mxArrayToUlong PL_(( mxArray *array_ptr ));
extern mxArray *CreateDouble PL_(( double input_double ));
extern mxArray *UlongToMxArray PL_(( unsigned long input_uint ));
extern mxArray *Complex_tToMxArray PL_(( Complex_t *cx, int n ));
extern mxArray *DoubleArrToMxArray PL_(( double *vals, int n ));
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
extern mxArray *Response2mxArray_parse PL_(( Response * ));
#ifdef HAVE_CGEOM
extern int get_cggrid PL_(( const mxArray *, CGGrid ** ));
extern int ATM_cggrid_register PL_(( CGGrid * ));
extern int ATM_cggrid_unregister PL_(( CGGrid * ));
extern int ATM_cggrid_is_registered PL_(( CGGrid * ));
extern CGGrid *mxArray2CGGrid PL_(( const mxArray * ));
extern CGGrid *plaid_mxArrays2CGGrid PL_(( const mxArray *, const mxArray *, const mxArray * ));
extern mxArray *CGGrid2mxArray PL_(( CGGrid * ));
extern mxArray *CGGrid2linear_mxArray PL_(( CGGrid * ));
extern int CGGrid2plaid_mxArrays PL_(( CGGrid *, mxArray **, mxArray **, mxArray ** ));
#endif
extern char *matlabPfprompt PL_(( char * ));
extern mxArray *mxPfprompt PL_(( char * ));
extern mxArray *mxPfprompt_string PL_(( char * ));
extern mxArray *mxTranslate_Boolean PL_(( const mxArray * ));
extern mxArray *pfstring2mxArray PL_(( char * ));
extern int pfput_mxArray PL_(( Pf *, char *, const mxArray * ));
extern int pfpeek PL_(( Pf *, char *, Pf ** ));
extern int get_dbptr PL_(( const mxArray *, Dbptr * ));
extern int get_trimmed_string PL_(( const mxArray *, char ** ));
extern int get_stringtbl PL_(( const mxArray *, Tbl ** ));
extern int get_inttbl PL_(( const mxArray *, Tbl ** ));
extern int get_pf PL_(( const mxArray *, Pf ** ));
extern int mtlb_get_response PL_(( const mxArray *, Response ** ));
extern int mtlb_get_string PL_(( const mxArray *, char ** ));
extern int get_scalar PL_(( const mxArray *, double * ));
extern void antelope_mex_clear_register PL_(( int ));
extern int antelope_mex_elog_callback( int severity, char *string, Tbl *Elog );
extern mxArray *dbfield2mxArray PL_(( Dbptr ));
extern mxArray *dbcolumn2mxArray PL_(( Dbptr ));
extern Dbvalue *mxArray2dbvalue PL_(( const mxArray *, long ));
extern void SCAFFOLD_fix_tr_endtime PL_(( Dbptr ));
extern mxArray *orbpkt2mxArray PL_(( char *, double, char *, int, char * ));
extern Dbptr dbprocess_error PL_(( Dbptr, char * ));

extern Arr *ATM_CGGrid_Registry;
