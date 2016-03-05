#ifndef __P_TCLRTD__
#define  __P_TCLRTD__

#include "tcl.h"
#include "db.h" 

#ifdef __STDC__
#define PL_(x) x
#else
#define PL_(x) ( )
#endif /* __STDC__ */

extern int dpadmCmd PL_(( void * clientData, Tcl_Interp * interp, int argc, char ** argv ));
extern int dcrtCmd PL_(( void * clientData, Tcl_Interp * interp, int argc, char ** argv ));
extern int dprtCmd PL_(( void * clientData, Tcl_Interp * interp, int argc, char ** argv ));
extern int dccCmd PL_(( void * clientData, Tcl_Interp * interp, int argc, char ** argv ));

#undef PL_
#endif

