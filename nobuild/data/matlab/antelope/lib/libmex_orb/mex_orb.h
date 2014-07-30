/*
 * Matlab interface to Datascope package
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#include <stdlib.h>
#include <mex.h>
/* Matlab R2010b defines printf as mexPrintf: */
#undef printf
#include "stock.h"
#include "orb.h"
#include "Pkt.h"

#if __STDC__
#define PL_(x) x
#else
#define PL_(x) ( )
#endif /* __STDC__ */

extern int mex_orbopen PL_(( char *, char * ));
extern int mex_orbclose PL_(( int ));
extern int mex_orbtell PL_(( int ));
extern int mex_orbseek PL_(( int, int ));
extern int mex_orbafter PL_(( int, double ));
extern int mex_orbselect PL_(( int, char * ));
extern int mex_orbreject PL_(( int, char * ));
extern int mex_orbping PL_(( int, int * ));
extern int mex_orbget PL_(( int, int, int *, char *, double *, char **, int *, int *  ));
extern int mex_orbreap PL_(( int, int *, char *, double *, char **, int *, int *  ));
extern int mex_orbreap_nd PL_(( int, int *, char *, double *, char **, int *, int *  ));
