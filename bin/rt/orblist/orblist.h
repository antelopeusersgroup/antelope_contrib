/* %W% %G%  */
/***********************************************************************
 *
 *  Include file of constants, macros, function declarations, etc.
 *
 ***********************************************************************/
#ifndef orblist_h_included
#define orblist_h_included

#include <errno.h>
#include <malloc.h>
#include <math.h>
#include "db.h"
#include "coords.h"
#include "stock.h"
#include "orb.h"
#include "pkt.h"


int MaxGap;
Arr *DTArr;

typedef struct Detect {
    double time;
    char srcid[ORBSRCNAME_SIZE];
    Orbpipe *apipe;
} Detect;
 
struct detect {
    double time;
    int state;
    char srcid[66];
    char chan[12];
    char filter[30];
};


#ifdef __STDC__
#define PL_(x) x
#else
#define PL_(x) ( )
#endif /* __STDC__ */
 
extern void usage PL_(( void ));
extern int main PL_(( int argc, char *argv[] ));
extern void init PL_(( void ));
extern Detect *new_tbl PL_(( char *srcid, double time ));

#undef PL_
 
#endif
