/***********************************************************************
 *
 *  dbms/dbms.h
 *
 *  Include file of constants, macros, function declarations, etc.
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 ***********************************************************************/
#ifndef dbms_h_included
#define dbms_h_included
#include <stdio.h>      
#include <string.h>
#include <math.h>
#include "db.h"
#include "tr.h"
#include "arrays.h"
#include "coords.h"
#include <regex.h>

#define DELC	111.11
#define SWVEL   4.0

extern regex_t orig_match;
extern regex_t auth_match;
extern regex_t net_match;

extern Arr *AllEv;
extern Arr *EvArr;
extern Dbptr dball;
extern int Crnt_record;
extern int Log;
extern int prev_orid;

#define FL (0.025)
#define FLO (7)
#define FH (0.1)
#define FHO (7)

typedef struct flt {
   float lf;
   float hf;
   int lo;
   int ho;
} Filter;

#define FLT_RVL(SP)  &(SP)->lf, &(SP)->lo, &(SP)->hf, &(SP)->ho
#define FLT_SCS "%f %d %f %d"
#define TRBADS4(X) 	((X)>=2147470000||(X)<=-2147400000)

extern Filter flt;

typedef struct evrec {
   int record;
   int evid;
} EvRec;
 
typedef struct event {
        double otime;
        double stime;
        double etime;
        double delta;
        double amp;
        double mb;
        double ms;
        int arec;
        int arid;
        int orid;
        int evid;
        char net[12];
        char sta[8];
        char phase[8];
        char auth[16];
} Event;

 
#ifdef __STDC__
#define PL_(x) x
#else
#define PL_(x) ( )
#endif /* __STDC__ */
 
extern void usage PL_(( void ));
extern int main PL_(( int argc, char *argv[] ));
extern Event *new_ev PL_(( void ));  
extern EvRec *new_id PL_(( int rec, int id ));
extern int get_events PL_((Dbptr db )); 
extern int join_db PL_(( Dbptr db, char *id ));
 
#undef PL_

#endif
