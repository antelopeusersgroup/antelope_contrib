/*************************************************************************
 *
 *                      
 * Include file of constants, macros, function declarations, etc.
 *                                                         
 *
 *
 ***********************************************************************/
#ifndef dbtc_h_included
#define dbtc_h_includeed
#include <math.h>
#include <stdio.h>
#include <errno.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <unistd.h>
#include "coords.h" 
#include "tr.h"
 
#define TRIM(s,l) {int i;for(i=l-1;i>0,s[i-1]==' ';i--);s[i]='\0';}
 
extern int Log;
extern long Foff;

typedef struct param {
  int datseg;
  int cor_win;
  int err_win;
  int ncorr;
} Param;

typedef struct data_set {
   Dbptr db;
   int dbrec;
   double srate;
   double stime;
   double etime;
   double mean;
   double dev;
   int npts;
   char key[512];
   char dbname[256];
   char net[12];
   char sta[8];
   char chan[12];
   float *data;
} Dset;

extern Dbptr Dbtc; 
  
extern int mean( float *data, int num, double *mean );
extern int deviation( float *data, int num, double mean, double *dev );
extern int correlation( Dset *set1, Dset *set2, Param *par, double *tcorr );
 
#endif

