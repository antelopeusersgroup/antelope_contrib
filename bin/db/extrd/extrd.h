/***********************************************************************
 *
 *
 *  Include file of constants, macros, function declarations, etc.
 *
 ***********************************************************************/
#ifndef extrd_h_included
#define extrd_h_included
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <math.h>
#include <time.h>
#include <regex.h>
#include "tr.h"
#include "db.h"
#include "scv2.h"
#include "arrays.h"
#include "coords.h"

#define WFDNAME  ".all.wfdisc"
#define CLEAN "rm -rf .all.wfdisc"
#define MODE  (0664)
#define MAX_NSAMP  450000

extern regex_t sta_match;
extern regex_t chan_match;

extern FILE *Df;
extern char *Data_file;
extern char Outdir[132];
extern char Dfile[132];
extern long Foff;

extern Dbptr db, dbout;

typedef struct segment {
	double time;
	double etime;
	double endtime;
	double calib;
	double calper;
	double samprate;
	long foff;
	long sbytes;
	int new;
        int nsamp;
	int dcode;
	char net[8];
	char sta[8];
	char chan[12];
	char instype[8];
	char datatype[4];
	char segtype[4];
	char *dbname;
	Steim *steim;
	Dbptr db;
        FILE *fp;
} SegData;


typedef union {
     char           *c;
     short          *s;
     int            *i;
     float          *f;
     double         *d;
 }  Trdata;

extern Trdata mydata;

#endif
