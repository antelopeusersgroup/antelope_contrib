/***********************************************************************
 *
 *  ref2db/ref2db.h
 *
 *  Include file of constants, macros, function declarations, etc.
 *
 ***********************************************************************/
#ifndef ref2db_h_included
#define ref2db_h_included
#include <sys/types.h>
#include <sys/ser_sync.h>
#include <sys/conf.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <stdio.h>      
#include <errno.h>
#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <stddef.h>
#include <math.h>
#include "pkt.h"
#include "tr.h"
#include "regex.h"


#define DSK_BLK_SIZE    1024	/* PASSCAL disk block size  */
#define IBUF_SIZE       4096
#define BIG_NUMBER      9.999e99
 
 /* Input Ports  */

#define  IN_CHR		11  /* Char device */
#define  IN_DISK	13  /* Raw Disk  */

extern struct Prm Par;
extern Arr *RawPkts;
extern Arr *Packets;
Arr *Dases;	    /* selected Dases  */

struct PsclDk {
  int leod;         /* last data block on diak  */
  int maxblk;       /* max number disk blocks  */
  char label[16];   /* disk label  */
};

struct PsclDk PsclDk;
int Pblcks;

#define  timerr_fname "TimErr.log"
FILE *timerr;
int Log;

typedef struct runarg {
   double stime;	/* start time of extracted data */
   double etime;	/* end time of extracted data */
   int dev_type;	/* type of device - disk, tape, file */
   int ifp;
   int nodata;		/* pattern was specified to extract specific stream_sta_ch */
   int select;		/* pattern was specified to extract specific stream_sta_ch */
   regex_t srcid;
   char iport[128] ; 	/* name of input  */
   char dbname[128] ; 	/* Db output name    */
   char match[64];	/* pattern in form of streamid_dasid_chan  */
} RunArg;

typedef struct stach  {
    double str_time;	/* FDST from EH packet  */
    double stime;	/* FDST from DT packett */
    double time_diff;   /* time gap  */
    float srate;	/* sample rate  */
    int nsamp;		/* current number of sample in event  */ 
    int stream;		/* stream ID  */
    int sta;		/* DAS ID  */
    int chan;		/* chan ID  */
} ChArr;

typedef struct SpecPar {
    Dbptr db ;		/* Db data pointer  */
    double segsiz ; 	/* size of data files on disk  */
    int byevent;         /* write wfdisc records by events  */
    int datacode ;	
    char datatype[8] ; 
    char network[12] ; 
    char *wfname ;	/* name of a wfdisc file  */ 
} SpecPar ;

typedef struct Ch_data { 
    Dbptr db ; 		/* Db pointer */
    double tmax ;       /* max # of samples in current record  */	
    double stime;       /* start time for a current record  */
    double etime;       /* end time for a current record  */
    double crnt_time;   /* current time for a current record  */
    double samprate ;
    int ev_over;
    int nsamp;
    int maxsamp ;	
    char net[12];
    char sta[12];
    char chan[12] ; 
    int *data ;
    FILE *file ; 	
    Steim *steim ; 	/* steim buffers  */
    SpecPar *params ;
    char *path ;        /* path to a output datafile name  */	
} Ch_data ; 

 
typedef struct Daslist {
    double stime;
    double etime;
    int dasid;
    char name[8];
} Daslist;
 
#define DAS_RVL(SP) \
    &(SP)->dasid, (SP)->stime, (SP)->etime, (SP)->name 
#define DAS_TRIM(SP) \
    TRIM((SP)->stime,18); TRIM((SP)->stime,18); TRIM((SP)->name,7)
#define DAS_SCS "%d %s %s %s [^\n] \n"
 
#ifdef __STDC__
#define PL_(x) x
#else
#define PL_(x) ( )
#endif /* __STDC__ */
 
extern void usage PL_(( void ));
extern void wrt_record PL_(( Ch_data *buf ));
extern int main PL_(( int argc, char *argv[] ));
extern int open_IN_ports PL_(( RunArg *par ));
extern int open_disk PL_(( RunArg *par ));
extern int open_chr PL_((  RunArg *par ));
extern int read_input PL_(( SpecPar *par, RunArg *arg ));
extern int record PL_(( Ch_data *buf, PktChannel *new ));
extern Steim *stinit PL_(( Ch_data *buf ));
extern Ch_data * new_chan PL_(( PktChannel *src, SpecPar *par ));
extern int new_dbrecord PL_(( Ch_data *buf, PktChannel *new, double t ));
extern int save_seed PL_(( Steim *conf, int n0, int n1 ));

#undef PL

#endif

