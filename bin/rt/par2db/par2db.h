#include <math.h>
#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <netinet/in.h>

#include "tr.h"
#include "orb.h"
#include "pkt.h"
#include "dsap_regex.h"

typedef struct Source {
    Orbpipe *apipe ;
    double last ; 
} Source ;

typedef struct Save_params {
    Dbptr db ;
    double segsiz ; 
    int datacode ;      
    char datatype[8] ; 
    char *wfname ; 
} Save_params ;

typedef struct Db_buffer { 
    Dbptr db ; 		/* database pointer to open record */
    double tmax ;       /* max # of samples in current record  */      
    double crnt_time ;
    double stime ;
    double etime ;
    double samprate ; 
    int nsamp ;
    int maxsamp ;
    char net[12]; 
    char sta[12];
    char chan[12] ;
    int *data ; 
    FILE *file ; 	/* open file descriptor */
    char *path ; 	/* path name */
    Steim *steim ;      /* steim buffers  */
    Save_params *params ;
} Db_buffer ; 

#define BIG_NUMBER 9.999e99

