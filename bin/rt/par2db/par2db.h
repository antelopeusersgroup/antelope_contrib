#include <math.h>
#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <netinet/in.h>

#include "tr.h"
#include "orb.h"
#include "pkt.h"
#include "regex.h"

typedef int Segsample ;

typedef struct Ste {
        int sid;                /* DAS ID  */
        char pkttype[12];       /* Packet type - CBBHS, CBBLS, etc */
        char net[8];           /* Site name */
        char name[8];           /* Site name */
        char up[3];             /* Y/N site is up/down  */
} Ste;
 
/* Definition for reading&trimming&writing the structure  */
 
#define S_RVL(SP)  \
(SP)->pkttype,(SP)->net, &(SP)->sid,(SP)->name,(SP)->up
#define S_SCS " %s %s %d %s %s [^\n] \n"

Arr *Pid;

typedef struct Source {
    Orbpipe *apipe ;
    double after, until ; 
    int count, duplicate, outoforder, ignored, ignored_after ; 
} Source ;

typedef struct Save_params {
    Dbptr db ;
    double segment_size ; 
    double memsize ;
    double gapmax ; 
    int gap_value ;
    char datatype[8] ; 
    int datacode ;
    char *wfname ; 
} Save_params ;

typedef struct Data_segment { 
    double t0 ;
    double samprate ; 
    double calib ; 
    int nsamp, maxsamp, ngapmax ;
    Segsample *data ; 
} Data_segment ;

typedef struct Db_buffer { 
    char net[PKT_NAMESIZE], sta[PKT_NAMESIZE], chan[PKT_NAMESIZE] ; 
    Data_segment *mem ; 
    Data_segment *disk ; 

    Dbptr db ; 		/* database pointer to open record */
    FILE *file ; 	/* open file descriptor */
    char *path ; 	/* path name */
    double tmax ;	/* maximum sample time in this record */

    Save_params *params ;
} Db_buffer ; 

#define VERY_LARGE_DOUBLE 9.999e99

int Ascii;
int Intv;
char Staid[16][PKT_NAMESIZE];

extern int Verbose ;
extern Arr *Selected, *Rejected ;

#ifdef __STDC__
#define PL_(x) x
#else
#define PL_(x) ( )
#endif /* __STDC__ */

extern void flushrecord PL_(( Db_buffer *buf ));
extern int flush2db PL_(( Db_buffer *buf, int finish ));
extern void free_data_segment PL_(( Data_segment * aseg ));
extern void free_db_buffer PL_(( Db_buffer * abuf ));
extern Db_buffer * get_match PL_(( PktChannel *achan, Save_params *params, struct re_pattern_buffer *acc_re ));
extern Data_segment * new_data_segment PL_(( int maxsamp ));
extern Db_buffer * new_db_buffer PL_(( PktChannel *src, Save_params *params ));
extern int new_dbrecord PL_(( Db_buffer *buf ));
extern Source * new_source PL_(( int maxpkts, double after, double until ));
extern double get_last_dbtimes PL_(( Dbptr db ));
extern void findmin PL_(( char *key, void *vvalue, void *vignored ));
extern int min_ignored_after PL_(( Arr *sources ));
extern void show_stat PL_(( char *key, void *vvalue, void *vignored ));
extern void show_statistics PL_(( Arr *sources ));
extern void free_asource PL_(( Source *asource ));
extern int main PL_(( int argc, char **argv ));
extern int pkt2db PL_(( char *srcname, double pkttime, char *packet, int sortcode, struct re_pattern_buffer * acc_re, Save_params * params ));
extern void report_gap PL_(( PktChannel *achan, Db_buffer *buf ));
extern void init_data_segment PL_(( PktChannel *new, Db_buffer *buf ));
extern int seg_append PL_(( PktChannel *new, Db_buffer *buf ));
extern void usage PL_(( void ));

#undef PL_
