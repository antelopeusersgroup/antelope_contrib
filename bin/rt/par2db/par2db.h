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
    double segment_size ; 
    double memsize ;
    double gapmax ; 
    char *wfname ; 
} Save_params ;

typedef struct Data_segment { 
    double t0 ;
    double samprate ; 
    int prev_val;
    int nsamp, maxsamp, ngapmax ;
    int *data ; 
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

extern int Verbose ;
extern Arr *Selected, *Rejected ;

#ifdef __STDC__
#define PL_(x) x
#else
#define PL_(x) ( )
#endif /* __STDC__ */

extern void flushrecord PL_(( Db_buffer *buf ));
extern int flush2db PL_(( Db_buffer *buf, int finish ));
extern Data_segment * new_data_segment PL_(( int maxsamp ));
extern Db_buffer * new_db_buffer PL_(( PktChannel *src, Save_params *params ));
extern int new_dbrecord PL_(( Db_buffer *buf ));
extern Source * new_source PL_(( ));
extern double get_last_dbtimes PL_(( Dbptr db ));
extern int main PL_(( int argc, char **argv ));
extern int seg_append PL_(( PktChannel *new, Db_buffer *buf ));
extern void usage PL_(( void ));

#undef PL_
