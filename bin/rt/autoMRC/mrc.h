/***********************************************************************
 *
 *  mrc/mrc.h
 *
 *  Include file of constants, macros, function declarations, etc.
 *
 ***********************************************************************/
#ifndef mrc_h_included
#define mrc_h_included
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/utsname.h>
#include <unistd.h>
#include <stdio.h>      
#include <errno.h>
#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <stropts.h>
#include <netdb.h>
#include "db.h"
#include "coords.h"
#include "arrays.h"
#include "pkt.h"

#define MAX_OFF  100000  /* Max LTA value which good data should not exceed */

Arr *Dasid;
Arr *PChan;

typedef struct Ch {
    double time;
    int nsamp;
    long lta;
} Ch;

typedef struct ChPipe {
    double time;
    int nsamp;
    int maxtbl;
    Ch crnt_chan;
    Tbl *tbl;
} ChPipe;

typedef struct ChRec {
    double time;
    long lta;
    int nsamp;
    int dasid;
    char srcid[ORBSRCNAME_SIZE];
    ChPipe *chpipe;
} ChRec;

typedef struct Das {
    double time;
    char srcid[ORBSRCNAME_SIZE];
    Orbpipe *apipe;
} Das;


int MaxOff;
int Ls;
char *logname;
FILE *fplog;

struct sockaddr_in myadd_in;           
struct sockaddr_in peer_in;

#ifdef __STDC__
#define PL_(x) x
#else
#define PL_(x) ( )
#endif /* __STDC__ */
 
extern void usage PL_(( void ));
extern int main PL_(( int argc, char *argv[] ));
extern int open_dc PL_(( char *ports ));
 
#undef PL_

#endif
