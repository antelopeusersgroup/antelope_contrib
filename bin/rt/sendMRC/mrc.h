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
#include <netdb.h>
#include "db.h"
#include "coords.h"
#include "stock.h"
#include "arrays.h"

#define ONEDAY  86400		/* number of seconds in one day  */
#define NUMDAS 48		/* max number of DASes in RT system  */
#define NSLEEP  5
#define LEN     8               /* command buffer length  */
#define CMDDAS_PORT 5001         /* input port; data from DAS  */

#define DSK_BLK_SIZE    1024	/* PASSCAL disk block size  */
#define IBUF_SIZE       4096

#define OFF_PIDA        4
#define OFF_PIDB        6       
#define OFF_UIDA        2
#define OFF_UIDB        4
#define OFF_PLENA       26
#define OFF_PLENB       2

typedef struct Prts  {
    FILE *verbatim;
    int ifp;			/* input port file pointer  */
    char ip_name[132];		/* input port name  */
} Prts;

typedef struct Das {
    double time;
    int unit;
    int on;
    char name[12];
} DAS;

extern Arr *Dases;
extern Arr *Dasid;

extern int Log;
extern int Ls;
extern struct sockaddr_in myadd_in;           
extern struct sockaddr_in peer_in;

#ifdef __STDC__
#define PL_(x) x
#else
#define PL_(x) ( )
#endif /* __STDC__ */
 
extern void usage PL_(( void ));
extern int main PL_(( int argc, char *argv[] ));
extern int open_dc PL_(( struct Prts *ports, int num ));
 
#undef PL_

#endif
