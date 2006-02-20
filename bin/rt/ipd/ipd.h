/***********************************************************************
 *
 *  ipd/ipd.h
 *
 *  Include file of constants, macros, function declarations, etc.
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 ***********************************************************************/
#ifndef ipd_h_included
#define ipd_h_included
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/ser_sync.h>
#include <sys/conf.h>
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
#include <poll.h>
#include <stddef.h>
#include <stropts.h>
#include <netdb.h>
#include "defunctpkt.h"

#define DEBUG
#define NUMDAS 48

#define RTDAS_PORT 5000         /* input port; data from DAS  */

#define DSK_BLK_SIZE    1024	/* PASSCAL disk block size  */
#define IBUF_SIZE       4096
 
 /* Input Ports  */

#define  IN_SOCKET	0x534f43  /* 'SOC'  */
#define  IN_HSI		0x485349  /* 'HSI'  */
#define  IN_CHR		0x434852  /* 'CHR'  */
#define  IN_DISK	0x44534b  /* 'DSK'  */

#define OFF_PIDA	4
#define OFF_PIDB        6	
#define OFF_UIDA	2
#define OFF_UIDB	4
#define OFF_PLENA	26
#define OFF_PLENB	2

struct Prts  {
    int ifp;			/* input port file pointer  */
    int orb;			/* orb file pointer  */
    char ip_name[132];		/* input port name  */
    char orbname[132];		/* orbserver name  */
};

  
struct PsclDk {
  int leod;
  int maxblk;
  char label[16];
};

struct PsclDk PsclDk;
int Pblcks;

int Log;
int Ls;
struct sockaddr_in myadd_in;           
struct sockaddr_in peer_in;

 
#ifdef __STDC__
#define PL_(x) x
#else
#define PL_(x) ( )
#endif /* __STDC__ */
 
extern void usage PL_(( void ));
extern int main PL_(( int argc, char *argv[] ));
extern int open_IN_ports PL_(( struct Prts *ports ));
extern int open_disk PL_(( struct Prts *iports ));
extern int open_chr PL_(( struct Prts *ports ));
extern int open_socket PL_(( struct Prts *ports ));
extern void *read_in_ports PL_(( struct Prts *ports, int hdrtype, int timeout ));
extern int read_hsi PL_(( struct Prts *inport, unsigned char **buffer ));
extern int read_chr PL_(( struct Prts *inport, unsigned char **buffer ));
extern int read_disc PL_(( struct Prts *inport, unsigned char **buffer ));
extern int read_socket PL_(( struct Prts *inport, int hdrtype , int timeout));
extern int send2orb PL_ ((int *orb, char *orbname, char *packet, char *srcname, double epoch, int size, int err));
extern int valid_pkt PL_(( unsigned char **data, char *srcname, double *epoch, int *psize, int length, int err , int hdrtype));
 
#undef PL_

#endif
