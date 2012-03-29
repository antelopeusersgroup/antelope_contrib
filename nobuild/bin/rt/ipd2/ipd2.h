/***********************************************************************
 *
 *  ipd2/ipd2.h
 *
 *  Include file of constants, macros, function declarations, etc.
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 ***********************************************************************/
#ifndef ipd2_h_included
#define ipd2_h_included

#include "deviants_include.h" 

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
#include H_POLL
#include <stddef.h>
#include <netdb.h>
#include "pkt2.h"

#define DEBUG
#define NUMDAS 48

#define RTDAS_PORT 5000         /* input port; data from DAS  */

#define IBUF_SIZE       4096
 
 /* Input Ports  */

#define  IN_SOCKET	0x534f43  /* 'SOC'  */

#define OFF_PIDA	4
#define OFF_PIDB        6	
#define OFF_PIDDA       10	
#define OFF_UIDA	2
#define OFF_UIDB	4
#define OFF_UIDDA	8
#define OFF_PLENA	26
#define OFF_PLENB	2
#define OFF_PLENDA	2

struct Prts  {
    int ifp;			/* input port file pointer  */
    int orb;			/* orb file pointer  */
    char ip_name[132];		/* input port name  */
    char orbname[132];		/* orbserver name  */
};

  
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
extern int open_IN_ports PL_(( struct Prts *ports ));
extern int open_socket PL_(( struct Prts *ports ));
extern void *read_in_ports PL_(( struct Prts *ports, int hdrtype, int timeout ));
extern int read_socket PL_(( struct Prts *inport, int hdrtype , int timeout));
extern int send2orb PL_ ((int *orb, char *orbname, char *packet, char *srcname, double epoch, int size, int err));
extern int valid_pkt PL_(( unsigned char **data, char *srcname, double *epoch, int *psize, int length, int hdrtype));
 
#undef PL_

#endif
