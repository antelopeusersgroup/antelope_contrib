/***********************************************************************
 *
 *
 *  Include file of constants, macros, function declarations, etc.
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 ***********************************************************************/
#ifndef liss2orb_h_included
#define liss2orb_h_included
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
#include <fcntl.h>
#include <poll.h>
#include <stddef.h>
#include <stropts.h>
#include <netdb.h>
#include <regex.h>
#include "pkt.h"

#define LISS_PORT 4000         /* input port; data from DAS  */
#define IBUF_SIZE       4096
#define Def_Seed_Size   512

int PSize;
int Log;
struct sockaddr_in peer_in;
 
#ifdef __STDC__
#define PL_(x) x
#else
#define PL_(x) ( )
#endif /* __STDC__ */
 
extern void usage PL_(( void ));
extern int main PL_(( int argc, char *argv[] ));
extern int open_socket PL_(( char *iport));
extern void *read_server PL_(( int ifp, int orb, int timeout, char *match ));
 
#undef PL_

#endif
