/***********************************************************************
 *
 *  dccmd/dccmd.h
 *
 *  Include file of constants, macros, function declarations, etc.
 *
 ***********************************************************************/
#ifndef dccmd_h_included
#define dccmd_h_included
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
#include "pkt.h"

#define CMDDC_PORT 5001         /* input port; data from DAS  */
#define IBUF_SIZE       64

#define DCCMD	0x11
#define CRCMD   0x22
#define DASCMD  0x33

extern char *pfile;
extern Arr *Dases;
extern Arr *Dasid;
extern Tbl *CmdArg;
extern Tbl *Dlist;
extern int Log;
extern regex_t argument;

extern struct sockaddr_in myadd_in;           
extern struct sockaddr_in peer_in;

#ifdef __STDC__
#define PL_(x) x
#else
#define PL_(x) ( )
#endif /* __STDC__ */
 
extern void usage PL_(( void ));
extern int main PL_(( int argc, char *argv[] ));
extern int opendc PL_(( char *ports, int *pf ));
 
#undef PL_

#endif
