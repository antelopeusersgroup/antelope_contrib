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
#include <netdb.h>
#include <regex.h>
#include <stdlib.h>

#include "Pkt.h"
#include "tr.h"
#include "orb.h"
#include "stock.h"
#include "orb.h"
#include "db.h"
#include "xtra.h"
#include "ant_steim.h"
#include "swapbytes.h"

#define LISS_PORT 4000         /* input port; data from DAS  */
#define IBUF_SIZE       4096
#define Def_Seed_Size   512

typedef struct newch {
    char oldkey[32];
    char newkey[32];
} Newchan;

extern Arr *NewCh;

#define NCH_SCS "%s %s[^\n] \n"
#define NCH_RVL(SP)  (SP)->oldkey, (SP)->newkey
#define NCH_TRIM(SP) TRIM((SP)->oldkey,31); TRIM((SP)->newkey, 11)

extern int PSize;
extern int Log;
extern struct sockaddr_in peer_in;
 
extern void *read_server ( int ifp, int orb, int timeout, char *match );
extern int open_socket ( char *name, int default_port );
extern int liss2orbpkt ( char *seed, int size, char *database, int remap, char *srcname, double *time, char **packet, int *nbytes, int *bufsize );
extern int parse_seed_data_header ( Steim *conf )  ;

#endif
