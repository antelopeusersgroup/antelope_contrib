/*  Copyright (c) 1999 Boulder Real Time Technologies, Inc.           */
/*                                                                    */
/*  This software module is wholly owned by Boulder Real Time         */
/*  Technologies, Inc. Any use of this software module without        */
/*  express written permission from Boulder Real Time Technologies,   */
/*  Inc. is prohibited.                                               */

#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/socket.h>

#include "liss2orb.h"

static void
parsename ( char *name, int default_port, char *server, int *port )
{
    char *port_string ;

    strcpy(server, name ) ; 
    port_string = strchr(server, ':' ) ;
    if ( port_string != 0 ) {
	*port_string++ = 0 ; 
	if ( *port_string != 0 ) { 
	    *port = atoi(port_string) ; 
	} else { 
	    *port = default_port ; 
	}
    } else { 
	*port = default_port ; 
    }
    if ( *server == 0 ) { 
	strcpy ( server, "localhost" ) ;
    }
}

int 
open_socket ( char *name, int default_port ) 
{
    int fd ; 
    struct sockaddr_in serv_addr ; 
    char server[256] ;
    char ipc[32] ;
    int port ;

    memset ( (char *) &serv_addr, 0, sizeof(serv_addr) ) ; 
    parsename ( name, default_port, server, &port ) ;
    
    name2ip(server, (struct in_addr *)&serv_addr.sin_addr.s_addr, ipc ) ;
    serv_addr.sin_family = AF_INET ; 
    serv_addr.sin_port = htons ( port ) ; 

    if ( (fd = socket(PF_INET, SOCK_STREAM, 0 )) < 0 ) {
	elog_log( 1, "Can't open stream socket\n" ) ; 
	fd = -1 ;
    } else if ( connect(fd, 
		(struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0 ) {
	close ( fd ) ; 
	fd = -1 ;
    } else { 
	ignoreSIGPIPE() ;
    }

    return fd ;
}

