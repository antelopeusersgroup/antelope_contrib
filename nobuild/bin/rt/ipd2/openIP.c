/*************************************************************************
 *
 *  openIP.c
 *
 *  Open input port(s) - SCSI, serial, HSI, socket.                          
 *
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 *************************************************************************/
#include "ipd2.h"
#include <termio.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
			       
extern int Psize;

int 
open_IN_ports( inport )
struct Prts *inport;

{
 
	struct stat buf;

/* What type of port we have?  */


	if((stat(inport->ip_name, &buf)) != 0)  {
  	   if(ENOENT)  {
		if( ( isdigit( inport->ip_name[0] ) && inport->ip_name[3] == '.' ) ||
		      !strncmp( inport->ip_name, "local", strlen("local") ) )  {
	              return open_socket( inport );
	        } 
                elog_complain(0, "open_IN_ports():Port:%s doesn't exist!\n", inport->ip_name );
                return 0; 
           } else {
                elog_complain(1, "open_IN_ports():can't stat %s.", inport->ip_name );
                return 0; 
           }
       }  else {
	   elog_complain(1, "open_IN_ports():'%s' port is not supported.", inport->ip_name);
	   return 0;
       }
       
}

/* Is Input Port socket?  */

#define DELIMITERS  "."

int open_socket( inport )
struct Prts *inport; 

{
  	long b_size;
  	struct hostent *hp;                /* remote host info pointer */
  	struct sockaddr_in Myaddr_in;
	struct utsname hname;
  	u_long addr;
  	int s, addrlen;                    /* size of a structure */
  	char *hostname, *str;              /* machine which wants connection */
  	char *port_str, *token[4];
  	char *port_ascii;
	char *server_name;
	int port_num = RTDAS_PORT ;
	char buffer[1024];
  	int i, ntoken, string, nonblk, digit;
  	int true = 1;
        int done = 0, tried = 0;
	struct linger linger = {1, 1};  /* allow a lingering, graceful close; */

	string = digit = 0;


    if (!strncmp (inport->ip_name, "local", strlen ("local")) ) {
			     
           uname (&hname);
           hostname = strdup (hname.nodename);
			     
           hp = gethostbyname (hostname);
           if (hp == NULL) {
              elog_complain(0, "openID(): Can't get info for HOST - %s.\n", hostname);
              return 0;
           }
    }
    server_name = strdup( inport->ip_name );
    if( port_ascii = strchr( server_name, ':'))  {
	*port_ascii++ = 0;
	port_num = atoi( port_ascii );
    }
    while( !done )  {

        /* clear out structures */
 
  	memset((char *)&peer_in, 0, sizeof(struct sockaddr_in));
  	memset((char *)&myadd_in, 0, sizeof(struct sockaddr_in));
	/* create a socket  */
 
       if( (Ls = socket(AF_INET, SOCK_STREAM, 0)) < 0 )  {
    	    elog_die( 1, "Can't open stream socket\n" ) ; 
       }

  /* Convert IP address from a.b.c.d to the hexadecimal number  */
	   
       if ((int)(addr = inet_addr(server_name)) == -1) {
          elog_complain(0, "IPD/open_socket():IP-address must be of the form a.b.c.d\n");
          return 0;
       }
/*
printf( "%s => %x \n", server_name, addr );
fflush(stdout);
*/

       hp = gethostbyaddr((char *)&addr, sizeof (addr), AF_INET);
       if(hp == NULL)

          peer_in.sin_addr.s_addr = htonl(addr); 
       else 
           peer_in.sin_addr.s_addr = ((struct in_addr *)(hp->h_addr))->s_addr;
  
       peer_in.sin_family = AF_INET; 
       peer_in.sin_port = htons( port_num );


/* Change size of the input/output buffer. 
   Size by Default is 8K which is not enough.   */

 
      b_size = Psize;  

      if( setsockopt( Ls, SOL_SOCKET, SO_SNDBUF, (char *)&b_size, sizeof(int)) != 0)  {
    	   elog_die( 1, "Unable to set size of send buffer.\n");
      }
      if( setsockopt(Ls, SOL_SOCKET, SO_RCVBUF, (char *)&b_size, sizeof(int)) != 0)  {
     	     elog_die( 1, "Unable to set size of send buffer.\n");
       }


       addrlen = sizeof(struct sockaddr_in);

       if ( connect (Ls, (struct sockaddr *) & peer_in, addrlen) == -1) {
           if( !tried )  {
	      tried = 1;
	      elog_complain( 1, "waiting for connection with %s \n", inport->ip_name );
              sleep(1);
	   }
	   close(Ls);
        } else done = 1;	
    } 

    if ( fcntl(Ls, F_SETFL, O_NONBLOCK) == -1 ) { 
        elog_die( 1, "Can't set non-blocking on accept socket\n" ) ; 
    }

     inport->ifp = Ls;
     return IN_SOCKET;

}
 
