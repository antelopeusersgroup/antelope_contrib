/*************************************************************************
 *
 *  open_dc.c
 *
 *  open a DC port;
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 *************************************************************************/
#include "mrc.h"
#include <termio.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
			       
#define DCCMD_PORT   5001 

int open_dc( char *inport )

{
  	struct hostent *hp;                /* remote host info pointer */
	struct utsname hname;
  	u_long addr;
  	int s, addrlen;                    /* size of a structure */
  	char *hostname, *str;              /* machine which wants connection */
  	char *port_ascii;
	char *server_name;
        int done = 0, tried = 0;
        int port_num;


    if (!strncmp (inport, "local", strlen ("local")) ||
        !strncmp (inport, "socket", strlen ("socket"))) {
			     
           uname (&hname);
           hostname = strdup (hname.nodename);
			     
           hp = gethostbyname (hostname);
           if (hp == NULL) {
              elog_complain(0, "openID(): Can't get info for HOST - %s.\n", hostname);
              return 0;
           }
    }
    server_name = strdup( inport );
    if( port_ascii = strchr( server_name, ':'))  {
	*port_ascii++ = 0;
	port_num = atoi( port_ascii );
    } else port_num = DCCMD_PORT;

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

       hp = gethostbyaddr((char *)&addr, sizeof (addr), AF_INET);
       if(hp == NULL)
          /*peer_in.sin_addr.s_addr = htonl( ADDR ); */
	  elog_die( 0, "gethostbyaddr() failed for %s\n", inport ); 
       else 
           peer_in.sin_addr.s_addr = ((struct in_addr *)(hp->h_addr))->s_addr;
  
       peer_in.sin_family = AF_INET; 
       peer_in.sin_port = htons( port_num );

       addrlen = sizeof(struct sockaddr_in);

       if ( connect (Ls, (struct sockaddr *) & peer_in, addrlen) == -1) {
           if( !tried )  {
	      tried = 1;
	      elog_complain( 1, "waiting for connection \n");
              sleep(1);
	      elog_complain( 1, "can't connect %s\n", inport );
	   }
	   close(Ls);
        } else done = 1;	
    } 

     return 1;

}
 
