/*************************************************************************
 *
 *  open_socket.c
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 *************************************************************************/
#include "liss2orb.h"
			       

/* Is Input Port socket?  */

extern struct hostent *gethostbyname();

int open_socket( char *inport )

{
  	long b_size;
  	struct hostent *hp;                /* remote host info pointer */
  	struct sockaddr_in peer_in;
        u_long addr;
  	char *port_ascii;
	char *server_name;
	int port_num = LISS_PORT ;
  	int Ls;
        int done = 0, tried = 0;


    server_name = strdup( inport );
    if( port_ascii = strchr( server_name, ':'))  {
	*port_ascii++ = 0;
	port_num = atoi( port_ascii );
    }
    while( !done )  {

        /* clear out structures */
 
  	memset((char *)&peer_in, 0, sizeof(struct sockaddr_in));
  
	/* name of server is in a "logical " form   */

       hp = gethostbyname(server_name);
       if( hp == 0 )
          die( 0, "%s unknown host\n", server_name);
   
	/* create a socket  */
 
       if( (Ls = socket(AF_INET, SOCK_STREAM, 0)) < 0 )  {
    	    die ( 1, "Can't open stream socket\n" ) ; 
       }
       
       memcpy((char *)&peer_in.sin_addr, hp->h_addr , hp->h_length);

       peer_in.sin_family = AF_INET; 
       peer_in.sin_port = htons( port_num );


/* Change size of the input/output buffer. 
   Size by Default is 8K which is not enough.   */


      b_size = 1024;

      if( setsockopt( Ls, SOL_SOCKET, SO_SNDBUF, (char *)&b_size, sizeof(int)) != 0)  {
    	   die( 1, "Unable to set size of send buffer.\n");
      }
      if( setsockopt(Ls, SOL_SOCKET, SO_RCVBUF, (char *)&b_size, sizeof(int)) != 0)  {
     	     die( 1, "Unable to set size of send buffer.\n");
       }


      if ( connect (Ls, (struct sockaddr *) & peer_in, sizeof(struct sockaddr_in)) == -1) {
           if( !tried )  {
	      tried = 1;
	      complain( 1, "waiting for connection \n");
              sleep(1);
	      complain( 1, "can't connect %s\n", inport );
	   }
	   close(Ls);
        } else done = 1;	
    } 

    if ( fcntl(Ls, F_SETFL, O_NONBLOCK) == -1 ) { 
        die ( 1, "Can't set non-blocking on accept socket\n" ) ; 
    }

     return Ls;

}
 
