/*********************************************************************
 *
 *  read_in_port.c 
 *
 *  Read data from input ports add specified header; pass packet to ORB
 *
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 ********************************************************************/
#include "liss2orb.h"

static int fdready( fp, timeout )
int fp;
int timeout;


 {

     struct pollfd fds[1] ;
     unsigned long nfds = 1 ;
     int retval ;  
	      
     fds[0].fd = fp ;
     fds[0].events = POLLIN | POLLRDNORM | POLLPRI | POLLRDBAND | POLLERR ;
		      
     switch ( poll( fds, nfds, timeout ) ) {
         case 1:
            if ( fds[0].revents & POLLIN || 
	         fds[0].revents & POLLRDNORM ||
		 fds[0].revents & POLLRDBAND || 
		 fds[0].revents & POLLPRI ) {
                 retval = 1 ;
            } else
                 retval = -1 ;
            break ;
         case 0:
	    retval = 0;
	    break;
	 default:
	    retval = -1;
	    break;
    }


    return retval;
 }

void *read_server( int ifp, int orb, int timeout, char *match )
{
     
    int             len;
    int 	   poll_err = 0;
    double epoch;
    uchar_t *buffer;
    char srcname[64];
    int total = 0;
    int sendpkt, off = 0, psize=0;
    uchar_t packet[IBUF_SIZE]; 
 

    timeout *= 1000;

    for (;;) {
	switch  ( fdready( ifp, timeout )  ) {
   	   case -1:
	     die (1, " socket error from poll\n");
	   case 0:
	     die( 1, "poll timeout \n" );
	   case 1:
            poll_err = 0;
	    len = recv ( ifp, (char *) &packet[total], PSize, 0 );
	    if (len == 0) 
	  	   die (0, "end of file on input socket\n");
	    else if (len <= 0) 
	  	   die (1, "network error\n");
	    else {
	   
                total += len;
                if ( total >= PSize ) { 
                   off = 0;
                   while ( total >= PSize ) {
                       buffer = &packet[off];
                       if( (psize = 
                           StuffLiss( &buffer, &srcname[0], &epoch, PSize, match )) > 0 )  
			   sendpkt = 1;
                       else sendpkt = 0;
		       if( sendpkt )  {
                              if (orbput(orb,&srcname[0],epoch,(char *)buffer,psize) < 0 ) { 
                                  orbclose( orb );
                                  die (1, "Can't send a packet to orbserver.\n");
                              }
                              if( Log )  {
                                  fprintf(stderr, "%s %lf\n", srcname, epoch );
                                  fflush(stderr);
                              }
                       } 
                       total -= PSize;
                       off += PSize;
                    }
		    if( total > 0 ) 
                       memcpy ( packet, &packet[off], total );
                }
            }
	    break;
         }
    }

}
