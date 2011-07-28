/*********************************************************************
 *   
 *  send2orb.c
 *
 *  Send  data packets to the Ring Buffer server.
 *
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 ********************************************************************/
#include "ipd2.h"

int send2orb( orb, orbname, packet, srcname, epoch, psize, err )
int *orb;
char *orbname;
char *packet;
char *srcname;
double epoch;
int psize;
int err;

 {

    if( *orb >= 0 )  {
      if( orbput( *orb, srcname, epoch, packet, psize ) < 0) { 
	 elog_complain( 0, "send2orb(): Can't send a packet to orbserver.\n"); 
	 err++; 
	 if(err >= 30)  { 
	    err = 0; 
	    if( orbclose( *orb) || ( *orb = orbopen( orbname, "w" )) < 0 )  { 
	        *orb = -1;
	        elog_complain(0,"send2orb(): Can't re-open ORB!\n"); 
            }
         }
      } else err = 0; 
    }  else { 
        err = 0;
        if( Log )
           elog_complain( 0, "send2orb(): Can't send a packet %s-%lf to orbserver.\n", 
	                                                  &srcname[0], epoch );
        if( ( *orb = orbopen( orbname, "w" )) < 0 ) 
            if( Log ) 
	       elog_complain(0,"send2orb(): Can't re-open ORB!\n");
    } 
    return err;

 }

