/*********************************************************************
 *
 *  send packets to ORB.
 *
 *
 ********************************************************************/
#include "b3s2.h"

extern struct Prm Par;

int send2orb( int orb, uchar_t *data )
{
     
   uchar_t buffer[1024], *dptr;   
   double epoch;
   char srcname[64];
   int  ptype, psize = 0;


    
    if( (ptype = whatis_pkttype( data )) == -1) return 1;
    
    memcpy( &buffer[0], data, PLEN );
    dptr = &buffer[0];
    
    if( !(psize = hdr2packet( &dptr, Par.hdrtype,  &srcname[0] )) )  {
	elog_complain( 0, "valid_pkt(): Not a valid packet. Wrong Header?\n");
	return 0;
    } 

    if( orbput( orb, &srcname[0], Par.time, dptr, psize ) < 0) {
	 elog_complain( 0, "send2orb(): Can't send a packet to orbserver.\n"); 
	 return 0;
    }
      
    if( Log ) 
       elog_complain( 0, "SRCNAME: %s-%lf \n", srcname, Par.time );
                 

    return 1;

}

