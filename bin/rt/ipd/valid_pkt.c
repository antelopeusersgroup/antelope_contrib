/*********************************************************************
 *
 *  valid_pkt.c
 *  
 *  test that input packet is legal
 *    
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 ********************************************************************/
#include "ipd.h"

extern struct Prm Par;
extern char *pffile;

int valid_pkt( data, srcname, epoch, psize, length, err, hdrtype )
unsigned char **data;
char *srcname;
double *epoch;
int *psize;
int length;
int err;
int hdrtype;

{  
   struct PktPar packet;
   int  ptype;

    if( (ptype = whatis_pkttype( *data )) == -1) return 1;
    
    *epoch = Par.time;
    if( hdrtype > 0 ) Par.hdrtype =  hdrtype;

    if( length > 0 ) Par.packet.size = length; 
    if( !(*psize = hdr2packet( (char **) data, Par.hdrtype,  srcname )) )  {
	newpf( pffile );
        if( !(*psize = hdr2packet( (char **) data, Par.hdrtype,  srcname )) )  {
	   complain( 0, "valid_pkt(): Not a valid packet. Wrong Header?\n");
  	   err = 1;
	} else err = 0;
    } else err = 0;

   if( Log ) 
       complain( 0, "SRCNAME: %s-%lf \n", srcname, *epoch );

    return err; 
 
}
 
