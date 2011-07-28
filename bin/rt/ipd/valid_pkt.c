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
extern int chrate;
static double check_time = 0.0;

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
   double timnow;
   int  ptype;

    if( (ptype = whatis_pkttype( *data )) == -1) return 1;
    
    *epoch = Par.time;
    if( hdrtype > 0 ) Par.hdrtype =  hdrtype;

    if( length > 0 ) Par.packet.size = length; 
    if( !(*psize = hdr2packet( (char **) data, Par.hdrtype,  srcname )) )  {
	timnow = now();
	if( timnow - check_time > chrate )  {
	    newpf( pffile );
            if( !(*psize = hdr2packet( (char **) data, Par.hdrtype,  srcname )) )  {
	        elog_complain( 0, "valid_pkt(): Not a valid packet. Wrong Header?\n");
  	        err = 1;
	    } else err = 0;
	    check_time = timnow;
	} else {
	    elog_complain( 0, "valid_pkt(): Not a valid packet. Wrong Header?\n");
  	    err = 1;
        }
    } else err = 0;

   if( Log ) 
       elog_complain( 0, "SRCNAME: %s-%lf \n", srcname, *epoch );

    return err; 
 
}
 
