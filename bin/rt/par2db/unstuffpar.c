/******************************************************************
 *
 *  unstuffpar.c 
 *
 ********************************************************************/
#include "pkt.h"

extern int DCSP;
extern int GPS_COOR;

int unstuffpar( char *packet,
	double pkttime,
	struct Packet **Pkt,
	char *srcname)
{
  struct PreHdr *hdr;
  int retcode ;
  int pkttype ;

  
  hdr = ( struct PreHdr *) packet;

  pkttype = ntohl (hdr->pkttype) ;

  if( pkttype == -1 )  {
    complain( 0, "Can't get packet type.\n" );
    return 0;
  }
 
  switch (pkttype) {
    case BSP:
	  if( !DCSP ) return 2;
	  retcode =  dc_par( packet, Pkt );
	  break ;

    case CBBHS:
    case CBBLS:
          if( GPS_COOR ) return gps_coord( packet, pkttime, srcname, Pkt );
	  retcode =  bba2_par( packet, pkttime, srcname, Pkt );
	  break ;
    case CAHS:
    case CALS:
	  retcode =  anza_par( packet, Pkt );
	  break ;

    default:
	complain( 0, "Can't get parameters for pkttype - %d\n", hdr->pkttype);
	retcode = 0 ; 
	break ;
  } 

    return retcode ; 
}

