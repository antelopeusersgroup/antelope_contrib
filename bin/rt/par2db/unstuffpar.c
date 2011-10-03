/******************************************************************
 *
 *  unstuffpar.c 
 *
 ********************************************************************/
#include "defunctpkt.h"

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
    elog_complain( 0, "Can't get packet type.\n" );
    return 0;
  }
 
  switch (pkttype) {
    case ASP:
	  retcode =  adc_par( packet, pkttime, srcname, Pkt );
	  break ;

    case BSP:
	  retcode =  dc_par( packet, Pkt );
	  break ;

    case CAHS:
    case CALS:
	  retcode =  anza_par( packet, pkttime, srcname, Pkt );
	  break ;
    case CBBHS:
    case CBBLS:
    case CBB1S:
          if( GPS_COOR ) return gps_coord( packet, pkttime, srcname, Pkt );
	  retcode =  bba_par( packet, pkttime, srcname, Pkt );
	  break ;
    default:
	retcode = 0 ; 
	break ;
  } 

    return retcode ; 
}

