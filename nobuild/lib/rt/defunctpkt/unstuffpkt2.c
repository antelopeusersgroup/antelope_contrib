
#include <sys/types.h>
#include <netinet/in.h>

#include "defunctpkt.h"
  
double htond();
double ntohd();

#define UCSD_DOFF 20
#define UCSD_TOFF 8
#define PSCL_DOFF 24
#define PSCL_TOFF 6

int unstuffpkt2( time, srcid, packet, Pkt)
double time;
char *srcid;
char *packet;
Packet **Pkt;

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
    case CBBHS:
    case CBBLS:
    case CEHS:
    case CELS:
	  retcode = un_CEDP( packet, Pkt );
	  break; 

    case UBBHS:
    case UBBLS:
    case UEHS:
    case UELS:
	  retcode = un_EDP( packet, Pkt );
	  break; 

    case SDESP:
	  retcode =  un_ESP( packet );
	  break ;

    case UCSDHS:
    case UCSDLS:
	  retcode = ucsdDP( packet, Pkt );
	  break; 

    case UCSDIP:
	  retcode =  ucsdSP( packet );
	  break ;

    case ORBDBUG:
	retcode = (un_dbug ( packet, Pkt ) == 0)  ?  1 : 0 ;
	break ;

    case PSCLIP:
        retcode = unIPSC( (unsigned char *) packet );
        break;

    case CPHS:
    case CPLS:
	  retcode =   psclCDT( packet, Pkt );
	  break ;

    case PSCLHS:
    case PSCLLS:
	  retcode =   psclDT( packet, Pkt );
	  break ;

    case IWTB:
	  retcode =   unstuff_iw_tracebuf( time, srcid, packet, Pkt );
	  break ;

    default:
	elog_complain( 0, "Unknown packet type - %d\n", hdr->pkttype);
	retcode = 0 ; 
	break ;
  } 

    return retcode ; 
}

/* $Id$ */
