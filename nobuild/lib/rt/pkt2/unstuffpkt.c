/******************************************************************
 *
 *  unstuffpkt.c
 *
 *  Recognize packet type; parse raw data packet; fill Packet structure. 
 *
 *
 ********************************************************************/
#include "pkt2.h"

extern Arr *RawPkts;

int unstuffpkt( double time, char *srcid, char *packet, Packet **Pkt )

{
  uchar_t *hdr;
  struct DataPar dphdr;
  char hdrbuf[512];
  Raw *raw;
  short hdrsize, pktsize;
  ushort_t hdrtype, pkttype;
  int code , ret ;
  char key[64];


  if( !strncmp( srcid, "/", 1) &&  strncmp( srcid, "/dcdas", 6) )
    return 2;

  hdr = (uchar_t *) &hdrbuf[0];
  memcpy( hdr, packet, sizeof( struct PreHdr) );

  memcpy( &hdrsize, hdr, sizeof( short ) );
  hdr += sizeof( short );
  hdrsize = ntohs( hdrsize);
  dphdr.hdrsize = hdrsize;

  memcpy( &pktsize, hdr, sizeof( short ) );
  hdr += sizeof( short );
  pktsize =  ntohs( pktsize);
  dphdr.pktsize = pktsize;

  memcpy( &hdrtype, hdr, sizeof( short ) );
  hdr += sizeof( short );
  hdrtype = ( ushort_t ) ntohs( hdrtype);
  dphdr.hdrtype = hdrtype;

  memcpy( &pkttype, hdr, sizeof( short ) );
  pkttype = ( ushort_t ) ntohs( pkttype);
  dphdr.pkttype = pkttype;
  
  if( hdrtype == -1 )  {
     elog_complain( 0, "Can't get packet/header type.\n" );
     return 0;
  }
  sprintf( key, "%d\0", pkttype );
  if( RawPkts == NULL ) init_RawPkts();
  raw = ( Raw *) getarr( RawPkts, (char *) &key[0] );
  if( raw == NULL ) { 
      elog_complain( 0, " unstuffpkt(): Can't get RawPkts info for %s\n", key );
      return 0;
  }
 

  return read_raw( time, srcid, (uchar_t *)packet, Pkt, (void *) &dphdr,raw->read);


}

