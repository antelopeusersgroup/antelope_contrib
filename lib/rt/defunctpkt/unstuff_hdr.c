/* @(#)unstuff_hdr.c	1.4 04/13/97 */ 
/**************************************************************************
 *
 *    unstuff packet headers
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 *
 ***********************************************************************/
#include "defunctpkt.h"

extern Arr *RawPkts;

/* Header type is BBA  */

int unstuff_BBAHdr(
    uchar_t *packet, 
    Packet **Pkt,
    void *par )
{

    int i, ch=0;
    struct DataPar *dhdr;
    BBAHdr bbahdr;
    PktChannel *achan;
    float calib;
    float samprate;
    short  chlen, nsamp, nchan, doff; 
    short hdrsize, pktsize;
    ushort_t hdrtype, pkttype;
    short datatype;
    char  *name, chnames[64];
    uchar_t *hdr, hdrbuf[512];

    dhdr = ( struct DataPar *) par;

    memcpy( &hdrbuf[0], packet, 512);
    hdr = &hdrbuf[0];

    memcpy( &hdrsize, hdr, sizeof(short ));
    hdr += sizeof(short );
    hdrsize =  ntohs(  hdrsize );
    bbahdr.prehdr.hdrsiz = hdrsize;
    memcpy( &pktsize, hdr, sizeof( short ));
    hdr += sizeof( short );
    pktsize =   ntohs(  pktsize );
    bbahdr.prehdr.pktsiz = pktsize;
    memcpy( &hdrtype, hdr, sizeof(short));
    hdr += sizeof(short);
    hdrtype =  (ushort_t) ntohs( hdrtype );
    bbahdr.prehdr.hdrtype = hdrtype;
    memcpy( &pkttype, hdr, sizeof(short));
    hdr += sizeof(short);
    pkttype =  (ushort_t)ntohs( pkttype );
    bbahdr.prehdr.pkttype = pkttype;
    
    memcpy( &calib, hdr, sizeof(float));
    hdr += sizeof(float);
    ntohfp( &calib, &calib );
    bbahdr.calib = calib;
    memcpy( &samprate, hdr, sizeof(float));
    hdr += sizeof(float);
    ntohfp( &samprate, &samprate );
    bbahdr.samprate = samprate;
    
    memcpy( &datatype, hdr, sizeof(short));
    hdr += sizeof(short);
    datatype = ntohs( datatype );
    bbahdr.datatype = datatype;
    memcpy( &nsamp, hdr, sizeof(short));
    hdr += sizeof(short);
    nsamp =  ntohs( nsamp );
    bbahdr.nsamp = nsamp;
    memcpy( &nchan, hdr, sizeof(short));
    hdr += sizeof(short);
    nchan =  ntohs( nchan );
    bbahdr.nchan = nchan;
    memcpy( &doff, hdr, sizeof(short));
    hdr += sizeof(short);
    doff =  ntohs( doff );
    bbahdr.doff = doff;
    memcpy( &chlen, hdr, sizeof(short));
    hdr += sizeof(short);
    chlen = ntohs( chlen );
    bbahdr.chanlen = chlen;
    memcpy( (char *) &bbahdr.channels[0], hdr, chlen );
    bbahdr.channels[chlen] = '\0';
    strcpy( chnames,&bbahdr.channels );

    dhdr->pktsize = (int) bbahdr.prehdr.pktsiz;
    dhdr->datatype = (int) bbahdr.datatype;
    dhdr->doff = (int) bbahdr.doff;
    dhdr->nsamp = (int) bbahdr.nsamp;

    if( *Pkt == 0 ) *Pkt = newpkt();
    
    (*Pkt)->pkttype = (int) bbahdr.prehdr.pkttype;
    (*Pkt)->hdrtype = (int) bbahdr.prehdr.hdrtype;
    (*Pkt)->nchannels = (int) bbahdr.nchan;
   
    name = strtok( bbahdr.channels, "_" ) ;
    
    while ( name != 0 ) {
      if( ch > nchan )  {
        elog_complain( 0,"There are more chan names ( %s ) than channels (%d)\n", chnames, nchan );
	return 0;
      }
      achan = (PktChannel *) gettbl((*Pkt)->chan, ch) ;
      if ( achan == 0 ) {
          allot ( PktChannel *, achan, 1 ) ;
          achan->data = NULL;
          strcpy (achan->segtype, "V");
      }
      strcpy( achan->chan, name ) ;
      achan->samprate = (double) bbahdr.samprate;
      achan->calib = (double) bbahdr.calib; 
      achan->nsamp = (int) bbahdr.nsamp;
      settbl((*Pkt)->chan, ch, (char *) achan ) ;
      name = strtok(0, "_" ) ;
      ch++ ;
   }  

    return 1;

}


/* Unstuff DPH; fill Packet structure   */

int unstuff_DPhdr( 
    unsigned char *packet,
    Packet **Pkt,
    void *par )

{

    float samprate;
    double calib;
    PktChannel *achan;
    struct DataPar *dphdr;
    DPHdr hdr;
    char *name , chnames[64];
    int nchan;
    int ch;


    dphdr = ( struct DataPar *) par;

    memcpy( (char *) &hdr, packet, sizeof( DPHdr ) );
     
    if( *Pkt == 0 ) *Pkt = newpkt();
    (*Pkt)->pkttype = (int) ntohs ( hdr.prehdr.pkttype);
    (*Pkt)->hdrtype = (int) ntohs ( hdr.prehdr.hdrtype);

    nchan = ntohl( hdr.nchan);
    dphdr->doff = ntohl (hdr.doff); 
    dphdr->pktsize = ntohs (hdr.prehdr.pktsiz); 
    dphdr->datatype = ntohl (hdr.datatype); 
    dphdr->nsamp = ntohl (hdr.nsamp); 

    strcpy( chnames, hdr.channels);

    ch = 0;
    name = strtok( hdr.channels, "_" ) ;
    while ( name != 0 ) {
      if( ch > nchan )  {
           elog_complain( 0,"There are more chan names ( %s ) than channels (%d)\n", chnames, nchan );
           return 0;
      }

      achan = (PktChannel *) gettbl((*Pkt)->chan, ch) ;
      if ( achan == 0 ) {
          allot ( PktChannel *, achan, 1 ) ;
          achan->data = NULL;
          strcpy (achan->segtype, "V");
      }
      strcpy( achan->chan, name ) ;
      ntohfp (&hdr.samprate, &samprate); 
      achan->samprate = (double) samprate;
      ntoh8p (&hdr.calib, &calib); 
      achan->calib = calib;
      achan->nsamp = ntohl ( hdr.nsamp);
      settbl((*Pkt)->chan, ch, achan ) ;
      name = strtok(0, "_" ) ;
      ch++ ;
   }
   (*Pkt)->nchannels = ch;

   return 1; 
}


/* UNSTUFF IPH ; Fill neccessary structure or extract values  */

int unstuff_IPhdr( uchar_t *packet )
 {

    Raw *raw;
    char hdrbuf[512];
    char key[64];
    short hdrsize, pktsize;
    ushort_t hdrtype, pkttype;
    char *phdr; 

    phdr = &hdrbuf[0];
    memcpy( phdr, packet, sizeof(IPHdr));
    memcpy( &hdrsize, phdr, sizeof(short));
    phdr += sizeof(short);
    hdrsize =  ntohs(  hdrsize );
    memcpy( &pktsize, phdr, sizeof(short));
    phdr += sizeof(short);
    pktsize =  ntohs(  pktsize );
    memcpy( &hdrtype, phdr, sizeof(short));
    phdr += sizeof(short);
    hdrtype =  (ushort_t) ntohs(  hdrtype );
    memcpy( &pkttype, phdr, sizeof(short));
    phdr += sizeof(short);
    pkttype =  (ushort_t) ntohs(  pkttype );


    sprintf( key, "%d\0", pkttype );
    if( RawPkts == NULL ) init_RawPkts();
    raw = ( Raw *) getarr( RawPkts, (char *) &key[0] );
    if( raw == NULL )  { 
        elog_complain( 0, " unstuff_IPhdr(): Can't get RawPkts info\n");
        return 0;
    }
    
    return 2;
  
}


