/******************************************************************
 *
 *  anza_par.c
 *
 *  get diagnostic parameters from a raw data packet; 
 *  fill Packet structure. 
 *
 *
 ********************************************************************/
#include "header.h"

extern int DINTV;
int
anza_par( char *packet,
          double pkttime,
          char *srcname,
	  struct Packet **Pkt
	  )

{

    struct PreHdr *hdr;
    struct PktChannel *achan;
    char net[64], sta[8];
    int i, off, ch;
    ushort_t *sptr, val;

    hdr = ( struct PreHdr *) packet;
    
    (*Pkt)->pkttype = ntohl (hdr->pkttype);
    (*Pkt)->hdrtype = ntohl (hdr->hdrtype);

    parse_srcname( srcname, &net[0], &sta[0], 0, 0 );
    memset( (char *) parbuf, 0, 512*sizeof(int));

    for( i = 0, ch = 0; i < ANZA_DAS; i++, ch++ )  {

        achan = (PktChannel *) gettbl((*Pkt)->chan, ch) ;
        if ( achan == 0 ) {
              allot ( PktChannel *, achan, 1 ) ;
        }
        strcpy( achan->chan, ANZA_DAS_NAME[i] ) ;
        strcpy( achan->net, net);
        strcpy( achan->sta, sta);
        achan->samprate = 1.0/DINTV;                                
        achan->calib = 0;                     
        achan->datatype = trINT;
        achan->nsamp = 1;                   
        achan->nbytes = sizeof(int);
        achan->time = pkttime;   

        off = hdr->hdrsiz + ANZA_DAS_OFF[i];
        if( strncmp(ANZA_DAS_NAME[i], "BUFDEL", strlen("BUFDEL")) == 0 )  
	    parbuf[ch] = packet[off];
	else  {
	    sptr = (ushort_t *) ( packet + off );
	    val = *sptr;
	    parbuf[ch] = val ;
	   /*
	    fprintf( stderr, "%lf %s_%s %d\n", achan->time, achan->sta, achan->chan, val ); 
	    if( val < 1100  )   hexdump( stderr, packet + hdr->hdrsiz, 62 ); 
	    fflush(stderr);
	    */
	}
        achan->data = &parbuf[ch];           

        settbl((*Pkt)->chan, ch, achan ) ;
     } 
  
     (*Pkt)->nchannels = ch;

  return 1; 
}



