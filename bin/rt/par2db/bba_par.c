/******************************************************************
 *
 *  bba_par.c
 *
 ********************************************************************/
#include "header.h"

extern int DINTV;
extern int Log;

int bba_par( packet, pkttime, srcname,  Pkt )
uchar_t *packet;
double pkttime;
char *srcname;
struct Packet **Pkt;

{

    struct PktChannel *achan;
    struct BBAHdr *hdr;
    int off, val, ch;
    int doff, i;
    short sval;
    long lval;
    char net[64], sta[64], key[64];

    hdr = ( struct BBAHdr *) packet;
    
    doff = hdr->prehdr.hdrsiz ; 

    (*Pkt)->pkttype = ntohl (hdr->prehdr.pkttype);
    (*Pkt)->hdrtype = ntohl (hdr->prehdr.hdrtype);

    parse_srcname( srcname, &net[0], &sta[0], 0, 0 );
    
    if( Log )  {
       hexdump( stderr, packet+doff, hdr->prehdr.pktsiz);
       fflush(stderr);
    }
    for( i = 0, ch = 0; i < MAXNUMPAR_BBA2; i++, ch++ )  {
      achan = (PktChannel *) gettbl((*Pkt)->chan, ch) ;
      if ( achan == 0 ) {
          allot ( PktChannel *, achan, 1 ) ;
          achan->data = (void *) malloc( sizeof(int)  );
      }
      strcpy( achan->chan, FILE_NAME_BBA2[ch] ) ;
      strcpy( achan->net, net);
      achan->samprate = 1.0/DINTV;                                
      achan->calib = 0;                     
      achan->datatype = trINT;
      achan->nsamp = 1;                   
      achan->nbytes = sizeof(int);
      strcpy( achan->sta, sta);
      achan->time = pkttime; 
      if( !strncmp( FILE_NAME_BBA2[ch], "BUFDEL", 6 ) ||
          !strncmp( FILE_NAME_BBA2[ch], "LLOCK", 5 ) )  {
          off = PAR_OFF_BBA2[ch]+doff;
          val = packet[off]; 
      }  else if( !strncmp( FILE_NAME_BBA2[ch], "TTAG", 4 ) ||
           !strncmp( FILE_NAME_BBA2[ch], "XMTTAG", 6 ) )  {
           off = PAR_OFF_BBA2[ch] + doff;
           memcpy( (char *) &lval, (char *) &packet[off], 4 );
           val = lval;
      } else if( !strncmp( FILE_NAME_BBA2[ch], "COORD", 5) )
         ;
      else {
         
	   off = PAR_OFF_BBA2[ch] + doff;
           memcpy( (char *) &sval, (char *) &packet[off], 2 );
           val = sval;
      }
      memcpy(achan->data, (char *) &val, sizeof(int) );

      settbl((*Pkt)->chan, ch, achan ) ;
              
      
    }

    (*Pkt)->nchannels = ch;
    return 1; 
}

