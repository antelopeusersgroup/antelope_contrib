/******************************************************************
 *
 *  bba_par.c
 *
 ********************************************************************/
#include "header.h"
extern int DINTV;
extern int Log;

int bba_par( packet, pkttime, srcname,  Pkt )
char *packet;
double pkttime;
char *srcname;
struct Packet **Pkt;

{

    int doff, i;
    struct PktChannel *achan;
    struct BBAHdr *hdr;
    int off, val, ch;
    short sval;
    long ttag;
    char net[64], sta[64], key[64];
    struct Pval *par;
    

    hdr = ( struct BBAHdr *) packet;
    
    doff = hdr->prehdr.hdrsiz ; 

    (*Pkt)->pkttype = ntohl (hdr->prehdr.pkttype);
    (*Pkt)->hdrtype = ntohl (hdr->prehdr.hdrtype);

    parse_srcname( srcname, &net[0], &sta[0], 0, 0 );

    for( i = 0, ch = 0; i < MAXNUMPAR_BBA2; i++, ch++ )  {
      achan = (PktChannel *) gettbl((*Pkt)->chan, ch) ;
      if ( achan == 0 ) {
          allot ( PktChannel *, achan, 1 ) ;
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
          bparameter[ch] = val; 
          if( Log )  {
             fprintf( stderr, "%d  ", val );
             fflush(stderr);
          }
      }  else if( !strncmp( FILE_NAME_BBA2[ch], "TTAG", 4 ) ||
           !strncmp( FILE_NAME_BBA2[ch], "XMTTAG", 6 ) )  {
           off = PAR_OFF_BBA2[ch] + doff;
           memcpy( (char *) &ttag, &packet[off], 4 );
           bparameter[ch] = ttag;
      } else if( !strncmp( FILE_NAME_BBA2[ch], "COORD", 5) )
         ;
      else {
         off = PAR_OFF_BBA2[ch] + doff;

           memcpy( (char *) &sval, &packet[off], 2 );
           bparameter[ch] = sval;
      }
      achan->data = &bparameter[ch];           
      settbl((*Pkt)->chan, ch, achan ) ;
              
      
    }

    (*Pkt)->nchannels = ch;
    return 1; 
}

