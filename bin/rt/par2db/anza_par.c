/* @(#)anza_par.c	1.1 12/26/96  */
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

int ANpar = MAXNUMPAR;
Arr *AParam = NULL;

int anza_par( packet, Pkt )
char *packet;
struct Packet **Pkt;

{

    struct PktChannel *achan;
    struct IPHdr *hdr;
    struct Pval *par;
    char key[64];
    char *staname;
    long ttag;
    int i, nsta, sta;
    int want, off, val, ch;
    
    want = 1;

    hdr = ( struct IPHdr *) packet;
    
    if( *Pkt == 0 ) *Pkt = newpkt();
    (*Pkt)->pkttype = ntohl (hdr->prehdr.pkttype);
    (*Pkt)->hdrtype = ntohl (hdr->prehdr.hdrtype);
     

    for( i = 0, ch = 0; i < MAXNUMPAR; i++ )  {
       if( PAR_DAS[i] ) nsta = NDAS;
       else  nsta = 1;

       for( sta = 1; sta < nsta + 1; sta++, ch++ )  {
          achan = (PktChannel *) gettbl((*Pkt)->chan, ch) ;
          if ( achan == 0 ) {
              allot ( PktChannel *, achan, 1 ) ;
          }
          if( nsta > 1 )  {
	     if( Staid[sta]  == NULL )
	        die( 0, "Can't get station name for staid=%d.\n", sta);
             strcpy(achan->sta, (char *) &Staid[sta][0]);
          }  else strcpy( achan->sta , comname);
         strcpy( achan->chan, FILE_NAME[i] ) ;
         strcpy( achan->net, hdr->net);
         achan->samprate = 0.2;                                
         achan->calib = 0;                     
         achan->datatype = trINT;
         achan->nsamp = 1;                   
         achan->nbytes = sizeof(int);
         achan->time = hdr->epoch; 

	complain( 0, "PKT= %lf\n", hdr->epoch );

        if( !strncmp( FILE_NAME[i], "CCODE", 5) ||
             !strncmp( FILE_NAME[i], "HAZARD", 6 ) )  {
             off = PAR_OFF[i]+sta*PAR_BYTE[i] + hdr->prehdr.hdrsiz;
             val = packet[off];
             aparameter[ch] = val; 
         }  else if( !strncmp( FILE_NAME[i], "LLOCK", 5 ) ) {
              off = PAR_OFF[i] + hdr->prehdr.hdrsiz;
              memcpy( (char *) &ttag, &packet[off], 4 );
              aparameter[ch] = ttag;
         }  else {
            off = PAR_OFF[i] + sta*PAR_BYTE[i] + hdr->prehdr.hdrsiz;
            aparameter[ch] = packet[off]*256 + packet[off+1];
         }
         achan->data = &aparameter[ch];           
         settbl((*Pkt)->chan, ch, achan ) ;
     } 
  }

  (*Pkt)->nchannels = ch;
  return 1; 
}

int anza_init( sta )
char *sta;

{

  int i;

  if( AParam == NULL ) 
      AParam = newarr( 0 );
  if( DiagPar == NULL ) 
        allot( Pval *, DiagPar, MAXNUMPAR );
  else 
        reallot ( Pval *, DiagPar, ANpar );

  if( DiagPar == NULL ) {
      complain( 1, "par malloc error\n");
      return 0;
  }
 
  for ( i = 0; i < MAXNUMPAR; i++ )  {
     DiagPar[i].val = -1;
     DiagPar[i].time = -1.0;
     sprintf( DiagPar[i].name, "%s_%s\0", sta, FILE_NAME[i] );
     if( Ascii )  
        if( ( DiagPar[i].fp = fopen( DiagPar[i].name, "w" ) ) == NULL )  {
           complain( 1, "Can't open %s file\n", DiagPar[i].name );
           return 0;
        }
     setarr( AParam, DiagPar[i].name, (char *) &DiagPar[i]); 

  } 
  return 1;

}


