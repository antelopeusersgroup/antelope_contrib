/******************************************************************
 *
 *  bba2_par.c
 *
 ********************************************************************/
#include "header.h"
int BNpar = MAXNUMPAR_BBA2;
Arr *BParam = NULL;
extern int DINTV;
extern FILE *F1;

int bba2_par( packet, pkttime, srcname,  Pkt )
char *packet;
double pkttime;
char *srcname;
struct Packet **Pkt;

{

    int doff, i;
    struct PktChannel *chan;
    struct PktChannel *achan;
    struct BBAHdr *hdr;
    int want, off, val, ch;
    short sval;
    long ttag;
    char net[64], sta[64], key[64];
    struct Pval *par;
    
    want = 1;

    hdr = ( struct BBAHdr *) packet;
    
    doff = hdr->prehdr.hdrsiz ; 

    if( *Pkt == 0 ) *Pkt = newpkt();
    (*Pkt)->pkttype = ntohl (hdr->prehdr.pkttype);
    (*Pkt)->hdrtype = ntohl (hdr->prehdr.hdrtype);

    parse_srcname( srcname, &net[0], &sta[0], 0, 0 );

    /*hexdump( F1, packet + doff, 62 );  
    
    fflush( F1 );    */ 
 
    for( i = 0, ch = 0; i < MAXNUMPAR_BBA2; i++, ch++ )  {
      achan = (PktChannel *) gettbl((*Pkt)->chan, ch) ;
      if ( achan == 0 ) {
          allot ( PktChannel *, achan, 1 ) ;
      }
      if( Ascii || Intv )  {
          sprintf( key, "%s_%s\0", sta, FILE_NAME_BBA2[ch] );
          if( BParam == NULL ) BParam = newarr( 0 );
          par = ( struct Pval *) getarr( BParam, key );
          if( par == NULL  )  {
              BNpar += MAXNUMPAR_BBA2;
              if( !init_bba2( sta ) )  {
                Ascii = 0;  Intv = 0;
              }  else par = ( struct Pval *) getarr( BParam, key );
          }
          want = 0;
          if( ( pkttime - par->time) >=  Intv )  {
              want = 1;
              par->time = pkttime;
              setarr( BParam, key, (char *) par );
          }
      }
      if( want )  {
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
             !strncmp( FILE_NAME_BBA2[ch], "RTXNUM", 6 ) ||
             !strncmp( FILE_NAME_BBA2[ch], "SRATE", 5 ) )  {
             off = PAR_OFF_BBA2[ch]+hdr->prehdr.hdrsiz;
             val = packet[off];
             bparameter[ch] = val; 
        } else if( !strncmp( FILE_NAME_BBA2[ch], "GAIN", 4 ) )  {
             off = PAR_OFF_BBA2[ch]+hdr->prehdr.hdrsiz;
             val = atoi( &packet[off] );
             bparameter[ch] = val; 
         }  else if( !strncmp( FILE_NAME_BBA2[ch], "TTAG", 4 ) ||
              !strncmp( FILE_NAME_BBA2[ch], "XMTTAG", 6 ) )  {
              off = PAR_OFF_BBA2[ch] + hdr->prehdr.hdrsiz;
              memcpy( (char *) &ttag, &packet[off], 4 );
              bparameter[ch] = ttag;
         } else if( !strncmp( FILE_NAME_BBA2[ch], "COORD", 5) )
            ;
         else {
            off = PAR_OFF_BBA2[ch] + hdr->prehdr.hdrsiz;
            /*bparameter[ch] = packet[off]*256 + packet[off+1]; */

              memcpy( (char *) &sval, &packet[off], 2 );
              bparameter[ch] = sval;
         }
         achan->data = &bparameter[ch];           
         settbl((*Pkt)->chan, ch, achan ) ;
              
         
         if( Ascii )  {
            par->val = bparameter[ch];
            setarr( BParam, key, (char *) par );
            fprintf( par->fp, "%lf %d\n", achan->time, bparameter[ch] );
            fflush( par->fp );
         }

       } else return 2; 
    }

    (*Pkt)->nchannels = ch;
    return 1; 
}

int init_bba2( sta )
char *sta;

{

  int i;

  if( BParam == NULL ) 
      BParam = newarr( 0 );
  if( DiagPar == NULL ) 
        allot( Pval *, DiagPar, MAXNUMPAR_BBA2 );
  else 
        reallot ( Pval *, DiagPar, BNpar );

  if( DiagPar == NULL ) {
      complain( 1, "par malloc error\n");
      return 0;
  }
 
  for ( i = 0; i < MAXNUMPAR_BBA2; i++ )  {
     DiagPar[i].val = -1;
     DiagPar[i].time = -1.0;
     sprintf( DiagPar[i].name, "%s_%s\0", sta, FILE_NAME_BBA2[i] );
     if( Ascii )  
        if( ( DiagPar[i].fp = fopen( DiagPar[i].name, "w" ) ) == NULL )  {
           complain( 1, "Can't open %s file\n", DiagPar[i].name );
           return 0;
        }
     setarr( BParam, DiagPar[i].name, (char *) &DiagPar[i]); 

  } 
  return 1;

}


void close_files() 

{

   struct Pval *val;
   int i;

   for ( i = 0; i < MAXNUMPAR_BBA2; i++ )  {
     val = ( struct Pval *) getarr( BParam, FILE_NAME_BBA2[i] );
     if( val != NULL && val->fp != NULL ) fclose( val->fp );
   }

}     


