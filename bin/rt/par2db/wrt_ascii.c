/* @(#)wrt_ascii.c	1.2 12/30/96  */
/******************************************************************
 *
 *  unstuffpar.c
 *
 *  get diagnostic parameters from a raw data packet; 
 *  fill Packet structure. 
 *
 *
 ********************************************************************/
#include "pkt.h"
#include "par2db.h"
#include "header.h"

static char *comname="COMM";
extern int DINTV;

int par_ESP( packet, Pkt )
char *packet;
struct Packet **Pkt;

{

    struct PktChannel *achan;
    struct DPHdr *hdr;
    struct Pval *par;
    char key[64];
    char *staname;
    long ttag;
    int i, sta;
    int want, off, val, ch;
    
    want = 1;

    hdr = ( struct DPHdr *) packet;
    
    if( *Pkt == 0 ) *Pkt = newpkt();
    (*Pkt)->pkttype = ntohl (hdr->prehdr.pkttype);
    (*Pkt)->hdrtype = ntohl (hdr->prehdr.hdrtype);
     

    for( i = 0; i < MAXNUMPAR; i++, ch++ )  {
       achan = (PktChannel *) gettbl((*Pkt)->chan, ch) ;
       if ( achan == 0 ) {
           allot ( PktChannel *, achan, 1 ) ;
       }
      for( sta = 0, ch = 0; sta < 16; sta++ )  {
         if( Staid[sta] ) == NULL )
	    die( 0, "Can't get station name for staid=%d.\n, sta);
      if( Ascii || Intv )  {
         sprintf( key, "%s_%s\0", Staid[sta], FILE_NAME[ch] );
         if( Param == NULL ) Param = newarr( 0 );
         par = ( struct Pval *) getarr( Param, key );
         if( par == NULL  )  {
             Npar += MAXNUMPAR;
             if( !init( Staid[sta] ) )  {
                Ascii = 0;  Intv = 0;
              }  else par = ( struct Pval *) getarr( Param, key );
          }
          want = 0;
          if( (hdr->epoch - par->time) >=  Intv )  {
              want = 1;
              par->time = hdr->epoch;
              setarr( Param, key, (char *) par );
          }
      }
      if( want )  {
         strcpy( achan->sta, Staid[sta] );
         strcpy( achan->chan, FILE_NAME[ch] ) ;
         strcpy( achan->net, hdr->net);
         achan->samprate = 1.0/DINTV;                                
         achan->calib = 0;                     
         achan->datatype = trINT;
         achan->nsamp = 1;                   
         achan->nbytes = sizeof(int);
         achan->time = hdr->epoch; 
         if( !strncmp( FILE_NAME[ch], "BUFDEL", 6 ) ||
             !strncmp( FILE_NAME[ch], "RTXNUM", 6 ) ||
             !strncmp( FILE_NAME[ch], "SRATE", 5 ) )  {
             off = PAR_OFF[ch]+hdr->prehdr.hdrsiz;
             val = packet[off];
             parameter[ch] = val; 
        } else if( !strncmp( FILE_NAME[ch], "GAIN", 4 ) )  {
             off = PAR_OFF[ch]+hdr->prehdr.hdrsiz;
             val = atoi( &packet[off] );
             parameter[ch] = val; 
         }  else if( !strncmp( FILE_NAME[ch], "TTAG", 4 ) ||
              !strncmp( FILE_NAME[ch], "XMTTAG", 6 ) )  {
              off = PAR_OFF[ch] + hdr->prehdr.hdrsiz;
              memcpy( (char *) &ttag, &packet[off], 4 );
              parameter[ch] = ttag;
         } else if( !strncmp( FILE_NAME[ch], "COORD", 5) )
            ;
         else {
            off = PAR_OFF[ch] + hdr->prehdr.hdrsiz;
            parameter[ch] = packet[off]*256 + packet[off+1];
         }
         achan->data = &parameter[ch];           
         settbl((*Pkt)->chan, ch, achan ) ;
              
         
         if( Ascii )  {
            par->val = parameter[ch];
            setarr( Param, key, (char *) par );
            fprintf( par->fp, "%lf %d\n", achan->time, parameter[ch] );
            fflush( par->fp );
         }

       } else return 2; 
    }


    }
    (*Pkt)->nchannels = ch;
    return 1; 
}

int unstuffpar( packet, Pkt)
char *packet;
struct Packet **Pkt;

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
    case CEHS:
    case CELS:
    case SDEHS:
    case SDELS:
	  retcode =  par_ESP( packet, Pkt );
	  break ;

    default:
	complain( 0, "Can't get parameters for pkttype - %d\n", hdr->pkttype);
	retcode = 0 ; 
	break ;
  } 

    return retcode ; 
}

int init( sta )
char *sta;

{

  int i;

  if( Param == NULL ) 
      Param = newarr( 0 );
  if( DiagPar == NULL ) 
        allot( Pval *, DiagPar, MAXNUMPAR );
  else 
        reallot ( Pval *, DiagPar, Npar );

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
     setarr( Param, DiagPar[i].name, (char *) &DiagPar[i]); 

  } 
  return 1;

}


void close_files() 

{

   struct Pval *val;
   int i;

   for ( i = 0; i < MAXNUMPAR; i++ )  {
     val = ( struct Pval *) getarr( Param, FILE_NAME[i] );
     if( val != NULL && val->fp != NULL ) fclose( val->fp );
   }

}     

