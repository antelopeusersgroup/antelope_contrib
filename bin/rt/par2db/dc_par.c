/******************************************************************R_
 *
 *  dc_par.c
 *
 ********************************************************************/
#include "dcsp.h"

double prev_lcmd_time;

extern FILE *fplog;
extern FILE *fplcmd;
extern int DINTV;
extern int DCFlag;

int dc_par( packet, Pkt )
char *packet;
struct Packet **Pkt;

{

    int doff, i;
    struct PktChannel *achan;
    struct IPHdr  *hdr;
    int  off, val, dc, ds, ch;
    int pktsize;
    long lval;
    short numdas, sval, size;
    char *daspkt;
    char *s, lcmd[40], dcname[64], name[64];
    struct Pval *par;
    double ptime, ytime, sec;
    ulong ysec;
    int year, day, hour, min;

    hdr = ( struct IPHdr *) packet;
    doff = hdr->prehdr.hdrsiz ; 

    (*Pkt)->pkttype = ntohl (hdr->prehdr.pkttype);
    (*Pkt)->hdrtype = ntohl (hdr->prehdr.hdrtype);
     size = ntohl (hdr->prehdr.pktsiz);

     /*
     hexdump( stderr, packet+ hdr->prehdr.hdrsiz, size);
     */

    off = UID_DC+hdr->prehdr.hdrsiz;
    memcpy( (char *) &sval, &packet[off], 2 );
    sprintf( (char *) &dcname[0], "%d\0", sval );
             
    off = DASNUM+hdr->prehdr.hdrsiz;
    memcpy( (char *) &numdas, &packet[off], 2 );

    for( dc = 0; dc < MAX_DCPAR; dc++ )  {
      achan = (PktChannel *) gettbl((*Pkt)->chan, dc) ;
      if ( achan == 0 ) {
          allot ( PktChannel *, achan, 1 ) ;
      }
      strcpy( achan->chan, DCFILE_NAME[dc] ) ;
      strcpy( achan->net, hdr->net);
      achan->samprate = 1.0/DINTV;                                
      achan->calib = 0;                     
      achan->datatype = trINT;
      achan->nsamp = 1;                   
      achan->nbytes = sizeof(int);
      strcpy( achan->sta, dcname );
      achan->time = hdr->epoch; 
      if( !strncmp( DCFILE_NAME[dc], "TCPSND", 4 ) )  {
              off = DCPAR_OFF[dc] + hdr->prehdr.hdrsiz;
              memcpy( (char *) &lval, &packet[off], 4 );
              dcpar[dc] = lval;
      }  else {
       
	     off = DCPAR_OFF[dc] + hdr->prehdr.hdrsiz;
              memcpy( (char *) &sval, &packet[off], 2 );
              dcpar[dc] = sval;
      }
      achan->data = &dcpar[dc];           
      settbl((*Pkt)->chan, dc, achan ) ;
   }             

   /* record LAST COMMAND  */

   ytime = now();
   e2h( ytime, &year, &day, &hour, &min, &sec);
   ytime = epoch( year * 1000 );
   off = DCCMD_TIM +  hdr->prehdr.hdrsiz;
   memcpy( (char *) &ysec, (char *) &packet[off], 4);
   ptime = ytime + ysec;
   if( ptime - prev_lcmd_time > 1 )  {
     prev_lcmd_time = ptime;
     off = DCCMD +  hdr->prehdr.hdrsiz;
     for( i = 0; i < 40 ; i++)   {
       sprintf( &lcmd[i], "%c", packet[off+i]);
     }
     fprintf( fplcmd, "%s : %-40s\n", s=strtime(ptime), lcmd);
     fflush( fplcmd);
     free(s);
   }
   
   /* Get DASes parameters  */
   
   if( numdas <= 0 || numdas > 48 )  {
     complain( 0, "wrong number of dases - %d\n", numdas );
     return 0;
   }

   ch = dc;
   
   for( i = 0; i < numdas; i++ )  {
      daspkt = packet + hdr->prehdr.hdrsiz + DCSIZ + i *  DASSIZ;     
      off = UID_DAS;
      memcpy( (char *) &sval, &daspkt[off], 2 );
      sprintf( (char *) &name[0], "%d\0", sval );
             
      for( ds = 0; ds < MAXDAS; ds++, ch++ )  {
        achan = (PktChannel *) gettbl((*Pkt)->chan, ch) ;
        if ( achan == 0 ) {
            allot ( PktChannel *, achan, 1 ) ;
        }
        strcpy( achan->chan, FILE_NAME_DAS[ds] ) ;
        strcpy( achan->net, hdr->net);
        achan->samprate = 1.0/DINTV;                                
        achan->calib = 0;                     
        achan->datatype = trINT;
        achan->nsamp = 1;                   
        achan->nbytes = sizeof(int);
        strcpy( achan->sta, name );
        achan->time = hdr->epoch; 
	off = PAR_OFF_DAS[ds] ;
        memcpy( (char *) &sval, &daspkt[off], 2 );
        daspar[ds+i*MAXDAS] = sval;
        achan->data = &daspar[ds + i*MAXDAS];           
        settbl((*Pkt)->chan, ch, achan ) ;
     }
   }
  
   if( DCFlag )  {
      off = hdr->prehdr.hdrsiz+DCCLOCK;
      if( (ch = dc_clock_stat( &packet[off] , Pkt, ch, hdr->net, dcname, hdr->epoch )) == 0 )
      return 0;
    
   }

  (*Pkt)->nchannels = ch;
   
   return 1;

 }

