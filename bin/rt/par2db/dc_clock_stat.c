/******************************************************************R_
 *
 *  dc_clock_stat.c
 *
 ********************************************************************/
#include "dcsp.h"

#define CLC_NPAR   12 

extern int Log;
extern int DINTV;

static char *CLC_NAME[CLC_NPAR] =  {
 "ACFAIL",  "HAZARD", "M1OC", "M2OC", "M3OC", "CLK",
 "MON", "AON", "MSTAT", "ASTAT", "MOC", "AOC"

};

int dc_clock_stat( packet, Pkt, nchan, net, sta, epoch )
uchar_t *packet;
struct Packet **Pkt;
int nchan;
char *net;
char *sta;
double epoch;

{

    int i, dc, clcsel,mon, aon;
    struct PktChannel *achan;
    ushort_t *sval;
    uchar_t mclc_stat, aclc_stat;

/*
sval = (ushort_t *) &packet[2];
fprintf(stderr, "%c %c - %0x:%d (%0x %0x %0x %0x)\n", 
packet[0], packet[1], *sval, packet[0], packet[1], packet[2], packet[3]);
*/

    if( Log )  {
       hexdump( stderr, packet, 4 );
       fflush(stderr);
    }


    aclc_stat = packet[1];
    mclc_stat = packet[0];
    clcsel = packet[3]&0x20;
    
   if( clcsel )  {
       if( ( mon = packet[3]&0x80) == 0  )  {
           complain( 0, "MAIN is selected, but it's OFF!\n");
           return 0;
       } 
       
   }  else  {  
       if( (aon = packet[3]&0x40) == 0 )  {
           complain( 0, "AUX is selected, but it's OFF!\n");
           return 0;
       } 
      
   }
    
    
    for( i=0, dc = nchan; i < CLC_NPAR; i++, dc++ )  {
      achan = (PktChannel *) gettbl((*Pkt)->chan, dc) ;
      if ( achan == 0 ) {
          allot ( PktChannel *, achan, 1 ) ;
      }
      strcpy( achan->net, net);
      strcpy( achan->sta, sta );
      achan->time = epoch; 
      achan->samprate = 1.0/DINTV;                                
      achan->calib = 0;                     
      achan->datatype = trINT;
      achan->nsamp = 1;                   
      achan->nbytes = sizeof(int);
      if( strncmp( CLC_NAME[i], "MSTAT", 4 ) == 0 ) {
         switch( mclc_stat )  {
            case ' ' :
               dcpar[dc] = 1;
               break;
            case '.' :
               dcpar[dc] = 10;
               break;
            case '*' :
               dcpar[dc] = 100;
               break;
            case '#' :
               dcpar[dc] = 1000;
               break;
            case '?' :
               dcpar[dc] = -1;
               break;
            case '!' :
               dcpar[dc] = 0;
               break;
            default:
               complain( 0, "unknown MAIN clock status %0x-%c\n", mclc_stat, mclc_stat);
               return 0;
         }  
         sprintf( achan->chan, "%s\0", CLC_NAME[i] ) ;
      } else if( strncmp( CLC_NAME[i], "ASTAT", 4 ) == 0 ) {
         switch( aclc_stat )  {
            case ' ' :
               dcpar[dc] = 1;
               break;
            case '.' :
               dcpar[dc] = 10;
               break;
            case '*' :
               dcpar[dc] = 100;
               break;
            case '#' :
               dcpar[dc] = 1000;
               break;
            case '?' :
               dcpar[dc] = 0;
               break;
            case '!' :
               dcpar[dc] = TRGAP_VALUE_S4;
               break;
            default:
               dcpar[dc] = TRGAP_VALUE_S4;
               complain( 0, "unknown AUX clock status %0x-%c\n", aclc_stat, aclc_stat);
               break;
         }  
         sprintf( achan->chan, "%s\0", CLC_NAME[i] ) ;
      }   else if( strncmp( CLC_NAME[i], "ACFAIL", 6 ) == 0 ) {
         dcpar[dc] = packet[2]&0x80;
         sprintf( achan->chan, "%s\0", CLC_NAME[i] ) ;
      }   else if( strncmp( CLC_NAME[i], "HAZARD", 6 ) == 0 ) {
         dcpar[dc] = packet[2]&0x40;
         sprintf( achan->chan, "%s\0", CLC_NAME[i] ) ;
      }   else if( strncmp( CLC_NAME[i], "CLK", 3 ) == 0 ) {
         dcpar[dc] = clcsel;
         sprintf( achan->chan, "%s\0", CLC_NAME[i] ) ;
      }   else if( strncmp( CLC_NAME[i], "M1OC", 4 ) == 0 ) {
         dcpar[dc] = packet[2]&0x20;
         sprintf( achan->chan, "%s\0",  CLC_NAME[i] ) ;
      }   else if( strncmp( CLC_NAME[i], "M2OC", 4 ) == 0 ) {
         dcpar[dc] = packet[2]&0x10;
         sprintf( achan->chan, "%s\0",  CLC_NAME[i] ) ;
      }   else if( strncmp( CLC_NAME[i], "M3OC", 4 ) == 0 ) {
         dcpar[dc] = packet[2]&0x08;
         sprintf( achan->chan, "%s\0",  CLC_NAME[i] ) ;
      }   else if( strncmp( CLC_NAME[i], "MOC", 3 ) == 0 ) {
              dcpar[dc] = packet[2]&0x04;
         sprintf( achan->chan, "%s\0", CLC_NAME[i] ) ;
      }   else if( strncmp( CLC_NAME[i], "AOC", 3 ) == 0 ) {
              dcpar[dc] = packet[2]&0x02;
         sprintf( achan->chan, "%s\0", CLC_NAME[i] ) ;
      }   else if( strncmp( CLC_NAME[i], "MON", 3 ) == 0 ) {
              dcpar[dc] = mon;
         sprintf( achan->chan, "%s\0", CLC_NAME[i] ) ;
      }   else if( strncmp( CLC_NAME[i], "AON", 3 ) == 0 ) {
              dcpar[dc] = aon;            
         sprintf( achan->chan, "%s\0", CLC_NAME[i] ) ;
      }

     achan->data = &dcpar[dc];           
   
      settbl((*Pkt)->chan, dc, achan ) ;
   }             

   return dc;

 }

