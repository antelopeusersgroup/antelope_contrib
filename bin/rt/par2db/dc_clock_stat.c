/******************************************************************R_
 *
 *  dc_clock_stat.c
 *
 ********************************************************************/
#include "dcsp.h"

#define CLC_NPAR   7 

extern int Log;
extern int DINTV;

static char *CLC_NAME[CLC_NPAR] =  {
 "STAT", "ACFAIL",  "HAZARD", "M1OC", "M2OC", "M3OC", "OC"
};

int dc_clock_stat( packet, Pkt, nchan, net, sta, epoch )
unsigned char *packet;
struct Packet **Pkt;
int nchan;
char *net;
char *sta;
double epoch;

{

    int i, dc, main_clc;
    struct PktChannel *achan;
    ushort_t *sval;
    uchar_t clc_stat, selec;

/*
sval = (ushort_t *) &packet[2];
fprintf(stderr, "%c %c - %0x:%d (%0x %0x %0x %0x)\n", 
packet[0], packet[1], *sval, packet[0], packet[1], packet[2], packet[3]);
*/

    if( Log )  {
       hexdump( stderr, packet, 4 );
       fflush(stderr);
    }


    main_clc = (packet[3]&0x20) ? 1:0;
    
   if( main_clc )  {
       if( ! packet[3]&0x80 )  {
           complain( 0, "MAIN is selected, but it's OFF!\n");
           return 0;
       } else {
           selec = 'M';
           clc_stat = packet[0];
       } 
   }  else  {  
       if( !packet[3]&0x40 )  {
           complain( 0, "AUX is selected, but it's OFF!\n");
           return 0;
       } else {
           selec = 'A';
           clc_stat = packet[1];
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
      if( strncmp( CLC_NAME[i], "STAT", 4 ) == 0 ) {
         switch( clc_stat )  {
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
               complain( 0, "unknown clock status %0x-%c\n", clc_stat, clc_stat);
               return 0;
         }  
         sprintf( achan->chan, "%c%s\0", selec, CLC_NAME[i] ) ;
      }   else if( strncmp( CLC_NAME[i], "ACFAIL", 6 ) == 0 ) {
         dcpar[dc] = (packet[2]&0x80)?1:0;
         sprintf( achan->chan, "%c%s\0", selec, CLC_NAME[i] ) ;
      }   else if( strncmp( CLC_NAME[i], "HAZARD", 6 ) == 0 ) {
         dcpar[dc] = (packet[2]&0x40)?1:0;
         sprintf( achan->chan, "%c%s\0", selec, CLC_NAME[i] ) ;
      }   else if( strncmp( CLC_NAME[i], "M1OC", 4 ) == 0 ) {
         dcpar[dc] = (packet[2]&0x20)?1:0;
         sprintf( achan->chan, "%s\0",  CLC_NAME[i] ) ;
      }   else if( strncmp( CLC_NAME[i], "M2OC", 4 ) == 0 ) {
         dcpar[dc] = (packet[2]&0x10)?1:0;
         sprintf( achan->chan, "%s\0",  CLC_NAME[i] ) ;
      }   else if( strncmp( CLC_NAME[i], "M3OC", 4 ) == 0 ) {
         dcpar[dc] = (packet[2]&0x08)?1:0;
         sprintf( achan->chan, "%s\0",  CLC_NAME[i] ) ;
      }   else if( strncmp( CLC_NAME[i], "OC", 2 ) == 0 ) {
         if(main_clc )  
              dcpar[dc] = (packet[2]&0x04)?1:0;
         else 
              dcpar[dc] = (packet[2]&0x02)?1:0;
         sprintf( achan->chan, "%c%s\0", selec, CLC_NAME[i] ) ;
      }

     achan->data = &dcpar[dc];           
   
      settbl((*Pkt)->chan, dc, achan ) ;
   }             

   return dc;

 }

