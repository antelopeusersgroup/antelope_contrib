/*  $Name $Revision$ $Date$  */
/*************************************************************************
 *
 *   pkttype.c 
 *
 *   Recognize a packet type   
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 ************************************************************************/
#include "defunctpkt.h"

extern Arr *RawPkts;
Arr *PsclSTRM=0;
struct Prm Par;

/* Parse PASSCAL Information packet  */

int parse_pscl_IP ( 
    uchar_t *packet,
    ushort_t pkttype )
{

int nsamp, yr, day, hr, min, sec, msec;
ushort_t newtype; 
PktPar pack;
char stime[64], key[64];
struct PsclPreHdr pkt;
int dasid;

/* Get parameters for current datatype  */

      newtype = PSCLIP;
      if( !get_packet( newtype, &pack) ) return -1;
    
      dasid = bcd2hex( &packet[4] , 4 );
      Par.staid = dasid;
      Par.chan = -1;
      Par.packet = pack;
      Par.hdrtype = ( int )decode( pack.hdrtype ); 

      memcpy( (char *) &pkt, packet, sizeof( struct PsclPreHdr ));
      yr = bcd2hex( pkt.year , 2 );
      sprintf( (char *) &stime[0], "%02x%02x%02x%02x%02x%02x\0", 
        pkt.bcdtime[0], pkt.bcdtime[1], pkt.bcdtime[2], 
        pkt.bcdtime[3], pkt.bcdtime[4], pkt.bcdtime[5]); 
      sscanf( (char *) &stime[0], "%3d%2d%2d%2d%3d", 
        &day, &hr, &min, &sec, &msec);
      if( yr < 50 ) 
          sprintf( (char *) &stime[0], "20%02d%03d:%02d:%02d:%02d.%03d\0", 
                                        yr, day, hr, min, sec, msec); 
      else
          sprintf( (char *) &stime[0], "19%02d%03d:%02d:%02d:%02d.%03d\0", 
                                        yr, day, hr, min, sec, msec); 
      Par.time = str2epoch( (char *) &stime[0] );
      
   
      sprintf( &Par.packet.pkttype[0], "PSCLIP\0");
      sprintf( (char *) &key[0], "%d", newtype );
      setarr( RawPkts, (char *) &key[0], (char *) &Par.raw);
      
      return (int) pkttype;
}

/* Parse ANZA Status Packet  */

int parse_anza_SP ( 
    uchar_t *packet,
   ushort_t pkttype )
{

double ptime, ytime, sec;
ulong ysec;
int year, day, hour, min;
PktPar pack;

/* Get parameters for current datatype  */

      if( !get_packet( pkttype, &pack) ) return -1;
    
      Par.staid = packet[6]*256 + packet[7];
      Par.packet = pack;
      Par.chan = -1; 
      Par.hdrtype = ( int )decode( pack.hdrtype ); 
      
      ytime = now();
      e2h( ytime, &year, &day, &hour, &min, &sec);
      ptime = epoch( year * 1000 );
      memcpy( (char *) &ysec, (char *) &packet[10], 4);
      Par.time = ptime + ysec;
      
  return (int) pkttype; 
      
      
}

/* Parse BBA data packet  */

int parse_bba_DP( 
    uchar_t *packet,
    ushort_t pkttype )
{

double ptime, ytime, sec;
unsigned long ysec;
int srate;
int year, day, hour, min;
PktPar pack;

/* Get parameters for current datatype  */

      if( !get_packet( pkttype, &pack) ) return -1;
      
      Par.packet = pack;
      Par.hdrtype = ( int )decode( pack.hdrtype ); 

/* Get packet time  */
 
      ytime = now();
      e2h( ytime, &year, &day, &hour, &min, &sec);
      ptime = epoch( year * 1000 );
/*
        Par.time = ptime + ysec;
*/

      memcpy( (char *) &ysec, &packet[10],  4);  
      Par.time = ptime + ysec;
      
      Par.chan = -1;                 
      Par.staid = packet[6]*256 + packet[7];

      /* 
         Now try to get sample rate from a header.
         Some old headers don't have that value filled - so be carefull!
       */

      srate = packet[20];  /* sample rate value in a raw packet  */
      if(srate > 0 )  {
	if(srate < 10)  {
           strcpy(pack.hdrtype, "BBA\0");
           strcpy(pack.pkttype, "CBB1S\0");
	   Par.packet = pack;
	   Par.packet.srate = (float) srate;
           Par.hdrtype = ( int )decode( pack.hdrtype ); 
	   Par.packet.nsamp = srate*60;  /* 60 sec of data */
           Par.raw.pkttype = CBB1S;
	}  else if(srate >= 100)  {
           strcpy(pack.hdrtype, "BBA\0");
           strcpy(pack.pkttype, "CBBHS\0");
	   Par.packet = pack;
	   Par.packet.srate = (float) srate;
           Par.hdrtype = ( int )decode( pack.hdrtype ); 
	   Par.packet.nsamp = srate*1;  /* 1 sec of data  */
           Par.raw.pkttype = CBBHS;
	}  else  { 
           strcpy(pack.hdrtype, "BBA\0");
           strcpy(pack.pkttype, "CBBLS\0");
	   Par.packet = pack;
	   Par.packet.srate = (float) srate;
           Par.hdrtype = ( int )decode( pack.hdrtype ); 
	   Par.packet.nsamp = srate*5;  /* 5 sec of data */
           Par.raw.pkttype = CBBLS;
	}
      } 
      return (int) pkttype;
}

/* Parse ANZA data Packet */

int parse_anza_DP( 
    uchar_t *packet,
    ushort_t pkttype )
{

double ptime, ytime, sec;
unsigned long ysec;
int year, day, hour, min;
PktPar pack;


/* Get parameters for current datatype  */

      if( !get_packet( pkttype, &pack) ) return -1;
      
      memcpy( (char *) &Par.packet, (char *) &pack, sizeof( PktPar));
      Par.hdrtype = ( int )decode( pack.hdrtype ); 

/* Get packet time  */
 
      ytime = now();
      e2h( ytime, &year, &day, &hour, &min, &sec);
      ptime = epoch( year * 1000 );
      memcpy( (char *) &ysec, &packet[52],  4);  
      Par.time = ptime + ysec;
      Par.chan = -1;                 
      Par.staid = packet[4]*256 + packet[5];

      return (int) pkttype;

}

/* Parse PASSCAL Data Packet  */

int parse_pscl_DP ( 
    uchar_t *packet,
    ushort_t  pkttype )
{
   double etime; 
   Stream *stream, *strm_tim;
   ushort_t  newtype ;
   int dasid, event, streamid, chan, pktnum;
   int nsamp, yr, day, hr, min, sec, msec;
   char dtype[2], ptype[64], key[64], skey[64], stime[64];
   PktPar pack;
   int i;


   dasid = bcd2hex( &packet[4] , 4 );
   pktnum = bcd2hex( &packet[14] , 4 );
   event = bcd2hex( &packet[16], 4 ); 
   streamid = bcd2hex( &packet[18], 2 ); 
   streamid++;
   chan = bcd2hex( &packet[19], 2 ); 
   chan++;

   nsamp = bcd2hex( &packet[20], 4 ); 
   sprintf( (char *) &dtype[0], "%02x", packet[23] ); 
 
   sprintf( skey, "%d_%d\0", dasid, streamid );
  
   if( PsclSTRM == NULL ) return 0;
   stream = ( Stream *) getarr( PsclSTRM, (char *)&skey[0]);
   if( stream == NULL ) {
      elog_complain( 0, "Can't get %s stream info\n", skey); 
      hexdump( stderr, packet, 64 );
      return 0;
   }
  
   if( stream->samprate <= 0 ) return 0;

   switch ( dtype[0] )  {

    case 'c':
       strcpy( (char *) &dtype[0], "c0");
       if( stream->samprate >= 100 )  {
         sprintf( &ptype[0], "CPSCLHS\0");
         newtype = CPSCLHS;
       } else { 
         sprintf( &ptype[0], "CPSCLLS\0");
         newtype = CPSCLLS;
       }
       break;

    case '1':
       strcpy( (char *) &dtype[0], "s2");
       if( stream->samprate >= 100 )  {
         sprintf( &ptype[0], "PSCLHS\0");
         newtype = PSCLHS;
       } else { 
         sprintf( &ptype[0], "PSCLLS\0");
         newtype = PSCLLS;
       }
       break;

    case '3':
       strcpy( (char *) &dtype[0], "s4");
       if( stream->samprate >= 100 )  {
         sprintf( &ptype[0], "PSCLHS\0");
         newtype = PSCLHS;
       } else { 
         sprintf( &ptype[0], "PSCLLS\0");
         newtype = PSCLLS;
       }
       break;

    default:
       elog_complain( 0, "Can't recognize a PASSCAL data packet type. HS?LS?\n");
       return -1;
   }
   
   Par.chan = chan;               
   Par.staid =  dasid;
   
   if( !get_packet( newtype, &pack) ) return -1;
   
   strcpy( &pack.datatype[0], &dtype[0]);
   strcpy( &pack.pkttype[0], &ptype[0]);
 
   pack.nsamp = nsamp;
   pack.nchan = 1;               
   pack.srate = stream->samprate;               
   Par.packet = pack;
   
   yr = bcd2hex( &packet[3] , 2 ); 
   if(yr < 50) yr += 2000;
   else yr += 1900;
   sprintf( (char *) &stime[0], "%02x%02x%02x%02x%02x%02x\0", 
     packet[6], packet[7], packet[8], packet[9],
     packet[10], packet[11]); 
   sscanf( (char *) &stime[0], "%3d%2d%2d%2d%3d", 
     &day, &hr, &min, &sec, &msec); 
   sprintf( (char *) &stime[0], "%02d%03d:%02d:%02d:%02d.%03d\0", 
     yr, day, hr, min, sec, msec); 
   Par.time = str2epoch( (char *) &stime[0] );

   sprintf( (char *) &key[0], "%d", newtype );
   setarr( RawPkts, (char *) &key[0], (char *) &Par.raw);
   
   Par.hdrtype = ( int )decode( pack.hdrtype );
   return (int) pkttype;
}

/* Get packet type from first two bytes of a raw racket; 
   call coresponding parsing routine  
*/

int whatis_pkttype( uchar_t *packet )

{
     Raw *raw;
     PktPar *pkt;
     ushort_t *val;
     ushort_t code;
     char *parse;
     int  i, nelem;
     char key[64];

     val = ( ushort_t *)  packet;
     code = *val;
 
     Par.raw.pkttype =  code;
     if( RawPkts == NULL) init_RawPkts();
     sprintf( key, "%d\0", code );
     if( code == 0 )  {
        elog_complain( 0, " whatis_pkttype(): Can't recognize a packet type %0x%0x\n", packet[0], packet[1] );
        return -1;
     }

     raw = ( Raw *) getarr( RawPkts, (char *) &key[0] );
     if( raw == NULL ) { 
        elog_complain( 0, " whatis_pkttype(): Can't get RawPkts info for %0x%0x\n", packet[0], packet[1] );
        return -1;
     }
     memcpy( (char *) &Par.raw, (char *) raw, sizeof( Raw ) );
     switch(  parse_raw( packet, raw->parse, raw->pkttype ) ) {

	case -1:
          elog_complain( 0, " whatis_pkttype(): Can't parse raw packet - %0x%0x\n", packet[0], packet[1] );
          return -1;
	case 0:
          return 0;
	case 1:
          return (int) raw->pkttype;
     }
	

}


/* $Id$ */
