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
#include "pkt2.h"

extern Arr *RawPkts;
Arr *PsclSTRM=0;
struct Prm Par;
char *Network_Name = "none";
int PktLog=0;
 
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
      Par.time = ptime + ntohl(ysec);
      
  return (int) pkttype; 
      
      
}

/* Here is new BBA data packet structure 

struct pack_struct
        {
        unsigned short sync;            -- Software sync pattern:
					DAS Data   sync pattern 0xDAAB
					DAS Status sync pattern 0xDABC
					DC  Status sync pattern 0xDACD
					RTX Status sync pattern 0xDADE 
        unsigned short checksum;        -- block checksum 
        unsigned short byte_count;      -- packet length 
        unsigned short hdrsize;         -- Length of header, start of data 
        unsigned short swver;           -- software version in BCD 
        unsigned short unitid;          -- unit id number 
        unsigned short buf_id;          -- buffer id for rexmit
        unsigned long  tag_seconds;     -- time tag in seconds since year begin 
        unsigned short nsamp;           -- Number of  samples in this packet 
        unsigned short smprat;          -- Sample rate * 10 
        unsigned char  datatype;        -- Data type    0x00=16bit,
                                                0x01=32bit,
                                                0x02=64bit,
                                                0x10=16bit compressed,
                                                0x11=32bit compressed,
                                                0x12=64bit compressed  
        unsigned char  nchan;           -- number of data channels 
	unsigned char  send_backlog;    -- send buffer backlog count 
	unsigned char  rexmit;          -- # of rexmit for this buffer 

-- 1st data channel header 
        unsigned char  chid;            -- channel id code (1) 
        unsigned char  gain;            -- channel 2 gain code
        unsigned short chbytes;         -- number of data bytes - header 
        unsigned char  data[chbytes];   -- channel data 

		.
		.
-- nchan data channel header 
        unsigned char  chid;            -- channel id code (nchan) 
        unsigned char  gain;            -- channel gain codes 
        unsigned short chbytes;         -- number of data bytes - header 
        unsigned char  data[chbytes];   -- channel data 

        };
*/

#define PSIZE_OFF  (4)
#define HSIZE_OFF  (6)
#define STAID_OFF  (10)
#define TIME_OFF   (14)
#define NSAMP_OFF  (18)
#define SRATE_OFF  (20)
#define DTYPE_OFF  (22)
#define NCHAN_OFF  (23)

#define CHBYTES_OFF (2)
#define CHHDR_SIZE  (4)

int parse_newbba(uchar_t *packet, ushort_t pkttype)
{

double ptime, ytime, sec;
unsigned long ysec;
int year, day, hour, min;
PktPar pack;
int i, doff, len, chid, chbytes;
unsigned short val;
char *chname, chan[128];

      memcpy((char *) &val, &packet[PSIZE_OFF],  2);   /* packet size  */
      if(val == 0) { 
 	 elog_complain(0, "Wrong header. Zero packet size detected.\n");
	 return(-1);
      } else pack.size = ntohs(val);
      memcpy((char *) &val, &packet[STAID_OFF],  2);   /* sta ID  */
      if(pack.size == 0) { 
 	 elog_complain(0, "Wrong header. Zero packet size detected.\n");
	 return(-1);
      } else Par.staid = ntohs(val);
      memcpy((char *) &val, &packet[NSAMP_OFF],  2);  /* # of samples */
      if(val == 0) { 
 	 elog_complain(0, "Wrong header. Zero number of samples detected.\n");
	 return(-1);
      } else pack.nsamp = ntohs(val);
      memcpy((char *) &val, &packet[SRATE_OFF],  2);  /* Sample rate  */
      if(val == 0) { 
 	 elog_complain(0, "Wrong header. Zero sample rate detected.\n");
	 return(-1);
      }  else pack.srate = ntohs(val);

      val = packet[HSIZE_OFF]*256 + packet[HSIZE_OFF+1]; /* header size */
      if(val == 0) { 
 	 elog_complain(0, "Wrong header. Zero header size detected.\n");
	 return(-1);
      }  else pack.hdrsiz = val;

      pack.nchan = packet[NCHAN_OFF];  /* Number of channels */
      if(pack.nchan == 0) { 
 	 elog_complain(0, "Wrong header. Zero number of channels detected.\n");
	 return(-1);
      }

if(PktLog) {
fprintf(stderr, "staid=%d rate=%f nsamp=%d psize=%d hsize=%d nchan=%d\n",
Par.staid, pack.srate, pack.nsamp, pack.size, pack.hdrsiz, pack.nchan);
fflush(stderr);
}      
      /* get data type */
      switch(packet[DTYPE_OFF])  {
	  case 0x0:
             strcpy(pack.datatype, "s2");
	     break;
	  case 0x01:
             strcpy(pack.datatype, "s4");
	     break;
          case 0x02:
             strcpy(pack.datatype, "t4");
	     break;
          case 0x10:
          case 0x11:
          case 0x12:
             strcpy(pack.datatype, "c0");
	     break;
          default:
             elog_complain(0, "Can't recognize a data type - %d(%02x)\n", 
                      packet[DTYPE_OFF], packet[DTYPE_OFF]);
             return -1;
      }
      Par.raw.pkttype = pkttype;
      strcpy(pack.hdrtype, "BBA\0");       /* Header type */
      Par.hdrtype = (int) decode(pack.hdrtype); 
      strcpy(pack.net_type, Network_Name);  
      
      /* Get packet time  */
      ytime = now();
      e2h(ytime, &year, &day, &hour, &min, &sec);
      ptime = epoch(year * 1000);

      memcpy((char *) &ysec, &packet[TIME_OFF],  4);  
      Par.time = ptime + ntohl(ysec);
      Par.chan = -1;                 
      /* now we must to get channel names */


     
       switch(pkttype)  {
         case 0xdaab:  /* Data packets */
             if(pack.srate < 10)  {
		strcpy(pack.pkttype, "BBA/LS\0");
             }  else if(pack.srate >= 100)  {
                 strcpy(pack.pkttype, "BBA/HS\0");
             }  else {
                 strcpy(pack.pkttype, "BBA/BS\0");
             }
             get_sta_name();
	     break;
         case 0xdabc:  /* DAS status packets */
	    strcpy(pack.pkttype, "BBA/DAS\0");
            get_sta_name();
	    break;
	 case 0xdacd:   /* DC status packets */
	    strcpy(pack.pkttype, "BBA/DC\0");
	    sprintf(Par.staname, "%d\0", Par.staid );
	    break;
	 case 0xdade:  /* RXT packets */
	    strcpy(pack.pkttype, "BBA/RTX\0");
            get_sta_name();
	    break;
         default:
            elog_complain(0, "Can't recognize a data packet type - %d(%02x)\n",
	             pkttype, pkttype);
	    return(-1);
      }
      Par.packet = pack;
/*
printf("dtype=%s net=%s \n ", pack.datatype, pack.net_type);
*/
      memset( chan, 0, 128);
      memcpy( chan, "_", strlen("_"));
      len++;
      
      doff = pack.hdrsiz;
      for(i = 0, len=0; i < pack.nchan; i ++)  {
	val = packet[doff];
        chid = val;
        val = packet[doff+CHBYTES_OFF]*256 + packet[doff+CHBYTES_OFF+1];
	chbytes = val;

/*
printf("chan=%d bytes=%d\n", chid, chbytes);
*/

	chname = get_chname_from_id(pkttype, Par.packet.pkttype, Par.staid, chid);
	if(chname == 0) return(-1);
	strcat(chan, chname);
        len += strlen(chname);
        free(chname);
        strcat( chan, "_");
        len++;
        doff += CHHDR_SIZE+chbytes;
      }
      chan[len] = '\0';
      strcpy(Par.chnames, chan);

/*
printf("chan=%s\n", chan);
*/

      return ((int) pkttype);
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
      Par.time = ptime + ntohl(ysec);
      
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
     code = ntohs(*val);

     Par.raw.pkttype =  code;
     if( RawPkts == NULL) init_RawPkts();
     sprintf( key, "%d\0", code );
     if( code == 0 )  {
        elog_complain( 0, " whatis_pkttype(): Can't recognize a packet type %s\n", key );
        return -1;
     }

     raw = ( Raw *) getarr( RawPkts, (char *) &key[0] );
     if( raw == NULL ) { 
        elog_complain( 0, " whatis_pkttype(): Can't get RawPkts info for %s\n", key );
        return -1;
     }
     memcpy( (char *) &Par.raw, (char *) raw, sizeof( Raw ) );
     switch(  parse_raw( packet, raw->parse, raw->pkttype ) ) {

	case -1:
          elog_complain( 0, " whatis_pkttype(): Can't get RawPkts info for %s\n", key );
          return -1;
	case 0:
          return 0;
	case 1:
          return (int) raw->pkttype;
     }

}


/* $Id$ */
