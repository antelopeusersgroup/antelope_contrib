#include "pkt.h"
#include "liss2orb.h"

extern int Log;

struct	net_time {
	ushort_t year;
	ushort_t day;
	uchar_t	hour;
	uchar_t	minute;
	uchar_t	seconds;
	uchar_t	dummy;
	ushort_t fracs;	
}; 

typedef struct lisspkt { 
	uchar_t		Seq_ID[6];		/* Sequence number of record */
	uchar_t		Record_Type;		/* Always a 'D' */
	uchar_t		Filler;			/* Always a space */

	uchar_t		Station_ID[5];		/* Station id (space filled) */
	uchar_t		Location_ID[2];		/* Array/Extended Station (filled) */
	uchar_t		Channel_ID[3];		/* Channel Id (space filled) */
	uchar_t		Network_ID[2];		/* Extended Network Type */

	struct net_time	Start_Time;		/* Start time of record */
	ushort_t 	Number_Samps;		/* Number of samples in record */

	short		Rate_Factor;		/* Sample rate factor */
	short		Rate_Mult;		/* Rate Multiplier */

	uchar_t		Activity_Flags;		/* Activity Information */

#define ACTFLAG_CALSIG 0x01		/* Calibration signals this record */
#define ACTFLAG_CLKFIX 0x02		/* Error caused by clock correction */
#define ACTFLAG_BEGEVT 0x04		/* Beginning of event */
#define ACTFLAG_ENDEVT 0x08		/* End of event */

	uchar_t		IO_Flags;		/* I/O Information */

#define IOFLAG_ORGPAR 0x01		/* Original tape had parity error */
#define IOFLAG_LONGRC 0x02		/* Original read long record */
#define IOFLAG_SHORTR 0x04		/* Original had short record */

	uchar_t		Qual_Flags;		/* Data Quality Information */

#define QULFLAG_AMPSAT 0x01		/* Amplifier saturation detected */
#define QULFLAG_SIGCLP 0x02		/* Signal clipping detected */
#define QULFLAG_SPIKES 0x04		/* Signal spiking detected */
#define QULFLAG_GLITCH 0x08		/* Signal glitch detected */
#define QULFLAG_PADDED 0x10		/* Data missing or padded */
#define QULFLAG_TLMSNC 0x20		/* Telemetry sync error/dropout */
#define QULFLAG_CHARGE 0x40		/* Digitial filter charging */
#define QULFLAG_TIMERR 0x80		/* Time tag questionable */

	uchar_t		Total_Blockettes;	/* Number blockettes to follow */
	int		Time_Correction;	/* Number of .0001 sec correction */
	ushort_t 	Data_Start;		/* Byte where data starts */
	ushort_t 	First_Blockette;	/* Byte of first blockette */
} LissPkt;

typedef struct Liss  {
     struct PreHdr prehdr;
     ushort_t nsamp;
     ushort_t doff;
     float    samprate;
} LissHdr;

#define TRIM(s,l) {int i;for(i=l-1;i>0,s[i-1]==' ';i--);s[i]='\0';}
extern regex_t srcmatch;
static char *new=0;

int StuffLiss( 
    char **packet, 
    char *srcname, 
    double *epoch, 
    int liss_size, 
    char *match )  {


     struct PreHdr prehdr;
     double      samprate;
     int 	yr, day, hr, min, sec, msec;
     int        rtf, rtm;
     int 	hsize, psize;
     char 	tim_str[64];
     char 	loc[4], sta[8],
          	chan[12],
	  	net[12];
     char 	*data;
     LissPkt  *seed_hdr;
     LissHdr    hdr;

    data = *packet;

    seed_hdr = ( LissPkt *) data;
   
    if( seed_hdr->Record_Type!= 'D' ) {
/*    
       complain( 0, "unknown record type %c-%x.\n", 
       seed_hdr->Record_Type, seed_hdr->Record_Type);
       hexdump( stderr, data, 48 );
 */
       return 0;
    }
    memcpy( net, seed_hdr->Network_ID,2);
    net[2] = '\0';
    TRIM(net,3);
    if ( net[0]=='\0')  sprintf( net, "IU\0");
    
    memcpy( sta, seed_hdr->Station_ID,5 );
    sta[5] = '\0';
    TRIM(sta, 6);
                                        
    memcpy( chan, seed_hdr->Channel_ID,3 );
    chan[3] = '\0';
    TRIM( chan, 4);
    
    memcpy( loc, seed_hdr->Location_ID,2 );
    loc[2] = '\0';
    TRIM( loc, 3);

    if( !strncmp( loc, "10", 2 ) || !strncmp( loc, "00", 2 ) )  {
    	sprintf( srcname, "%s_%s_%s%s\0", net, sta, chan, loc );
    }  else sprintf( srcname, "%s_%s_%s11\0", net, sta, chan );
    

    if( match ) 
       if( regexec( &srcmatch, srcname, (size_t) 0, NULL, 0 ) != 0 )
          return 0; 

    yr   = lbyte_order(seed_hdr->Start_Time.year, 2 ) ;
    day  = lbyte_order(seed_hdr->Start_Time.day, 2 ) ;
    hr   =  seed_hdr->Start_Time.hour ;
    min  =  seed_hdr->Start_Time.minute ;
    sec  =  seed_hdr->Start_Time.seconds ;
    msec = lbyte_order(seed_hdr->Start_Time.fracs, 2 ) ;
/*    msec /= 10;  */

    sprintf( tim_str, "%04d%03d:%02d:%02d:%02d.%04d\0",  
           yr, day, hr, min, sec, msec);   
 
    *epoch = str2epoch( &tim_str[0] );

    rtf =  lbyte_order( seed_hdr->Rate_Factor, 2 );
    rtm =  lbyte_order( seed_hdr->Rate_Mult, 2 );

    if ( rtm == 0 ) {
/*       complain( 0, "Illegal rate %d\n", rtm );
       hexdump( stderr, data, 64 );
       fflush(stderr);
*/
       return 0;
    } else {
      if (rtf < 0 ) hdr.samprate = 1.0/(-( (float) rtf ));
      else hdr.samprate = (float) rtf;
      if ( rtm < 0 ) hdr.samprate /= -((float) rtm );
      else hdr.samprate *= (float) rtm ;
 
    }
    hdr.prehdr.pkttype = ( ushort_t) htons( LISSPKT);
    hdr.prehdr.hdrtype = ( ushort_t) htons( LISSPKT) ;
    hdr.prehdr.pktsiz =  htons(liss_size); 
    hdr.prehdr.hdrsiz = htons( sizeof( LissHdr) );  
    hdr.nsamp =  htons( lbyte_order( seed_hdr->Number_Samps, 2) );      
    hdr.doff  = htons( lbyte_order( seed_hdr->Data_Start, 2) ); 

if(Log)  {
    fprintf( stderr, "%s %lf %f %d %d (%d) \n", 
    srcname, *epoch,  hdr.samprate, hdr.nsamp, hdr.doff,seed_hdr->Number_Samps );
    fflush(stderr);
}
    if( new == 0 )
       allot( char *, new, 2048 );
    
    /* Add a header */

      hsize = sizeof( LissHdr);
      memcpy ( new, (char *)&hdr,  hsize );
 
    /* Add data  */

      psize = hsize+liss_size; 
      memcpy(new+hsize, (char *) *packet, liss_size );
      memcpy(*packet, new, psize);
 
      return psize;
}

