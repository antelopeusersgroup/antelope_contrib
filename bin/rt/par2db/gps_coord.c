/******************************************************************
 *
 *  bba2_par.c
 *
 ********************************************************************/
#include "header.h"

#define NCOORD 11

static int GPS[NCOORD] = {LATDD_BBA2, LATMM_BBA2,LATSS_BBA2,LATHH_BBA2,
                    NS_BBA2,LONDD_BBA2,LONMM_BBA2,LONSS_BBA2,
		    LONHH_BBA2,EW_BBA2,ALTD_BBA2 };

static char *GPS_NAME[NCOORD] = { "LATDD", "LATMM","LATSS","LATHH",
                           "NS","LONDD","LONMM","LONSS",
		           "LONHH","EW","ALTD" };

static char *GPS_UNITS[] = { "DEGREES", "MIN","SEC","MSEC",
                           "GPS_POSITION","DEGREE","MIN","SEC",
		           "MSEC","GPS_POSITION","GPS_ALTITUDE" };
int gpspar[NCOORD];

int gps_coord( uchar_t *packet,
               double etime,
               char *srcname, 
	       struct Packet **Pkt )
{

    struct PktChannel *chan;
    struct PktChannel *achan;
    struct BBAHdr *hdr;
    int  i;
    int off;
    short sval;
    char net[PKT_NAMESIZE], sta[PKT_NAMESIZE], key[64];
    
    hdr = ( struct BBAHdr *) packet;
    parse_srcname( srcname, &net[0], &sta[0], 0, 0 );

    if( *Pkt == 0 ) *Pkt = newpkt();
    (*Pkt)->pkttype = ntohl (hdr->prehdr.pkttype);
    (*Pkt)->hdrtype = ntohl (hdr->prehdr.hdrtype);
		 

    for( i = 0; i < NCOORD; i++ )  { 
	 achan = (PktChannel *) gettbl((*Pkt)->chan, i) ;
         if ( achan == 0 ) {
             allot ( PktChannel *, achan, 1 ) ;
         }
         strcpy( achan->chan, GPS_NAME[i] ) ;
         strcpy( achan->net, net);
	 achan->samprate = 1.0;                                
	 achan->calib = 0;                     
	 achan->datatype = trINT;
	 achan->nsamp = 1;                   
	 achan->nbytes = sizeof(int);
	 strcpy( achan->sta, sta);
	 achan->time = etime; 
        
         off = GPS[i]+hdr->prehdr.hdrsiz;
         if( i == 5 || i == 10 )  {
            memcpy( (char *) &sval, &packet[off], 2 );
         }  else
            sval = packet[off];
         gpspar[i] = ( int ) sval;
	 achan->data = &gpspar[i];           
	 settbl((*Pkt)->chan, i, achan ) ;
		           
   } 
   (*Pkt)->nchannels = i;
    return 1; 

}


