/*  $Name $Revision$ $Date$  */
/*************************************************************************
 *
 *   b3s2.c 
 *
 *   Parse a b3s2 DA & ST packets
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 ************************************************************************/
#include "pkt2.h"
extern Prm Par;

int byt3_to_word( char *data ) 
	
{
	     

    int i;

    /* Convert 3byte value to int  */

		    
    i = *data++;
    i = i << 8;
    i = i + *((uchar_t *) data++);
    i = i << 8;
    i = i + *(( uchar_t *) data++);
							            
    return (i);
	
 }

int parse_b3s2 ( 
    uchar_t *packet,
    ushort_t pkttype )
{

int yr, day, hr, min, sec;
double msec, ytime;
PktPar pack;
char stime[64];
int val;

/* Get parameters for current datatype  */

      if( !get_packet( pkttype, &pack) ) return -1;

      ytime = now();
      e2h( ytime, &yr, &day, &hr, &min, &msec);

      /* Get packet time  */

      val = (packet[14] >> 4) & 0x0f;
      val = val * 10;
      val += packet[14] & 0x0f;
      val = val * 10;
      val += (packet[15] >> 4) & 0x0f;
      day = val;   
      
      val = packet[15] & 0x0f;
      val = val * 10;
      val += (packet[16] >> 4) & 0x0f;
      hr = val;
										        
      val = packet[16] & 0x0f;
      val = val * 10;   
      val += (packet[17] >> 4) & 0x0f;
      min = val;
														         
      val = packet[17] & 0x0f;
      val = val * 10;
      val += (packet[18] >> 4) & 0x0f;
      sec = val;

   
      if (day > 366  || day < 1 ||
          hr  > 23   || hr  < 0 ||
          min > 59   || min < 0 ||
          sec > 59   || sec < 0 )  return 0;
   

      sprintf( stime, "%d%03d:%02d:%02d:%02d.0\0", yr, day, hr, min, sec );
      Par.time = str2epoch( (char *) &stime[0] );
     

      Par.packet = pack; 
      Par.hdrtype = ( int )decode( pack.hdrtype );

/* Data packet? Then get channel number */

      if( pkttype == B3S2SP )  {
	  Par.staid = 0;
          Par.chan = -1;
      }  else  {
         switch( packet[5] )  {
	    case 0x01:
		Par.chan = 1;
		break;

	    case 0x02:
		Par.chan = 2;
		break;

	    case 0x03:
		Par.chan = 3;
		break;

	 }
	 Par.staid = 1;
      }

      
      return (int) pkttype;
}

int read_b3s2_SP( 
    double time, 
    char *srcid, 
    uchar_t  *packet, 
    Packet **Pkt, 
    void *par )
{
    return 2;
}

/* Read Data packet  */

int read_b3s2_DP( 
    double time, 
    char *srcname, 
    uchar_t *packet, 
    Packet **Pkt, 
    void *par )
{

    struct DataPar *hdr;
    int doff,  i, j;
    PktChannel *achan;
    int npts, nbytes;
    int udata[20]; 
    static uchar_t data[80];
    char net[PKT_NAMESIZE], sta[PKT_NAMESIZE];
    int *adata;

    hdr = ( struct DataPar *) par;

    switch ( hdr->hdrtype ) {
     
       case BBA:
		 
           unstuff_BBAHdr( packet, Pkt, par );
           break;
				       
       default:
           elog_complain( 0, "can't recognize data packet header - %d\n", hdr->hdrtype );
           return 0;
    }

    nbytes = hdr->pktsize - hdr->doff;
    npts = nbytes / 3;
   
   memcpy( &data[0], packet + hdr->hdrsize + hdr->doff, 60 ); 

   for (i = 0, j = 0; i < nbytes; j++, i += 3)
      udata[j] = byt3_to_word(&data[i]);

				    
    parse_srcname( srcname, &net[0], &sta[0], 0, 0 );
    for( doff = 0, i = 0; i < (*Pkt)->nchannels; i++ )  {
      achan = (PktChannel *) gettbl((*Pkt)->chan, i) ;
      if ( achan == 0 ) {
          allot ( PktChannel *, achan, 1 ) ;
          achan->data = NULL;
          strcpy (achan->segtype, "V");
      }
      strcpy( achan->net,  net);
      strcpy( achan->sta,  sta);
      achan->time = time;
      achan->datatype = trINT;
      if (achan->data == NULL) {
      	  achan->nbytes = achan->nsamp * sizeof(int);
      	  allot ( int *, adata, achan->nbytes ) ;
      	  achan->data = adata;
      } else if (achan->nsamp * sizeof(int) > achan->nbytes) {
      	  achan->nbytes = achan->nsamp * sizeof(int);
      	  adata = achan->data;
      	  reallot ( int *, adata, achan->nbytes ) ;
      	  achan->data = adata;
      }
      memcpy (achan->data, (char *) &udata[doff], achan->nsamp * sizeof(int));
      doff += achan->nsamp;              
      settbl((*Pkt)->chan, i, achan ) ;
   }

 
    return 1; 
}


