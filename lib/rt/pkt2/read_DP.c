/* @(#)read_DP.c	1.8 04/13/97  */ 
/******************************************************************
 *
 *  read_DP.c 
 *  
 *  Read ANZA & BBA Data & Status packets
 *  
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 *
 ********************************************************************/
#include "pkt2.h"

int read_DP( 
    double time, 
    char *srcname, 
    uchar_t *packet, 
    Packet **Pkt, 
    void *par )
{

    struct DataPar *hdr;
    int doff,  i;
    PktChannel *achan;
    int nbytes, hdrsize;
    int udata[16384]; 
    short sdata[16384]; 
    uchar_t data[16384];
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
    doff = hdr->hdrsize + hdr->doff;
   
    /* Get data from data packet; uncompress if neccessary  */

    switch ( hdr->datatype  )  {
	
	case trINT: 
            memcpy ( (char *) &udata[0], packet + doff , nbytes );
	    break;
	
	case trSHORT: 
            memcpy ( (char *) &sdata[0], packet + doff, nbytes );
	    for( i = 0; i < nbytes/2; i++ )
	      udata[i] = sdata[i];
	    break;
	
	case 0: 
            memcpy( (char *) &data[0], packet + doff, nbytes );
            if( ucsd_ucompress( &udata[0], (char *) &data[0], hdr->nsamp, 
	                                    (*Pkt)->nchannels ) <= 0 )  {
                elog_complain( 0, "Can't uncompress EDP data.\n");
                return 0;
            }  
	    break;
	
    }

    /* Get net & sta name from srcname   */

    parse_srcname( srcname, &net[0], &sta[0], 0, 0 );

    /* Fill Packet structure with values of all existing  channels  */

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
      achan->nbytes = achan->nsamp * sizeof(int);
      if (achan->data == NULL) {
      	  allot ( int *, adata, achan->nbytes ) ;
      	  achan->data = adata;
      } else {
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

/* Routine which will read orb data type of NEW_BBA;
   Packet has 'orb' header + common_raw header.
   Each packets may contain multichannel data wich 
   are organized in sub-packets in the following form:
 
     uchar      chid;            -- channel ID=nchan  
     uchar      gain;            -- channel gain    
     ushort     chbytes          -- number of bytes of data for this channel,
                                    excluding 'channel' header 
     uchar      data[chbytes]    -- data for second channel 
*/
 
#define CHDATA_OFF (4)  /* size of the pre-channel header */
#define CHDATA_LEN_OFF (2) /* offset to a "channel data size" value */
#define STAID_OFF     (6)  /* offset to sta ID in a common packet header */
int read_newbba_DP( 
    double time, 
    char *srcname, 
    uchar_t *packet, 
    Packet **Pkt, 
    void *par )
{

    struct DataPar *hdr;
    int doff,  i;
    PktChannel *achan;
    int nbytes, hdrsize;
    int udata[16384]; 
    short sdata[16384]; 
    uchar_t data[16384];
    char net[PKT_NAMESIZE], 
         sta[PKT_NAMESIZE], 
         pkttype[PKT_NAMESIZE];
    char *chname = 0;
    int *adata;
    int staid=0, chid=0, chdatalen_off=0;

    hdr = (struct DataPar *) par;
    
    /* New BBA packets will have ONLY BBA header */
    unstuff_BBAHdr(packet, Pkt, par); 
    
    /* Get net & sta name from srcname   */

    parse_srcname(srcname, &net[0], &sta[0], 0, pkttype);
    
    /* get staid from a raw packets */
    doff = hdr->hdrsize + STAID_OFF;
    staid = packet[doff]*256 + packet[doff+1];

    /* find offset to the data */
    doff = hdr->hdrsize + hdr->doff + CHDATA_OFF;
    chid = packet[hdr->hdrsize + hdr->doff];
    chdatalen_off = hdr->hdrsize + hdr->doff + CHDATA_LEN_OFF;
    nbytes = packet[chdatalen_off]*256 + packet[chdatalen_off+1];
   
    /* Get data from data packet; uncompress if neccessary  */
    /* New BBA packets will have ONLY compressed data */
    /* Fill Packet structure with values of all existing  channels */

    for(i = 0; i < (*Pkt)->nchannels; i++)  {
      achan = (PktChannel *) gettbl((*Pkt)->chan, i) ;
      if (achan == 0) {
          allot (PktChannel *, achan, 1) ;
          achan->data = NULL;
          strcpy (achan->segtype, "V");
      }
      strcpy(achan->net,  net);
      strcpy(achan->sta,  sta);

      achan->time = time;
      achan->datatype = trINT;
      achan->nbytes = achan->nsamp * sizeof(int);
      if (achan->data == NULL) {
          allot (int *, adata, achan->nbytes) ;
          achan->data = adata;
      } else {
          adata = achan->data;
          reallot (int *, adata, achan->nbytes) ;
          achan->data = adata;
      }

      switch ( hdr->datatype  )  {
        
        case trINT: 
            memcpy ( (char *) &udata[0], packet + doff , nbytes );
            break;
        
        case trSHORT: 
            memcpy ( (char *) &sdata[0], packet + doff, nbytes );
            for( i = 0; i < nbytes/2; i++ )
              udata[i] = sdata[i];
            break;
        
        case 0: 
            memcpy( (char *) &data[0], packet + doff, nbytes );
            if(ucsd_ucompress(&udata[0], (char *) &data[0], hdr->nsamp, 1) <= 0)  {
                 elog_complain(0, "Can't uncompress EDP data.\n");
                 return 0;
            }
	    break;
      }
      memcpy (achan->data, (char *) &udata[0], achan->nsamp * sizeof(int));
      settbl((*Pkt)->chan, i, achan) ;
      /* adjust data pointer to the next channel packet */
      doff += nbytes; 
      /* get ch ID for this channel packet */
      chid = packet[doff];
      /* get size of data for current channel */
      nbytes = packet[doff+CHDATA_LEN_OFF]*256 + packet[doff+CHDATA_LEN_OFF+1];      
      /* set offset to the data itself */
      doff += CHDATA_OFF;             
   }

    return 1; 
}

