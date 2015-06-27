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
#include "defunctpkt.h"

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

	case DPH:
          
           unstuff_DPhdr( packet, Pkt, par ); 
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


