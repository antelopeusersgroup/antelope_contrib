/* $Name $Revision$ $Date$  */ 
/**************************************************************************
 *
 *    read_psclDP.c  
 *  
 *    read PASCAL Data packets; uncompress if neccessary.
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 *
 ***********************************************************************/
#include "defunctpkt.h"

int read_psclDP( 
    double time, 
    char *srcname, 
    uchar_t *packet, 
    Packet **Pkt, 
    void *par )
{

    int i, doff;
    struct DataPar *hdr ;
    PktChannel *achan;
    int hdrsize, nbytes;
    int *adata;
    int udata[4096];
    short sdata[4096]; 
    char data[4096], *cptr;
    char net[PKT_NAMESIZE], 
         chan[PKT_NAMESIZE],
         sta[PKT_NAMESIZE];

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
														    
    doff = hdr->hdrsize + hdr->doff;
    nbytes = hdr->nsamp*sizeof(int);
   
    /* Get net and sta names from srcname  */

    net[0] = '\0'; sta[0] = '\0'; 
    parse_srcname( srcname, &net[0], &sta[0], &chan[0], 0 );

    /* Get data from data packet  */

    switch ( hdr->datatype )  {
	
	case trINT: 
            memcpy ( (char *) &udata[0], packet + doff, nbytes );
	    break;
	
	case trSHORT: 
            memcpy ( (char *) &sdata[0], packet + doff, nbytes );
	    for( i = 0; i < nbytes/2; i++ )
	      udata[i] = sdata[i];
	    break;
	
	case 0: 
	    cptr = &data[0];
            memcpy ( cptr, (char *) (packet + doff), 1024 - hdr->doff );
            if( (nbytes = pscl_ucompress( &cptr, hdr->nsamp, hdr->doff ) )<= 0 )  {
                elog_complain( 0, "Can't uncompress PSCL data for %s_%s_%s.\n", net, sta, chan );
                return 0;
            }
	    memcpy( (char *) &udata[0], cptr, nbytes ) ;
	    break;
	
    }


    /* Fill Packet structure with values for all existing channels  */
    
    for( doff = 0, i = 0; i < (*Pkt)->nchannels; i++)  {
      achan = (PktChannel *) gettbl((*Pkt)->chan, i) ;
      if ( achan == 0 ) {
          allot ( PktChannel *, achan, 1 ) ;
	  achan->data = NULL;
	  strcpy (achan->segtype, "V");
      }
      strcpy( achan->net,  net);
      strcpy( achan->sta,  sta);
      strcpy( achan->chan,  chan);
      achan->time = time; 
      achan->datatype = trINT;
      achan->nbytes = achan->nsamp * sizeof(int);
      if (achan->data == NULL) {
          allot ( int *, adata, achan->nbytes ) ;
          achan->data = adata;
      } else  {
          adata = achan->data;
          reallot ( int *, adata, achan->nbytes ) ;
          achan->data = adata;
      }
      memcpy (achan->data, (char *) &udata[doff], achan->nbytes );
      doff += achan->nsamp;   
      settbl((*Pkt)->chan, i, achan ) ;
   }  

 
    return 1; 
}



/* $Id$ */
