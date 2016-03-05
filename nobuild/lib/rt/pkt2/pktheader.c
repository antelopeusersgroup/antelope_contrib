/* %W% %W%  */
/******************************************************************
 *
 *  pktheader.c
 *
 *  Packat header manipulations. 
 *
 *
 *  Author: Marina Harkins-Glushko
 *  	    UCSD, IGPP
 *	    glushko@ucsd.edu
 *
 ********************************************************************/
#include "pkt2.h"

static char *new_pkt = 0;
extern Prm Par;

int  hdr2packet( 
    char **packet,
    int  hdrtype,
    char *srcname )

{

      double calib, time;
      float fcalib, samprate;
      struct PreHdr prehdr;
      uchar_t hdr[512] ;
      uchar_t hdrbuf[512];
      uchar_t *phdr;
      char sta[PKT_NAMESIZE], chan[512];
      int nbytes, psize, chanlen;
      short datatype, nsamp, nchan, chlen, hdrsize, hsiz;

      prehdr.pkttype = ( ushort_t) htons( Par.raw.pkttype);
      prehdr.hdrtype = ( ushort_t) htons(Par.hdrtype) ;
      prehdr.pktsiz =  htons(Par.packet.size); 
      
      nbytes = 0;
      switch( Par.hdrtype )  {
          
          case BBA :

    	        phdr = &hdrbuf[0];	
    		memset( (char *) &chan[0], 0, 64);
                /* overwrite chan # chan names and staname  for DA* packets */
	        if(Par.raw.pkttype == DAAB || Par.raw.pkttype == DABC || 
		   Par.raw.pkttype == DACD || Par.raw.pkttype == DADE)  {
		    nchan = Par.packet.nchan;
	            strcpy(&chan[0], Par.chnames);
		    chanlen = strlen(chan);
		    strcpy(sta, Par.staname);
                    calib = Par.calib;
	        }  else { 
		    if((nchan = (short ) stapar((uchar_t *) *packet, &sta[0], &chan[0], &chanlen, &calib )) == 0) {
       		        elog_complain( 0, "Can't get stapar from CF.\n");
       		        return 0;
    		    }
                } 
		fcalib = ( float) calib ;
		htonfp( &fcalib, &fcalib );
	        memcpy(phdr, &fcalib, sizeof(float) );
                nbytes += sizeof(float);
		phdr +=sizeof(float);
		htonfp( &Par.packet.srate, &samprate );
	        memcpy(phdr, &samprate, sizeof(float) );
                nbytes += sizeof(float);
		phdr += sizeof(float);
    		if(strncmp(Par.packet.datatype, "s2", 2) == 0)  {
		    datatype = (short) trSHORT;
		} else if(strncmp(Par.packet.datatype, "s4", 2) == 0 )  {
		    datatype = (short) trINT;
	        } else if ( strncmp(Par.packet.datatype, "c0", 2) == 0 )  {
		    datatype = 0;
		}
		datatype = htons(datatype);
	        memcpy(phdr, &datatype, sizeof(short) );
                nbytes += sizeof(short);
		phdr += sizeof(short);
	        nsamp = (short) htons( Par.packet.nsamp);
	        memcpy(phdr, &nsamp, sizeof(short) );
                nbytes += sizeof(short);
		phdr += sizeof(short);
	        nchan = (short) htons(Par.packet.nchan); 
	        memcpy(phdr, &nchan, sizeof(short) );
                nbytes += sizeof(short);
		phdr += sizeof(short);
		hdrsize = htons( Par.packet.hdrsiz);
		memcpy(phdr, &hdrsize, sizeof(short) );
                nbytes += sizeof(short);
		phdr += sizeof(short);
		chlen = htons( (short) chanlen );
		memcpy(phdr, &chlen, sizeof(short) );
                nbytes += sizeof(short);
		phdr += sizeof(short);
	        memcpy(	phdr, &chan[0], chanlen );
		nbytes += chanlen;
		phdr += chanlen;

    		hsiz = nbytes + sizeof( struct PreHdr ); 
    		psize = hsiz + ntohs(prehdr.pktsiz);
    		prehdr.hdrsiz = htons (hsiz); 
    
	        memcpy( &hdr[0], (char *) &prehdr, sizeof( struct PreHdr ) );
	        memcpy( &hdr[sizeof( struct PreHdr )], (char *) &hdrbuf[0], nbytes );
      	
		if( nchan == 1 )  {
          	    chan[strlen(chan)-1] = '\0';
          	    sprintf( srcname, "%s_%s%s/%s\0", Par.packet.net_type, 
		                            sta, chan, Par.packet.pkttype);
      		}  else
          	    sprintf( srcname, "%s_%s/%s\0",Par.packet.net_type, 
		                                sta, Par.packet.pkttype);
               break;

	   case IPH :

    		if( ! read_raw( 0, 0, (uchar_t *)*packet, 0, 0, Par.raw.read ))  {
       		    elog_complain(0, "Can't read IP|SP raw packet - %d\n", 
		                                       Par.raw.pkttype);
       	       	    return 0;
    		}
    	        phdr = &hdrbuf[0];	
	        memcpy(phdr, &Par.time, sizeof(double) );
                nbytes += sizeof(double);
		phdr += sizeof(double);
	        memcpy(	phdr, Par.packet.net_type, PKT_NAMESIZE );
		nbytes += PKT_NAMESIZE;
		phdr +=  PKT_NAMESIZE;
      		
    		hsiz = nbytes + sizeof( struct PreHdr ); 
    		psize = hsiz + ntohs(prehdr.pktsiz);
    		prehdr.hdrsiz = htons( hsiz ); 
		
	        memcpy( &hdr[0], (char *) &prehdr, sizeof( struct PreHdr ) );
	        memcpy( &hdr[sizeof( struct PreHdr )], (char *) &hdrbuf[0], nbytes );
      	
                sprintf( srcname, "%s_%d/%s\0",
		         Par.packet.net_type, Par.staid, Par.packet.pkttype );
		break;

          default:
            elog_complain(0, "hdr2packet():Unknown HDR type - %d. Can't prepend to a packet.\n",
                    Par.hdrtype);
            return 0;
      }  
 
/* Build a data packet  */
    
      if( new_pkt == 0 )  
         allot( char *, new_pkt, psize );
      else  
         reallot( char *, new_pkt, psize );

 /* Add a header */
 
      memcpy ( new_pkt, &hdr[0],  hsiz );

/* Add data  */

      memcpy(new_pkt+hsiz, (char *) *packet, ntohs(prehdr.pktsiz) );

      memcpy(*packet, new_pkt, psize);
 
      return psize;
}


/* $Id$ */
