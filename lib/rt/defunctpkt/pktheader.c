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
#include "defunctpkt.h"

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
      char sta[PKT_NAMESIZE], chan[64];
      int nbytes, psize, chanlen;
      short datatype, nsamp, nchan, chlen, hdrsize, hsiz;
      DPHdr dhdr;

      prehdr.pkttype = ( ushort_t) htons( Par.raw.pkttype);
      prehdr.hdrtype = ( ushort_t) htons(  hdrtype) ;
      prehdr.pktsiz =  htons(Par.packet.size); 
      
      nbytes = 0;
      switch( hdrtype )  {
          
	   case DPH :

		hsiz = sizeof( DPHdr  );
		prehdr.hdrsiz = htons( hsiz );
    		dhdr.prehdr = prehdr;
    		dhdr.nsamp = htonl(Par.packet.nsamp);
    		dhdr.nchan = htonl(Par.packet.nchan);
	        htonfp( &Par.packet.srate, &dhdr.samprate);
    		dhdr.doff = htonl(Par.packet.hdrsiz);
      		htondp( &Par.time, &dhdr.epoch);
    		if(strncmp(Par.packet.datatype, "s2", 2) == 0)  {
		    datatype = trSHORT;
		} else if(strncmp(Par.packet.datatype, "s4", 2) == 0 )  {
		    datatype = trINT;
	        } else if ( strncmp(Par.packet.datatype, "c0", 2) == 0 )  {
		    datatype = 0;
		}
		dhdr.datatype = htonl( (int)datatype );
      		strcpy( dhdr.net, Par.packet.net_type ); 
    
    		psize = hsiz + Par.packet.size;       
    
    		if( (nchan = stapar( (uchar_t *) *packet, &sta[0], &chan[0], &chanlen, &calib )) == 0) {
       		    elog_complain( 0, "Can't get stapar from CF.\n");
       		    return 0;
    		}
		
    	        htondp( &calib, &dhdr.calib );
    		strcpy( dhdr.sta, sta );
    		memset( (char *) &dhdr.channels[0], 0, 64);
    		memcpy( (char *) &dhdr.channels[0], chan, strlen(chan));

      		if( nchan == 1 )  {
          	    chan[strlen(chan)-1] = '\0';
          	    sprintf( srcname, "%s_%s%s/%s\0", dhdr.net, sta, chan, Par.packet.pkttype);
      		}  else
          	    sprintf( srcname, "%s_%s/%s\0", dhdr.net, sta, Par.packet.pkttype);
	       memcpy( (char *) &hdr[0], (char *) &dhdr, hsiz);
	    
               break;

          case BBA :

    	        phdr = &hdrbuf[0];	
    		memset( (char *) &chan[0], 0, 64);
		if( (nchan = (short ) stapar( (uchar_t *) *packet, &sta[0], &chan[0], &chanlen, &calib )) == 0) {
       		    elog_complain( 0, "Can't get stapar from CF.\n");
       		    return 0;
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
		datatype = ( short ) htonl( (int) datatype);
	        memcpy(phdr, &datatype, sizeof(short) );
                nbytes += sizeof(short);
		phdr += sizeof(short);
	        nsamp = (short) htonl( Par.packet.nsamp);
	        memcpy(phdr, &nsamp, sizeof(short) );
                nbytes += sizeof(short);
		phdr += sizeof(short);
	        nchan = (short) htonl(Par.packet.nchan); 
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
    		prehdr.hdrsiz = htons (hsiz); 
    		psize = hsiz + prehdr.pktsiz;
    
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
    		prehdr.hdrsiz = htons( hsiz ); 
    		psize = hsiz + prehdr.pktsiz;
		
	        memcpy( &hdr[0], (char *) &prehdr, sizeof( struct PreHdr ) );
	        memcpy( &hdr[sizeof( struct PreHdr )], (char *) &hdrbuf[0], nbytes );
      	
                sprintf( srcname, "%s_%d/%s\0",
		         Par.packet.net_type, Par.staid, Par.packet.pkttype );
		break;

          default:
            elog_complain(0, "hdr2packet():Unknown HDR type - %d. Can't prepend to a packet.\n",
                    hdrtype);
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

      memcpy(new_pkt+hsiz, (char *) *packet, prehdr.pktsiz );

      memcpy(*packet, new_pkt, psize);
 
      return psize;
}


/* $Id$ */
