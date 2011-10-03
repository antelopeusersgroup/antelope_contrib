/* Routine to stuff Datascope orb packet-channels into Iceworm trace-buf packets 
 *
 * Kent Lindquist, with instructions from Danny Harvey
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * March, 1997
 */

#include <sys/types.h>
#include <netinet/in.h>
 
#include "defunctpkt.h"

#define STREQ(a, b) (strcmp((a), (b)) == 0)

#define IW_ORB_TRACE_HEADER_SIZE 16
  
double htond();
double ntohd();
   
int mystuff_iw_tracebuf( quality, pinno, pktchan, packet, nbytes, bufsiz  )
unsigned short quality;
unsigned short pinno;
struct PktChannel *pktchan;
char **packet;
int *nbytes;
int *bufsiz;

{
    struct PreHdr prehdr;    
    int i;
    int	bytes_needed;
    char *datap;
    unsigned short nsamp;
    float samprate, calib;
    char datatype[4];
    char *ptr;

    prehdr.hdrtype = htonl (IWH);
    prehdr.pkttype = htonl (IWTB);
    prehdr.hdrsiz = htonl(sizeof( struct PreHdr ) + IW_ORB_TRACE_HEADER_SIZE );

    samprate = pktchan->samprate;
    calib = pktchan->calib;
    nsamp = pktchan->nsamp;
    ntohfp (&calib, &calib);
    ntohfp (&samprate, &samprate);

    if( pktchan->datatype != trINT )
    {
	elog_complain( 0, "stuff_iw_tracebuf handles only the trINT datatype\n" );
	return( 0 );
    }
    else
    {
	strcpy( datatype, "s4" );
    }

    bytes_needed = prehdr.hdrsiz + pktchan->nsamp * sizeof( int );
    prehdr.pktsiz = htons(bytes_needed);

    if( *packet == NULL )
    {
	allot( char *, *packet, bytes_needed );
	*nbytes = bytes_needed;
	*bufsiz = bytes_needed;
    }
    else if( *bufsiz < bytes_needed )
    {
	reallot( char *, *packet, bytes_needed );
	*nbytes = bytes_needed;
	*bufsiz = bytes_needed;
    }
    else
    {
	*nbytes = bytes_needed;
    }

    datap = *packet + prehdr.hdrsiz;
    ptr = *packet;

    pinno = htons(pinno);
    nsamp = htons(nsamp);
    quality = htons(quality);

    memcpy( ptr, &prehdr, sizeof( struct PreHdr ) );
    ptr += sizeof( struct PreHdr );
    memcpy( ptr, &samprate, sizeof(float));
    ptr += sizeof( float );
    memcpy( ptr, &calib, sizeof(float));
    ptr += sizeof( float );
    memcpy( ptr, &pinno, sizeof(unsigned short));
    ptr += sizeof( unsigned short );
    memcpy( ptr, &nsamp, sizeof(unsigned short));
    ptr += sizeof( unsigned short );
    memcpy( ptr, datatype, 2);
    ptr += 2;
    memcpy( ptr, &quality, sizeof(unsigned short));
    ptr += sizeof( unsigned short );
    memcpy( datap, pktchan->data, pktchan->nsamp * sizeof( int ) );

    return 1; 
}
   
int stuff_iwc_tracebuf( quality, pinno, pktchan, packet, nbytes, bufsiz  )
unsigned short quality;
unsigned short pinno;
struct PktChannel *pktchan;
char **packet;
int *nbytes;
int *bufsiz;

{
    struct PreHdr prehdr;    
    int i;
    int	bytes_needed;
    char *datap;
    unsigned short nsamp;
    float samprate, calib;
    char datatype[4];
    char *ptr;
    int *data, *idata;
    unsigned char *buf=NULL;
    int nout, bsize;

    prehdr.hdrtype = htonl (IWH);
    prehdr.pkttype = htonl (IWTB);
    prehdr.hdrsiz = htons(sizeof( struct PreHdr ) + IW_ORB_TRACE_HEADER_SIZE );

    samprate = pktchan->samprate;
    calib = pktchan->calib;
    nsamp = pktchan->nsamp;
    ntohfp (&calib, &calib);
    ntohfp (&samprate, &samprate);

    if( pktchan->datatype != trINT )
    {
	elog_complain( 0, "stuff_iw_tracebuf handles only the trINT datatype\n" );
	return( 0 );
    }
    else
    {
	strcpy( datatype, "gc" );
    }

    data = (int *) malloc (pktchan->nsamp*sizeof(int));
    if (data == NULL) {
    	elog_log(0, "stuff_iwc_tracebuf: malloc() error.\n");
    	return 0;
    }

    idata = (int *) pktchan->data;
    data[0] = idata[0];
    for (i=1; i<pktchan->nsamp; i++) data[i] = idata[i] - idata[i-1];

    if (gencompress (&buf, &nout, &bsize, data, pktchan->nsamp, 25) < 0) {
    	elog_log(0, "stuff_iwc_tracebuf: gencompress() error.\n");
    	return 0;
    }

    free (data);

    bytes_needed = prehdr.hdrsiz + nout;

    prehdr.pktsiz = htons(bytes_needed);

    if( *packet == NULL )
    {
	allot( char *, *packet, bytes_needed );
	*nbytes = bytes_needed;
	*bufsiz = bytes_needed;
    }
    else if( *bufsiz < bytes_needed )
    {
	reallot( char *, *packet, bytes_needed );
	*nbytes = bytes_needed;
	*bufsiz = bytes_needed;
    }
    else
    {
	*nbytes = bytes_needed;
    }

    datap = *packet + prehdr.hdrsiz;
    ptr = *packet;

    pinno = htons(pinno);
    nsamp = htons(nsamp);
    quality = htons(quality);

    memcpy( ptr, &prehdr, sizeof( struct PreHdr ) );
    ptr += sizeof( struct PreHdr );
    memcpy( ptr, &samprate, sizeof(float));
    ptr += sizeof( float );
    memcpy( ptr, &calib, sizeof(float));
    ptr += sizeof( float );
    memcpy( ptr, &pinno, sizeof(unsigned short));
    ptr += sizeof( unsigned short );
    memcpy( ptr, &nsamp, sizeof(unsigned short));
    ptr += sizeof( unsigned short );
    memcpy( ptr, datatype, 2);
    ptr += 2;
    memcpy( ptr, &quality, sizeof(unsigned short));
    ptr += sizeof( unsigned short );
    memcpy( datap, buf, nout ); 

    free (buf);

    return 1; 
}

/* $Id$ */
