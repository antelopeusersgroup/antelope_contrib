#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include "mygcf.h"

/* Modified from Murray McGowan's original by K. Lindquist
 * Geophysical Institute
 * University of Alaska
 * 1998
 */

static WORD  
swap2( WORD i ) 
{ 
	return (i & 0xFF) << 8 | i >> 8; 
}

static DWORD 
swap4( DWORD i ) 
{ 
	return swap2((WORD)(i & 0xFFFF)) << 16 | swap2((WORD)(i >> 16)); 
}

#define SECONDS_PER_DAY (24*60*60)

/* Convert a guralp datecode to epoch time */

double
datecode2epoch( long datecode )
{
	double	epoch;
	int	days_since_Nov_17_1989;
	int	seconds_since_midnight;

	epoch = 627264000.0; /* Seconds from Midnight 1/1/70 to Nov. 17, 1989 */

	days_since_Nov_17_1989 = datecode >> 17;
	seconds_since_midnight = datecode & 0x0001FFFF;

	epoch += days_since_Nov_17_1989 * SECONDS_PER_DAY;
	epoch += seconds_since_midnight;

	return epoch;
}

/*  Convert a Base36 number into a character string */

char *
decodebase36( DWORD v, char *s, int space ) {
	int imed;
	int i;

	memset( s, 0, space );

	for ( i=space-1; i>=0; i-- ) {
		imed = v % 36;
		if ( imed > 9 ) imed += 7;
		s[i] = imed + '0';
		v /= 36;
	}

	return( s );
}

/*  Byte-swap an entire GCF block. Block must have 24 bit differences 
    expanded to 32 bits, if necessary (i.e. call Expandblock first) */

void
swapGCFblock( TGCFblock *b ) 
{
	int i;

	b->sysid = swap4( b->sysid );
	b->streamid = swap4( b->streamid );
	b->datecode = swap4( b->datecode );

	if ( b->samplerate ) {

		b->c.wf.fic = swap4( b->c.wf.fic );
		b->c.wf.ric = swap4( b->c.wf.ric );

		switch ( b->compressioncode ) {

		case 1 : for ( i=0; i < b->numrecords; i++ )
                 	b->c.wf.data.D[i] = swap4(b->c.wf.data.D[i]);
               		break;

		case 2 : for ( i=0; i < ( b->numrecords*2 ); i++ )
                 	b->c.wf.data.W[i] = swap2( b->c.wf.data.W[i] );
               		break;
		}
	}
}

/* difference returns difference 'i' from a given data block */

long
difference( TGCFblock *b, int i ) 
{
	if ( !b->samplerate ) return 0;
	switch ( b->compressioncode ) {
		case 1 : return b->c.wf.data.D[i];
		case 2 : return b->c.wf.data.W[i];
		case 4 : return b->c.wf.data.B[i];
		default : return 0;
	}
}
 
 
/* add24bit Adds two values together. If the result goes outside 24 bit
   range, change sign of second value, and repeat add operation.
   (required since two 24 bit numbers can have a 25 bit difference.
   Only 24 bits are transmitted, so the sign bit is lost. This must be
   determined at decompression time.) */

long
add24bit( long val, long diff )
{
	int res;

	res = val + diff;

	if ( ( res > MAX24 ) || ( res < MIN24 ) ) {

		diff = diff ^ 0xFF000000;
		res = val + diff;
	}

	return res;
}
 
int
decompressGCF_UDPmessage( char *buffer, TGCFpacket *pkt )
{
	TMessageBlock msg; 
	char	*bufptr;
	int	byteorder;
	int	swap;
	int	val;
	int	ric;
	int	i;

	bufptr = buffer;

	memcpy( &msg.block, buffer, sizeof( TGCFblock ) );
	bufptr += sizeof( TGCFblock );

	memcpy( &msg.ver, bufptr++, 1 );

	memcpy( &msg.source, bufptr, 32 );
	msg.source[32] = '\0';
	bufptr += 33;

	memcpy( &msg.blockseq, bufptr, sizeof( WORD ) );
	bufptr += sizeof( WORD );

	memcpy( &msg.byteorder, bufptr, sizeof( BYTE ) );
	bufptr += sizeof( BYTE );
	byteorder = msg.byteorder; 

	switch( byteorder ) {

		case INTEL_BYTEORDER:

			#if defined( _BIG_ENDIAN )
				swap = 1;
			#else 
				swap = 0;
			#endif

			break;

		case MOTOROLA_BYTEORDER:

			#if defined( _BIG_ENDIAN )
				swap = 0;
			#else 
				swap = 1;
			#endif

			break;

		default:

			fprintf( stderr, "Bad byte-order code\n" );
			return( -1 );
	}

	if( swap ) {
		msg.blockseq = swap2( msg.blockseq );
		swapGCFblock( &msg.block );
	}

	if( msg.block.samplerate == 0 ) {/* 0 indicates state-of-health packet*/
		return -1;
	}

	decodebase36( msg.block.sysid, pkt->system_id, 6 );
	decodebase36( msg.block.streamid, pkt->stream_id, 6 );
	pkt->system_id[6] = 0;
	pkt->stream_id[6] = 0;
	pkt->samprate = msg.block.samplerate;
	pkt->compressioncode = msg.block.compressioncode;
	pkt->nrec = msg.block.numrecords;
	pkt->reserved = msg.block.reserved1;
	pkt->epoch = datecode2epoch( msg.block.datecode );
	pkt->nsamp = msg.block.numrecords * msg.block.compressioncode;

	/* This truncation will of course throw off the RIC for 
	   packets with non-integral-second length */

	pkt->nsamp = (long) ( floor( (double) pkt->nsamp /
				     (double) pkt->samprate ) *
			      (double) pkt->samprate );

	if( pkt->nsamp != msg.block.numrecords * msg.block.compressioncode ) {
		fprintf( stderr, 
			"truncating packet from %f to %f seconds, %s %s timestamp %f\n",
			(double) ((double)msg.block.numrecords *
			msg.block.compressioncode/(double)pkt->samprate),
			(double) ((double)pkt->nsamp / (double)pkt->samprate),
			pkt->system_id,
			pkt->stream_id,
			pkt->epoch );
	}

	val = msg.block.c.wf.fic; 
	
	for( i=0; i< pkt->nsamp; i++ ) {

		val = add24bit( val, difference( &msg.block, i ) );
		pkt->data[i] = val;
	}

	if( msg.block.numrecords < MAX_NUM_RECORDS ) {

		ric = msg.block.c.wf.data.D[msg.block.numrecords];
		if( swap ) ric = swap4( ric );

	} else {

		ric = msg.block.c.wf.ric;
	}

	if( val != ric ) fprintf( stderr, 
		"Final sample value %d, doesn't match RIC %d, %s %s timestamp %f\n",
		val,
		ric, 
		pkt->system_id,
		pkt->stream_id,
		pkt->epoch );

	return 0;
}
