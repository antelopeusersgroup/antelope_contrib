/*
   SWAP.C

   Byte swapping functions
*/

#include <string.h>
#include "data_buf.h"

void SwapShort( data )
short *data;
{
   char temp;

   union {
      short i;
      char  c[2];
   } dat;

   dat.i    = *data;
   temp     = dat.c[0];
   dat.c[0] = dat.c[1];
   dat.c[1] = temp;
   *data    = dat.i;
}


void SwapLong( data )
long *data;
{
   char temp;

   union {
      long i;
      char c[4];
   } dat;

   dat.i    = *data;
   temp     = dat.c[0];
   dat.c[0] = dat.c[3];
   dat.c[3] = temp;
   temp     = dat.c[1];
   dat.c[1] = dat.c[2];
   dat.c[2] = temp;
   *data    = dat.i;
}


void SwapTraceBuf( char *buf )
{
   WF_HEADER wf;
   int       nbyte;

/* Swap the buffer header
   **********************/
   memcpy( &wf, buf, sizeof(WF_HEADER) );
   SwapLong ( &wf.tssec );
   SwapLong ( &wf.tsmic );
   SwapLong ( (long *)&wf.first_scan );
   SwapShort( &wf.series );
   SwapShort( &wf.sample_dt );
   SwapShort( &wf.nsample );
   SwapShort( (short *)&wf.errors );
   SwapShort( &wf.nchan );
   SwapShort( &wf.nscan );
   SwapShort( &wf.netid );
   memcpy( buf, &wf, sizeof(WF_HEADER) );

/* Swap the pin numbers and data samples
   *************************************/
   nbyte = 2 * wf.nchan * (wf.nscan + 1);
   swab( &buf[sizeof(WF_HEADER)], &buf[sizeof(WF_HEADER)], nbyte );
}
