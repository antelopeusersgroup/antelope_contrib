/*
 * SWAP.C
 *
 *  Byte swapping functions
 */

#include <string.h>
#include "swap.h"

void SwapShort( short *data )
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

void SwapInt( int *data )
{
   char temp;

   union {
      int i;
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


void SwapLong( long *data )
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

void SwapDouble( double *data )
{
   char temp;

   union {
       double d;
       char   c[8];
   } dat;

   dat.d = *data;
   temp     = dat.c[0];
   dat.c[0] = dat.c[7];
   dat.c[7] = temp;

   temp     = dat.c[1];
   dat.c[1] = dat.c[6];
   dat.c[6] = temp;

   temp     = dat.c[2];
   dat.c[2] = dat.c[5];
   dat.c[5] = temp;

   temp     = dat.c[3];
   dat.c[3] = dat.c[4];
   dat.c[4] = temp;
   *data = dat.d;
}

/**************************** swapWaveMsg ***************************
*       Byte-swap a univerals Waveform message in place.            *
*       Changes the 'datatype' field in the message header          *
*       Returns -1 if unknown data type, 0 elsewise                 *
*********************************************************************/


int WaveMsgMakeLocal( TRACE_HEADER* wvmsg )
{
   int dataSize;  /* flag telling us how many bytes in the data */
   char  byteOrder;
   long* longPtr;
   short* shortPtr;
   int i;

   /* See what sort of data it carries
    **********************************/
   dataSize=0;
   if ( strcmp(wvmsg->datatype, "s4")==0)
        {
        dataSize=4; byteOrder='s';
        }
   else if ( strcmp(wvmsg->datatype, "i4")==0)
        {
        dataSize=4; byteOrder='i';
        }
   else if ( strcmp(wvmsg->datatype, "s2")==0)
        {
        dataSize=2; byteOrder='s';
        }
   else if ( strcmp(wvmsg->datatype, "i2")==0)
        {
        dataSize=2; byteOrder='i';
        }
   else
        return(-1); /* We don't know this message type*/

#if defined( _SPARC )
   if (byteOrder =='i')
        {
        /* swap the header
        *****************/
        SwapInt( &(wvmsg->pinno) );
        SwapInt( &(wvmsg->nsamp) );
        SwapDouble( &(wvmsg->starttime) );
        SwapDouble( &(wvmsg->endtime) );
        SwapDouble( &(wvmsg->samprate) );

        /* Swap the data
         ***************/
        longPtr=(long*) ((char*)wvmsg + sizeof(TRACE_HEADER) ); /* pointer to first data byte */
        shortPtr=(short*) ((char*)wvmsg + sizeof(TRACE_HEADER) ); /* pointer to first data byte */
        for( i=0; i<wvmsg->nsamp; i++)
                {
                if(dataSize==2) SwapShort( &shortPtr[i] );
                if(dataSize==4) SwapLong(&(longPtr[i]) );
                }
        /* Re-write the data type field in the message
        **********************************************/
        if(dataSize==2) strcpy(wvmsg->datatype,"s2");
        if(dataSize==4) strcpy(wvmsg->datatype,"s4");
        }

#elif defined( _INTEL )
   if (byteOrder =='s')
        {
        /* swap the header
        *****************/
        SwapInt( &(wvmsg->pinno) );
        SwapInt( &(wvmsg->nsamp) );
        SwapDouble( &(wvmsg->starttime) );
        SwapDouble( &(wvmsg->endtime) );
        SwapDouble( &(wvmsg->samprate) );

        /* Swap the data
         ***************/
        longPtr=(long*) ((char*)wvmsg + sizeof(TRACE_HEADER) ); /* pointer to first data byte */
        shortPtr=(short*) ((char*)wvmsg + sizeof(TRACE_HEADER) ); /* pointer to first data byte */
        for( i=0; i<wvmsg->nsamp; i++)
                {
                if(dataSize==2) SwapShort( &shortPtr[i] );
                if(dataSize==4) SwapLong(&(longPtr[i]) );
                }
        /* Re-write the data type field in the message
        **********************************************/
        if(dataSize==2) strcpy(wvmsg->datatype,"i2");
        if(dataSize==4) strcpy(wvmsg->datatype,"i4");
        }
#else
        printf( "WaveMsgMakeLocal warning: _INTEL and _SPARC are both undefined." );
#endif
   return 0;
}

void SwapFloat( float *data )
{
   char temp;

   union {
      float f;
      char c[4];
   } dat;

   dat.f    = *data;
   temp     = dat.c[0];
   dat.c[0] = dat.c[3];
   dat.c[3] = temp;
   temp     = dat.c[1];
   dat.c[1] = dat.c[2];
   dat.c[2] = temp;
   *data    = dat.f;
}

