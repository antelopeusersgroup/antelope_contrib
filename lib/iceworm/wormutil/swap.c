
/*
 *   THIS FILE IS UNDER RCS - DO NOT MODIFY UNLESS YOU HAVE
 *   CHECKED IT OUT USING THE COMMAND CHECKOUT.
 *
 *    $Id$
 *
 *    Revision history:
 *     $Log$
 *     Revision 1.3  2003/06/01 08:25:40  lindquis
 *     Upgrade Iceworm libraries to Earthworm6.2. Add some rudimentary man
 *     pages. Preparation for the rewritten ew2orb.
 *
 *     Revision 1.5  2002/06/18 18:33:55  davidk
 *     fixed the sanity check of the header in WaveMsgMakeLocal().
 *     It contained a bug that made it useless.
 *
 *     Revision 1.4  2002/06/11 23:08:17  davidk
 *     Paul Friberg submitted a fix to WaveMsgMakeLocal, that copies the
 *     TRACE_HEADER to local variables to avoid byte alignment problems
 *     before examining the header.  (Bug was introduced by most
 *     recent change by davek that included code which examined the
 *     contents of the header without 8-byte aligning data.
 *
 *     Revision 1.3  2002/03/20 22:13:28  davidk
 *     Modified WaveMsgMakeLocal():
 *      Function now converts the header, performs a checksum on the header,
 *       and then if successful converts the binary data portion of the tracebuf.
 *      Function now returns -2 if the header checksum conversion fails.
 *      Function now returns -1 if _INTEL or _SPARC is not defined.
 *       (Formerly, no conversion was done and success was returned.)
 *
 *     Revision 1.2  2000/06/22 17:49:33  kohler
 *     Modified by WMK to allow in-place swapping of non-byte-alligned data.
 *
 *     Revision 1.1  2000/02/14 18:51:48  lucky
 *     Initial revision
 *
 *
 */

/*
 * SWAP.C
 *
 *  Byte swapping functions
 */

#include <string.h>
#include <swap.h>

void SwapShort( short *data )
{
   char temp;

   union {
      char  c[2];
   } dat;

   memcpy( &dat, data, sizeof(short) );
   temp     = dat.c[0];
   dat.c[0] = dat.c[1];
   dat.c[1] = temp;
   memcpy( data, &dat, sizeof(short) );
   return;
}

void SwapInt( int *data )
{
   char temp;

   union {
      char c[4];
   } dat;

   memcpy( &dat, data, sizeof(int) );
   temp     = dat.c[0];
   dat.c[0] = dat.c[3];
   dat.c[3] = temp;
   temp     = dat.c[1];
   dat.c[1] = dat.c[2];
   dat.c[2] = temp;
   memcpy( data, &dat, sizeof(int) );
   return;
}


void SwapLong( long *data )
{
   char temp;

   union {
      char c[4];
   } dat;

   memcpy( &dat, data, sizeof(long) );
   temp     = dat.c[0];
   dat.c[0] = dat.c[3];
   dat.c[3] = temp;
   temp     = dat.c[1];
   dat.c[1] = dat.c[2];
   dat.c[2] = temp;
   memcpy( data, &dat, sizeof(long) );
   return;
}

void SwapDouble( double *data )
{
   char temp;

   union {
       char   c[8];
   } dat;

   memcpy( &dat, data, sizeof(double) );
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
   memcpy( data, &dat, sizeof(double) );
   return;
}


/**************************** swapWaveMsg ***************************
*       Byte-swap a univerals Waveform message in place.            *
*       Changes the 'datatype' field in the message header          *
*       Returns -1 if unknown data type.                            *
*       Returns -1 if _SPARC or _INTEL not defined.                 *
*       Returns -2 if checksumish calculation of header fails.      *
*       Elsewise (SUCCESS) returns 0.                               *
*********************************************************************/

int WaveMsgMakeLocal( TRACE_HEADER* wvmsg )
{
   int dataSize;  /* flag telling us how many bytes in the data */
   char  byteOrder;
   long* longPtr;
   short* shortPtr;
   int i;
   int nsamp;
   double samprate,starttime,endtime;
   double tShouldEnd; 
   double dFudgeFactor;

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

   /* SWAP the header (if neccessary) */
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
   }
#else
        printf( "WaveMsgMakeLocal warning: _INTEL and _SPARC are both undefined." );
        return(-1);
#endif


/* perform a CheckSumish kind of calculation on the header 
    ensure that the tracebuf ends within 5 samples of the given endtime.
    DK 2002/03/18
   *******************************************************************/

/* moved nsamp memcpy to here to avoid byte-alignment with next statement */
  memcpy( &nsamp, &wvmsg->nsamp, sizeof(int) );
  memcpy( &samprate, &wvmsg->samprate, sizeof(double) );
  memcpy( &starttime, &wvmsg->starttime, sizeof(double) );
  memcpy( &endtime, &wvmsg->endtime, sizeof(double) );

  tShouldEnd = starttime + ((nsamp - 1) / samprate);
  dFudgeFactor = 5.0 / samprate;
  if(tShouldEnd != endtime)
  {
    /* DK 20020618  Realized the previous if was essentially if(0).
       This is supposed to be a simple sanity check to ensure that the
       endtime is within 5 samples of where it should be.  We're not
       trying to be judgemental here, we're just trying to ensure that
       we protect ourselves from complete garbage, so that we don't segfault
       when allocating samples based on a bad nsamp
     *************************************************************************/
    if(endtime < (tShouldEnd - dFudgeFactor)  ||  endtime > (tShouldEnd + dFudgeFactor))
    {
      /* logit("","WaveMsgMakeLocal():  found funky packet with suspect header values!!\n"); */
      return(-2);
    }
  }
  


   /* SWAP the data (if neccessary) */
#if defined( _SPARC )

   if (byteOrder =='i')
   {
/* Swap the data.  */
        longPtr=(long*) ((char*)wvmsg + sizeof(TRACE_HEADER) );
        shortPtr=(short*) ((char*)wvmsg + sizeof(TRACE_HEADER) );
        for( i=0; i<nsamp; i++)
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
/* Swap the data.  */
        longPtr=(long*) ((char*)wvmsg + sizeof(TRACE_HEADER) );
        shortPtr=(short*) ((char*)wvmsg + sizeof(TRACE_HEADER) );
        for( i=0; i<nsamp; i++)
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

   return(0);
}


void SwapFloat( float *data )
{
   char temp;

   union {
      char c[4];
   } dat;

   memcpy( &dat, data, sizeof(float) );
   temp     = dat.c[0];
   dat.c[0] = dat.c[3];
   dat.c[3] = temp;
   temp     = dat.c[1];
   dat.c[1] = dat.c[2];
   dat.c[2] = temp;
   memcpy( data, &dat, sizeof(float) );
   return;
}

