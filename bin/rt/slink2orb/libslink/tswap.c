/***************************************************************************
 * tswap.c:
 *
 * Utility functions for triggered byte swapping between LSB and MSB
 * byte orders "in-place".
 *
 * Swapping is triggered by the 'swapflag' argument to each function.
 * 'swapflag' is evaluated as 'if ( swapflag )', set it appropriately
 * (i.e. 0 = false, 1 = true).
 *
 * The functions use standardized integer types, namely uint8_t, uint16_t,
 * uint32_t and uint64_t (these are normally declared by including
 * inttypes.h or stdint.h).  Each function expects it's input to be a
 * pointer to an integer of the appropriate size/type except for
 * swap3() which expects a pointer to a uint8 (which should be the
 * first of three).
 *
 * Written by Chad Trabant, ORFEUS/EC-Project MEREDIAN
 *   Design elements taken from various code bases.
 *
 * Version: 2003.168
 ***************************************************************************/


#include <memory.h>

/* In this particular code base our int types come from here */
#include "slplatform.h"

void
tswap2 ( uint16_t *data, int swapflag )
{
  uint8_t temp;
  
  union
  {
    uint8_t  c[2];
  } dat;
  
  if ( swapflag )
    {
      memcpy( &dat, data, sizeof(dat) );
      temp     = dat.c[0];
      dat.c[0] = dat.c[1];
      dat.c[1] = temp;
      memcpy( data, &dat, sizeof(dat) );
    }
  
  return;
}


void
tswap3 ( uint8_t *data, int swapflag )  /* Pointer to the 3-byte array */
{
  uint8_t temp;
  
  union
  {
    uint8_t  c[3];
  } dat;
  
  if ( swapflag )
    {
      memcpy( &dat, data, sizeof(dat) );
      temp     = dat.c[0];
      dat.c[0] = dat.c[2];
      dat.c[2] = temp;
      memcpy( data, &dat, sizeof(dat) );
    }
  
  return;
}


void
tswap4 ( uint32_t *data, int swapflag )
{
  uint8_t temp;

  union {
    uint8_t c[4];
  } dat;
  
  if ( swapflag )
    {
      memcpy( &dat, data, sizeof(dat) );
      temp     = dat.c[0];
      dat.c[0] = dat.c[3];
      dat.c[3] = temp;
      temp     = dat.c[1];
      dat.c[1] = dat.c[2];
      dat.c[2] = temp;
      memcpy( data, &dat, sizeof(dat) );
    }
  
  return;
}


void
tswap8 ( uint64_t *data, int swapflag )
{
  uint8_t temp;
  
  union
  {
    uint8_t   c[8];
  } dat;
  
  if ( swapflag )
    {
      memcpy( &dat, data, sizeof(dat) );
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
      memcpy( data, &dat, sizeof(dat) );
    }

   return;
}

