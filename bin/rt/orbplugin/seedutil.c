/*********************************************************************
 * seedutil.c:
 *
 * Utility routines for Mini-SEED records.
 *
 * Written by Chad Trabant, IRIS Data Management Center
 *********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <swapbytes.h>

#include "seedutil.h"


/*********************************************************************
 * find_reclen:
 *
 * Search for a 1000 blockette in a MiniSEED data record and return
 * the record size.
 *
 * Returns size of the record in bytes, 0 if 1000 blockette was not
 * found or -1 on error.
 *********************************************************************/
int
find_reclen ( const char *msrecord, int maxheaderlen )
{
  unsigned short begin_blockette; /* byte offset for next blockette */
  char swap_flag = 0;             /* is swapping needed? */
  char found1000 = 0;             /* found 1000 blockette? */
  char idx;
  int reclen = -1;                /* size of record in bytes */
  
  struct s_fsdh_data *fsdh_data;
  struct s_blk_1000  *blk_1000;
  struct s_blk_head  blk_head;

  /* Simple verification of a data record */
  if ( *(msrecord+6) != 'D' &&
       *(msrecord+6) != 'R' &&
       *(msrecord+6) != 'Q' )
    return -1;
  
  fsdh_data = (struct s_fsdh_data *) (msrecord+20);
  
  /* Check to see if byte swapping is needed (bogus year makes good test) */
  if ( (fsdh_data->start_time.year < 1960) ||
       (fsdh_data->start_time.year > 2050) )
    swap_flag = 1;
  
  begin_blockette = fsdh_data->begin_blockette;
  
  /* Swap order of begin_blockette field if needed */
  swap_2bytes(&begin_blockette, swap_flag);
  
  /* loop through blockettes as long as number is non-zero and viable */
  while ((begin_blockette != 0) &&
	 (begin_blockette <= (unsigned short) maxheaderlen))
    {
      memcpy(&blk_head, msrecord+begin_blockette,
	     sizeof(struct s_blk_head));
      swap_2bytes(&blk_head.blk_type, swap_flag);
      swap_2bytes(&blk_head.next_blk, swap_flag);
      
      if (blk_head.blk_type == 1000)  /* Found the 1000 blockette */
	{
	  blk_1000 = (struct s_blk_1000 *) (msrecord+begin_blockette);
	  
	  found1000 = 1;
	  
	  /* Calculate record size in bytes as 2^(blk_1000->rec_len) */
	  for (reclen=1, idx=1; idx <= blk_1000->rec_len; idx++)
	    reclen *= 2;      
	  
	  break;
	}
      
      begin_blockette = blk_head.next_blk;
    }
  
  if ( !found1000 )
    return 0;
  else
    return reclen;
}


/*********************************************************************
 * insert_blkt:
 *
 * Insert a blockette (or anything really) of size blktlen at the end
 * of the blockette chain.  The number of blockettes specified in the
 * fixed header will be incremented.  This is a very shoe-horn method:
 * there must be enough room between the last blockette and the start
 * of the data for blktlen bytes and the blockette to insert must be
 * complete and in the correct byte order, etc., etc.
 *
 * Returns the positive offset into the record on success on success
 * and -1 on error.
 *********************************************************************/
int
insert_blkt ( unsigned char *msrecord, int maxheaderlen,
	      const char *blkt, int blktlen )
{
  unsigned short begin_blockette; /* byte offset for next blockette */
  unsigned short begin_data;      /* byte offset for data */
  unsigned short usoffset;        /* offset with 2 byte size */
  int offset = -1;                /* offset to the new blkt in bytes */
  char swap_flag = 0;             /* is swapping needed? */
  char idx;
  
  struct s_fsdh_data *fsdh_data;
  struct s_blk_head  blk_head;
  
  /* Simple verification of a data record */
  if ( *(msrecord+6) != 'D' &&
       *(msrecord+6) != 'R' &&
       *(msrecord+6) != 'Q' )
    return -1;
  
  fsdh_data = (struct s_fsdh_data *) (msrecord+20);
  
  /* Check to see if byte swapping is needed (bogus year makes good test) */
  if ( (fsdh_data->start_time.year < 1960) ||
       (fsdh_data->start_time.year > 2050) )
    swap_flag = 1;
  
  begin_data      = fsdh_data->begin_data;
  begin_blockette = fsdh_data->begin_blockette;
  
  /* Swap order of begin_blockette/data fields if needed */
  swap_2bytes(&begin_data, swap_flag);
  swap_2bytes(&begin_blockette, swap_flag);
  
  /* If zero then there are no blockettes */
  if (begin_blockette == 0)
    offset = 48;
  else
    /* loop through blockettes as long as number is non-zero and viable */
    while ((begin_blockette != 0) &&
	   (begin_blockette <= (unsigned short) maxheaderlen))
      {
	memcpy(&blk_head, msrecord+begin_blockette,
	       sizeof(struct s_blk_head));
	swap_2bytes(&blk_head.blk_type, swap_flag);
	swap_2bytes(&blk_head.next_blk, swap_flag);
	
	if (blk_head.next_blk == 0)  /* Found the last blockette */
	  {
	    idx = get_blktlen (blk_head.blk_type, msrecord+begin_blockette);
	    
	    if ( idx )
	      offset = begin_blockette + idx;
	    
	    break;
	  }
	
	begin_blockette = blk_head.next_blk;
      }
  
  /* Insert the blockette if we have a good place to put it */
  if ( offset != -1 )
    {
      if ( (begin_data - offset) >= blktlen )
	{
	  /* Copy the blockette and update the fixed header */
	  memcpy (msrecord+offset, blkt, blktlen);
	  fsdh_data->num_blockettes += 1;
	  
	  /* Update the 'next' field of the previous last blkt */
	  usoffset = offset;
	  memcpy ((msrecord+begin_blockette+2), &usoffset, 2);
	  swap_2bytes((unsigned short *)(msrecord+begin_blockette+2), swap_flag);
	}
      else
	offset = -1;
    }
  
  return offset;
}


/***************************************************************************
 * get_blktlen():
 *
 * Returns the total length of a given blockette type in bytes or 0 if
 * type unknown.
 ***************************************************************************/
int
get_blktlen (int blkttype, const unsigned char *blktdata)
{
  unsigned short int blktlen = 0;
  
  switch (blkttype)
    {
    case 100:  /* Sample Rate */
      blktlen = 12;
      break;
    case 200:  /* Generic Event Detection */
      blktlen = 28;
      break;
    case 201:  /* Murdock Event Detection */
      blktlen = 36;
      break;
    case 300:  /* Step Calibration */
      blktlen = 32;
      break;
    case 310:  /* Sine Calibration */
      blktlen = 32;
      break;
    case 320:  /* Pseudo-random Calibration */
      blktlen = 28;
      break;
    case 390:  /* Generic Calibration */
      blktlen = 28;
      break;
    case 395:  /* Calibration Abort */
      blktlen = 16;
      break;
    case 400:  /* Beam */
      blktlen = 16;
      break;
    case 500:  /* Timing */
      blktlen = 8;
      break;
    case 1000: /* Data Only SEED */
      blktlen = 8;
      break;
    case 1001: /* Data Extension */
      blktlen = 8;
      break;
    case 2000: /* Opaque Data */
      /* First 2-byte field after the blockette header is the length */
      memcpy ((void *) &blktlen, blktdata, 2);
      break;
    }                           /* end switch */

  return blktlen;
  
} /* End of get_blktlen() */


void
swap_2bytes (unsigned short *a, char f)
{
  union {
    unsigned short i;
    char b[2];
  } word;
  char temp;

  if (f == 1){  /* f is the flag to trigger byte swapping */
  word.i = *a;
  temp = word.b[0];
  word.b[0] = word.b[1];
  word.b[1] = temp;
  memcpy((void *)a,(void *)&(word.i),sizeof(short));
  }
}
