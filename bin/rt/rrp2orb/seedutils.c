/***************************************************************************
 * seedutils.c:
 *
 * Routines to do simple Mini-SEED record processing.
 *
 * Written by Chad Trabant, IRIS Data Management Center
 ***************************************************************************/

#include <stdlib.h>
#include <string.h>

#include "seedutils.h"


/***************************************************************************
 * parse_record:
 *
 * Parse and populate the fsdh and blk_1000 structures from the given
 * record (recptr).
 *
 * Returns 1 if 1000 blockette was found, 0 if not.
 ***************************************************************************/
int
parse_record( char *recptr, int reclen,
	      t_fsdh *fsdh, t_blk_1000 *blk_1000)
{
  
  static t_blk_head *blk_head = NULL;
  
  int found_1000b = 0;            /* found the 1000 blockette? */
  char swap_flag = 0;             /* is swapping needed? */
  unsigned short begin_blockette; /* byte offset for next blockette */
  
  /* Make sure there is room for this struct */
  blk_head  = (t_blk_head *) realloc(blk_head, sizeof(t_blk_head));
  
  /* Copy fixed section data header */
  memcpy((void *)fsdh,recptr,sizeof(t_fsdh));
  
  /* check to see if word swapping is needed (bogus year makes good test) */
  if ( (fsdh->start_time.year < 1960) || 
       (fsdh->start_time.year > 2050) )
    swap_flag = 1;
  
  /* Swap bytes in FSDH */
  if ( swap_flag )
    {
      swap_2bytes(&fsdh->start_time.year);
      swap_2bytes(&fsdh->start_time.day);
      swap_2bytes(&fsdh->start_time.fracts);
      swap_2bytes(&fsdh->num_samples);
      swap_2bytes((unsigned short *)&fsdh->sample_rate);
      swap_2bytes((unsigned short *)&fsdh->multiplier);
      swap_4bytes((unsigned int *)&fsdh->time_correct);
      swap_2bytes(&fsdh->begin_data);
      swap_2bytes(&fsdh->begin_blockette);
    }
  
  begin_blockette = fsdh->begin_blockette;
  
  /* loop through blockettes as long as number is non-zero and viable */
  while ((begin_blockette != 0) &&
	 (begin_blockette <= (unsigned short) reclen)) {
    
    memcpy((void *)blk_head,recptr+begin_blockette,sizeof(t_blk_head));
    if ( swap_flag )
      {
	swap_2bytes(&blk_head->blk_type);
	swap_2bytes(&blk_head->next_blk);
      }
    
    if (blk_head->blk_type == 1000) /* found the 1000 blockette */
      {
	memcpy((void *)blk_1000,recptr+begin_blockette,sizeof(t_blk_1000));
	found_1000b = 1;
      }
    
    if (begin_blockette != 0)  /* check not useful now, but for the future */ 
      begin_blockette = blk_head->next_blk;
  }
  
  return found_1000b;
}  /* End of parse_record() */


/***************************************************************************
 * calcsamprate:
 *
 * Calculate a double precision nominal sample rate from the sample
 * rate factor and multiplier in the FSDH struct of the specified
 * MSrecord.
 *
 * Returns the positive sample rate on success and -1.0 on error.
 ***************************************************************************/
double
calcsamprate (t_fsdh *fsdh)
{
  double samprate = 0.0;
  
  if ( ! fsdh )
    return -1.0;
  
  /* Calculate the nominal sample rate */
  if ( fsdh->sample_rate > 0 )
    samprate = (double) fsdh->sample_rate;
  else if ( fsdh->sample_rate < 0 )
    samprate = -1.0 / (double) fsdh->sample_rate;
  if ( fsdh->multiplier > 0 )
    samprate = samprate * (double) fsdh->multiplier;
  else if ( fsdh->multiplier < 0 )
    samprate = -1.0 * (samprate / (double) fsdh->multiplier);
  
  return samprate;
}  /* End of calcsamprate() */


/***************************************************************************
 * swap_2bytes:
 * 
 * Swap two bytes in-place in an alignment independant way.
 ***************************************************************************/
void swap_2bytes (unsigned short *a)
{
  union {
    unsigned short i;
    char b[2];
  } word;
  char temp;

  word.i = *a;
  temp = word.b[0];
  word.b[0] = word.b[1];
  word.b[1] = temp;
  memcpy((void *)a,(void *)&(word.i),sizeof(short));
}  /* End of swap_2bytes() */


/***************************************************************************
 * swap_4bytes:
 * 
 * Swap four bytes in-place in an alignment independant way.
 ***************************************************************************/
void
swap_4bytes (unsigned int *a)
{
  union {
    unsigned int l;
    char b[4];
  } word;
  char temp;

  word.l = *a;
  temp = word.b[0];
  word.b[0] = word.b[3];
  word.b[3] = temp;
  temp = word.b[1];
  word.b[1] = word.b[2];
  word.b[2] = temp;
  memcpy((void *)a,(void *)&(word.l),sizeof(int));
}  /* End of swap_4bytes() */


/***************************************************************************
 * get_encoding:
 * 
 * Returns a string describing a data encoding format.
 ***************************************************************************/
char *
get_encoding (const char encoding)
{
  switch (encoding)
    {
    case 0:
      return "ASCII text";
    case 1:
      return "16 bit integers";
    case 2:
      return "24 bit integers";
    case 3:
      return "32 bit integers";
    case 4:
      return "IEEE floating point";
    case 5:
      return "IEEE double precision float";
    case 10:
      return "STEIM 1 Compression";
    case 11:
      return "STEIM 2 Compression";
    case 12:
      return "GEOSCOPE Muxed 24 bit int";
    case 13:
      return "GEOSCOPE Muxed 16/3 bit gain/exp";
    case 14:
      return "GEOSCOPE Muxed 16/4 bit gain/exp";
    case 15:
      return "US National Network compression";
    case 16:
      return "CDSN 16 bit gain ranged";
    case 17:
      return "Graefenberg 16 bit gain ranged";
    case 18:
      return "IPG - Strasbourg 16 bit gain";
    case 19:
      return "STEIM 3 Compression";
    case 30:
      return "SRO Format";
    case 31:
      return "HGLP Format";
    case 32:
      return "DWWSSN Gain Ranged Format";
    case 33:
      return "RSTN 16 bit gain ranged";
    default:
      return "Unknown format code";
    }  /* end switch */
}  /* End of get_encoding() */
