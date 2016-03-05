/* A little extra snippet to create a network byte order (big-endian)
   SEED 1001 blockette from QCDAT ORB packet.

   pieced together by
   Chad Trabant, ORFEUS/EC-Project MEREDIAN
*/

#include <string.h>
#include <stdio.h>

#include <swapbytes.h>

/* Quanterra SOH flags */
#define SOH_INACCURATE 0x80            /* inaccurate time tagging, in SOH */
#define SOH_GAP 0x40                   /* time gap detected, in SOH */
#define SOH_EVENT_IN_PROGRESS 0x20     /* record contains event data */
#define SOH_BEGINNING_OF_EVENT 0x10    /* record is first of event sequence */
#define SOH_CAL_IN_PROGRESS 0x08       /* record contains calibration data */
#define SOH_EXTRANEOUS_TIMEMARKS 0x04  /* too many time marks received during this record */
#define SOH_EXTERNAL_TIMEMARK_TAG 0x02 /* record is time-tagged at a mark */
#define SOH_RECEPTION_GOOD 0x01        /* time reception is adequate */

/* data extension blockette structure for data records */
typedef struct { 
  unsigned short blockette_type; /* 1001 always */
  unsigned short next_offset;    /* offset to next blockette */
  unsigned char qual;            /* 0 to 100% quality indicator */
  unsigned char usec99;          /* 0 to 99 microseconds */
  unsigned char deb_flags;       /* DEB_XXXXX */
  unsigned char frame_count;     /* number of 64 byte data frames */
} data_extension_blockette ;


/* Convert QCDAT (ORB Quanterra packet) header info to a Data Extension
   Blockette (1001).  Expects a pointer to a Quanterra header, the byte
   offset to the next blockette and the total number of data frames.

   The resulting blockette will be in network (big endian) byte order.

   Returns a pointer to a 1001 blockette (8 bytes).
*/
void *
qcdat_to_deb(char *qcdat_record, int next_offset, int data_frames)
{
  unsigned short tshort;
  short perc[7] = {0, 40, 10, 20, 60, 80, 100};
  static data_extension_blockette DEB;
  unsigned short *sptr;
  unsigned char soh_flags;  /* SOH bits */
  char clock_flags;         /* Quanterra clock quality */
  int clk_qual;             /* SEED 1001 blockette clock quality */
  
  memcpy( &soh_flags, &qcdat_record[8], sizeof(unsigned char) );
  memcpy( &clock_flags, &qcdat_record[9], sizeof(char) );
  
  /* Set blockette type and next fields to network byte order values */
  tshort = 1001; sptr = &DEB.blockette_type;
  uhs2ms (&tshort, &sptr, 1);
  tshort = next_offset; sptr = &DEB.next_offset;
  uhs2ms (&tshort, &sptr, 1);
  
  DEB.frame_count = (unsigned char) data_frames; /* given as argument */
  DEB.usec99 = (unsigned char) 0;                /* applied in qt2orb */
  
  /* Calculate the 1001 blockette timing quality */
  clk_qual = perc[clock_flags + 1];
  
  if (soh_flags & SOH_INACCURATE)
    clk_qual = 15 + (short) clock_flags;
  else {
    if (soh_flags & SOH_EXTRANEOUS_TIMEMARKS)
      clk_qual -= 30;
    if ((soh_flags & SOH_EXTERNAL_TIMEMARK_TAG) == 0)
      clk_qual -= 15;
  }
  if (clk_qual < 0)
    DEB.qual = 0;
  else
    DEB.qual = (unsigned char) clk_qual;

  /* Originally the fixed header io and clock flags were modified here
     if (clock_flags >= 3)
     then
     seedheader.IO_flags = 0x20 ;
     ...
  */

  return (void *) &DEB;
}





































