/* BEGIN write_ak_ah.c */
/********************************************************************/
/****          Function write_ak_ah() of program jade2ah.c          ****/
/********************************************************************/
/****  This function does the actual writing of the ah header.   ****/
/****         Written by G. H. Cole Sonafrank, 3/22/88.          ****/
/********************************************************************/
/****       Copyright (c) University of Alaska, 1988,89,90       ****/
/****                    All rights reserved.                    ****/
/********************************************************************/

#include <stdio.h>
#include "ak_ahheader.h"

#define ABORT	1
#define SUCCESS_ZERO	0
int write_ak_ah (output_fd, ah)
  int output_fd;
  ak_ahhed *ah;
{

  int status, ah_size;

#ifdef SUN
  /*******************************************************************/
  /* The following code will NOT generate the proper output on the   */
  /* MASSCOMPs.  Each float variable (member) of the ah structure is */
  /* forced to be aligned on a longword boundry and two bytes of     */
  /* padding is added here and there to make this possible.          */
  /* It works fine on the Sun 3/60s...                               */
  /*******************************************************************/
  ah_size = sizeof(ak_ahhed);
  if (ah_size == AK_AHHEADSIZE) {	/* Let's just be sure about this */
    status = write (output_fd, (char *)ah, ah_size);
    if (status != ah_size) {
      fprintf (stderr,
      "\tError writing header (%d<%d).\n", status, ah_size);
      return (ABORT);
      }
    return (SUCCESS_ZERO);
    } /* End if ah_size is ok write quickly */
#endif

  /*******************************************************************/
  /* Instead, we have to kludge something together that will write   */
  /* pieces of the structure that are contiguous, skip the padding,  */
  /* and write some more...                                          */
  /*******************************************************************/

  /* beginning of ah through ah.station.calib */
  status = write (output_fd, (char *)&ah->station, 520);
  if (status != 520) {
    perror("write_ak_ah");
    fprintf (stderr, "\twrite_ak_ah: Error writing ah section 1 (%d<%d)\n",
             status, 520);
    return (ABORT);
    }

  /* beginning of ah.event through ah.event.ot.mn */
  status = write (output_fd, (char *)&ah->event, 22);
  if (status != 22) {
    fprintf (stderr, "\twrite_ak_ah: Error writing ah section 2 (%d<%d)\n",
             status, 22);
    return (ABORT);
    }

  /* ah.event.ot.sec through ah.record.type */
  status = write (output_fd, (char *)&ah->event.ot.sec, 86);
  if (status != 86) {
    fprintf (stderr, "\twrite_ak_ah: Error writing ah section 3 (%d<%d)\n",
              status, 86);
    return (ABORT);
    }

  /* ah.record.ndata through ah.record.abstime.mn */
  status = write (output_fd, (char *)&ah->record.ndata, 22);
  if (status != 22) {
    fprintf (stderr, "\twrite_ak_ah: Error writing ah section 4 (%d<%d)\n",
             status, 22);
    return (ABORT);
    }

  /* ah.record.abstime.sec through ah.record.log */
  status = write (output_fd, (char *)&ah->record.abstime.sec, 290);
  if (status != 290) {
    fprintf (stderr, "\twrite_ak_ah: Error writing ah section 5 (%d<%d)\n",
             status, 290);
    return (ABORT);
    }

  /* ah.extra */
  status = write (output_fd, (char *)ah->extra, 84);
  if (status != 84) {
    fprintf (stderr, "\twrite_ak_ah: Error writing ah section 6 (%d<%d)\n",
             status, 84);
    return (ABORT);
    }

  return (SUCCESS_ZERO);

} /* End of write_ak_ah() */
