/* @(#)wad.c	1.2 11/22/93 */
/*======================================================================
 *
 *  sacio/wa_sacdata.c
 *
 *  Write SAC data to disk as ascii.
 *
 *====================================================================*/
#include <stdio.h>
#include "sacio.h"

int sacio_wad(fp, buffer, npts, count)
FILE  *fp;
float *buffer;
long  npts;
long *count;
{
int i;

    clearerr(fp);

    for (i = 0; i < npts; i++, *count += 1) {
        fprintf(fp, "%15.6f", buffer[i]);
        if (((*count + 1) % 5) == 0 && *count > 0) fprintf(fp, "\n");
    }

    return ferror(fp) ? -1 : npts;
}
