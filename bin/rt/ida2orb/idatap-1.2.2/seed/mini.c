/* @(#)mini.c	1.6 05/02/97 */
/*======================================================================
 *
 *  comp.c
 *
 *  Compress an array of longs (host byte order) into mini-SEED packets
 *  of user specified length.  The function will compress as many words
 *  as it can before the output buffer is filled (including the fixed
 *  section of data header and the blockette 1000), and returns the
 *  number of samples compressed.  The calling program should deal with
 *  the full output packet, update the input pointer and number of
 *  samples to reflect the amount of processed data and repeat, until
 *  such time that the entire array is processed.
 *
 *  Assumes nominal sample rates (ie, no blockette 100).
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <sys/types.h>
#include <math.h>
#include "seed.h"
#include "util.h"

long seed_mini(input, count, fsdh, output, outexp, used)
long *input;
long count;
struct seed_fsdh *fsdh;
char *output;
int outexp;
long *used;
{
long nc, outlen;
struct seed_b1000 b1000;
u_char length;
static char *fid = "seed_mini";

/* Determine actual record length and clear the buffer */

    if (outexp < SEED_MINRECEXP || outexp > SEED_MAXRECEXP) {
        util_log(1, "%s: illegal outexp (%d)", fid, outexp);
        return -1;
    }
        
    outlen = (long) pow(2.0, (double) outexp);
    b1000.length = (u_char) outexp;

    memset(output, 0, outlen);

/* Since we don't have blockette 100, Steim 1 data start at offset 64 */

    fsdh->bod   = 64;
    fsdh->first = 48;
    nc = util_csteim1(output+fsdh->bod,outlen-fsdh->bod,input,count,used);
    *used += fsdh->bod;

/* Set end of time series flag if appropriate */

    if (nc == count) {
        fsdh->ioclck |= SEED_FLAG_STOP;
        util_log(2, "set STOP flag");
    }

/* Load the FSDH and blockette 1000 in the front of the buffer */

    fsdh->nsamp = nc;
    fsdh->more  = 1; /* just a blockette 1000 to follow */
    seed_loadfsdh(output, fsdh, BIG_ENDIAN_ORDER);

    b1000.next   = 0;
    b1000.format = SEED_STEIM1;
    b1000.order  = 1; /* BIG_ENDIAN_ORDER */
    seed_load1000(output+fsdh->first, &b1000, BIG_ENDIAN_ORDER);

/* Return the number of words compressed */

    return nc;
}
