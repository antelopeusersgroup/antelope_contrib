/* @(#)fsdh.c	1.4 06/30/97 */
/*======================================================================
 *
 *  Load a Fixed Section of Data Header to memory and vice-versa.
 *
 *  Requires 2-byte shorts and 4-byte longs.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <assert.h>
#include "seed.h"
#include "util.h"

void seed_loadfsdh(char *dest, struct seed_fsdh *src, u_long order)
{
int swap;
short stmp;
long ltmp;
char tbuff[10];
static char *fid = "seed_loadfsdh";

    assert(sizeof(short) == 2);
    assert(sizeof(long)  == 4);

    if (src->seqno < 0 || src->seqno > 999999) src->seqno = 1;

/* The front is all ASCII stuff... no swapping required */

    sprintf(dest   , "%06dD             ", src->seqno);
    sprintf(dest+ 8, "%s",                 src->staid);
    sprintf(dest+13, "%s",                 src->locid);
    sprintf(dest+15, "%s",                 src->chnid);
    sprintf(dest+18, "%s",                 src->netid);

/* Time stamp is byte swapped internally (if required) */

    seed_dtconv(dest+20, src->start, order);

/* Byte swap the remaining stuff into the desired order, if required */

    /* Numer of samples */
    stmp = src->nsamp; if (swap) util_sswap(&stmp, 1);
    memcpy(dest+30, &stmp,  2);

    /* sample rate factor */
    stmp = src->srfact; if (swap) util_sswap(&stmp, 1);
    memcpy(dest+32, &stmp, 2);

    /* sample rate multipler */    
    stmp = src->srmult; if (swap) util_sswap(&stmp, 1);
    memcpy(dest+34, &stmp, 2);

    /* Activity flags */
    memcpy(dest+36, &src->active, 1);

    /* I/O and clock flags */
    memcpy(dest+37, &src->ioclck, 1);

    /* Data quality flags */
    memcpy(dest+38, &src->qual,   1);

    /* Number of blockettes that follow */
    memcpy(dest+39, &src->more, 1);

    /* Time correction */
    ltmp = src->tcorr; if (swap) util_lswap(&ltmp, 1);
    memcpy(dest+40,                      &src->tcorr,  4);

    /* Begining of data */
    stmp = src->bod; if (swap) util_sswap(&stmp, 1);
    memcpy(dest+44, &stmp, 2);

    /* First blockette */
    stmp = src->first; if (swap) util_sswap(&stmp, 1);
    memcpy(dest+46, &stmp, 2);
}

int seed_fsdh(struct seed_fsdh *dest, char *src)
{
int swap;
char tbuff[10];

    assert(sizeof(short) == 2);
    assert(sizeof(long)  == 4);

    sscanf(src, "%06dD             ", &dest->seqno);

    memcpy(dest->staid, src+ 8, 5); dest->staid[5] = 0;
    util_strtrm(dest->staid);
    memcpy(dest->locid, src+13, 2); dest->locid[2] = 0;
    util_strtrm(dest->locid);
    memcpy(dest->chnid, src+15, 3); dest->chnid[3] = 0;
    util_strtrm(dest->chnid);
    memcpy(dest->netid, src+18, 2); dest->netid[2] = 0;
    util_strtrm(dest->netid);

    dest->start = seed_ttodt(src+20, &dest->order);
    memcpy(&dest->nsamp,  src+30, 2);
    memcpy(&dest->srfact, src+32, 2);
    memcpy(&dest->srmult, src+34, 2);
    memcpy(&dest->active, src+36, 1);
    memcpy(&dest->ioclck, src+37, 1);
    memcpy(&dest->qual,   src+38, 1);
    memcpy(&dest->more,   src+39, 1);
    memcpy(&dest->tcorr,  src+40, 4);
    memcpy(&dest->bod,    src+44, 2);
    memcpy(&dest->first,  src+46, 2);

    if (dest->swap = (dest->order != util_order())) {
        util_sswap(&dest->nsamp,  1);
        util_sswap(&dest->srfact, 1);
        util_sswap(&dest->srmult, 1);
        util_lswap(&dest->tcorr,  1);
        util_sswap(&dest->bod,    1);
        util_sswap(&dest->first,  1);
    }

    return 0;
}

#ifdef DEBUG_TEST

main(int argc, char **argv)
{
struct seed_fsdh dest;
char src[48];

    if (fread(src, 1, 48, stdin) != 48) {
        perror("fread");
        exit(1);
    }

    if (seed_fsdh(&dest, src) != 0) {
        fprintf(stderr, "seed_fsdh failed\n");
        exit(1);
    }

    printf("seqno  = %ld\n", dest.seqno);
    printf("staid  = %s\n",  dest.staid);
    printf("locid  = %s\n",  dest.locid);
    printf("chnid  = %s\n",  dest.chnid);
    printf("netid  = %s\n",  dest.netid);
    printf("start  = %s\n",  util_dttostr(dest.start, 0));
    printf("nsamp  = %hd\n", dest.nsamp);
    printf("srfact = %hd\n", dest.srfact);
    printf("srmult = %hd\n", dest.srmult);
    printf("active = %d\n",  (int) dest.active);
    printf("ioclck = %d\n",  (int) dest.ioclck);
    printf("qual   = %d\n",  (int) dest.qual);
    printf("more   = %d\n",  (int) dest.more);
    printf("tcorr  = %ld\n", dest.tcorr);
    printf("bod    = %hd\n", dest.bod);
    printf("first  = %hd\n", dest.first);
    printf("order  = %s\n",  dest.order == LTL_ENDIAN_ORDER ? "little endian" : "big endian");
    printf("swap   = %d\n",  dest.swap);
    exit(0);
}

#endif /* DEBUG_TEST */
