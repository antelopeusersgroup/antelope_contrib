/* @(#)steim.c	1.8 02/03/97 */
/*======================================================================
 *
 *  steim.c
 *
 *  Steim 1 compression/decompression.  Here we force the output to be
 *  in big endian byte order, as it looks like many of the big endian
 *  decompressors out there assume that the data are that way.
 *
 *  The decompressor is meant to handle data from both systems.
 *
 *  Requires 4 byte longs.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <assert.h>
#include <memory.h>
#include <string.h>
#include <errno.h>
#include "util.h"

#define FLEN 64           /* number of bytes per frame     */
#define NSEQ 16           /* number of sequences per frame */
#define WLEN FLEN / NSEQ  /* number of bytes per sequence  */

/* local globals */

long *out;   /* output buffer                                     */
long *frame; /* current frame                                     */
long fi;     /* index of current frame                            */
long maxfrm; /* number of frames that will fit into output buffer */
int  si;     /* sequence index in current frame                   */
long nc;     /* number of compressed samples in output            */
long ocount; /* number of bytes used in output buffer             */
int  swap;   /* swap flag, set if host is not big endian          */

static int ns = 0; /* number of diffs currently saved */

/* internal function: load one compressed word into output */

int _util_csteim1_addword(d, ndiff)
long *d;
int ndiff;
{
union {
    char  *c;
    short *s;
    long  *l;
} ptr;
static char key[5] = {0, 3, 2, 0, 1};

    if (ndiff == 0) return 0;

/* Load in the differences and associated descriptor */

    ptr.l = frame + si;
    if (ndiff == 4) {
        ptr.c[0]  = (char) (d[0] & 0xff);
        ptr.c[1]  = (char) (d[1] & 0xff);
        ptr.c[2]  = (char) (d[2] & 0xff);
        ptr.c[3]  = (char) (d[3] & 0xff);
    } else if (ndiff == 2) {
        ptr.s[0]  = (short) (d[0] & 0xffff);
        ptr.s[1]  = (short) (d[1] & 0xffff);
        if (swap) util_sswap(ptr.s, 2);
    } else if (ndiff == 1) {
        ptr.l[0]  = d[0];
        if (swap) util_lswap(ptr.l, 1);
    }
    *frame |= (key[ndiff] << (2 * (15 - si)));

    ocount += 4;
    nc += ndiff;

/* Increment the sequence counter */

    if (++si < NSEQ) return 0;

/* At this point the frame has been completed. */

    if (swap) util_lswap(frame, 1);
    if (++fi == maxfrm) return 1; /* No more frames available, done */

/* Initialize new frame */

    frame    = out + (fi * NSEQ);
    *frame   = 0;
    ocount  += 4;
    si       = 1;

    return 0;
}

/* internal function: build a frame */
    
int _util_csteim1_frm(diff, flush)
long diff;
int flush;
{
/* Possible states */

#define STATE_NUL  0 /* no differences saved so far      */
#define STATE_1D1  1 /* 1 1-byte difference saved so far */
#define STATE_2D1  2 /* 2 1-byte difference saved so far */
#define STATE_3D1  3 /* 3 1-byte difference saved so far */
#define STATE_1D2  4 /* 1 2-byte difference saved so far */
#define STATE_BAD -1 /* will never occur                 */

struct activity {
    int flush1;   /* no. diffs to flush right away                        */
    int flush2;   /* no. diffs to flush after that and before saving crnt */
    int flush3;   /* no. diffs to flush after saving crnt diff            */
};

static struct activity action_table[5][5] = {
            /*                  size of difference                      */
            /*        0          1          2          3          4     */
/* STATE_NUL */ {  {0,0,0},   {0,0,0},   {0,0,0},   {0,0,0},   {0,0,1} },
/* STATE_1D1 */ {  {1,0,0},   {0,0,0},   {0,0,2},   {0,0,0},   {1,0,1} },
/* STATE_2D1 */ {  {2,0,0},   {0,0,0},   {2,0,0},   {0,0,0},   {2,0,1} },
/* STATE_3D1 */ {  {2,1,0},   {0,0,4},   {2,0,2},   {0,0,0},   {2,1,1} },
/* STATE_1D2 */ {  {1,0,0},   {0,0,2},   {0,0,2},   {0,0,0},   {1,0,1} }
};

static int state_table[5][5] = {
            /*                  size of difference                      */
            /*        0          1          2          3          4     */
/* STATE_NUL */ {STATE_NUL, STATE_1D1, STATE_1D2, STATE_BAD, STATE_NUL},
/* STATE_1D1 */ {STATE_NUL, STATE_2D1, STATE_NUL, STATE_BAD, STATE_NUL},
/* STATE_2D1 */ {STATE_NUL, STATE_3D1, STATE_1D2, STATE_BAD, STATE_NUL},
/* STATE_3D1 */ {STATE_NUL, STATE_NUL, STATE_NUL, STATE_BAD, STATE_NUL},
/* STATE_1D2 */ {STATE_NUL, STATE_NUL, STATE_NUL, STATE_BAD, STATE_NUL}
};

int i, j, shift, size, nf, done;
struct activity action;
static long d[4];
static int state = STATE_NUL;

/* Figure out how many bytes are needed for this difference */

    if (flush) {
        size = 0;
    } else if (diff >= -128 && diff <= 127) {
        size = 1;
    } else if (diff > -32768 && diff <= 32767) {
        size = 2;
    } else {
        size = 4;
    }

/* Figure out what to do with this difference */

    action = action_table[state][size];
    state  =  state_table[state][size]; /* new state, after action */

    if (nf = action.flush1) {
        done = _util_csteim1_addword(d, nf);
        for (i = 0, j = nf; j < ns; i++, j++) d[i] = d[j];
        ns -= nf;
        if (done) return 1;
    }

    if (nf = action.flush2) {
        done = _util_csteim1_addword(d, nf);
        for (i = 0, j = nf; j < ns; i++, j++) d[i] = d[j];
        ns -= nf;
        if (done) return 1;
    }

    if (size) d[ns++] = diff;

    if (nf = action.flush3) {
        done = _util_csteim1_addword(d, nf);
        for (i = 0, j = nf; j < ns; i++, j++) d[i] = d[j];
        ns -= nf;
        if (done) return 1;
    }

    return 0;
}

/* Steim 1 compression */

/* Given a fixed length array to store the output, this routine
 * will compress as many samples as possible into this array,
 * starting at offset 0.  The number of samples which were 
 * compressed (which may be less that the input count) is
 * returned.
 */

long util_csteim1(dest, destlen, src, count, used)
char *dest;    /* output array                         */
long destlen;  /* size of dest, in bytes               */
long *src;     /* input array                          */
long count;    /* number of entries in src             */
long *used;    /* number of output bytes actually used */
{
long s, diff;

    if (sizeof(long) != WLEN) {
        errno = EINVAL;
        return -1;
    }

/* Initialize everything */

    memset(dest, 0, destlen);

    swap   = (util_order() == LTL_ENDIAN_ORDER);
    out    = (long *) dest;
    out[1] = src[0];
    out[2] = src[0];
    ocount = 12; /* W0, X0, Xn to start */
    maxfrm = destlen / FLEN; /* maximum number of output frames */
    frame  = out;
    *frame = 0;
    fi     = 0;
    nc     = 0;
    si     = 3;

/* Loop for each sample (will bail when we run out of output memory) */

    for (s = 0; s < count; s++) {
        diff = src[s] - out[2];
        if (_util_csteim1_frm(diff, 0) != 0) {
            if (used != (long *) NULL) *used = ocount;
            if (swap) {
                util_lswap(out+1, 2);
            }
            return nc;
        }
        out[2] = src[s];
    }

/* All the input fit into the output.  Flush accumulated differences. */

    _util_csteim1_frm(0, 1);
    if (used != (long *) NULL) *used = ocount;
    if (swap) {
        util_lswap(frame, 1);
        util_lswap(out+1, 2);
    }
    return nc;
}

/* Steim 1 decompression */

int util_dsteim1(dest, destlen, src, srclen, order, count)
long *dest;    /* output array                            */
long destlen;  /* dimension of output array               */
char *src;     /* input Steim 1 compressed data           */
long srclen;   /* size of src, in bytes                   */
u_long order;  /* byte order (as per util_order()) of src */
long count;    /* number of uncompressed samples in src   */
{
int i, j, k, nsamp, nfrm;
long  ltmp, val, beg, end, key, code[NSEQ];
short stmp;
char *frm;
union {
    char  *c;
    short *s;
    long  *l;
} ptr;

    if (sizeof(long) != WLEN) {
        errno = EINVAL;
        return -1;
    }

    swap = (order != util_order());
    nfrm = srclen / FLEN;

/* Get the block start/stop values */

    ptr.c = src;

    memcpy(&beg, ptr.c + 4, 4);
    if (swap) util_lswap(&beg, 1);

    memcpy(&end, ptr.c + 8, 4);
    if (swap) util_lswap(&end, 1);

/* Loop over each frame */
/* We do not verify that the 0x00 codes are where they should be */

    val   = dest[0] = beg;
    nsamp = 1;

    for (i = 0; i < nfrm; i++) {

        frm = src + (i * FLEN);  /* point to start of current frame */
        key = *((long *) frm);   /* codes are in first 4 bytes      */
        if (swap) util_lswap(&key, 1);
        for (j = NSEQ - 1; j >= 0; j--) {
            code[j] = key & 0x03;
            key >>= 2;
        }

        for (j = 1; j < NSEQ; j++) {

            if (nsamp >= destlen) {
                return -2;
            }

            ptr.c = frm + (j * 4);  /* point to current 4 byte sequence */

            switch (code[j]) {
              case 0:
                break;

              case 1:
                for (k = (nsamp == 1) ? 1 : 0; k < 4; k++) {
                    dest[nsamp++] = (val += (long) ptr.c[k]);
                }
                break;

              case 2:
                for (k = (nsamp == 1) ? 1 : 0; k < 2; k++) {
                    stmp = ptr.s[k];
                    if (swap) util_sswap(&stmp, 1);
                    dest[nsamp++] = (val += (long) stmp);
                }
                break;

              case 3:
                if (nsamp > 1) {
                    ltmp = ptr.l[0];
                    if (swap) util_lswap(&ltmp, 1);
                    dest[nsamp++] = (val += ltmp);
                }
                break;

              default:
                return -3;
            }
        }
    }

/* Sanity checks */

    if (count != nsamp) return -4;

    if (dest[nsamp-1] != end) return -5;

    return 0;

}

#ifdef DEBUG_TEST

#define NSAMP 17
#define OUTLEN 64
#define NEWLEN 512 /* way bigger than required! */

main()
{
int i;
static long raw[NSAMP] =
{306, 306, 301, 298, 297, 1024, 3000, 3100, 2000, 0, -100, -200, 0, 15, 80111, 80111, -80123};

u_long order;
int retval;
long nc;
char output[OUTLEN];
long new[NEWLEN], used;

    order = BIG_ENDIAN_ORDER;
        
    printf("beg data = ");
    for (i = 0; i < NSAMP; i++) printf("%3ld ", raw[i]); printf("\n");

    nc = util_csteim1(output, OUTLEN, raw, NSAMP, &used);
    printf("util_csteim1 returns %ld (%ld bytes used)\n", nc, used);
    printf("\n");
    util_hexdmp((unsigned char *) output, OUTLEN, 0, 'd');
    printf("\n");

    retval = util_dsteim1(new, NEWLEN, output, OUTLEN, order, NSAMP);
    printf("util_dsteim1 returns %d\n", retval);

    printf("beg data = ");
    for (i = 0; i < NSAMP; i++) printf("%3ld ", raw[i]); printf("\n");
    printf("end data = ");
    for (i = 0; i < NSAMP; i++) printf("%3ld ", new[i]); printf("\n");
}

#endif /* DEBUG_TEST */
