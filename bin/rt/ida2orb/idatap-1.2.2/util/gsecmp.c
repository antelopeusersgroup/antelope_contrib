/* @(#)gsecmp.c	2.3 01/28/97 */
/*======================================================================
 *
 *  GSE2.0 CM6 compression/decompression routines
 *
 *  util_cm6:    compress an array of longs using CM6 algorithm
 *  util_dcm6: decompress an array of CM6 data into longs
 *  util_cm8:    compress an array of longs using CM8 algorithm
 *  util_dcm8: decompress an array of CM8 data into longs
 *
 *  util_fstdif: compute first difference
 *  util_rmfdif: remove  first difference
 *  util_chksum: compute GSE2.0 checksum
 *
 *  In the routines util_cm6() and util_cm8(), specify the negative of
 *  the number of samples if you want the original data to be unmodified.
 *  Otherwise the input array holds the differenced version of the
 *  original data upon return.
 *
 *  All bit patterns which result from CM8 compression are accepted as
 *  is, as it is assumed that tcp/ip or some other such protocol will
 *  be used to transmit the data and consequently there is no longer
 *  any need for concern about confusing the circuits.
 *
 *  Only tested for little-endian machines.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 *
 *  Prepared for the University of California - San Diego, by:
 *
 *  David E. Chavez
 *  Engineering Services & Software        email: dec@essw.com
 *  3950F Sorrento Valley Blvd             voice: 619.587.2765
 *  San Diego, CA 92121                    fax:   619.587.0444
 *
 *  with thanks to Ken Muirhead and Urs Kradolfer for advice and
 *  help in debugging.
 *
 *====================================================================*/
#include <stdlib.h>
#include <errno.h>
#include <math.h>
#include "util.h"

#define MAXMAG 0x07ffffff

#define B00100000 0x20 /* CM6 continuation bit         */
#define B00010000 0x10 /* CM6 sign bit                 */
#define B00001111 0x0f /* mask for first CM6 byte      */
#define B00011111 0x1f /* mask for all other CM6 bytes */

#define B10000000 0x80 /* CM8 continuation bit         */
#define B01000000 0x40 /* CM8 sign bit                 */
#define B00111111 0x3f /* mask for first CM8 byte      */
#define B01111111 0x7f /* mask for all other CM8 bytes */
 
#define MAXCM6 6 /* for 32 bit longs */
#define MAXCM8 4 /* for 32 bit longs */
#define MAXOUT 6

static char cmptmp[MAXOUT];

static long cm6_mask1[MAXCM6] = {
    0x0000000f, /* 0000 0000 0000 0000 0000 0000 0000 1111 */
    0x000001e0, /* 0000 0000 0000 0000 0000 0001 1110 0000 */
    0x00003c00, /* 0000 0000 0000 0000 0011 1100 0000 0000 */
    0x00078000, /* 0000 0000 0000 0111 1000 0000 0000 0000 */
    0x00f00000, /* 0000 0000 1111 0000 0000 0000 0000 0000 */
    0x1e000000, /* 0001 1110 0000 0000 0000 0000 0000 0000 */
};
static int cm6_shift1[MAXCM6] = {0, 5, 10, 15, 20, 25};

static long cm6_mask2[MAXCM6] = {
    0x00000000, /* 0000 0000 0000 0000 0000 0000 0000 0000 */ /* unused */
    0x0000001f, /* 0000 0000 0000 0000 0000 0000 0001 1111 */
    0x000003e0, /* 0000 0000 0000 0000 0000 0011 1110 0000 */
    0x00007c00, /* 0000 0000 0000 0000 0111 1100 0000 0000 */
    0x000f8000, /* 0000 0000 0000 1111 1000 0000 0000 0000 */
    0x01f00000, /* 0000 0001 1111 0000 0000 0000 0000 0000 */
};
static int cm6_shift2[MAXCM6] = {0, 0, 5, 10, 15, 20};

static char lookup[64] = {
    '+', '-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B',
    'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd',
    'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
    's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
};

static long decode[123] = {
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,
     2,  3,  4,  5,  6,  7,  8,  9, 10, 11,  0,  0,  0,  0,  0,  0,
     0, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
    27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37,  0,  0,  0,  0,  0,
     0, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,
    53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63
};

static long cm8_mask1[MAXCM6] = {
    0x0000003f, /* 0000 0000 0000 0000 0000 0000 0011 1111 */
    0x00001f80, /* 0000 0000 0000 0000 0001 1111 1000 0000 */
    0x000fc000, /* 0000 0000 0000 1111 1100 0000 0000 0000 */
    0x07e00000, /* 0000 0111 1110 0000 0000 0000 0000 0000 */
};
static int cm8_shift1[MAXCM6] = {0, 7, 14, 21};

static long cm8_mask2[MAXCM6] = {
    0x00000000, /* 0000 0000 0000 0000 0000 0000 0000 0000 */ /* unused */
    0x0000007f, /* 0000 0000 0000 0000 0000 0000 0111 1111 */
    0x00003f80, /* 0000 0000 0000 0000 0011 1111 1000 0000 */
    0x001fc000, /* 0000 0000 0001 1111 1100 0000 0000 0000 */
};
static int cm8_shift2[MAXCM6] = {0, 0, 7, 14};

long sign;

void util_clip(input, nsamp, maxmag)
long *input;
long nsamp;
long maxmag;
{
long i;

    for (i = 0; i < nsamp; i++) {
        if (input[i] < 0) {
            input[i] = -input[i];
            sign     = -1;
        } else {
            sign     =  1;
        }
        if (input[i] > maxmag) input[i] = maxmag;
        input[i] *= sign;
    }
}

void util_fstdif(input, nsamp)
long *input;
long nsamp;
{
long i, temp, prev = 0;

    for (i = 0; i < nsamp; i++) {
        temp = input[i];
        input[i] -= prev;
        prev = temp;
    }
}

void util_rmfdif(input, nsamp)
long *input;
long nsamp;
{
long i;

    for (i = 1; i < nsamp; i++) input[i] += input[i-1];
}

#define MODULO_VALUE 100000000

long util_chksum(input, nsamp)
long *input;
long nsamp;
{
long i, value;
long modulo;
long checksum = 0;

    modulo = MODULO_VALUE;

    for (i = 0; i < nsamp; i++) {
        value = input[i];
        if (abs(value) >= modulo) value -= (value/modulo)*modulo;
        checksum += value;
        if (abs(checksum) >= modulo) checksum -= (checksum/modulo)*modulo;
    }

    return abs(checksum);
}

long util_cm6(input, output, nsamp, maxout, ndif, clip)
long *input;  /* original data                                   */
char *output; /* array to hold differenced and compressed output */
long nsamp;   /* number of input samples                         */
long maxout;  /* dimension of out array                          */
int  ndif;    /* 1 for first diff, 2 for second, 0 for none      */
int  clip;    /* if set, clip input data at algorithm max        */
{
char *ptr;
int nbyte;
long i, j, k, value, piece;
long numout = 0;
int restore = 0;

/* Truncate data with magnitude greater than MAXMAG */

    if (clip) util_clip(input, nsamp, MAXMAG);

/* Convert from raw data to optional difference */

    if (nsamp < 0) {
        restore = 1;
        nsamp = -nsamp;
    }

    for (i = 0; i < ndif; i++) util_fstdif(input, nsamp);

/* Apply CM6 compression  */

    for (ptr = output, i = 0; i < nsamp; i++) {

        value = input[i];

    /* Convert to sign and magnitude */

        if (sign = (value < 0 ? B00010000 : 0)) value = -value;

    /* Determine number of compressed bytes */

        if (       value < 0x00000010) {
            nbyte = 1;
        } else if (value < 0x00000200) {
            nbyte = 2;
        } else if (value < 0x00004000) {
            nbyte = 3;
        } else if (value < 0x00080000) {
            nbyte = 4;
        } else if (value < 0x01000000) {
            nbyte = 5;
        } else {
            if (value > MAXMAG) value = MAXMAG;
            nbyte = 6;
        }

        if ((numout += nbyte) > maxout) {
            errno = ERANGE;
            return -1;
        }

        j = nbyte - 1;

    /* Do first (most significant) byte */

        cmptmp[0] = sign | ((value & cm6_mask1[j]) >> cm6_shift1[j]);

    /* Do remaining bytes */

        for (j = nbyte - 1, k = 1; j > 0; j--, k++) {
            cmptmp[k] = (value & cm6_mask2[j]) >> cm6_shift2[j];
        }

    /* Set continuation bits */

        for (j = 0; j < nbyte - 1; j++) cmptmp[j] |= B00100000;

    /* Convert to ASCII */

        for (j = 0; j < nbyte; j++) cmptmp[j] = lookup[cmptmp[j]];

    /* Copy to output */

        memcpy(ptr, cmptmp, nbyte);
        ptr += nbyte;
    }

/* Restore original data if necessary */

    if (restore) for (i = 0; i < ndif; i++) util_rmfdif(input, nsamp);

/* Return number of bytes in compressed array  */

    return numout;
}

long util_dcm6(input, output, nbyte, nsamp, ndif)
char *input;  /* compressed data array                      */
long *output; /* output array                               */
long nbyte;   /* number of bytes in compressed data array   */
long nsamp;   /* number of uncompressed samples expected    */
int  ndif;    /* 1 for first diff, 2 for second, 0 for none */
{
long piece;
long i, j, n, value, k;
int continued, negative, cbyte;

/* Decompress */

    i = -1;
    n =  0;
    while (n < nsamp) {
        if (i >= nbyte) return -1;

    /* Determine how many bytes in this compressed sample */

        i++;
        j = 0;
        do {
            cmptmp[j] = decode[input[i]];
            if (continued = cmptmp[j] & B00100000) i++;
            j++;
        } while (continued);

        cbyte = j;

    /* Position first byte in output word */

        value = ((cmptmp[0] & B00001111) << cm6_shift1[cbyte-1]);

    /* Or in remaining bytes */

        for (j = cbyte - 1, k = 1; j > 0; j--, k++) {
            value |= ((cmptmp[k] & B00011111) << cm6_shift2[j]);
        }

    /* Store uncompressed word */

        negative = cmptmp[0] & B00010000;
        output[n++] = negative ? -value : value;
    }

    if (  n != nsamp) return -3;
    if (++i != nbyte) return -4;

/* Undo any differencing */

    for (i = 0; i < ndif; i++) util_rmfdif(output, nsamp);

/* Return checksum */

    return util_chksum(output, nsamp);
}

long util_cm8(input, output, nsamp, maxout, ndif, clip)
long *input;  /* original data                                   */
char *output; /* array to hold differenced and compressed output */
long nsamp;   /* number of input samples                         */
long maxout;  /* dimension of out array                          */
int  ndif;    /* 1 for first diff, 2 for second, 0 for none      */
int  clip;    /* if set, clip input data at algorithm max        */
{
char *ptr;
int nbyte;
long i, j, k, value, piece;
long numout = 0;
int restore = 0;

/* Truncate data with magnitude greater than MAXMAG */

    if (clip) util_clip(input, nsamp, MAXMAG);

/* Convert from raw data to optional difference */

    if (nsamp < 0) {
        restore = 1;
        nsamp = -nsamp;
    }

    for (i = 0; i < ndif; i++) util_fstdif(input, nsamp);

/* Apply CM8 compression  */

    for (ptr = output, i = 0; i < nsamp; i++) {

        value = input[i];

    /* Convert to sign and magnitude */

        if (sign = (value < 0 ? B01000000 : 0)) value = -value;

    /* Determine number of compressed bytes */

        if (value <        0x00000040) {
            nbyte = 1;
        } else if (value < 0x00002000) {
            nbyte = 2;
        } else if (value < 0x00100000) {
            nbyte =3;
        } else {
            if (value > MAXMAG) value = MAXMAG;
            nbyte =4;
        }

        if ((numout += nbyte) > maxout) {
            errno = ERANGE;
            return -1;
        }

        j = nbyte - 1;

    /* Do first (most significant) byte */

        cmptmp[0] = sign | ((value & cm8_mask1[j]) >> cm8_shift1[j]);

    /* Do remaining bytes */

        for (j = nbyte - 1, k = 1; j > 0; j--, k++) {
            cmptmp[k] = (value & cm8_mask2[j]) >> cm8_shift2[j];
        }

    /* Set continuation bits */

        for (j = 0; j < nbyte - 1; j++) cmptmp[j] |= B10000000;

    /* Copy to output */

        memcpy(ptr, cmptmp, nbyte);
        ptr += nbyte;
    }

/* Restore original data if necessary */

    if (restore) for (i = 0; i < ndif; i++) util_rmfdif(input, nsamp);

/* Return number of bytes in compressed array  */

    return numout;
}

long util_dcm8(input, output, nbyte, nsamp, ndif)
char *input;  /* compressed data array                      */
long *output; /* output array                               */
long nbyte;   /* number of bytes in compressed data array   */
long nsamp;   /* number of uncompressed samples expected    */
int  ndif;    /* 1 for first diff, 2 for second, 0 for none */
{
long piece;
long i, j, n, value, k;
int continued, negative, cbyte;

/* Decompress */

    i = -1;
    n =  0;
    while (n < nsamp) {
        if (i >= nbyte) return -1;

    /* Determine how many bytes in this compressed sample */

        i++;
        j = 0;
        do {
            cmptmp[j] = input[i];
            if (continued = cmptmp[j] & B10000000) i++;
            j++;
        } while (continued);

        cbyte = j;

    /* Position first byte in output word */

        value = ((cmptmp[0] & B00111111) << cm8_shift1[cbyte-1]);

    /* Or in remaining bytes */

        for (j = cbyte - 1, k = 1; j > 0; j--, k++) {
            value |= ((cmptmp[k] & B01111111) << cm8_shift2[j]);
        }

    /* Store uncompressed word */

        negative = cmptmp[0] & B01000000;
        output[n++] = negative ? -value : value;
    }

    if (  n != nsamp) return -3;
    if (++i != nbyte) return -4;

/* Undo any differencing */

    for (i = 0; i < ndif; i++) util_rmfdif(output, nsamp);

/* Return checksum */

    return util_chksum(output, nsamp);
}

#ifdef DEBUG_TEST

#define NSAMP 9
#define NBYTE 100

main()
{
int i, test;
static long raw[NSAMP] = {306,306,301,298,297,1024,1031,1099,2147483647};
long tst[NSAMP];
char cmp[NBYTE];
long checksum;
long nbyte = NBYTE;
long nsamp;

    for (test = 0; test < 2; test++) {
        if (test == 0) {
            printf("test with large datum (checksums won't match)\n");
            nsamp = NSAMP;
        } else {
            printf("\ntest with realistic data (checksums should match)\n");
            nsamp = NSAMP - 1;
        }
        printf("beg checksum = %ld\n", util_chksum(raw, nsamp));
        printf("beg data = ");
        for (i = 0; i < nsamp; i++) printf("%3d ", raw[i]); printf("\n");

        if ((nbyte = util_cm6(raw, cmp, -nsamp, nbyte, 2, 1)) > 0) {
            printf("cm6 compresses to %d bytes\n", nbyte);
            if ((checksum = util_dcm6(cmp, tst, nbyte, nsamp, 2)) >= 0) {
                printf("cm6 data = ");
                for (i = 0; i < nsamp; i++) printf("%3d ",tst[i]); printf("\n");
                printf("cm6 checksum = %ld\n", checksum);
            } else {
                printf("cm6 decompression fails with status %d\n", checksum);
            }
        } else {
            printf("cm6 compression fails with status %d\n", nbyte);
        }

        if ((nbyte = util_cm8(raw, cmp, -nsamp, nbyte, 2, 1)) > 0) {
            printf("cm8 compresses to %d bytes\n", nbyte);
            if ((checksum = util_dcm8(cmp, tst, nbyte, nsamp, 2)) >= 0) {
                printf("cm8 data = ");
                for (i = 0; i < nsamp; i++) printf("%3d ",tst[i]); printf("\n");
                printf("cm8 checksum = %ld\n", checksum);
            } else {
                printf("cm8 decompression fails with status %d\n", checksum);
            }
        } else {
            printf("cm8 compression fails with status %d\n", nbyte);
        }
    }
}

#endif /* DEBUG_TEST */
