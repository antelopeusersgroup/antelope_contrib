/* @(#)site.c	1.7 03/18/97 */
/*======================================================================
 *
 * IDA data exchange protocols, site specific routines.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include "xfer.h"

#ifdef IDA_SUPPORT
#include "ida.h"
#include "util.h"

/* Compress an IDA data logger record */

int ida_compress(rev, comp, input, output)
int rev;      /* data logger format revision                 */
int *comp;    /* compression flag                            */
char *input;  /* input data record                           */
char *output; /* output array (MUST be different from input) */
{
int cbyte;
char *cptr;
struct data_head dhead;
int hlen, dlen;
long nsamp;
static union {
    long  l[IRIS_MAXDLEN/4];   /* uncompressed long   */
    short s[IRIS_MAXDLEN/2];   /* uncompressed shorts */
} data;
static char  cdata[(IRIS_MAXDLEN/4)*5];  /* compressed data */
static char  *fid = "ida_compress";

/* Non-data and corrupt data records are sent uncompressed */

    if (
        iris_rtype(input, rev) != IRIS_DATA ||
        iris_dhead(&dhead, input, rev) != 0 ||
        (dhead.wrdsiz != 2 && dhead.wrdsiz != 4)
    ) {
        *comp = XFER_CMPNONE;
    }

    iris_dhlen(rev, dhead.stream, &hlen, &dlen);
    nsamp = (long) dhead.nsamp;
    if (dlen != (dhead.wrdsiz * dhead.nsamp)) {
        util_log(1, "%s: dlen %d != wrdsiz %d * nsamp %hd\n",
            fid, dlen, dhead.wrdsiz, dhead.nsamp
        );
        *comp = XFER_CMPNONE;
    }

/* Do the compression */

    switch (*comp) {

      case XFER_CMPIGPP:

        /*  Compress, first converting to host byte order if necessary */

        if (dhead.wrdsiz == 4) {
            memcpy((char *) data.l, input + hlen, dlen);
            if (dhead.order != util_order()) util_lswap(data.l, nsamp);
            cbyte = util_lcomp(cdata, data.l, nsamp);
        } else {
            memcpy((char *) data.s, input + hlen, dlen);
            if (dhead.order != util_order()) util_sswap(data.s, nsamp);
            cbyte = util_scomp(cdata, data.s, nsamp);
        }

        /* Check results and only keep it if the size reduced */

        if (cbyte > 0 && cbyte < dlen) {
            cptr = cdata;
        } else {
            *comp = XFER_CMPNONE;
        }

        break;

      case XFER_CMPNONE:
      default:
        *comp = XFER_CMPNONE;
    }

    if (*comp == XFER_CMPNONE) {
        cptr  = input + hlen;
        cbyte = dlen;
    }

/* Copy the results to the output buffer */

    memcpy(output, input, hlen);
    memcpy(output+hlen, cptr, cbyte);

    return cbyte + hlen;
}

/* Decompress an IDA data logger record */

char *ida_decompress(rev, comp, nbyte, input)
int rev;      /* data logger format revision  */
int *comp;    /* compression flag             */
long *nbyte;  /* number of input/output bytes */
char *input;  /* compressed record            */
{
int cbyte;
struct data_head dhead;
int hlen, dlen, unused;
long i, nsamp;
static char  output[IRIS_BUFSIZ];        /* header + compressed data */
static union {
    long  l[IRIS_MAXDLEN/4];   /* uncompressed long   */
    short s[IRIS_MAXDLEN/2];   /* uncompressed shorts */
} data;
static char  *fid = "ida_decompress";

    if (*comp == XFER_CMPNONE) return input;

/* This had better be a (D)ata record, as all others were supposed to
 * have been sent uncompressed.
 */
    if (
        iris_rtype(input, rev) != IRIS_DATA ||
        iris_dhead(&dhead, input, rev) != 0 ||
        (dhead.wrdsiz != 2 && dhead.wrdsiz != 4)
    ) {
        util_log(1, "%s: non-data or corrupt record sent compressed?", fid);
        return NULL;
    }

    iris_dhlen(rev, dhead.stream, &hlen, &dlen);
    nsamp = (long) dhead.nsamp;
    if (dlen != (dhead.wrdsiz * dhead.nsamp)) {
        util_log(1, "%s: dlen %d != wrdsiz %d * nsamp %hd\n",
            fid, dlen, dhead.wrdsiz, dhead.nsamp
        );
        return NULL;
    }

/* Do the decompression, restoring original byte order if required */

    memcpy(output, input, hlen);

    switch (*comp) {

      case XFER_CMPIGPP:

        if (dhead.wrdsiz == 4) {
            util_ldcmp(data.l, input + hlen, nsamp);
            if (dhead.order != util_order()) util_lswap(data.l, nsamp);
            memcpy(output + hlen, (char *) data.l, dlen);
        } else {
            util_sdcmp(data.s, input + hlen, nsamp);
            if (dhead.order != util_order()) util_sswap(data.s, nsamp);
            memcpy(output + hlen, (char *) data.s, dlen);
        }

        break;

      default:
        return NULL;
    }

    *comp  = XFER_CMPNONE;
    *nbyte = hlen + dlen;
    if (*nbyte < IDA_BUFSIZ) {
        unused = IDA_BUFSIZ - (*nbyte);
        memset(output + hlen + dlen, 0, unused);
        *nbyte += unused;
    }
    return output;
}
#endif
