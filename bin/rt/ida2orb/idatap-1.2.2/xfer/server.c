/* @(#)server.c	1.14 03/18/97 */
/*======================================================================
 *
 * IDA data exchange protocols, server side routines.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <errno.h>
#include <stdio.h>
#include <memory.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "xfer.h"
#include "util.h"

static int slen = sizeof(u_short);
static int llen = sizeof(u_long);
u_short sval;
u_long lval;

static size_t len;

#define MAX_ALLOWED 128
static char *Allowed[MAX_ALLOWED];
static int Num_allowed = 0;

/*============================ internal use ==========================*/

/* Compress waveform data */

int xfer_Compress(cnf, wav)
struct xfer_cnf *cnf; /* disk loop configuration             */
struct xfer_wav *wav; /* waveform record, complete with data */
{
unsigned long order;
int cbyte, nsamp, wrdsiz, standx, chnndx, rev;
long *nbyte;
int *comp;
char *data, *output;
static char cdata[XFER_MAXDAT*4];
static union {
   short s[XFER_MAXDAT];
   long  l[XFER_MAXDAT];
} buffer;
static char *fid = "xfer_Compress";

/* Get all the required info from the config and waveform records */

    switch (wav->format) {

      case XFER_WAVGEN1:

        if (*(comp = &wav->type.gen1.comp) == XFER_CMPNONE) return XFER_OK;

        standx =  wav->type.gen1.standx;
        chnndx =  wav->type.gen1.chnndx;
        nsamp  =  wav->type.gen1.nsamp;
        nbyte  = &wav->type.gen1.nbyte;
        data   =  wav->type.gen1.data;
        break;

      case XFER_WAVRAW:
      case XFER_WAVSEED:
      case XFER_WAVPASSCAL:

        return XFER_OK;

#ifdef IDA_SUPPORT
      case XFER_WAVIDA:

        if (*(comp = &wav->type.ida.comp) == XFER_CMPNONE) return XFER_OK;

        rev   =  wav->type.ida.rev;
        data  =  wav->type.ida.data;
        nbyte = &wav->type.ida.nbyte;
        data  =  wav->type.ida.data;

        cbyte = ida_compress(rev, comp, data, cdata);
        if (cbyte < 0) {
            xfer_errno = XFER_EINVAL;
            return XFER_ERROR;
        } else if (cbyte > XFER_MAXDAT) {
            util_log(1, "%s: increase XFER_MAXDAT (%d) to %d",
                fid, XFER_MAXDAT, cbyte
            );
            xfer_errno = XFER_ELIMIT;
            return XFER_ERROR;
        } else {
            *nbyte = cbyte;
            memcpy(data, cdata, (*nbyte = cbyte));
            return XFER_OK;
        }
        break;
#endif

      default:

        xfer_errno = XFER_EFORMAT;
        return XFER_ERROR;
    }

    switch (cnf->format) {

      case XFER_CNFGEN1:

        wrdsiz = cnf->type.gen1.sta[standx].chn[chnndx].wrdsiz;
        order  = cnf->type.gen1.sta[standx].chn[chnndx].order;
        break;

      case XFER_CNFNRTS:

        wrdsiz = cnf->type.nrts.sta[standx].chn[chnndx].wrdsiz;
        order  = cnf->type.nrts.sta[standx].chn[chnndx].order;
        break;

      default:

        xfer_errno = XFER_EFORMAT;
        return XFER_ERROR;
    }

/* Sanity checks */

    if (wrdsiz != *nbyte / nsamp) {
        util_log(1, "%s: configuration and waveform discrepancy", fid);
        util_log(1, "%s: %d != %d / %d", fid, wrdsiz, *nbyte, nsamp);
        xfer_errno = XFER_EINVAL;
        return XFER_ERROR;
    }

    if (wrdsiz != 2 && wrdsiz != 4) {
        util_log(1, "%s: unexpected wrdsiz `%d'", fid, wrdsiz);
        xfer_errno = XFER_EINVAL;
        return XFER_ERROR;
    }

/* Do the compression */

    switch (*comp) {

      case XFER_CMPIGPP:

        if (wrdsiz == 4) {
            memcpy((char *) buffer.l, data, *nbyte);
            if (order != util_order()) util_lswap(buffer.l, nsamp);
            cbyte = util_lcomp(cdata, buffer.l, nsamp);
        } else {
            memcpy((char *) buffer.s, data, *nbyte);
            if (order != util_order()) util_sswap(buffer.s, nsamp);
            cbyte = util_scomp(cdata, buffer.s, nsamp);
        }

        /* Check compression and only keep it if the size reduced */

        if (cbyte > 0 && cbyte < *nbyte) {
            output = cdata;
        } else {
            *comp = XFER_CMPNONE;
        }
        break;

      case XFER_CMPNONE:
        break;

      default:
        *comp = XFER_CMPNONE;
    }

    if (*comp == XFER_CMPNONE) return XFER_OK;

/* Copy the compressed data to the waveform record's data buffer */

    memcpy(data, output, (*nbyte = cbyte));

    return XFER_OK;
}

/* Decode a request preamble */

int xfer_DecodePreamble(preamble, src, protocol)
union xfer_preamble *preamble;
char **src;
int protocol;
{
static char *fid = "xfer_DecodePreamble";

    switch (protocol) {

      case 0x01:

        memcpy(&lval, *src, llen);
        *src += llen;
        preamble->ver01.client_id = (long) ntohl(lval);

        memcpy(&sval, *src, slen);
        *src += slen;
        preamble->ver01.format = (int) ntohs(sval);

        break;

      default:
        xfer_errno = XFER_EPROTOCOL;
        return XFER_ERROR;
    }

    return XFER_OK;

}

/* Decode a waveform request */

int xfer_DecodeWavReq(req, src, protocol)
union xfer_wavreq *req;
char **src;
int protocol;
{
int i, j;
struct xfer_time xtime;
static char *fid = "xfer_DecodeWavReq";

    switch (protocol) {

      case 0x01:

        memcpy(&sval, *src, slen);
        *src += slen;
        req->ver01.format = (int) ntohs(sval);

    /* Make sure this is a supported return format 
     * (See comments in xfer_EncodeWav, below)
     */

        switch (req->ver01.format) {
          case XFER_WAVGEN1:
          case XFER_WAVIDA:
          case XFER_WAVRAW:
          case XFER_WAVSEED:
            break;
          default:
            xfer_errno = XFER_EFORMAT;
            return XFER_ERROR;
        }

    /* Supported format, continue decoding */

        memcpy(&sval, *src, slen);
        *src += slen;
        req->ver01.keepup = (int) ntohs(sval);

        memcpy(&sval, *src, slen);
        *src += slen;
        req->ver01.comp = (int) ntohs(sval);

        memcpy(&sval, *src, slen);
        *src += slen;
        req->ver01.nsta = (int) ntohs(sval);

        for (i = 0; i < req->ver01.nsta; i++) {
            memcpy(req->ver01.sta[i].name, *src, XFER01_SNAMLEN + 1);
            *src += XFER01_SNAMLEN + 1;

            memcpy(&sval, *src, slen);
            *src += slen;
            req->ver01.sta[i].nchn = (int) ntohs(sval);

            for (j = 0; j < req->ver01.sta[i].nchn; j++) {
                memcpy(req->ver01.sta[i].chn[j].name,*src,XFER01_CNAMLEN+1);
                *src += XFER01_CNAMLEN + 1;

                memcpy(&lval, *src, llen);
                *src += llen;
                xtime.sec = (long) ntohl(lval);
                memcpy(&lval, *src, llen);
                *src += llen;
                xtime.usec = (u_long) ntohl(lval);
                req->ver01.sta[i].chn[j].beg = xfer_dtime(&xtime);

                memcpy(&lval, *src, llen);
                *src += llen;
                xtime.sec = (long) ntohl(lval);
                memcpy(&lval, *src, llen);
                *src += llen;
                xtime.usec = (u_long) ntohl(lval);
                req->ver01.sta[i].chn[j].end = xfer_dtime(&xtime);
            }
        }

        break;

      default:
        xfer_errno = XFER_EPROTOCOL;
        return XFER_ERROR;
    }

    return XFER_OK;
}

/* Decode an arbitrary request */

int xfer_DecodeReq(req, src)
struct xfer_req *req;
char *src;
{
int retval;
long ltemp;
static char *fid = "xfer_DecodeReq";

/* ALL requests specify protocol as first two bytes */

    memcpy(&sval, src, slen);
    src += slen;
    req->protocol = (int) ntohs(sval);

/* Followed by 4 bytes of request code */

    memcpy(&lval, src, llen);
    src += llen;
    req->type = (long) ntohl(lval);

/* Followed by 2 bytes of socket I/O timeout */

    memcpy(&sval, src, slen);
    src += slen;
    req->timeout = (int) ntohs(sval);
    if (req->timeout < XFER_DEFTO) req->timeout = XFER_DEFTO;

/* AND followed by 4 bytes of client send (server recv) buffer size */

    memcpy(&lval, src, llen);
    src += llen;
    req->rcvbuf = (long) ntohl(lval);

/* AND followed by 4 bytes of client recv (server send) buffer size */

    memcpy(&lval, src, llen);
    src += llen;
    req->sndbuf = (long) ntohl(lval);

/* Set the global socket I/O timeout */

    _xfer_timeout = req->timeout;

/* What follows depends on the protocol */

    switch (req->protocol) {

      case 0x01:

        retval = xfer_DecodePreamble(&req->preamble, &src, req->protocol);
        if (retval != XFER_OK) return retval;

        switch (req->type) {

          case XFER_CNFREQ:
            return XFER_OK;

          case XFER_WAVREQ:
            return xfer_DecodeWavReq(&req->request.wav,&src,req->protocol);

          default:
            xfer_errno = XFER_EREQUEST;
            return XFER_ERROR;
        }
    }
}

/* Encode a channel configuration descriptor */

long xfer_EncodeChnCnf(start_of_message, info, format)
char *start_of_message;
void *info;
int format;
{
int i, sign;
long factor;
float test;
char *ptr;
struct xfer_time *xtime;
struct xfer_gen1chncnf *gen1;
struct xfer_nrtschncnf *nrts;
static char *fid = "xfer_EncodeChnCnf";

    ptr = start_of_message;

    switch (format) {

      case XFER_CNFGEN1:

        gen1 = (struct xfer_gen1chncnf *) info;

        memcpy(ptr, gen1->name, XFER01_CNAMLEN + 1);
        ptr += XFER01_CNAMLEN + 1;

        memcpy(ptr, gen1->instype, XFER01_INAMLEN + 1);
        ptr += XFER01_INAMLEN + 1;

        sval = htons((u_short) gen1->wrdsiz);
        memcpy(ptr, &sval, slen);
        ptr += slen;

        lval = htonl((u_long) gen1->order);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) (1000000.0 * gen1->sint));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        sign = (gen1->calib < 0) ? -1 : 1;
        test = (float) sign * gen1->calib;
        if (test < 1000.0) {
            factor = sign * 1000000;
        } else if (test < 10000.0) {
            factor = sign * 100000;
        } else if (test < 100000.0) {
            factor = sign * 10000;
        } else if (test < 1000000.0) {
            factor = sign * 1000;
        } else if (test < 10000000.0) {
            factor = sign * 100;
        } else if (test < 100000000.0) {
            factor = sign * 10;
        } else {
            factor = sign;
        }
        lval = htonl((u_long) factor);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) ((float) factor * gen1->calib));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) (100000.0 * gen1->calper));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) (1000000.0 * gen1->vang));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) (1000000.0 * gen1->hang));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        xtime = (struct xfer_time *) xfer_time(gen1->beg);
        lval = htonl((u_long) xtime->sec);
        memcpy(ptr, &lval, llen);
        ptr += llen;
        lval = htonl(xtime->usec);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        xtime = (struct xfer_time *) xfer_time(gen1->end);
        lval = htonl((u_long) xtime->sec);
        memcpy(ptr, &lval, llen);
        ptr += llen;
        lval = htonl(xtime->usec);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        break;

#ifdef NRTS_SUPPORT
      case XFER_CNFNRTS:

        nrts = (struct xfer_nrtschncnf *) info;

        memcpy(ptr, nrts->name, XFER01_CNAMLEN + 1);
        ptr += XFER01_CNAMLEN + 1;

        memcpy(ptr, nrts->instype, XFER01_INAMLEN + 1);
        ptr += XFER01_INAMLEN + 1;

        sval = htons((u_short) nrts->wrdsiz);
        memcpy(ptr, &sval, slen);
        ptr += slen;

        lval = htonl((u_long) nrts->order);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) (1000000.0 * nrts->sint));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        sign = (nrts->calib < 0) ? -1 : 1;
        test = (float) sign * nrts->calib;
        if (test < 1000.0) {
            factor = sign * 1000000;
        } else if (test < 10000.0) {
            factor = sign * 100000;
        } else if (test < 100000.0) {
            factor = sign * 10000;
        } else if (test < 1000000.0) {
            factor = sign * 1000;
        } else if (test < 10000000.0) {
            factor = sign * 100;
        } else if (test < 100000000.0) {
            factor = sign * 10;
        } else {
            factor = sign;
        }
        lval = htonl((u_long) factor);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) ((float) factor * nrts->calib));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) (100000.0 * nrts->calper));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) (1000000.0 * nrts->vang));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) (1000000.0 * nrts->hang));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        xtime = (struct xfer_time *) xfer_time(nrts->beg);
        lval = htonl((u_long) xtime->sec);
        memcpy(ptr, &lval, llen);
        ptr += llen;
        lval = htonl(xtime->usec);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        xtime = (struct xfer_time *) xfer_time(nrts->end);
        lval = htonl((u_long) xtime->sec);
        memcpy(ptr, &lval, llen);
        ptr += llen;
        lval = htonl(xtime->usec);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        sval = htons((u_short) nrts->type);
        memcpy(ptr, &sval, slen);
        ptr += slen;

        sval = htons((u_short) nrts->hlen);
        memcpy(ptr, &sval, slen);
        ptr += slen;

        sval = htons((u_short) nrts->dlen);
        memcpy(ptr, &sval, slen);
        ptr += slen;

        lval = htonl((u_long) nrts->nrec);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) nrts->nhide);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) nrts->latency);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        break;
#endif /* NRTS_SUPPORT */

      default:
        xfer_errno = XFER_EFORMAT;
        return XFER_ERROR;
    }

    return (long) (ptr - start_of_message);
}

/* Encode a station configuration descriptor */

long xfer_EncodeStaCnf(start_of_message, info, format)
char *start_of_message;
void *info;
int format;
{
int i;
char *ptr;
struct xfer_gen1stacnf *gen1;
struct xfer_nrtsstacnf *nrts;
static char *fid = "xfer_EncodeStaCnf";

    ptr = start_of_message;

    switch (format) {

      case XFER_CNFGEN1:

        gen1 = (struct xfer_gen1stacnf *) info;

        memcpy(ptr, gen1->name, XFER01_SNAMLEN + 1);
        ptr += XFER01_SNAMLEN + 1;

        lval = htonl((u_long) (1000000.0 * gen1->lat));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) (1000000.0 * gen1->lon));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) (1000000.0 * gen1->elev));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) (1000000.0 * gen1->depth));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        sval = htons((u_short) gen1->nchn);
        memcpy(ptr, &sval, slen);
        ptr += slen;

        for (i = 0; i < gen1->nchn; i++) {
            len = xfer_EncodeChnCnf(ptr, (void*)(gen1->chn+i), format);
            if (len <= 0) {
                util_log(1, "%s: xfer_EncodeChnCnf: %d", fid, len);
                return XFER_ERROR;
            }
            ptr += len;
        }

        break;

#ifdef NRTS_SUPPORT
      case XFER_CNFNRTS:

        nrts = (struct xfer_nrtsstacnf *) info;

        memcpy(ptr, nrts->name, XFER01_SNAMLEN + 1);
        ptr += XFER01_SNAMLEN + 1;

        lval = htonl((u_long) (1000000.0 * nrts->lat));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) (1000000.0 * nrts->lon));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) (1000000.0 * nrts->elev));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) (1000000.0 * nrts->depth));
        memcpy(ptr, &lval, llen);
        ptr += llen;

        sval = htons((u_short) nrts->nchn);
        memcpy(ptr, &sval, slen);
        ptr += slen;

        for (i = 0; i < nrts->nchn; i++) {
            len = xfer_EncodeChnCnf(ptr, (void*)(nrts->chn+i), format);
            if (len <= 0) {
                util_log(1, "%s: xfer_EncodeChnCnf: %d", fid, len);
                return XFER_ERROR;
            }
            ptr += len;
        }

        break;
#endif /* NRTS_SUPPORT */

      default:
        xfer_errno = XFER_EFORMAT;
        util_log(1, "%s: unsupported format `%d'", fid, format);
        return -1;
    }

    return (long) (ptr - start_of_message);
}

/* Encode a configuration record */

long xfer_EncodeCnf(start_of_message, cnf)
char *start_of_message;
struct xfer_cnf *cnf;
{
int i;
long len;
char *ptr;
static char *fid = "xfer_EncodeCnf";

    ptr = start_of_message;

    sval = htons((u_short) cnf->format);
    memcpy(ptr, &sval, slen);
    ptr += slen;

    switch (cnf->format) {

      case XFER_CNFGEN1:

        lval = htonl((u_long) cnf->type.gen1.order);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        sval = htons((u_short) cnf->type.gen1.nsta);
        memcpy(ptr, &sval, slen);
        ptr += slen;

        for (i = 0; i < cnf->type.gen1.nsta; i++) {
            len = xfer_EncodeStaCnf(
                    ptr, (void *)(cnf->type.gen1.sta + i), cnf->format
            );
            if (len < 0) return XFER_ERROR;
            ptr += len;
        }

        break;

#ifdef NRTS_SUPPORT
      case XFER_CNFNRTS:

        lval = htonl((u_long) cnf->type.nrts.order);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        sval = htons((u_short) cnf->type.nrts.nsta);
        memcpy(ptr, &sval, slen);
        ptr += slen;

        for (i = 0; i < cnf->type.nrts.nsta; i++) {
            len = xfer_EncodeStaCnf(
                    ptr, (void *)(cnf->type.nrts.sta + i), cnf->format
            );
            if (len < 0) return XFER_ERROR;
            ptr += len;
        }

        break;
#endif /* NRTS_SUPPORT */

      default:
        xfer_errno = XFER_EFORMAT;
        return XFER_ERROR;
    }

    return (long) (ptr - start_of_message);
}

/* Encode a waveform record */

long xfer_EncodeWav(start_of_message, wav)
char *start_of_message;
struct xfer_wav *wav;
{
int i;
long len;
char *ptr;
struct xfer_time *xtime;
static char *fid = "xfer_EncodeWav";

    ptr = start_of_message;

    sval = htons((u_short) wav->format);
    memcpy(ptr, &sval, slen);
    ptr += slen;

/* Since we won't come here until after the initial request has been
 * received and acknowledged, we need to check for supported formats
 * outside of this function.  This is done above, in xfer_DecodeWavReq.
 * There is no simple way (that I am aware of) to code things so that
 * the check falls out automatically.  Therefore, when support for new
 * formats are added here, one should be very careful to update the
 * check statement in xfer_DecodeWavReq.
 */

    switch (wav->format) {

      case XFER_WAVGEN1:

        sval = htons((u_short) wav->type.gen1.standx);
        memcpy(ptr, &sval, slen);
        ptr += slen;

        sval = htons((u_short) wav->type.gen1.chnndx);
        memcpy(ptr, &sval, slen);
        ptr += slen;

        xtime = (struct xfer_time *) xfer_time(wav->type.gen1.tofs);
        lval = htonl((u_long) xtime->sec);
        memcpy(ptr, &lval, llen);
        ptr += llen;
        lval = htonl(xtime->usec);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        sval = htons((u_short) wav->type.gen1.tear);
        memcpy(ptr, &sval, slen);
        ptr += slen;

        sval = htons((u_short) wav->type.gen1.comp);
        memcpy(ptr, &sval, slen);
        ptr += slen;

        lval = htonl((u_long) wav->type.gen1.nsamp);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        lval = htonl((u_long) wav->type.gen1.nbyte);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        if (wav->type.gen1.nbyte > XFER_MAXDAT) {
            util_log(1, "%s: increase XFER_MAXDAT (%d) to %d",
                fid, XFER_MAXDAT, wav->type.gen1.nbyte
            );
            xfer_errno = XFER_ELIMIT;
            return XFER_ERROR;
        }
        memcpy(ptr, wav->type.gen1.data, wav->type.gen1.nbyte);
        ptr += wav->type.gen1.nbyte;

        break;

      case XFER_WAVRAW:

        lval = htonl((u_long) wav->type.raw.nbyte);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        if (wav->type.raw.nbyte > XFER_MAXDAT) {
            util_log(1, "%s: increase XFER_MAXDAT (%d) to %d",
                fid, XFER_MAXDAT, wav->type.raw.nbyte
            );
            xfer_errno = XFER_ELIMIT;
            return XFER_ERROR;
        }
        memcpy(ptr, wav->type.raw.data, wav->type.raw.nbyte);
        ptr += wav->type.raw.nbyte;

        break;

#ifdef IDA_SUPPORT
      case XFER_WAVIDA:

        sval = htons((u_short) wav->type.ida.rev);
        memcpy(ptr, &sval, slen);
        ptr += slen;

        sval = htons((u_short) wav->type.ida.comp);
        memcpy(ptr, &sval, slen);
        ptr += slen;

        lval = htonl((u_long) wav->type.ida.nbyte);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        if (wav->type.ida.nbyte > XFER_MAXDAT) {
            util_log(1, "%s: increase XFER_MAXDAT (%d) to %d",
                fid, XFER_MAXDAT, wav->type.ida.nbyte
            );
            xfer_errno = XFER_ELIMIT;
            return XFER_ERROR;
        }
        memcpy(ptr, wav->type.ida.data, wav->type.ida.nbyte);
        ptr += wav->type.ida.nbyte;

        break;
#endif /* IDA_SUPPORT */

#ifdef SEED_SUPPORT
      case XFER_WAVSEED:
        lval = htonl((u_long) wav->type.seed.nbyte);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        memcpy(ptr, wav->type.seed.data, wav->type.seed.nbyte);
        ptr += wav->type.seed.nbyte;

        break;
#endif /* SEED_SUPPORT */

#ifdef PASSCAL_SUPPORT
      case XFER_WAVPASSCAL:
        lval = htonl((u_long) wav->type.pas.nbyte);
        memcpy(ptr, &lval, llen);
        ptr += llen;

        break;
#endif /* PASSCAL_SUPPORT */


      default:
        xfer_errno = XFER_EFORMAT;
        return XFER_ERROR;
    }

    return (long) (ptr - start_of_message);
}

/*============================ external use ==========================*/

/* Send an ack */

void Xfer_Ack(sd, code)
int sd;
int code;
{
char *ptr;
static char *fid = "Xfer_Ack";

/* Send a break to alert the client that something unexpected is coming */

    if (xfer_SendMsg(sd, Xfer_Buffer, 0) != XFER_OK) {
        if (xfer_errno != XFER_ETIMEDOUT && xfer_errno != XFER_EPIPE) {
            util_log(1, "%s: xfer_SendMsg: %s", fid, Xfer_ErrStr());
        } else {
            util_log(1, "%s", Xfer_ErrStr());;
        }
        Xfer_Exit(sd, xfer_errno);
    }

/* Send over the cause code */

    ptr = Xfer_Buffer + 4;
    lval = htonl((u_long) code);
    memcpy(ptr, &lval, llen);
    if (xfer_SendMsg(sd, Xfer_Buffer, llen) != XFER_OK) {
        if (xfer_errno != XFER_ETIMEDOUT && xfer_errno != XFER_EPIPE) {
            util_log(1, "%s: xfer_SendMsg: %s", fid, Xfer_ErrStr());
        } else {
            util_log(1, "%s", Xfer_ErrStr());;
        }
        Xfer_Exit(sd, xfer_errno);
    }
}

/* Send a configuration record */

void Xfer_SendCnf(sd, cnf)
int sd;
struct xfer_cnf *cnf;
{
long msglen;
static char *fid = "Xfer_SendCnf";

    if ((msglen = xfer_EncodeCnf(Xfer_Buffer+4, cnf)) < 0) {
        util_log(1, "%s: xfer_EncodeCnf: %s", fid, Xfer_ErrStr());
        Xfer_Ack(sd, xfer_errno);
        Xfer_Exit(sd, xfer_errno);
    }

    if (xfer_SendMsg(sd, Xfer_Buffer, msglen) != XFER_OK) {
        if (xfer_errno != XFER_ETIMEDOUT && xfer_errno != XFER_EPIPE) {
            util_log(1, "%s: xfer_SendMsg: %s", fid, Xfer_ErrStr());
        } else {
            util_log(1, "%s", Xfer_ErrStr());;
        }
        Xfer_Ack(sd, xfer_errno);
        Xfer_Exit(sd, xfer_errno);
    }
}

/* Server initialization */

int Xfer_ServInit(home, timeo)
char *home;
int timeo;
{
int lineno;
FILE *fp;
char *fname, *allowed_ip;
static char *fid = "Xfer_ServInit";

/* Set default time out interval */

    _xfer_timeout = timeo;

/* Install SIGPIPE handler */

    if (xfer_Signals() != XFER_OK) {
        xfer_errno = XFER_EHANDLER;;
        return XFER_ERROR;
    }

/* Read and store access list */

    fname = Xfer_Buffer;

    sprintf(fname, "%s/%s", home, XFER_CLIENTS);
    if ((fp = fopen(fname, "r")) == NULL) {
        util_log(1, "%s: %s", fname, syserrmsg(errno));
        util_log(1, "access control disabled");
    }

    allowed_ip = Xfer_Buffer;

    Num_allowed = 0;
    while (
        (Num_allowed < MAX_ALLOWED) &&
        util_getline(fp, allowed_ip, 128, '#', &lineno) == 0
    ) {
        Allowed[Num_allowed++] = strdup(allowed_ip);
    }

    fclose(fp);

    return XFER_OK;
}

/*  Check client access rights */

int Xfer_Allowed(sd)
int sd;
{
int i, addrlen;
struct sockaddr_in cli_addr, *cli_addrp;
char *ip_address;
static char *fid = "Xfer_Allowed";

/* If there is no access list, then automatically grant access */

    if (Num_allowed == 0) return 1;

/* Otherwise, compare the client IP address against the access list */

    addrlen = sizeof(cli_addr);
    cli_addrp = &cli_addr;
    if (getpeername(sd, (struct sockaddr *)cli_addrp, &addrlen) != 0) {
        if (errno == ENOTSOCK) {
            return 1;
        } else {
            util_log(1, "%s: getpeername: %s", fid, syserrmsg(errno));
            Xfer_Exit(sd, errno);
        }
    }
    ip_address = inet_ntoa(cli_addrp->sin_addr);

    for (i = 0; i < Num_allowed; i++) {
        if (strcmp(ip_address, Allowed[i]) == 0) return 1;
    }

    return 0;
}

/* Determine name of peer */

char *Xfer_Peer(sd)
int sd;
{
int addrlen;
struct sockaddr_in cli_addr, *cli_addrp;
struct hostent *hp, hostent;
static char hostname[MAXHOSTNAMELEN+1];
static char *not_a_socket = "stdio";
static char *fid = "Xfer_Peer";

    addrlen = sizeof(cli_addr);
    cli_addrp = &cli_addr;
    if (getpeername(sd, (struct sockaddr *)cli_addrp, &addrlen) != 0) {
        if (errno == ENOTSOCK) {
            return not_a_socket;
        } else {
            util_log(1, "%s: getpeername: %s", fid, syserrmsg(errno));
            Xfer_Exit(sd, errno);
        }
    }
    hp = gethostbyaddr((char *)
        &cli_addrp->sin_addr, sizeof(struct in_addr), cli_addrp->sin_family
    );
    if (hp != NULL) {
        strcpy(hostname, hp->h_name);
    } else {
        strcpy(hostname, inet_ntoa(cli_addrp->sin_addr));
    }

    return hostname;
}

/* Receive an arbitrary request */

int Xfer_RecvReq(sd, req)
int sd;
struct xfer_req *req;
{
int retval, ilen;
long dummy;
static char *fid = "Xfer_RecvReq";

    ilen = sizeof(int);

    retval = xfer_RecvMsg(sd, Xfer_Buffer, Xfer_Buflen, &dummy);
    if (retval != XFER_OK) {
        if (xfer_errno != XFER_ETIMEDOUT) {
            util_log(1, "%s: xfer_RecvMsg: %s", fid, Xfer_ErrStr());
        }
        return XFER_ERROR;
    }

    if (xfer_DecodeReq(req, Xfer_Buffer) != XFER_OK) {
        return XFER_ERROR;
    }

/* If TCP buffers were specified, configure the socket accordingly */

    if (req->sndbuf > 0) {
        if (
            setsockopt(
                sd, SOL_SOCKET, SO_SNDBUF, (char *) &req->sndbuf, ilen
            ) != 0
        ) {
            util_log(1, "%s: warning: setsockopt(req->sndbuf = %d): %s",
                fid, req->sndbuf, syserrmsg(errno)
            );
        } else {
            util_log(2, "socket sndbuf set to %d bytes", req->sndbuf);
        }
    }

    if (req->rcvbuf > 0) {
        if (
            setsockopt(
                sd, SOL_SOCKET, SO_SNDBUF, (char *) &req->rcvbuf, ilen
            ) != 0
        ) {
            util_log(1, "%s: warning: setsockopt(req->rcvbuf = %d): %s",
                fid, req->rcvbuf, syserrmsg(errno)
            );
        } else {
            util_log(2, "socket rcvbuf set to %d bytes", req->rcvbuf);
        }
    }

    return XFER_OK;
}

/* Send a waveform record */

int Xfer_SendWav(sd, cnf, wav)
int sd;
struct xfer_cnf *cnf;
struct xfer_wav *wav;
{
long msglen;
static char *fid = "Xfer_SendWav";

    if (xfer_Compress(cnf, wav) != XFER_OK) {
        util_log(1, "%s: xfer_Compress: %s", fid, Xfer_ErrStr());
        return XFER_ERROR;
    }

    if ((msglen = xfer_EncodeWav(Xfer_Buffer+4, wav)) < 0) {
        util_log(1, "%s: xfer_EncodeWav: %s", fid, Xfer_ErrStr());
        return XFER_ERROR;
    }

    if (xfer_SendMsg(sd, Xfer_Buffer, msglen) != XFER_OK) {
        if (xfer_errno != XFER_ETIMEDOUT && xfer_errno != XFER_EPIPE) {
            util_log(1, "%s: xfer_SendMsg: %s", fid, Xfer_ErrStr());
        } else {
            util_log(1, "%s", Xfer_ErrStr());;
        }
        return XFER_ERROR;
    }

    return XFER_OK;
}
