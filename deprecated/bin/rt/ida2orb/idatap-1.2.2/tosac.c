/* @(#)tosac.c	1.3 11/07/97 */
/*======================================================================
 * 
 *  Given a FILE of xfer_packet data, convert it to SAC format in
 *  the current directory.
 *
 *  Output files have names of the form sta.chan[.n] where sta.chan are
 *  the (lower case) station and channel names.  If there are more than
 *  one output files for the same station and channel, then subsequent
 *  files are given the .n appendix, where n=1,2,3...
 *
 *  Any pre-existing files in the current directory with the same names
 *  are silently overwritten.
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/param.h>
#include "util.h"
#include "sacio.h"
#include "null_sac.h"
#include "xfer.h"

#define BUFLEN 1024

struct chn_hist {
    char name[XFER_CNAMLEN+1];
    char fname[XFER_SNAMLEN + XFER_CNAMLEN + 10];
    int nrec;
    int ident;
    FILE *fp;
    double tofs;
    double prev_tofs;
    struct sac_header sac;
};

static struct sta_hist {
    char name[XFER_SNAMLEN+1];
    int nchn;
    struct chn_hist chn[XFER_MAXCHN];
} sta[XFER_MAXSTA];

static int nsta;
static int ascii;
static struct sac_header *defaults;
            
static int wrthdr(fp, hdr, packet)
FILE *fp;
struct sac_header *hdr;
struct xfer_packet *packet;
{
int yr, da, hr, mn, sc, ms, chan_code = -1;
static char *fid = "Xfer_ToSAC:wrthdr";

/*  Initialize header if packet is given  */

    if (packet != NULL) {

        util_log(2, "create %s:%s SAC header", packet->sname, packet->cname);

        hdr->npts   = 0;
        hdr->delta  = packet->sint;
        hdr->b      = 0.0;

        util_tsplit(packet->beg, &yr, &da, &hr, &mn, &sc, &ms);
        hdr->nzyear = yr;
        hdr->nzjday = da;
        hdr->nzhour = hr;
        hdr->nzmin  = mn;
        hdr->nzsec  = sc;
        hdr->nzmsec = ms;

        util_ucase(strcpy(hdr->kstnm,  packet->sname));
        util_ucase(strcpy(hdr->kcmpnm, packet->cname));
 
        hdr->iftype = ITIME;
        hdr->leven  = TRUE;
        hdr->lpspol = TRUE;
        hdr->lovrok = TRUE;
        hdr->lcalda = TRUE;
        hdr->idep   = IUNKN;
        hdr->iztype = IB;

        hdr->stla = packet->lat;
        hdr->stlo = packet->lon;
        hdr->stel = packet->elev;
        hdr->stdp = packet->depth;

    }

    rewind(fp);

    if (ascii) {
        if (sacio_wah(fp, hdr) != 0) {
            util_log(1, "error writing SAC header: sacio_wah: %s",
                syserrmsg(errno)
            );
            return -1;
        }
    } else {
        if (sacio_wbh(fp, hdr) != 0) {
            util_log(1, "error writing SAC header: sacio_wbh: %s",
                syserrmsg(errno)
            );
            return -1;
        }
    }

    return 0;
}

static int mkfile(packet, ident, chn)
struct xfer_packet *packet;
int ident;
struct chn_hist *chn;
{
static char *fid = "Xfer_ToSAC:mkfile";

    strcpy(chn->name, packet->cname);
    chn->nrec = 0;
    if (ident) {
        sprintf(chn->fname, "%s.%s.%d", packet->sname, packet->cname, ident);
    } else {
        sprintf(chn->fname, "%s.%s", packet->sname, packet->cname);
    }
    util_lcase(chn->fname);
    util_log(2, "%s: create file %s", fid, chn->fname);
    if ((chn->fp = fopen(chn->fname, "w")) == NULL) {
        util_log(1, "%s: fopen: %s: %s", fid, chn->fname, syserrmsg(errno));
        return -1;
    }

    chn->sac = *defaults;
    chn->sac.npts = 0;
    chn->prev_tofs = -1.0;
    chn->tofs = packet->beg;
    return wrthdr(chn->fp, &chn->sac, packet);
}

int finish(chn, packet)
struct chn_hist *chn;
struct xfer_packet *packet;
{
static char *fid = "Xfer_ToSAC:finish";

    if (wrthdr(chn->fp, &chn->sac, NULL) != 0) {
        util_log(1, "%s: wrthdr after gap failed");
        return -1;
    }

    fclose(chn->fp);
    return (packet == NULL) ? 0 :
                              mkfile(packet, chn->ident + 1, chn);
}

static struct chn_hist *getchn(packet)
struct xfer_packet *packet;
{
FILE *fp;
int i, j, new;
struct chn_hist *chn;
static char *fid = "Xfer_ToSAC:getchn";

    for (i = 0; i < nsta; i++) {

        if (strcasecmp(sta[i].name, packet->sname) == 0) {

            for (j = 0; j < sta[i].nchn; j++) {
                chn = sta[i].chn + j;
                if (strcasecmp(chn->name, packet->cname) == 0) {
                    if (packet->tear) {
                        if (finish(chn, packet) != 0) return NULL;
                    }
                    return chn;
                }
            }

        /*  Must be a new channel for this station... add it to the list */

            if (sta[i].nchn == XFER_MAXCHN) {
                util_log(1, "%s: too many channels, increase XFER_MAXCHN", fid);
                return NULL;
            }

            chn = sta[i].chn + sta[i].nchn; ++sta[i].nchn;
            if (mkfile(packet, 0, chn) != 0) return NULL;
            return chn;
        }
    }

/*  Must be a new station ... add it to the list  */

    if (nsta == XFER_MAXSTA) {
        util_log(1, "%s: too many stations, increase XFER_MAXSTA", fid);
        return NULL;
    }
   
    new = nsta;
    strcpy(sta[new].name, packet->sname);
    chn = sta[new].chn; sta[new].nchn = 1;
    if (mkfile(packet, 0, chn) != 0) return NULL;
    ++nsta;

    return chn;
}

static int wrtdat(chn, packet)
struct chn_hist *chn;
struct xfer_packet *packet;
{
int i;
short *sdata;
long  *ldata;
float fdata[BUFLEN];
static char *fid = "Xfer_ToSAC:wrtdat";


    if (packet->nsamp > BUFLEN) {
        util_log(1, "%s: error: BUFLEN < %ld", fid, packet->nsamp);
        return -1;
    }

    for (i = 0; i < packet->nsamp; i++) fdata[i] = (float) packet->data[i];

    if (++chn->nrec == 1) {
        chn->sac.depmin = fdata[0];
        chn->sac.depmax = fdata[0];
    }
    
    for (i = 0; i < packet->nsamp; i++) {
        if (fdata[i] < chn->sac.depmin) chn->sac.depmin = fdata[i];
        if (fdata[i] > chn->sac.depmax) chn->sac.depmax = fdata[i];
    }

    if (ascii) {
        return packet->nsamp -
               sacio_wad(chn->fp, fdata, packet->nsamp, &chn->sac.npts);
    } else {
        chn->sac.npts += packet->nsamp;
        return packet->nsamp -
               fwrite(fdata, sizeof(float), packet->nsamp, chn->fp);
    }
}

int Xfer_ToSAC(fp, doascii, hdrdefaults, dupflag)
FILE *fp;
int doascii;
struct sac_header *hdrdefaults;
int dupflag;
{
int i, j;
struct chn_hist *chn;
struct xfer_packet packet;
static char *fid = "Xfer_ToSAC";

    nsta = 0;

    if (sizeof(unsigned long) != sizeof(float)) {
        util_log(1, "%s: error: sizeof(unsigned long) != sizeof(float)", fid);
        return -1;
    }

    ascii = doascii;
    if (hdrdefaults != NULL) {
        defaults = hdrdefaults;
    } else {
        defaults = &null_sac_header;
    }

    while (Xfer_RdPacket(fp, &packet) == 0) {
        if ((chn = getchn(&packet)) == NULL) {
            util_log(1, "%s: error: can't get output channel info", fid);
            return -2;
        }
        if (dupflag && chn->prev_tofs == chn->tofs) {
            util_log(2, "%s: dup %s:%s ignored",
                fid, chn->sac.kstnm, chn->sac.kcmpnm
            );
        } else if (wrtdat(chn, &packet) != 0) {
            util_log(1, "%s: error: wrtdat: %s", fid, syserrmsg(errno));
            return -3;
        }
        chn->prev_tofs = chn->tofs;
    }

    util_log(2, "udpate headers and close files");
    for (i = 0; i < nsta; i++) {
        for (j = 0; j < sta[i].nchn; j++) finish(sta[i].chn + j, NULL);
    }

    return 0;
}
