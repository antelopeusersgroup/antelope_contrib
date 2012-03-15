/* @(#)tocss.c	1.4 11/07/97 */
/*======================================================================
 * 
 *  Demultiplex xfer_packet format data stream into CSS 3.0 format.
 *
 *  Output files have names of the form sta.chan.w where sta.chan are
 *  the (lower case) station and channel names.  The wfdisc for all data
 *  created is named css.wfdisc.
 *
 *  Any pre-existing files in the current directory with the same names
 *  are silently overwritten.
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <stdio.h>
#include <errno.h>
#include "util.h"
#include "cssio.h"
#include "xfer.h"

#define WFDISC_FNAME "xfer.wfdisc"

static char buffer[1024];
static char *datatype;

static int  drmflag = 0;

FILE *wd = NULL;

struct info {
    FILE *fp;
    struct wfdisc wfdisc;
    long foff;
    int count;
} master_info[XFER_MAXSTA][XFER_MAXCHN];

double tofs[XFER_MAXSTA][XFER_MAXCHN];

static int finish(status)
int status;
{
int i, j;
struct info *info;
static char *fid = "Xfer_ToCSS:finish";

    for (i = 0; i < XFER_MAXSTA; i++) {
        for (j = 0; j < XFER_MAXCHN; j++) {
            info = &master_info[i][j];
            if (info->fp != NULL) {
                fclose(info->fp);
                if (wwfdisc(wd, &info->wfdisc) != 0) {
                    util_log(1, "%s: wwfdisc: %s: %s",
                        fid, WFDISC_FNAME, syserrmsg(errno)
                    );
                    return -100 - status;
                }
            }
        }
    }

    fclose(wd);

    return status;
}

static struct wfdisc *load_wfdisc(packet)
struct xfer_packet *packet;
{
static struct wfdisc wfdisc;
int yr, da, hr, mn, sc, ms;
static char *fid = "Xfer_ToCSS:wfdisc";

    wfdisc = wfdisc_null;
    strcpy(wfdisc.sta,     packet->sname);
    strcpy(wfdisc.chan,    packet->cname);
    strcpy(wfdisc.instype, packet->instype);
    wfdisc.time    = packet->beg;
    util_tsplit(wfdisc.time, &yr, &da, &hr, &mn, &sc, &ms);
    wfdisc.jdate   = (yr * 1000) + da;

    strcpy(wfdisc.datatype, datatype);

    wfdisc.nsamp   = packet->nsamp;
    wfdisc.smprate = 1.0 / packet->sint;
    wfdisc.calib   = packet->calib;
    wfdisc.calper  = packet->calper;
    strcpy(wfdisc.dir, ".");
    sprintf(wfdisc.dfile, "%s-%s.w", wfdisc.sta, wfdisc.chan);

    wfdisc.foff = 0;

    return &wfdisc;
}

static struct info *getinfo(struct xfer_packet *packet, int dupflag)
{
int i, j;
struct info *info;
static char *fid = "Xfer_ToCSS:getinfo";

/* See if data for this station/channel have already been seen  */

    for (i = 0; i < XFER_MAXSTA; i++) {
        for (j = 0; j < XFER_MAXCHN; j++) {
            if (master_info[i][j].fp != NULL) {
                info = &master_info[i][j];
                if (strcmp(packet->sname, info->wfdisc.sta) == 0) {
                    if (strcmp(packet->cname, info->wfdisc.chan) == 0) {
                        if (!dupflag) return info;
                        if (tofs[i][j] != info->wfdisc.time) {
                            tofs[i][j]  = info->wfdisc.time;
                            return info;
                        } else {
                            return (struct info *) NULL;
                        }
                    }
                }
            }
        }
    }

/*  Must not have seen it yet... add it to the list  */

    for (i = 0; i < XFER_MAXSTA; i++) {
        for (j = 0; j < XFER_MAXCHN; j++) {
            if (master_info[i][j].fp == NULL) {
                info = &master_info[i][j];
                info->wfdisc = *load_wfdisc(packet);
                if ((info->fp = fopen(info->wfdisc.dfile, "w")) == NULL) {
                    util_log(1, "%s: fopen: %s: %s",
                        fid, info->wfdisc.dfile, syserrmsg(errno)
                    );
                    return NULL;
                }
                return info;
            }
        }
    }

/*  This should never happen  */

    util_log(1, "%s: no free slots! This error should never occur!", fid);
    return (struct info *) NULL;
}

static int wrtpacket(struct xfer_packet *packet, int dupflag)
{
long nbytes, dlen;
struct info *info;
static char *fid =  "wrtpacket";

    dlen = packet->nsamp * sizeof(long);

    if ((info = getinfo(packet, dupflag)) == (struct info *) NULL) return 0;

    if (info->count++ > 0) {
        if (packet->tear) {
            util_log(2, "%s:%s time jump", packet->sname, packet->cname);
            if (wwfdisc(wd, &info->wfdisc) != 0) {
                util_log(1, "%s: wwfdisc: %s: %s",
                    fid, WFDISC_FNAME, syserrmsg(errno)
                );
                return -20;
            }
            info->wfdisc = *load_wfdisc(packet);
            info->wfdisc.foff = info->foff;
        } else {
            info->wfdisc.nsamp  += packet->nsamp;
            info->wfdisc.endtime = packet->end;
            info->foff += dlen;
        }
    }

    nbytes = fwrite(packet->data, sizeof(char), dlen, info->fp);
    if (nbytes != dlen) {
        util_log(1, "%s: fwrite: %s: %s", 
            fid, info->wfdisc.dfile, syserrmsg(errno)
        );
        return -30;
    }

    return 0;
}

int Xfer_ToCSS(FILE *fp, int dupflag)
{
int i, j, status;
long count = 0;
struct xfer_packet packet;
static char *fid = "Xfer_ToCSS";

    datatype = cssio_datatype(sizeof(long), util_order(), 1);

    for (i = 0; i < XFER_MAXSTA; i++) {
        for (j = 0; j < XFER_MAXCHN; j++) {
            master_info[i][j].fp    = NULL;
            master_info[i][j].foff  = 0;
            master_info[i][j].count = 0;
            tofs[i][j] = -1.0;
        }
    }

    if ((wd = fopen(WFDISC_FNAME, "w")) == NULL) {
        util_log(1, "%s: fopen: %s: %s", fid, WFDISC_FNAME, syserrmsg(errno));
        return finish(-1);
    }

    while (Xfer_RdPacket(fp, &packet) == 0) {
        if ((status = wrtpacket(&packet, dupflag)) != 0) {
            return finish(status);
        }
    }

    return finish(0);
}
