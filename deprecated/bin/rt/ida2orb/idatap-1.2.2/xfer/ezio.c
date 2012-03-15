/* @(#)ezio.c	1.7 12/10/97 */
/*======================================================================
 *
 * A set of client side routines to take care of much of the drudgery
 * associated with a data feed following the NRTS data exchange protocol.
 * This code hides most of the details of the protocol at the cost of
 * loss of flexibility in format selection, however it gains in that the
 * programming interface is about as simple as it can get.  The user
 * does not have to worry about how to deal with timeouts, retrys, etc.
 * and just loops around calls to Xfer_Read.  The output can be piped
 * to one of the data conversion filters.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <stdio.h>
#include <errno.h>
#include <memory.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include "util.h"
#include "xfer.h"

#define WRP(xp) (&((xp)->req.request.wav.ver01))
#define CNP(xp) (&((xp)->cnf.type.gen1))

#define STACMP(xp, i, j)    strcasecmp(\
    WRP(xp)->sta[i].name, CNP(xp)->sta[j].name\
)
#define CHNCMP(xp, i,j,k,l) strcasecmp(\
    WRP(xp)->sta[i].chn[j].name, CNP(xp)->sta[k].chn[l].name\
)

static void remove_chn(xp, rs, rc)
XFER *xp;
int rs;
int rc;
{
int i;
struct xfer01_stareq *sp;

    sp = WRP(xp)->sta + rs;
    for (i = rc; i < sp->nchn - 1; i++) sp->chn[i] = sp->chn[i+1];
    if (--sp->nchn == 0) {
        for (i = rs; i < WRP(xp)->nsta - 1; i++) {
            WRP(xp)->sta[i] = WRP(xp)->sta[i+1];
        }
        --WRP(xp)->nsta;
    }
}

#define MYBUFLEN 256

XFER *Xfer_Open2(host, port, sc, beg, end, keepup, retry, to, tto)
char *host;
int port;
char *sc;
double beg;
double end;
int keepup;
int retry;
int to;
int tto;
{
XFER *xp;
int i, j, status;
int rs;  /* req sta index  */
int rc;  /* req chn index  */
int cs;  /* cnf sta index  */
int cc;  /* cnf chn index  */
static char buf[MYBUFLEN+1];
static char *fid = "Xfer_Open2";

    xp = (XFER *) malloc(sizeof(XFER));
    if (xp == (XFER *) NULL) return (XFER *) NULL;

    if (host == (char *) NULL) {
        errno = EINVAL;
        return (XFER *) NULL;
    }
    if (strlen(host) < 1) {
        errno = EINVAL;
        return (XFER *) NULL;
    }
    if (strlen(host) > MAXHOSTNAMELEN) {
        errno = E2BIG;
        return (XFER *) NULL;
    }

    if (sc == (char *) NULL) {
        errno = EINVAL;
        return (XFER *) NULL;
    }
    if (strlen(sc) < 1 || strlen(sc) > MYBUFLEN) {
        errno = EINVAL;
        return (XFER *) NULL;
    }
    if (strlen(sc) > MYBUFLEN) {
        errno = E2BIG;
        return (XFER *) NULL;
    }

    strcpy(xp->host, host);
    strcpy(buf, sc);

    xp->port     = port > 1024 ? port : XFER_PORT;
    xp->keepup   = keepup;
    xp->retry    = retry;
    xp->tto      = tto > 0 ? tto : XFER_DEFTTO;
    xp->req.type = XFER_WAVREQ;
    xp->sd       = -1;

/* Initialize map between cnf and req indices */

    if (xp->retry) {
        for (i = 0; i < XFER_MAXSTA; i++) {
            xp->rsi[i] = -1;
            xp->csi[i] = -1;
            for (j = 0; j < XFER_MAXCHN; j++) {
                xp->rci[i][j] = -1;
                xp->cci[i][j] = -1;
            }
        }
    }

/* Fill in the request structure */

    xp->req.type = XFER_WAVREQ;
    status = Xfer_FillReq(
        &xp->req, XFER_CNFGEN1, XFER_WAVGEN1, buf, beg, end,
        xp->keepup, to
    );

    if (status != 0) {
        util_log(1, "%s: Xfer_FillReq failed, status %d", fid, status);
        free(xp);
        return (XFER *) NULL;
    }

/* Establish connection with server and submit request */

    xp->sd = Xfer_Connect2(
        xp->host, NULL, xp->port, "tcp", &xp->req, &xp->cnf,
        xp->retry, xp->tto
    );

    if (xp->sd < 0) {
        free(xp);
        return (XFER *) NULL;
    }

    if (!xp->retry) return xp;

/* Establish the cnf <-> req indices maps */

    for (rs = 0; rs < WRP(xp)->nsta; rs++) {
        for (cs = 0; cs < CNP(xp)->nsta; cs++) {
            if (STACMP(xp, rs, cs) == 0) {
                xp->rsi[cs] = rs;
                xp->csi[rs] = cs;
                for (rc = 0; rc < WRP(xp)->sta[rs].nchn; rc++) {
                    for (cc = 0; cc < CNP(xp)->sta[cs].nchn; cc++) {
                        if (CHNCMP(xp, rs, rc, cs, cc) == 0) {
                            xp->rci[cs][cc] = rc;
                            xp->cci[rs][rc] = cc;
                        }
                    }
                }
            }
        }
    }

/* Eliminate any requests for non existant stations */

    for (rs = 0; rs < WRP(xp)->nsta; rs++) {
        if (xp->csi[rs] < 0) WRP(xp)->sta[rs].nchn = 0;
    }

    return xp;

}

int Xfer_Read(xp, packet)
XFER *xp;
struct xfer_packet *packet;
{
int status;
int rs;  /* req sta index  */
int rc;  /* req chn index  */
int cs;  /* cnf sta index  */
int cc;  /* cnf chn index  */
static struct xfer_wav wav;
struct xfer01_stareq *sp;
struct xfer01_chnreq *cp;
static char *fid = "Xfer_Read";

    do {

    /* Connect if necessary */

        if (xp->sd < 0) {
            xp->sd = Xfer_Connect2(
                xp->host, NULL, xp->port, "tcp", &xp->req, &xp->cnf,
                xp->retry, xp->tto
            );
            if (xp->sd < 0) return XFER_ERROR;
        }

    /* Read the next packet from the server */

        status = Xfer_RecvWav(xp->sd, &xp->cnf, &xp->wav);

        if (status != XFER_OK) {
            close(xp->sd);
            xp->sd = -1;
            if (status == XFER_FINISHED) return status;

        /* Deal with time outs following prescribed policy */

            if (xfer_errno == XFER_ETIMEDOUT) {

            /* If we don't want to deal with timeouts we are done */

                if (!xp->retry) return status;

            /* Minimal delay means request the youngest packet     */
            /* Minimal gap has already been dealt with (see below) */

                if (xp->retry == XFER_MINIMAL_DELAY) {
                    for (rs = 0; rs < WRP(xp)->nsta; rs++) {
                        sp = WRP(xp)->sta + rs;
                        for (rc = 0; rc < sp->nchn; rc++) {
                            sp->chn[rc].beg = XFER_YNGEST;
                        }
                    }
                }
            }
        }

    } while (status != XFER_OK);

/* Must have been a good read... convert packet to output format */

    if (xfer_Convert(&xp->cnf, &xp->wav, packet) != 0) {
        util_log(1, "%s: xfer_Convert failure!", fid);
        close(xp->sd);
        xp->sd = -1;
        return XFER_ERROR;
    }

    if (!xp->retry) return XFER_OK;

/* Update request structure in case we have to reconnect after timeout */

    cs = xp->wav.type.gen1.standx;
    cc = xp->wav.type.gen1.chnndx;
    rs = xp->rsi[cs];
    rc = xp->rci[cs][cc];

    WRP(xp)->sta[rs].chn[rc].beg = packet->end + packet->sint;

/* If all data for this channel have arrived, purge it from the request */

    if (
        WRP(xp)->sta[rs].chn[rc].end > (double) 0 && 
        WRP(xp)->sta[rs].chn[rc].beg >= WRP(xp)->sta[rs].chn[rc].end
    ) remove_chn(xp, rs, rc);

    return XFER_OK;
}

void Xfer_Close(xp)
XFER *xp;
{
    if (xp == (XFER *) NULL) return;
    if (xp->sd > 0) close(xp->sd);
    free(xp);
}

#ifdef DEBUG_TEST

#include <stdio.h>
#include "xfer.h"
 
main(argc, argv)
int argc;
char *argv[];
{
static char *host = "idahub.ucsd.edu";
static char *sc   = "pfo:bhz+tau:bhz";
int status;
XFER *xp;
struct xfer_packet packet;
int header_only = 1; /* use 0 to enable stdout packet dumps */

    util_logopen(NULL, 1, 9, 3, NULL, argv[0]);
 
    xp = Xfer_Open(host, sc, XFER_YNGEST, XFER_YNGEST, 1, 1);
    if (xp == (XFER *) NULL) {
        fprintf(stderr, "%s: %s\n", host, Xfer_ErrStr());
        exit(1);
    }
 
    while ((status = Xfer_Read(xp, &packet)) == XFER_OK) {
        if (header_only) {
            printf("%s:%s %13.3lf %13.3lf %4d %6.2f %7ld %7ld\n",
                packet.sname, packet.cname, packet.beg, packet.end,
                packet.nsamp, packet.sint, packet.data[0],
                packet.data[packet.nsamp-1]
            );
        } else {
            fwrite(&packet, sizeof(packet), 1, stdout);
        }
    }
 
    if (status != XFER_FINISHED) {
        fprintf(stderr, "%s: %s\n", host, Xfer_ErrStr());
        exit(1);
    }
    
    Xfer_Close(xp);

    exit(0);
}

#endif DEBUG_TEST
