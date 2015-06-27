#pragma ident "$Id$"
/*======================================================================
 *
 * Read data from a single Refraction Technology RTPD server and load
 * them into an ORB.
 *
 *====================================================================*/
#include "rtp2orb.h"
#include "orb.h"

#define MY_MOD_ID RTP2ORB_MOD_MAIN

static RTP *Rtp = (RTP *) NULL;

void GracefulExit(int status)
{
    if (Rtp != (RTP *) NULL) {
        rtp_log(RTP_INFO, "breaking connection with %s:%hu\n",
            Rtp->peer, Rtp->port
        );
        rtp_break(Rtp);
        rtp_close(Rtp);
    }
    rtp_log(RTP_INFO, "exit %d\n", status);
    exit(status);
}

static void fexit(char *myname, int status)
{
    fprintf(stderr, "%s: exit %d\n", myname, status);
    exit(status);
}

static void usage(char *prog)
{
    fprintf(stderr, "%s %s\n", prog, ReleaseIdent);
    fprintf(stderr, "usage: %s [-v] pfpath\n", prog);
    exit(1);
}

int
main(int argc, char **argv)
{
int i, verbose=0;
char *pfpath = NULL;
PARAM par;
UINT8 das[RTP_MAXMSGLEN];
INT32  nbytes;
UINT32 count = 0;
struct rtp_attr attr;

    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-v") == 0) {
            ++verbose;
        } else if (strcmp(argv[i], "-V") == 0) {
            usage(argv[0]);
        } else if (pfpath != NULL) {
            usage(argv[0]);
        } else {
            pfpath = argv[i];
        }
    }

/* Turn on RTP libraray logging to stdout */

    if (!rtp_loginit("stdout", 0, NULL, argv[0])) {
        fprintf(stderr, "%s: FATAL ERROR: rtp_loginit: %s\n",
            argv[0], strerror(errno)
        );
        fexit(argv[0], MY_MOD_ID + 2);
    }

/* Read the parameter file */

    if (!ReadPar(argv[0], pfpath, &par)) fexit(argv[0], MY_MOD_ID + 2);
    LogPar(argv[0], pfpath, &par);

/* Initialize the channel name mapping and packet converter */

    if (!InitDB(par.dbname)) {
        fprintf(stderr, "%s: FATAL ERROR: InitDB failed!\n", argv[0]);
        fexit(argv[0], MY_MOD_ID + 3);
    }

    InitPkt();

/* Attach to the ORB */

    rtp_log(RTP_INFO, "connecting to ORB@%s\n", par.orb.ident);
    while ((par.orb.fd = orbopen(par.orb.ident, "w&")) < 0) {
        if (count++ % 6 == 0) {
            rtp_log(RTP_INFO, "ORB@%s not responding, still trying\n",
                par.orb.ident, errno
            );
        }
        sleep(10);
    }
    rtp_log(RTP_INFO, "connected to ORB@%s\n", par.orb.ident);

/* read/write until we don't anymore */

    Rtp = NULL;

    while (1) {

        while (Rtp == NULL) {
            attr = par.attr;
            rtp_log(RTP_INFO, "initiating connection to RTPD@%s\n",
                par.rtpd.ident
            );

            Rtp = rtp_open(
                par.rtpd.host, par.rtpd.port, &attr, RTP_ERR_NONFATAL
            );

            if (Rtp == (RTP *) NULL) {
                rtp_log(RTP_ERR, "FATAL ERROR: rtp_open: %s\n",
                    strerror(errno)
                );
                GracefulExit(MY_MOD_ID + 4);
            }

            rtp_log(RTP_INFO, "connected to RTPD@%s", par.rtpd.ident);
        }

    /* Get a packet */


        nbytes = 0;
        if (!rtp_daspkt(Rtp, das, &nbytes)) {
            rtp_log(RTP_INFO, "RTPD@%s not responding:  %s\n",
                par.rtpd.ident, strerror(errno)
            );

            if (rtp_errno(Rtp) > RTP_ERR_NONFATAL) {
                GracefulExit(MY_MOD_ID + 5);
            } else {
                rtp_close(Rtp);
                Rtp = NULL;
                sleep(10);
            }
        } else if (nbytes > 0 && rtp_want(Rtp, das)) {
            WriteToOrb(&par.orb, das, verbose, par.UseLoc);
        }
    }
}

/* Revision History
 *
 * $Log$
 * Revision 1.1  2004/08/25 22:47:59  vernon
 * Rebuilt all the software and was able to get it to complie now.
 *
 * Revision 1.1.1.1  2004/03/09 18:28:23  vernon
 * adding rtp2orb first attempt
 *
 *
 */
