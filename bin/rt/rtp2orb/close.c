#pragma ident "$Id$"
/*======================================================================
 * 
 * Close a connection, and free all resources.
 *
 *====================================================================*/
#include "rtp.h"

VOID rtp_close(RTP *rtp)
{
    if (rtp == (RTP *) NULL) return;

/* Shutdown the connection */

    if (rtp->sd >= 0) {
        shutdown(rtp->sd, 2);
#ifndef WINNT
        close(rtp->sd);
#else
        closesocket(rtp->sd);
#endif
    }

/* Free resources */

    if (rtp->soh.sh_nslot > 0) free(rtp->soh.sh_stat);
    free(rtp->peer);
    free(rtp);
}

/* Revision History
 *
 * $Log$
 * Revision 1.1  2004/08/25 22:47:58  vernon
 * Rebuilt all the software and was able to get it to complie now.
 *
 * Revision 1.1.1.1  2004/03/09 18:28:23  vernon
 * adding rtp2orb first attempt
 *
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
