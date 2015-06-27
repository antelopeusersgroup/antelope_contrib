#include "defunctpkt.h" 
#include "stock.h"

Orbpipe        *
new_orbpipe (int maxpkts)
{
    Orbpipe        *pipe;
    allot (Orbpipe *, pipe, 1);
    pipe->tbl = inittbl (0, maxpkts + 1, 1, 0, sizeof (Orbpipe_packet));
    pipe->maxpkts = maxpkts;
    pipe->last = 0.0;
    pipe->rpkt.packet = 0;
    return pipe;
}

void 
free_orbpipe (
	      Orbpipe * pipe,
	      void (*free_packet) (char *))
{
    int             i,
                    n;
    Orbpipe_packet *ppkt;
    n = maxtbl (pipe->tbl);
    if (free_packet != 0) {
	for (i = 0; i < n; i++) {
	    ppkt = (Orbpipe_packet *) gettbl (pipe->tbl, i);
	    free_packet (ppkt->packet);
	}
	if (pipe->rpkt.packet != 0)
	    free_packet (pipe->rpkt.packet);
    }
    freetbl (pipe->tbl, 0);
    free (pipe);
}

int 
orbsort (Orbpipe * pipe,
	 int *rpktid,
	 double *rtime,
	 char *rsrcname,
	 char **rpacket,
	 int *rnbytes,
	 int *rbufsize)
{
    Orbpipe_packet *rpkt = 0,
                   *ipkt;
    int             i,
                    npkts,
                    retcode = 1;

    pipe->rpkt.pktid = *rpktid;
    pipe->rpkt.time = *rtime;
    strcpy (pipe->rpkt.srcname, rsrcname);
    pipe->rpkt.packet = *rpacket;
    pipe->rpkt.nbytes = *rnbytes;
    pipe->rpkt.bufsize = *rbufsize;

    npkts = maxtbl (pipe->tbl);
    for (i = npkts - 1; i >= 0; i--) {
	ipkt = (Orbpipe_packet *) gettbl (pipe->tbl, i);
	if (ipkt->time <= *rtime)
	    break;
    }
    if (i == -1 && pipe->last > *rtime) {
	rpkt = &pipe->rpkt;
	retcode = 2;
    } else {
	instbl (pipe->tbl, i + 1, &pipe->rpkt);
	pipe->rpkt.packet = 0;
	if (npkts >= pipe->maxpkts)
	    rpkt = shifttbl (pipe->tbl);
    }
    if (rpkt != 0) {
	*rpktid = rpkt->pktid;
	if (rpkt->time == pipe->last)
	    retcode = 3;
	*rtime = pipe->last = rpkt->time;
	*rpacket = rpkt->packet;
	*rnbytes = rpkt->nbytes;
	*rbufsize = rpkt->bufsize;
	strcpy (rsrcname, rpkt->srcname);
    } else {
	retcode = 0;
	*rpktid = -1;
	*rtime = 0.0;
	*rpacket = 0;
	*rnbytes = 0;
	*rbufsize = 0;
	*rsrcname = '\0';
    }
    return retcode;
}

/* $Id$ */
