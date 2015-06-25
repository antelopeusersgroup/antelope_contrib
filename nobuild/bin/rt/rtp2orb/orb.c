#pragma ident "$Id$"
/*======================================================================
 *
 * Convert DT packet and send to ORB
 *
 *====================================================================*/
#include "rtp2orb.h"
#include "orb.h"
#include "Pkt.h"
#include "coords.h"

#define MY_MOD_ID RTP2ORB_MOD_ORB
static Packet *Pkt;
static PktChannel *PktChan;

void InitPkt()
{
    Pkt = newPkt ();
    Pkt->pkttype = suffix2pkttype ("GENC");

    PktChan = newPktChannel ();
    Pkt->nchannels=1;
    settbl(Pkt->channels, 0, PktChan);
}

static void ConvertPacket(struct reftek_dt *dt, double samprate, BOOL UseLoc)
{
int i;
IDENTIFIER ident;
double calib, calper;

    LookupDB(dt, &ident, &PktChan->calib, &PktChan->calper);
    if (!UseLoc) ident.loc[0] = 0;
    strcpy(Pkt->parts.src_sta,  ident.sta);
    strcpy(Pkt->parts.src_chan, ident.cha);
    strcpy(Pkt->parts.src_net,  ident.net);
    strcpy(Pkt->parts.src_loc,  ident.loc);
    Pkt->time = PktChan->time = dt->tstamp;
    PktChan->samprate = samprate;
    PktChan->nsamp = dt->nsamp;
    strcpy(PktChan->sta,      ident.sta);
    strcpy(PktChan->chan,     ident.cha);
    strcpy(PktChan->net,      ident.net);
    strcpy(PktChan->loc,      ident.loc);
    strcpy (PktChan->segtype, ident.seg);
    PktChan->data = (int *) dt->data;
    PktChan->datasz = dt->nsamp * sizeof(INT32);
}

#ifdef DEBUG
static void DumpPkt(Packet *pkt)
{
char buf[256];

    printf("Pkt:     %s_%s_%s_%s   %s\n", 
        pkt->parts.src_sta, 
        pkt->parts.src_chan, 
        pkt->parts.src_net, 
        pkt->parts.src_loc,
        util_dttostr(pkt->time, 0, buf)
    );
}

static void DumpPktChan(PktChannel *pktchan)
{
char buf[256];
    printf("PktChan: %s_%s_%s_%s_%s %s %.3f %4d  %10d %10d\n", 
        pktchan->sta,
        pktchan->chan,
        pktchan->net,
        pktchan->loc,
        pktchan->segtype,
        util_dttostr(pktchan->time, 0, buf),
        pktchan->samprate,
        pktchan->nsamp,
        pktchan->data[0],
        pktchan->data[pktchan->nsamp-1]
    );
}
#endif /* DEBUG */

void WriteToOrb(SERVER *orb, UINT8 *das, int verbose, BOOL UseLoc)
{
static int count = 0;
double samprate, tstamp;
char buf[256];
struct reftek_dt dt;
int nbytes;
char *packet;
static int packetsz = 0;
static char srcname[ORBSRCNAME_SIZE];
static char *fid = "WriteToOrb";

/* Only deal with DT packets for which we can get sample rate */

    if (reftek_type(das) != REFTEK_DT) return;

    if (!reftek_dt(&dt, das, TRUE)) {
        rtp_log(RTP_ERR, "%s: DAS pkt dropped (can't decode)\n", fid);
        return;
    }

    if (!DeriveSampleRate(&dt, &samprate)) {
        rtp_log(RTP_ERR, "%s: DAS pkt dropped (unknown sample rate): %s\n",
            fid, reftek_str(das, buf)
        );
        return;
    }

/* Note decompression errors, if any */

    if (dt.dcerr) rtp_log(RTP_ERR, "%s: decompression error %d: %s\n",
        fid, dt.dcerr, reftek_str(das, buf)
    );

/* Convert to GENC format and send to ORB */

    ConvertPacket(&dt, samprate, UseLoc);

    if (stuffPkt(Pkt, srcname, &tstamp, &packet, &nbytes, &packetsz) < 0) {
        rtp_log(RTP_ERR, "stuffPkt routine failed for %s\n", Pkt->pkttype->name);
        return;
    }
    ++count;

    if (verbose) {
        showPkt(0, srcname, tstamp, packet, nbytes, stdout, PKT_TERSE);
    }

    if (orbput(orb->fd, srcname, dt.tstamp, packet, nbytes) != 0) {
        rtp_log(RTP_ERR, "orbput fails\n");
    }
}

/* Revision History
 *
 * $Log$
 * Revision 1.1  2004/03/09 18:28:23  vernon
 * Initial revision
 *
 */
