#pragma ident "$Id$"
/*======================================================================
 *
 * Figure out sample rate by comparing sequential packets
 *
 *====================================================================*/
#include <math.h>
#include "rtp2orb.h"

#define MY_MOD_ID RTP2ORB_MOD_SRATE

#define INCREMENT_OK(p, c) (((c) == (p) + 1) || ((p) == 9999 && (c) == 0))

#define THRESHOLD 1.0 /* hardcode for now (forever?) */

typedef struct {
    REAL64 tstamp;
    UINT16 nsamp;
    BOOL   set;
} DTCHAN;

typedef struct {
    UINT16 unit;
    UINT16 seqno;
    UINT16 stream;
    REAL64 rate;
    DTCHAN chan[RTP_MAXCHAN];
} DTPARM;

struct dt_list {
    DTPARM dt;
    struct dt_list *next;
};
    
static struct dt_list head = {
    {0, 0, 0, 0.0,
        {
            {0.0, 0, FALSE}, {0.0, 0, FALSE}, {0.0, 0, FALSE}, {0.0, 0, FALSE},
            {0.0, 0, FALSE}, {0.0, 0, FALSE}, {0.0, 0, FALSE}, {0.0, 0, FALSE},
            {0.0, 0, FALSE}, {0.0, 0, FALSE}, {0.0, 0, FALSE}, {0.0, 0, FALSE},
            {0.0, 0, FALSE}, {0.0, 0, FALSE}, {0.0, 0, FALSE}, {0.0, 0, FALSE}
        },
    },
    (struct dt_list *) NULL
};

/* Search the list for a previous entry for this stream */

static DTPARM *PreviousDTparam(struct reftek_dt *dt)
{
struct dt_list *crnt;

    crnt = head.next;
    while (crnt != (struct dt_list *) NULL) {
        if (
            crnt->dt.unit   == dt->unit   &&
            crnt->dt.stream == dt->stream
        ) return &crnt->dt;
        crnt = crnt->next;
    }

    return (DTPARM *) NULL;
}

/* Add a new entry to the list */

static void AddNewEntry(struct reftek_dt *dt)
{
int i;
struct dt_list *new;

    new = (struct dt_list *) malloc(sizeof(struct dt_list));
    if (new == (struct dt_list *) NULL) {
        rtp_log(RTP_ERR, "FATAL ERROR: malloc: %s\n", strerror(errno));
        GracefulExit(MY_MOD_ID + 1);
    }

    new->dt.unit   = dt->unit;
    new->dt.seqno  = dt->seqno;
    new->dt.stream = dt->stream;
    new->dt.rate   = -1.0;
    for (i = 0; i < RTP_MAXCHAN; i++) new->dt.chan[i].set = FALSE;
    new->dt.chan[dt->chan].tstamp = dt->tstamp;
    new->dt.chan[dt->chan].nsamp  = dt->nsamp;
    new->dt.chan[dt->chan].set    = TRUE;

    new->next = head.next;
    head.next = new;
}

/* Update entry with new values */

static void UpdateEntry(DTPARM *prev, struct reftek_dt *crnt)
{
    prev->seqno  = crnt->seqno;
    prev->chan[crnt->chan].tstamp = crnt->tstamp;
    prev->chan[crnt->chan].nsamp  = crnt->nsamp;
    prev->chan[crnt->chan].set    = TRUE;
}

/* Compare this packet with the previous one and get sample rate */

BOOL DeriveSampleRate(struct reftek_dt *dt, double *output)
{
int i;
REAL64 NewRate, PercentChange;
DTPARM *prev;

/* Make sure we can index off the channel number */

    if (dt->chan >= RTP_MAXCHAN) {
        rtp_log(RTP_ERR, "FATAL ERROR: illegal chan id: %d\n", dt->chan);
        GracefulExit(MY_MOD_ID + 2);
    }

/* Get the previous parameters for this stream */

    if ((prev = PreviousDTparam(dt)) == (DTPARM *) NULL) {
        AddNewEntry(dt);
        return FALSE;
    }

/* Wipe out prior data if sample numbers fail to increment OK */

    if (!INCREMENT_OK(prev->seqno,dt->seqno)) {
        for (i = 0; i < RTP_MAXCHAN; i++) prev->chan[i].set = FALSE;
    }

/* If we don't have prior data for this channel then use the previous
 * sample rate, if we've got one */

    if (!prev->chan[dt->chan].set) {
        UpdateEntry(prev, dt);
        if (prev->rate > 0.0) {
            *output = prev->rate;
            return TRUE;
        } else {
            return FALSE;
        }
    }

/* Should be able to determine a sample rate at this point */

    NewRate = (double) prev->chan[dt->chan].nsamp /
              (dt->tstamp - prev->chan[dt->chan].tstamp);
    UpdateEntry(prev, dt);
    if (prev->rate < 0.0) prev->rate = NewRate;

/* Note if sample rate changes significantly */

    PercentChange = (fabs(NewRate - prev->rate)/NewRate) * 100.0;
    if (PercentChange > THRESHOLD) {
        rtp_log(RTP_WARN, "WARNING - %hu:%hu srate change from %.3lf to %.3lf\n",
            prev->unit, prev->stream, prev->rate, NewRate
        );
        prev->rate = NewRate;
    }

/* Give it to the caller */

    *output = prev->rate;
    return TRUE;
}

/* Revision History
 *
 * $Log$
 * Revision 1.1  2004/03/09 18:28:23  vernon
 * Initial revision
 *
 */
