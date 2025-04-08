/***************************************************************************
 * mseed2orbpkt.c
 *
 * Convert a miniSEED record to an Antelope ORB packet.
 *
 * Written by Chad Trabant, ORFEUS/EC-Project MEREDIAN
 *                    then: IRIS Data Management Center
 *                     now: EarthScope Data Services
 **************************************************************************/

#include <stdlib.h>
#include <unistd.h>

#include <libslink.h>

#include "libmseed/libmseed.h"

#include <Pkt.h>
#include <coords.h>
#include <db.h>
#include <elog.h>
#include <stock.h>
#include <swapbytes.h>
#include <tr.h>
#include <xtra.h>

/***************************************************************************
 * mseed2orbpkt():
 *
 * Convert a miniSEED record to an Antelope ORB packet.
 *
 * Returns 0 on success, -1 on error.
 ***************************************************************************/
int
mseed2orbpkt (char payloadformat, const char *msrec, uint32_t mssize,
              char *calibdb, char *mappingdb,
              int remap, char *srcname, double *time, char **packet,
              int *nbytes, int *packetsz)
{
  MS3Record *msr  = NULL;
  int version     = 0;
  double calib    = 0.0;
  double calper   = -1.0;
  double samprate = 0.0;
  char segtype[6] = {0};
  char snet[10], ssta[10], sloc[10], schan[10];
  char sta[16], chan[16];
  char *cp;
  Srcname parts;

  /* Parse miniSEED record header */
  if (msr3_parse (msrec, mssize, &msr, 0, 0))
  {
    elog_complain (0, "%s: Error unpacking miniSEED record\n", __func__);
    return -1;
  }

  /* Parse SEED (or extended) codes from Source ID */
  if (ms_sid2nslc (msr->sid, snet, ssta, sloc, schan) < 0)
  {
    elog_complain (0, "%s: Error parsing Source ID: %s\n", __func__, msr->sid);
    msr3_free (&msr);
    return -1;
  }

  /* Set packet time, first sample */
  *time = (double)MS_NSTIME2EPOCH (msr->starttime);

  /* Get the sample rate */
  samprate = msr3_sampratehz (msr);

  /* Construct source name */
  strcpy (parts.src_net, snet);
  strcpy (parts.src_sta, ssta);
  strcpy (parts.src_chan, schan);
  strcpy (parts.src_loc, sloc);
  strcpy (parts.src_suffix, "SEED");
  parts.src_subcode[0] = '\0';

  if (mappingdb)
  {
    if (map_seed_netsta (snet, ssta, sta) < 0)
    {
      elog_complain (0, "%s: map_seed_netsta() error [%s_%s]\n",
                     __func__, snet, ssta);
      msr3_free (&msr);
      return -1;
    }
    if (map_seed_chanloc (sta, schan, sloc, chan) < 0)
    {
      elog_complain (0, "%s: map_seed_chanloc() error [%s_%s]\n",
                     __func__, schan, sloc);
      msr3_free (&msr);
      return -1;
    }

    if (remap)
    {
      strcpy (parts.src_sta, sta);
      strcpy (parts.src_chan, chan);
      strcpy (parts.src_loc, "");
    }
  }

  /* Create SEED type packet for miniSEED v2 record */
  if (payloadformat == SLPAYLOAD_MSEED2)
  {
    /* The ORB packet size is SEED type header (14 bytes) plus record length */
    SIZE_BUFFER (char *, *packet, *packetsz, mssize + 14);
    cp = *packet;

    /* Get calibration information */
    if (calibdb && mappingdb)
      dbget_calib (sta, chan, *time, calibdb, &calib, &calper, segtype);

    version = msr->pubversion;

    /* Build the ORB packet */
    HI2NC (cp, &version, 1);
    cp += 1 * 1;
    memcpy (cp, &segtype, 1);
    cp += 1 * 1;
    HD2NF (cp, &samprate, 1);
    cp += 4 * 1;
    HD2NF (cp, &calib, 1);
    cp += 4 * 1;
    HD2NF (cp, &calper, 1);
    cp += 4 * 1;

    /* Append the miniSEED record */
    memcpy (cp, msrec, mssize);
    cp += mssize;
    *nbytes = cp - *packet;

    /* Generate srcname from parts */
    join_srcname (&parts, srcname);
  }
  /* Create GENC type packet for miniSEED v3 record */
  else if (payloadformat == SLPAYLOAD_MSEED3)
  {
    /* Unpack the data samples */
    if (msr3_unpack_data (msr, 0) < 0)
    {
      elog_complain (0, "%s: Error unpacking data samples\n", __func__);
      msr3_free (&msr);
      return -1;
    }

    /* Create single channel GENC packet */
    Packet *pkt         = newPkt ();
    PktChannel *pktchan = newPktChannel ();

    pkt->pkttype = suffix2pkttype ("GENC");
    pushtbl (pkt->channels, pktchan);
    pkt->nchannels = 1;
    pkt->version   = msr->pubversion;

    strcpy (pkt->parts.src_net, parts.src_net);
    strcpy (pkt->parts.src_sta, parts.src_sta);
    strcpy (pkt->parts.src_chan, parts.src_chan);
    strcpy (pkt->parts.src_loc, parts.src_loc);

    strcpy (pktchan->net, parts.src_net);
    strcpy (pktchan->sta, parts.src_sta);
    strcpy (pktchan->chan, parts.src_chan);
    strcpy (pktchan->loc, parts.src_loc);

    strcpy (pktchan->segtype, segtype);

    pktchan->nsamp    = msr->numsamples;
    pktchan->datasz   = msr->numsamples;
    pktchan->samprate = msr3_sampratehz (msr);
    pktchan->time     = (double)MS_NSTIME2EPOCH (msr->starttime);
    pktchan->calib    = calib;
    pktchan->calper   = calper;

    if (msr->sampletype == 'i')
    {
      pktchan->data    = msr->datasamples;
      pktchan->isfloat = 0;
    }
    else if (msr->sampletype == 'f')
    {
      pktchan->data    = msr->datasamples;
      pktchan->isfloat = 1;
    }
    else
    {
      elog_complain (0, "%s: Error - unsupported sample type: %c\n",
                     __func__, msr->sampletype);
      msr3_free (&msr);
      freePkt (pkt);
      return -1;
    }

    /* Disconnect datasamples from msr, owned by pktchan now */
    msr->datasamples = NULL;
    msr->numsamples  = 0;

    if (stuffPkt (pkt, srcname, time, packet, nbytes, packetsz) < 0)
    {
      elog_complain (0, "%s: Error stuffing packet\n", __func__);
      msr3_free (&msr);
      freePkt (pkt);
      return -1;
    }

    freePkt (pkt);
  }
  else
  {
    elog_complain (0, "%s: Error - unsupported payload format: %d\n",
                   __func__, payloadformat);
    msr3_free (&msr);
    return -1;
  }

  msr3_free (&msr);

  return 0;
}
