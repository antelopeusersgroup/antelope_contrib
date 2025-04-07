/*
 * mseed2orbpkt.c
 * Wrap a Mini-SEED record to make an Antelope ORB packet of type 'SEED'
 *
 * Written by Chad Trabant, ORFEUS/EC-Project MEREDIAN
 *                     now: IRIS Data Management Center
 *
 * version 2012.244
 */

#include <stdlib.h>
#include <unistd.h>

#include <Pkt.h>
#include <coords.h>
#include <db.h>
#include <elog.h>
#include <msd.h>
#include <stock.h>
#include <swapbytes.h>
#include <tr.h>
#include <xtra.h>

int
mseed2orbpkt (char *msrec, int mssize, char *calibdb, char *mappingdb,
              int remap, char *srcname, double *time, char **packet,
              int *nbytes, int *bufsize)
{
  int reclen      = 0;
  int retcode     = 0;
  int version     = 2;
  double calib    = 0.0;
  double calper   = -1.0;
  double samprate = 0.0;
  char segtype[6];
  char snet[10], ssta[10], sloc[10], schan[10];
  char sta[16], chan[16];
  char *cp;
  Srcname parts;
  Msd *msd;

  /* Initialize an empty segtype */
  segtype[0] = '\0';

  /* Initialize a new Msd struct and set record pointer and size */
  msd = msdnew ();
  msdput (msd,
          MSD_RECORD, msrec,
          MSD_RECORD_SIZE, mssize,
          0);

  retcode = msdhdr_unpack (msd);

  if (retcode == 0)
  {
    msdget (msd,
            MSD_RECORD_SIZE, &reclen,
            MSD_SAMPRATE, &samprate,
            MSD_TIME, time,
            MSD_NET, snet,
            MSD_STA, ssta,
            MSD_LOC, sloc,
            MSD_CHAN, schan,
            0);

    if (reclen < mssize)
      return -1;

    /* The ORB packet size is SEED type header (14 bytes) plus record length */
    SIZE_BUFFER (char *, *packet, *bufsize, reclen + 14);
    cp = *packet;

    strcpy (parts.src_net, snet);
    strcpy (parts.src_sta, ssta);
    strcpy (parts.src_chan, schan);
    strcpy (parts.src_loc, sloc);
    strcpy (parts.src_suffix, "SEED");
    *parts.src_subcode = 0;

    if (mappingdb)
    {
      if (map_seed_netsta (snet, ssta, sta) < 0)
      {
        elog_complain (0, "mseed2orbpkt: map_seed_netsta() error [%s_%s]\n",
                       snet, ssta);
        return -1;
      }
      if (map_seed_chanloc (sta, schan, sloc, chan) < 0)
      {
        elog_complain (0, "mseed2orbpkt: map_seed_chanloc() error [%s_%s]\n",
                       schan, sloc);
        return -1;
      }

      if (remap)
      {
        strcpy (parts.src_sta, sta);
        strcpy (parts.src_chan, chan);
        strcpy (parts.src_loc, "");
      }
    }

    join_srcname (&parts, srcname);

    /* Get calibration information */
    if (calibdb && mappingdb)
      dbget_calib (sta, chan, *time, calibdb, &calib, &calper, segtype);

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

    /* Stick on the Mini-SEED record */
    memcpy (cp, msrec, reclen);
    cp += reclen;
    *nbytes = cp - *packet;
  }
  else
  {
    if (samprate != 0.0)
    {
      elog_complain (0, "Cannot parse this packet:");
      hexdump (stderr, msrec, reclen);
    }
  }

  /* free the Msd struct */
  msdfree (msd);

  return retcode;
}
