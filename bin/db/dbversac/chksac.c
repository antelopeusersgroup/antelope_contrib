
#include <stdio.h>
#include <sys/param.h>
#include "sac.h"
#include "coords.h"
#include "db.h"
#include "stock.h"


#define ARR 1
#define ASSOC 2
#define NOARRIVALS 3

/* static char    *segtype[] =   { "D", "V", "A"}; */

#define FLOATS_DIFFER(a,b) (ABS((a)-(b))/(ABS(a) + ABS(b)) > 1.0e-5)

int
chksac (Dbptr db)
{
    sac_t           sach;
    FILE           *infile;
    char            sta[MAX_STA_SIZE];
    char            chan[MAX_CHAN_SIZE];
    double          evtime,
                    evla,
                    evlo,
                    stla,
                    stlo,
                    evdp,
                    stel,
                    stdp,
                    cmpaz,
                    cmpinc;
    char            segtype[16];
    char            datatype[10];
    double          sec;
    char            instype[50],
                    clip[5];
    long            commid;
    char            filename[MAXPATHLEN];
    long            nsamp;
    double          time,
                    endtime,
                    samprate,
                    calib;
    long            foff;
    int             nzyear,
                    nzjday,
                    nzhour,
                    nzmin,
                    nzsec,
                    nzmsec;
    char            kstnm[10],
                    skstnm[10],
                    kcmpnm[10],
                    skcmpnm[10];

    dbgetv (db, 0,
	    "sta", sta,
	    "chan", chan,
	    "nsamp", &nsamp,
	    "time", &time,
	    "endtime", &endtime,
	    "samprate", &samprate,
	    "calib", &calib,
	    "segtype", segtype,
	    "instype", instype,
	    "datatype", datatype,
	    "clip", clip,
	    "commid", &commid,
	    "foff", &foff,
	    "site.lat", &stla,
	    "site.lon", &stlo,
	    "site.elev", &stel,
	    "sitechan.edepth", &stdp,
	    "sitechan.hang", &cmpaz,
	    "sitechan.vang", &cmpinc,
	    "origin.time", &evtime,
	    "origin.lat", &evla,
	    "origin.lon", &evlo,
	    "origin.depth", &evdp,
	    NULL);

    dbfilename (db, filename);

    if (calib != 1.0)
	complain (0, "Invalid calib = %5.1f for record #%ld file %s\n",
		  calib, db.record, filename);

    if (foff != sizeof (struct sac))
	complain (0, "Warning: odd header length for SAC file: %ld for record #%ld file %s\n",
		  foff, db.record, filename);

    if ((infile = fopen (filename, "r")) == NULL) {
	complain (1, "Can't open SAC waveform file %s\n", filename);
	return -1;
    }
    /* read the SAC header */
    if (fread ((char *) &sach, sizeof (struct sac), 1, infile) != 1) {
	complain (1, "Can't read SAC header from %s\n", filename);
	return -1;
    }
    if (sach.npts != nsamp)
	complain (0, "nsamp wrong: %5d (sac) != %5ld (css) for record #%ld file %s\n",
		  sach.npts, nsamp, db.record, filename);

    if (sach.b != 0.0)
	complain (0, "b nonzero = %10.3f for record #%ld file %s\n",
		  sach.b, db.record, filename);

    if (FLOATS_DIFFER (sach.e, (endtime - time)))
	complain (0, "e (%10.5f) not equal to (endtime-time) = %10.5f for record #%ld file %s\n",
		  sach.e, endtime - time, db.record, filename);

    if (sach.iftype != ITIME)
	complain (0, "iftype = %d, not ITIME (%d) for record #%ld file %s\n",
		  sach.iftype, ITIME, db.record, filename);

    sach.leven = TRUE;

    if (FLOATS_DIFFER (sach.delta, 1.0 / samprate))
	complain (0, "incompatible samprate's 1/sac.delta=%10.5le samprate=%10.5le for record #%ld file %s\n",
		  1.0 / sach.delta, samprate, db.record, filename);

    switch (segtype[0]) {
      case 'A':
	if (sach.idep != IACC)
	    complain (0, "incompatible segtypes: sac.idep=%d segtype=%s for record #%ld file %s\n",
		      sach.idep, segtype, db.record, filename);
	break;

      case 'V':
	if (sach.idep != IVEL)
	    complain (0, "incompatible segtypes: sac.idep=%d segtype=%s for record #%ld file %s\n",
		      sach.idep, segtype, db.record, filename);

	break;

      case 'D':
      default:
	if (sach.idep != IDISP)
	    complain (0, "incompatible segtypes: sac.idep=%d segtype=%s for record #%ld file %s\n",
		      sach.idep, segtype, db.record, filename);

	break;
    }

    /* depmin/depmax/depmen - min max and mean of dependent variable : no
     * checking */

    e2h (time, &nzyear, &nzjday, &nzhour, &nzmin, &sec);
    nzsec = sec;
    nzmsec = rint ((sec - nzsec) * 1000.);
    if (sach.nzyear != nzyear
	    || sach.nzjday != nzjday
	    || sach.nzhour != nzhour
	    || sach.nzmin != nzmin
	    || sach.nzsec != nzsec
	    || sach.nzmsec != nzmsec
	)
	complain (0, "starting time wrong: sac: %4d (%3d) %02d:%02d:%02d.%03d  %s for record #%ld file %s\n",
		  sach.nzyear, sach.nzjday, sach.nzhour, sach.nzmin, sach.nzsec, sach.nzmsec,
		  strtime (time),
		  db.record, filename);

    sncopy (kstnm, sta, 8);
    szcopy (skstnm, sach.kstnm, 8);
    if (strncmp (kstnm, sach.kstnm, 8) != 0)
	complain (0, "station names differ: sac= %s  css=%s for record #%ld file %s\n",
		  skstnm, sta, db.record, filename);

    sncopy (kcmpnm, chan, 8);
    szcopy (skcmpnm, sach.kcmpnm, 8);
    if (strncmp (kcmpnm, sach.kcmpnm, 8) != 0)
	complain (0, "channel names differ: sac= %s  css=%s for record #%ld file %s\n",
		  skcmpnm, chan, db.record, filename);

    if (FLOATS_DIFFER (sach.stla, stla))
	complain (0, "Different station latitudes %10.3f   %10.3f for record #%ld file %s\n",
		  sach.stla, stla, db.record, filename);


    if (FLOATS_DIFFER (sach.stlo, stlo))
	complain (0, "Different station longitudes %10.3f   %10.3f for record #%ld file %s\n",
		  sach.stlo, stlo, db.record, filename);

    if (FLOATS_DIFFER (sach.stel, stel * 1000.0))
	complain (0, "Different station elevations %10.3f   %10.3f for record #%ld file %s\n",
		  sach.stel, stel * 1000.0, db.record, filename);

    if (FLOATS_DIFFER (sach.stdp, stdp * 1000.0))
	complain (0, "Different station depths %10.3f   %10.3f for record #%ld file %s\n",
		  sach.stdp, stdp * 1000.0, db.record, filename);


    if (FLOATS_DIFFER (sach.cmpaz, cmpaz))
	complain (0, "Different channel azimuth (vang) %10.3f   %10.3f for record #%ld file %s\n",
		  sach.cmpaz, cmpaz, db.record, filename);

    if (FLOATS_DIFFER (sach.cmpinc, cmpinc))
	complain (0, "Different channel incident angle (hang)  %10.3f   %10.3f for record #%ld file %s\n",
		  sach.cmpinc, cmpinc, db.record, filename);


    if (FLOATS_DIFFER (sach.o, evtime - time))
	complain (0, "Different event times %10.3f   %10.3f for record #%ld file %s\n",
		  sach.o, evtime - time, db.record, filename);

    if (FLOATS_DIFFER (sach.evla, evla))
	complain (0, "Different event latitudes %10.3f   %10.3f for record #%ld file %s\n",
		  sach.evla, evla, db.record, filename);

    if (FLOATS_DIFFER (sach.evlo, evlo))
	complain (0, "Different event longitudes %10.3f   %10.3f for record #%ld file %s\n",
		  sach.evlo, evlo, db.record, filename);

    if (FLOATS_DIFFER (sach.evdp, evdp * 1000.0))
	complain (0, "Different event depths %10.3f   %10.3f for record #%ld file %s\n",
		  sach.evdp, evdp * 1000.0, db.record, filename);


    fclose (infile);
    return 0;
}
