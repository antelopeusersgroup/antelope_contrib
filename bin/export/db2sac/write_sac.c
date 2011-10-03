#include <stdio.h>
#include <string.h>
#include <math.h>

#include "ahhead.h"
#include "sac.h"
#include "scv2.h"
#include "elog.h"
#include "csstime.h"
#include "dbl2.h"
#include "ahsac.h"

#define BINARY (TRUE)
#define CSS_28        (1)
#define CSS_30        (2)
#define	STREQ(a, b) \

extern Trace   *SCV_get_rawtrace (SCV * scv, double tstart, double tend);

static void
swap_sac (int intel, sac_t * sachdr)
{
    int             swap;
    char           *s;

#ifdef WORDS_BIGENDIAN
    if (intel) {
	swap = 1;
    } else {
	swap = 0;
    }
#else
    if (intel) {
	swap = 0;
    } else {
	swap = 1;
    }
#endif

    if (swap) {
	s = (char *) sachdr;
	swap4 (s, s, 110);
    }
}


void
write_sac (scv, dbin, dbout, tstrt, tend, intel, fixgaps, counts, wfdir)
SCV            *scv;
Dbptr           dbin,
                dbout;
double          tstrt,
                tend;
char           *fixgaps;
int             counts;
int             intel;
char           *wfdir;

{
    char            newDir[65];	       /* Relative directory for sac files */
    sac_t           sach;	       /* SAC header structure             */
    FILE           *outfile;	       /* output file pointer              */
    char            output_file[33];   /* output file name                 */
    char            full_name[97];     /* complete sac file name           */
    int             i;		       /* counter                          */
    char            character[8 + 1];  /* for character transfer           */
    int             nsegs;	       /* Number of data segments          */
    int             narrs;	       /* Number of arrivals               */
    int             arr_count;	       /* Arrival counter                  */
    int             seg_index;	       /* SCV Segment index                */
    int             num_samps;	       /* Number of samples in the segment */
    float          *seg_data;	       /* Measured segment data            */
    char           *station;	       /* Station name                     */
    char           *channel;	       /* Channel name                     */
    double          tstart;	       /* Segment start-time               */
    struct date_time human_time;       /* Epoch time for humans            */
    char            data_type[15];
    char            wf_record[1024];
    char           *table_dirname;
    Trace          *trace,
                   *original;
    double          calib;
    float          *fp;

    /* Dummy variables for retrieving data.  */
    double          dummy_double;
    char           *dummy_charp;

    /* Set the database output directory. */
    dbout = dblookup (dbout, 0, "wfdisc", 0, 0);
    dbquery (dbout, dbTABLE_DIRNAME, &table_dirname);
    concat_paths (table_dirname, wfdir, newDir);
    makedir (newDir);

    dbin = dblookup (dbin, 0, "wfdisc", 0, 0);
    /* Build the SAC header  */
    /* SCV station specifiers */
    SCV_get ((char *) scv, SCV_NSEGS, &nsegs,
	     SCV_STA, &station,
	     SCV_CHAN, &channel, NULL);

    /* Write out each segment to a new sac file */
    trace = SCV_get_rawtrace (scv, tstrt, tend);
    if (trace == NULL)
	elog_die(1, "Can't read waveform data\n");

    /* Convert data */
    if (!strcmp (fixgaps, "zero") || !strcmp (fixgaps, "interp")) {
	trace = (Trace *) SCV_trace_fixgaps (trace, fixgaps);
	if (trace == NULL) {
	    elog_die(1, "Can't fix trace gaps\n");
	}
    }
    original = trace = (Trace *) SCV_trace_tofloat (trace, counts);
    if (trace == NULL) {
	elog_die(1, "Can't convert trace to counts\n");
    }
    for (seg_index = 0;
	    trace != 0 && seg_index < nsegs;
	    trace = trace->next, seg_index++) {
	fprintf (stderr, "Translating...\n");

	/* initialize to the default values. */
	sach = sac_def;

	/* Read in the next segment  */
	dbin.record = scv->segments[seg_index].ituple;
	if (trace->nsamps < 1) {
	    continue;
	}
	num_samps = trace->nsamps;

	seg_data = trace->data;
	if (counts) {
	    calib = trace->calib;
	} else {
	    calib = 1.0;
	}
	sach.scale = calib;

	/* npts - number of points in the data segment */
	sach.npts = trace->nsamps;

	/* b - beginning value of independent variable */
	tstart = trace->tstart;
	sach.b = (float) 0.0;

	/* e - ending value of independent variable */
	dummy_double = tstart + (num_samps - 1) * trace->dt;
	sach.e = (float) (dummy_double - tstart);

	/* iftype - type of file  (known a priori) */
	sach.iftype = ITIME;

	/* leven - true if data is evenly spaced  (known a priori) */
	sach.leven = TRUE;

	/* delta - time increment between samples */
	dummy_double = trace->dt;
	sach.delta = dummy_double;

	/* Find the wfdisc tuple associated with this sac file  */
	dbget (dbin, wf_record);
	dbout.record = dbadd (dbout, wf_record);

	dbgetv (dbin, 0, "segtype", data_type, NULL);

	switch (data_type[0]) {
	  case ('A'):
	    sach.idep = IACC;
	    break;
	  case ('V'):
	    sach.idep = IVEL;
	    break;
	  case ('D'):
	  default:
	    sach.idep = IDISP;
	    break;
	}

	/* depmin/depmax/depmen - min max and mean of dependent variable */
	sach.depmin = sach.depmax = seg_data[0];
	sach.depmen = 0;
	for (i = 1; i < num_samps; i++) {
	    sach.depmen += seg_data[i];
	    if (sach.depmin > seg_data[i])
		sach.depmin = seg_data[i];
	    if (sach.depmax < seg_data[i])
		sach.depmax = seg_data[i];
	}
	sach.depmen /= num_samps;

	/* nz* - reference time  (known a priori) */
	human_time.epoch = tstart;
	etoh (&human_time);
	sach.nzyear = human_time.year;
	sach.nzjday = human_time.doy;
	sach.nzhour = human_time.hour;
	sach.nzmin = human_time.minute;
	sach.nzsec = human_time.second;
	sach.nzmsec = ((human_time.second
			- (int) human_time.second) * 1000);

	/* a / ka - first arrival information */
	/* t(n) / kt(n) - upto 10 arrival time picks  */
	SCV_get ((char *) scv, SCV_NARRS, &narrs, NULL);
	arr_count = 0;
	for (i = 0; i < narrs; i++) {
	    SCV_get_arrival ((char *) scv, i, SCV_ARR_TIME, &dummy_double, NULL);
	    if ((dummy_double >= sach.b) && (dummy_double <= sach.e)) {
		SCV_get_arrival ((char *) scv, i, SCV_ARR_PHASE, &dummy_charp, NULL);
		sprintf (character, "%-8.8s", dummy_charp);
		if (arr_count == 0) {
		    sach.a = (float) dummy_double;
		    strncpy (sach.ka, character, 8);
		}
		if ((arr_count >= 0) && (arr_count < 10)) {
		    sach.t[arr_count] = (float) dummy_double;
		    strncpy (sach.kt[arr_count], character, 8);
		}
		arr_count++;
	    }
	}

	/* iztype - reference time for picks */
	sach.iztype = IDAY;

	/* kstnm - station name */
	sprintf (character, "%-8.8s", station);
	strncpy (sach.kstnm, character, 8);

	/* stla / stlo / stel - station layout */
	SCV_get ((char *) scv,
		 SCV_LAT, &sach.stla,
		 SCV_LON, &sach.stlo,
		 SCV_ELEV, &sach.stel,
		 SCV_DEAST, &sach.user[7],
		 SCV_DNORTH, &sach.user[8], NULL);
	sach.stel = sach.stel * 1000;

	/* stdp / cmpaz/ cmpinc - station layout */
	SCV_get_segment (scv, seg_index,
			 SCV_SEG_DEPTH, &sach.stdp,
			 SCV_SEG_HANG, &sach.cmpaz,
			 SCV_SEG_VANG, &sach.cmpinc, NULL);
	sach.stdp = sach.stdp * 1000;

	/* kcnpnm - component name */
	sprintf (character, "%-8.8s", channel);
	strncpy (sach.kcmpnm, character, 8);

	/* lpspol - true if station components have positive polarity */
	sach.lpspol = FALSE;

	/* lcalca - true if event info is to be calculated from position */
	sach.lcalda = TRUE;

	/* isynth - synthetic data flag */
	if (data_type[1] == 'r') {
	    sach.isynth = IRLDTA;
	}
	/* lovrok - true if the file can be overwritten */
	sach.lovrok = FALSE;

	/* build name for and open output file  */
	/* 1988023153408.28.ANMO.SPZ is the filename for a seismogram from
	 * year 1988, Julian day 23, 15:34:08.2800 UT, recorded at station
	 * ANMO from component SPZ. */
	sprintf (output_file, "%04d%03d%02d%02d%02d.%02d.%s.%s",
		 human_time.year, human_time.doy,
		 human_time.hour, human_time.minute,
		 (int) human_time.second,
		 (int) ((human_time.second - (int) human_time.second) * 100),
		 station, channel);
	sprintf (full_name, "%s/%s", newDir, output_file);

	if ((outfile = fopen (full_name, "w")) == NULL) {
	    fprintf (stderr, "\tWARNING (output_data):  ");
	    fprintf (stderr, "Output file %s is not available for writing.\n",
		     full_name);
	    fprintf (stderr, "\tExecution continuing.\n");
	    return;
	}
	/* Update the wfdisc file  */
	dbputv (dbout, 0,
		"dir", wfdir,
		"dfile", output_file,
		"calib", calib,
		"datatype", intel ? "ic" : "sc",
		"time", trace->tstart,
		"endtime", trace->tstart + (trace->nsamps - 1) * trace->dt,
		"nsamp", (long) trace->nsamps,
		NULL);

#if (!BINARY)
	/* describe the file being written */
	fprintf (stderr, "Writing %s, %s, %5d samples (ASCII),",
		 station, channel, num_samps);
	fprintf (stderr, " starting %04d %03d %02d:%02d:%02d:%04d UT\n",
		 human_time.year, human_time.doy, human_time.hour,
		 human_time.minute, (int) human_time.second,
	     (int) ((human_time.second - (int) human_time.second) * 10000));

	/* write the SAC header */
	fprintf (outfile, FCS, sach.delta,
		 sach.depmin,
		 sach.depmax,
		 sach.scale,
		 sach.odelta);
	fprintf (outfile, FCS, sach.b,
		 sach.e,
		 sach.o,
		 sach.a,
		 sach.internal1);
	fprintf (outfile, FCS, sach.t[0],
		 sach.t[1],
		 sach.t[2],
		 sach.t[3],
		 sach.t[4]);
	fprintf (outfile, FCS, sach.t[5],
		 sach.t[6],
		 sach.t[7],
		 sach.t[8],
		 sach.t[9]);
	fprintf (outfile, FCS, sach.f,
		 sach.resp[0],
		 sach.resp[1],
		 sach.resp[2],
		 sach.resp[3]);
	fprintf (outfile, FCS, sach.resp[4],
		 sach.resp[5],
		 sach.resp[6],
		 sach.resp[7],
		 sach.resp[8]);
	fprintf (outfile, FCS, sach.resp[9],
		 sach.stla,
		 sach.stlo,
		 sach.stel,
		 sach.stdp);
	fprintf (outfile, FCS, sach.evla,
		 sach.evlo,
		 sach.evel,
		 sach.evdp,
		 sach.unused1);
	fprintf (outfile, FCS, sach.user[0],
		 sach.user[1],
		 sach.user[2],
		 sach.user[3],
		 sach.user[4]);
	fprintf (outfile, FCS, sach.user[5],
		 sach.user[6],
		 sach.user[7],
		 sach.user[8],
		 sach.user[9]);
	fprintf (outfile, FCS, sach.dist,
		 sach.az,
		 sach.baz,
		 sach.gcarc,
		 sach.internal2);
	fprintf (outfile, FCS, sach.internal3,
		 sach.depmen,
		 sach.cmpaz,
		 sach.cmpinc,
		 sach.unused2);
	fprintf (outfile, FCS, sach.unused3,
		 sach.unused4,
		 sach.unused5,
		 sach.unused6,
		 sach.unused7);
	fprintf (outfile, FCS, sach.unused8,
		 sach.unused9,
		 sach.unused10,
		 sach.unused11,
		 sach.unused12);

	fprintf (outfile, ICS, sach.nzyear,
		 sach.nzjday,
		 sach.nzhour,
		 sach.nzmin,
		 sach.nzsec);
	fprintf (outfile, ICS, sach.nzmsec,
		 sach.internal4,
		 sach.internal5,
		 sach.internal6,
		 sach.npts);
	fprintf (outfile, ICS, sach.internal7,
		 sach.internal8,
		 sach.unused13,
		 sach.unused14,
		 sach.unused15);
	fprintf (outfile, ICS, sach.iftype,
		 sach.idep,
		 sach.iztype,
		 sach.unused16,
		 sach.iinst);
	fprintf (outfile, ICS, sach.istreg,
		 sach.ievreg,
		 sach.ievtyp,
		 sach.iqual,
		 sach.isynth);
	fprintf (outfile, ICS, sach.unused17,
		 sach.unused18,
		 sach.unused19,
		 sach.unused20,
		 sach.unused21);
	fprintf (outfile, ICS, sach.unused22,
		 sach.unused23,
		 sach.unused24,
		 sach.unused25,
		 sach.unused26);
	fprintf (outfile, ICS, sach.leven,
		 sach.lpspol,
		 sach.lovrok,
		 sach.lcalda,
		 sach.unused27);

	fprintf (outfile, CCS2, sach.kstnm, sach.kevnm);

	fprintf (outfile, CCS1, sach.khole, sach.ko, sach.ka);
	fprintf (outfile, CCS1, sach.kt[0], sach.kt[1], sach.kt[2]);
	fprintf (outfile, CCS1, sach.kt[3], sach.kt[4], sach.kt[5]);
	fprintf (outfile, CCS1, sach.kt[6], sach.kt[7], sach.kt[8]);
	fprintf (outfile, CCS1, sach.kt[9], sach.kf, sach.kuser[0]);
	fprintf (outfile, CCS1, sach.kuser[1], sach.kuser[2], sach.kcmpnm);
	fprintf (outfile, CCS1, sach.knetwk, sach.kdatrd, sach.kinst);

	/* write the SAC seismic data */
	for (i = 0; i < num_samps; i++) {
	    fprintf (outfile, "%15.7f", seg_data[i]);
	    if ((((i + 1) % 5) == 0) && (i > 0))
		fprintf (outfile, "\n");
	}

	/* write a SAC binary file  */

#else
	/* describe the file being written */
	fprintf (stderr, "Writing %s, %s, %5d samples (BINARY),",
		 station, channel, num_samps);
	fprintf (stderr, " starting %04d %03d %02d:%02d:%02d:%04d UT\n",
		 human_time.year, human_time.doy, human_time.hour,
		 human_time.minute, (int) human_time.second,
	     (int) ((human_time.second - (int) human_time.second) * 10000));

	/* write the SAC header */
	swap_sac (intel, &sach);
	if (fwrite ((char *) &sach, sizeof (struct sac), 1, outfile) != 1) {
	    fprintf (stderr, "WARNING (output_data):  ");
	    fprintf (stderr, "failed to properly write SAC header to %s.\n",
		     output_file);
	    fprintf (stderr, "\tExecution continuing.\n");
	}
	/* write the SAC data */
	fp = seg_data;
	if (intel) {
	    hf2vf (seg_data, &fp, num_samps);
	} else {
	    hf2mf (seg_data, &fp, num_samps);
	}
	if (fwrite ((char *) seg_data, sizeof (float), num_samps, outfile)
		!= num_samps) {
	    fprintf (stderr, "WARNING (output_data):  ");
	    fprintf (stderr, "failed to properly write SAC data to %s.\n",
		     output_file);
	    fprintf (stderr, "\tExecution continuing.\n");
	}
#endif

	fclose (outfile);
    }
    SCV_free_trace (original);
}

/* $Id$ */
