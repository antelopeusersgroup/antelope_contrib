#include <stdio.h>
#include <string.h>
#include <rpc/rpc.h>
#include <unistd.h>

#include "sac.h"
#include "scv2.h"
#include "elog.h"
#include "dbl2.h"
#include "ahsac.h"

extern Trace   *SCV_get_rawtrace (SCV * scv, double tstart, double tend);

void
write_ah (scv, dbin, dbout, tstrt, tend, fixgaps, counts, wfdir)
SCV            *scv;
Dbptr           dbin,
                dbout;
double          tstrt,
                tend;
char           *fixgaps;
int             counts;
char           *wfdir;

{
    char            newDir[65];	       /* Relative directory for ah  files */
    ahhed           header;	       /* ah header                        */
    int             nsegs;	       /* Number of data segments          */
    int             seg_index;	       /* Segment counter                  */
    int             num_samps;	       /* Number of samples in the segment */
    float          *seg_data;	       /* Measured segment data            */
    char           *station;	       /* Station name                     */
    char           *channel;	       /* Channel name                     */
    double          tstart;	       /* Segment start-time               */
    struct date_time human_time;       /* Epoch time for humans            */
    FILE           *outfile;	       /* output file pointer              */
    char            output_file[33];   /* output file name                 */
    char            full_name[97];     /* complete ah file name            */
    int             index;	       /* Generic index                    */
    char            wf_record[1024];
    char            data_type[10];
    char           *table_dirname;
    XDR             xdr_out;
    Trace          *trace,
                   *original;
    double          calib;

    /* Dummy variables for retrieving data  */
    double          dummy_double;

    /* Set the database output directory. */
    dbout = dblookup (dbout, 0, "wfdisc", 0, 0);
    dbquery (dbout, dbTABLE_DIRNAME, &table_dirname);
    concat_paths (table_dirname, wfdir, newDir);
    makedir (newDir);

    dbin = dblookup (dbin, 0, "wfdisc", 0, 0);

    /* Write out each segment separately  */
    SCV_get ((char *) scv, SCV_NSEGS, &nsegs, NULL);

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
	/* Read in the next segment  */
	dbin.record = scv->segments[seg_index].ituple;

	if (trace->nsamps < 1) {
	    continue;
	}
	fprintf (stderr, "Translating...\n");

	/* Fill out the header station info  */
	SCV_get ((char *) scv, SCV_STA, &station, NULL);
	(void) strncpy (header.station.code, station, CODESIZE);

	SCV_get ((char *) scv, SCV_CHAN, &channel, NULL);
	(void) strncpy (header.station.chan, channel, CHANSIZE);
	(void) strncpy (header.station.stype, "null", STYPESIZE);

	SCV_get ((char *) scv, SCV_LAT, &header.station.slat,
		 SCV_LON, &header.station.slon,
		 SCV_ELEV, &header.station.elev, NULL);
	header.station.elev = header.station.elev * 1000;

	/* %%%%  */
	header.station.DS = (float) 1.0;
	header.station.A0 = (float) 1.0;
	for (index = 0; index < NOCALPTS; index++) {
	    header.station.cal[index].pole.r = (float) 0.0;
	    header.station.cal[index].pole.i = (float) 0.0;
	    header.station.cal[index].zero.r = (float) 0.0;
	    header.station.cal[index].zero.i = (float) 0.0;
	}

	/* Fill out the header event info  */
	header.event.lat = (float) 0.0;
	header.event.lon = (float) 0.0;
	header.event.dep = (float) 0.0;
	header.event.ot.yr = (short) 0;
	header.event.ot.mo = (short) 0;
	header.event.ot.day = (short) 0;
	header.event.ot.hr = (short) 0;
	header.event.ot.mn = (short) 0;
	header.event.ot.sec = (float) 0.0;
	(void) strncpy (header.event.ecomment, "null", COMSIZE);

	/* Fill out the header record info  */
	header.record.type = (short) FLOAT;

	num_samps = trace->nsamps;
	header.record.ndata = num_samps;

	dummy_double = trace->dt;
	header.record.delta = (float) dummy_double;

	tstart = trace->tstart;

	seg_data = trace->data;
	if (counts) {
	    calib = trace->calib;
	} else {
	    calib = 1.0;
	}

	/* Find the wfdisc tuple associated with this sac file  */
	dbget (dbin, wf_record);
	dbout.record = dbadd (dbout, wf_record);

	dbgetv (dbin, 0, "segtype", data_type, NULL);

	header.record.maxamp = seg_data[0];
	for (index = 1; index < num_samps; index++) {
	    if (header.record.maxamp < seg_data[index]) {
		header.record.maxamp = seg_data[index];
	    }
	}

	human_time.epoch = tstart;
	etoh (&human_time);
	header.record.abstime.yr = (short) human_time.year;
	header.record.abstime.mo = (short) human_time.month;
	header.record.abstime.day = (short) human_time.day;
	header.record.abstime.hr = (short) human_time.hour;
	header.record.abstime.mn = (short) human_time.minute;
	header.record.abstime.sec = (float) human_time.second;

	header.record.rmin = 0.0;
	(void) strncpy (header.record.rcomment, "null", COMSIZE);
	(void) strncpy (header.record.log, "null", LOGSIZE);

	/* Create the ah file name  */
	(void) sprintf (output_file, "%04d%03d%02d%02d%02d.%02d.%s.%s",
			human_time.year, human_time.doy,
			human_time.hour, human_time.minute,
			(int) human_time.second,
			(int) ((human_time.second - (int) human_time.second)
			       * 100),
			header.station.code, header.station.chan);
	sprintf (full_name, "%s/%s", newDir, output_file);

	/* Flush out the freebies  */
	for (index = 0; index < NEXTRAS; index++) {
	    header.extra[index] = 0;
	}

	/* Open the file  */
	(void) fprintf (stderr, "Writing: %s\n", output_file);
	if ((outfile = fopen (full_name, "w")) == NULL) {
	    (void) fprintf (stderr, "\tWARNING (write_wfah):  ");
	    (void) fprintf (stderr,
			    "Output file %s is not available for writing.\n",
			    full_name);
	    fprintf (stderr, "\tExecution continuing.\n");
	    return;
	}
	xdrstdio_create (&xdr_out, outfile, XDR_ENCODE);


	/* describe the file being written */
	fprintf (stderr, "Writing %s, %s, %5d samples,",
		 station, channel, num_samps);
	fprintf (stderr, " starting %04d %03d %02d:%02d:%02d:%04d UT\n",
		 human_time.year, human_time.doy, human_time.hour,
		 human_time.minute, (int) human_time.second,
	     (int) ((human_time.second - (int) human_time.second) * 10000));

	/* Write the header  */
	if (xdr_puthead (&header, &xdr_out) < 1)
	    elog_complain(1, "Error writing the header file\n");


	/* Update the wfdisc file  */
	dbputv (dbout, 0,
		"dir", wfdir,
		"dfile", output_file,
		"foff", (long) ftell (outfile),
		"calib", calib,
		"datatype", "t4",
		"time", trace->tstart,
		"endtime", trace->tstart + (trace->nsamps - 1) * trace->dt,
		"nsamp", (long) trace->nsamps,
		NULL);

	/* Write the data  */
	if (xdr_putdata (&header, (char *) seg_data, &xdr_out) < 1)
	    elog_complain(1, "Error writing the data\n");

	xdr_destroy (&xdr_out);
	fclose (outfile);
    }
    SCV_free_trace (original);
}

/* $Id$ */
