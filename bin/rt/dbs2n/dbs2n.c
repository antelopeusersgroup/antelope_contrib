#include <stdio.h> 
#include <math.h> 

#include "db.h"
#include "arrays.h"
#include "scv2.h"

main (argc, argv)

int argc;
char **argv;

{
	char *dbname, *arid_expr, *filter;
	double tsn, twinn, tss, twins;
	Dbptr db, dba;
	int i, nsc, n, arid;
	char sta[32];
	char chan[32];
	char stac[32];
	char chanc[32];
	char iphase[32];
	double time;
	Tbl *sc;
	Trace *ntrace, *strace;
	double t0n, t1n;
	double t0s, t1s;
	double tm, ss, sn, s2n;
	int nn, ns;


	/* Parse command line */

	if (argc < 8) {
		usage();
	        banner (Program_Name, "$Revision$ $Date$");
		exit (1);
	}
	dbname = argv[1];
	arid_expr = argv[2];
	filter = argv[3];
	tsn = atof(argv[4]);
	twinn = atof(argv[5]);
	tss = atof(argv[6]);
	twins = atof(argv[7]);

	/* Open database */

	if (dbopen (dbname, "r+", &db) == dbINVALID) {
		elog_clear_register(1);
		fprintf (stderr, "dbs2n: Unable to open database '%s'\n",
								dbname);
		exit (1);
	}

	/* Get arrivals */

	dba = dblookup (db, 0, "arrival", 0, 0);
	dba = dbsubset (dba, arid_expr, 0);
	dbquery (dba, dbRECORD_COUNT, &n);
	if (n < 1) {
		fprintf (stderr, "dbs2n: No arrivals to process.\n");
		exit (1);
	}

	/* Make sta-chans */

	if (!make_scs (db, 0.0, 9999999999.99, &sc)) {
		printf (stderr, "dbs2n: make_scs() error.\n");
		exit (1);
	}
	nsc = maxtbl(sc);

	/* Arrival processing loop */

	for (dba.record=0; dba.record<n; dba.record++) {

		/* Get arrival info */

		dbgetv (dba, 0, "sta", sta, "chan", chan,
					"arid", &arid,
					"iphase", iphase,
					"time", &time, 0);

		/* Find sta-chan */

		for (i=0; i<nsc; i++) {
			get_sc_stachan (gettbl(sc, i), stac, chanc);
			if (!strcmp(sta, stac) && !strcmp(chan, chanc))
								break;
		}
		if (i == nsc) continue;

		/* Get noise trace */

		t0n = time + tsn;
		t1n = t0n + twinn;
		ntrace = (Trace *) read_sc (gettbl(sc, i), t0n-10.0, 
								t1n+10.0);
		if (ntrace == NULL) continue;
		t0s = time + tss;
		t1s = t0s + twins;
		strace = (Trace *) read_sc (gettbl(sc, i), t0s-10.0, 
								t1s+10.0);
		if (strace == NULL) {
			SCV_free_trace (ntrace);
			continue;
		}
		if (strcmp(filter, "none")) {
			ntrace = (Trace *) filter_trace (ntrace, filter, 0);
			strace = (Trace *) filter_trace (strace, filter, 0);
			if (ntrace == NULL || strace == NULL) {
				fprintf (stderr, 
					"dbs2n: filter_trace() error.\n");
				SCV_free_trace (ntrace);
				SCV_free_trace (strace);
				continue;
			}
		}

		/* Compute s2ns */

		for (i=0,sn=0.0,nn=0; i<ntrace->nsamps; i++) {
			tm = ntrace->tstart + i*ntrace->dt;
			if (tm < t0n) continue;
			if (tm > t1n) break;
			sn += ntrace->data[i]*ntrace->data[i];
			nn++;
		}
		for (i=0,ss=0.0,ns=0; i<strace->nsamps; i++) {
			tm = strace->tstart + i*strace->dt;
			if (tm < t0s) continue;
			if (tm > t1s) break;
			ss += strace->data[i]*strace->data[i];
			ns++;
		}
		SCV_free_trace (ntrace);
		SCV_free_trace (strace);
		if (nn == 0 || ns == 0) continue;
		s2n = sqrt((ss*nn)/(sn*ns));

		/* Write to arrival */

		printf ("Setting %6.6d %s %s %s snr to %.2f\n", arid, sta, chan, iphase, s2n);
		dbputv (dba, 0, "snr", s2n, 0);
	}

	/* Normal exit */

	exit (0);
}

usage()

{
	fprintf (stderr, 
		"usage: dbs2n db arid_expr filter tsn twinn tss twins\n");
}
