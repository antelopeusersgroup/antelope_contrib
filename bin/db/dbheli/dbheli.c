/*
 *    I offer this software with no promise of support.
 *    It is a very old program originating from my
 *    days at the University. I have only put this into
 *    contrib at the request of many folks who wanted
 *    to see it in the antelope releases. This source code 
 *    is my ONLY contribution. Please modify it as much as 
 *    you wish, but please don't send me e-mails asking questions -
 *    I have long since recycled any memory associated with
 *    this ancient program.
 *
 *    Danny Harvey - BRTT - August, 2010
 */

#include <stdio.h>
#include <math.h>
#include <time.h>

#include "db.h"
#include "scv2.h"
#include "arrays.h"
#include "coords.h"
#include "tttaup.h"
#include "qplot.h"
#include "sc_subs.h"

#include "filter_subs.h"

void usage(void);
int init_plot (char *fname, char *dbname, char *sta, char *chan, double ts, double twin, int nl, double scal, char *filter, int idisplay);

void finitt_();
void setfor_();
void setdim_();
void line_();
void text_();
void initt_();
void box_();
void cfont_();
void chrsiz_();
void setscl_();

int
main (int argc, char **argv)
{
	char *dbname;
	char *sta;
	char *chan;
	char *tstart;
	char *twinline;
	char *nlines;
	char *scale;
	char *filter;
	char *dborigin;
	char *psfile;
	double ts, t0, twin, te, scal;
	double slat, slon;
	int nl;
	Dbptr db;
	Tbl *sc;
	int i, n;
	Trace *trace, *tr;
	QPlot *qpl;
	float xdim=8.0;
	float ydim=6.0;
	float xlow=1.0;
	float ylow=1.0;
	float yydim, yylow, ybot, ytop;
	float xmin, xmax, ymin, ymax;
	float hue, light, sat;
	static char expr[512];
	int display = 0;

/*
 *	Get command args.
 */
	if (argc < 8) {
		usage();
		exit (1);
	}
	Program_Name = argv[0] ;

	dbname = argv[1];
	sta = argv[2];
	chan = argv[3];
	tstart = argv[4];
	twinline = argv[5];
	nlines = argv[6];
	scale = argv[7];
	filter = NULL;
	dborigin = NULL;
	psfile = NULL;
	argv += 8;
	argc -= 8;
	for (; argc>0; argv++,argc--) {
		if (!strcmp(*argv, "-f")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "dbheli: No -f value.\n");
				usage();
				exit (1);
			}
			filter = *argv;
		} else if (!strcmp(*argv, "-o")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "dbheli: No -o value.\n");
				usage();
				exit (1);
			}
			dborigin = *argv;
		} else if (!strcmp(*argv, "-ps")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf (stderr, "dbheli: No -ps value.\n");
				usage();
				exit (1);
			}
			psfile = *argv;
		} else if (!strcmp(*argv, "-display")) {
			display = 1;
		} else {
			fprintf (stderr, "dbheli: Illegal command line argument '%s'.\n", *argv);
			usage();
			exit (1);
		}
	}

	ts = str2epoch (tstart);
	t0 = str2epoch (tstart);
	twin = atof(twinline);
	nl = atoi(nlines);
	scal = atof(scale);
	te = t0 + twin*nl;
/*
 *	Open database.
 */
	if (dbopen (dbname, "r+", &db) == dbINVALID) {
		elog_clear_register(1);
		fprintf (stderr, "dbheli: Unable to open database.\n");
		exit (1);
	}
/*
 *	Make sc table
 */
 	if (!make_scs (db, sta, chan, ts, te, &sc)) {
		elog_clear_register(1);
		fprintf (stderr, "dbheli: Unable to make sc.\n");
		exit (1);
 	}
/*
 *	Initialize plot
 */
 	init_plot (psfile, dbname, sta, chan, ts, twin, nl, scal, filter, display);
/*
 *	loop through lines
 */
 	for (i=0; i<nl; i++,ts=te) {
 		te = ts + twin;
 		trace = read_sc ((void *)gettbl(sc, 0), ts-20.0, te+20.0);
 		if (trace == NULL) continue;
 		if (filter) {
 			trace = (Trace *) filter_trace (trace, filter, 0);
 			if (trace == NULL) {
 				fprintf (stderr, "dbheli: filter_trace() error.\n");
 				continue;
			}
 		}
 		for (tr=trace,qpl=0; tr!=NULL; tr=tr->next) {
 			qpl = qpbin (tr->tstart, tr->dt, tr->nsamps, tr->data, ts, twin, 4000, qpl);
 		}
 		SCV_free_trace (trace);
 		yydim = ydim/nl;
 		yylow = ylow + ydim - yydim*(i+1);
 		ytop = 0.5*yydim*scal;
 		ybot = -ytop;
 		nqplotsegs (qpl, xdim, yydim, xlow, yylow, &ybot, &ytop, 0, 1, 1.0);
 		qpfree (qpl);
 	}
/*
 *	Event processing.
 */
 	if (!dborigin) {
 		finitt_ ();
 		exit (0);
 	}
	db = dblookup (db, 0, "site", 0, 0);
	dbquery (db, dbRECORD_COUNT, &n);
	for (db.record=0; db.record<n; db.record++) {
		dbgetv (db, 0, "lat", &slat,
				"lon", &slon,
				"sta", expr,
				0);
		if (!strcmp(expr, sta)) break;
	}
	if (db.record == n) {
		fprintf (stderr, "dbheli: Unable to find lat-lon for sta '%s'.\n", sta);
 		finitt_ ();
 		exit (1);
	}
	slat *= M_PI/180.0;
	slon *= M_PI/180.0;
	if (dbopen (dborigin, "r+", &db) == dbINVALID) {
		elog_clear_register(1);
		fprintf (stderr, "dbheli: Unable to open database.\n");
		exit (1);
	}
	db = dblookup (db, 0, "origin", 0, 0);
	te = t0 + twin*nl;
	sprintf (expr, "( time > %.5lf && time < %.5lf )", t0-7200.0, te+7200.0);
	db = dbsubset (db, expr, 0);
	dbquery (db, dbRECORD_COUNT, &n);
	xmin = 0.0;
	xmax = twin;
	ymin = -0.5;
	ymax = nl-0.5;
       	hue = 0.0;
       	light = 0.25;
       	sat = 0.0;
       	setfor_ (&hue, &light, &sat);
	for (db.record=0; db.record<n; db.record++) {
		double elat, elon, edepth, mb;
		double del, az;
		int orid;
		double tm;
		float x1, y1, x2, y2;
		float thick = 0.02;
		int ithick = 0;
		int iclip = 1;
		float angle = 0.0;
		int iref = 7;

		dbgetv (db, 0, "lat", &elat,
				"lon", &elon,
				"depth", &edepth,
				"mb", &mb,
				"orid", &orid,
				"time", &tm,
				0);
		elat *= M_PI/180.0;
		elon *= M_PI/180.0;
		dist_ (&slat, &slon, &elat, &elon, &del, &az);
		del *= 180.0/M_PI;
		az *= 180.0/M_PI;
		tm += ptime(del, edepth);
		if (tm < t0) continue;
		if (tm > te) continue;
		tm -= t0;
		i = tm/twin;
		tm -= i*twin;
		x1 = tm;
		y1 = i-0.5;
		x2 = tm;
		y2 = i+0.5;
		xmin = 0.0;
		xmax = twin;
		ymax = -0.5;
		ymin = nl-0.5;
		setdim_ (&xdim, &ydim, &xlow, &ylow);
		setscl_ (&xmin, &xmax, &ymin, &ymax);
        	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
        	x1 = tm - 0.005*twin;
        	y1 = i-0.5;
        	sprintf (expr, "%.1lf %.1lf %.1lf %d", del, az, mb, orid);
        	text_ (&x1, &y1, &angle, &iref, expr, &iclip, strlen(expr));
	}
/*
 *	Normal exit.
 */
 	finitt_ ();
 	exit (0);
}

int
init_plot (char *fname, char *dbname, char *sta, char *chan, double ts, double twin, int nl, double scal, char *filter, int idisplay)

{
	int itran=1;
	float ssize=0.9;
	float xwin=0.0;
	float ywin=0.0;
	char plotfile[256];
	char display[16];
	char program[1024];
	static int nplot=1;
        float xplt, yplt;
        float angle=0.0;
        int iclip=0;
        int iref=5;
        float height=0.08;
        float ratio=1.0;
        float slant=0.0;
        int jfont=114; 
	float xdim, ydim, xlow, ylow;
	float xmin, xmax, ymin, ymax;
	float thick=0.0;
	int ithick=0;
	float x1, y1, x2, y2;
	float hue, light, sat;
	float fac;
	long itime;
	int i;

 	if (fname) {
 		if (fname[0]) {
 			strcpy (plotfile, fname);
		} else {
			sprintf (plotfile, "%s.%s.%s.%s.%d%d.ps", "dbheli", dbname, sta, chan, getpid(), nplot++);
		}
	} else {
		sprintf (plotfile, "%s.%s.%s.%s.%d%d.ps", "dbheli", dbname, sta, chan, getpid(), nplot++);
	}
	if (idisplay) strcpy (display, " ");
	else strcpy (display, "none");
	strcpy (program, "dbheli");
	initt_ (&itran, plotfile, display, program, &ssize,
		&xwin, &ywin, strlen(plotfile), strlen(display),
		strlen(program));
	xdim = 10.0;
	ydim = 7.5;
	xlow = 0.0;
	ylow = 0.0;
	xmin = 0.0;
	xmax = 1.0;
	ymin = 0.0;
	ymax = 1.0;
	setdim_ (&xdim, &ydim, &xlow, &ylow);
	setscl_ (&xmin, &xmax, &ymin, &ymax);
	iclip = 1;
	box_ (&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
	xdim = 9.8;
	ydim = 7.3;
	xlow = 0.2;
	ylow = 0.2;
	xmin = 0.0;
	xmax = 9.8;
	ymin = 0.0;
	ymax = 7.3;
	setdim_ (&xdim, &ydim, &xlow, &ylow);
	setscl_ (&xmin, &xmax, &ymin, &ymax);
        xplt = 0.0;
        yplt = 0.0;
	jfont = 113;
	height = 0.10;
	iref = 0;
        cfont_ (&jfont);
        chrsiz_ (&height, &ratio, &slant);
        text_ (&xplt, &yplt, &angle, &iref, "BRTT", &iclip, strlen("BRTT"));
	jfont = 115;
        cfont_ (&jfont);
        itime = time(NULL);
	sprintf (program, "%s %s %s %s %s", "dbheli:", dbname, plotfile, cuserid(NULL), ctime(&itime));
	program[strlen(program)-1] = '\0';
	xplt = 0.5;
        text_ (&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
	if (filter == NULL) {
        	sprintf (program, "Filter: None");
	} else {
        	sprintf (program, "Filter: %s", filter);
	}
	xplt = 0.0;
        yplt = 0.18;
        text_ (&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
	sprintf (program, "%s %s", sta, chan);
	xplt = 0.0;
        yplt = ydim - 0.35;
        iref = 0;
        jfont = 114;
        cfont_ (&jfont);
        height = 0.15;
        chrsiz_ (&height, &ratio, &slant);
        text_ (&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
	sprintf (program, "Start time: %s", epoch2str(ts, "%Y\%j %D %H:%M:%S"));
	xplt = 1.5;
        jfont = 115;
        cfont_ (&jfont);
        height = 0.12;
        chrsiz_ (&height, &ratio, &slant);
        text_ (&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
	xdim=8.0;
	ydim=6.0;
	xlow=1.0;
	ylow=1.0;
	xmin = 0.0;
	xmax = twin;
	ymin = 0.0;
	ymax = ydim;
	setdim_ (&xdim, &ydim, &xlow, &ylow);
	setscl_ (&xmin, &xmax, &ymin, &ymax);
        iref = 5;
        jfont = 114;
        cfont_ (&jfont);
        height = 0.10;
        chrsiz_ (&height, &ratio, &slant);
        iclip = 1;
        y1 = ymin;
        y2 = ymax;
        if (twin < 30.0) {
        	ts = 0.0;
        	strcpy (display, "+%.1fs");
        	fac = 1.0;
        	while (1) {
        		if (ts > twin) break;
        		hue = 0.0;
        		light = 0.5;
        		sat = 0.0;
        		setfor_ (&hue, &light, &sat);
        		x1 = ts;
        		x2 = ts;
        		line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
        		hue = 0.0;
        		light = 0.0;
        		sat = 0.0;
        		setfor_ (&hue, &light, &sat);
        		xplt = ts;
        		yplt = -0.15;
			sprintf (program, "%.0fs", ts);
        		text_ (&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
        		ts += 1.0;
        	}
	} else if (twin < 150.0) {
        	ts = 0.0;
        	strcpy (display, "+%.1fs");
        	fac = 1.0;
        	while (1) {
        		if (ts > twin) break;
        		hue = 0.0;
        		light = 0.5;
        		sat = 0.0;
        		setfor_ (&hue, &light, &sat);
        		x1 = ts;
        		x2 = ts;
        		line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
        		hue = 0.0;
        		light = 0.0;
        		sat = 0.0;
        		setfor_ (&hue, &light, &sat);
        		xplt = ts;
        		yplt = -0.15;
			sprintf (program, "%.0fs", ts);
        		text_ (&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
        		ts += 10.0;
        	}
	} else if (twin < 60*30.0) {
        	ts = 0.0;
        	strcpy (display, "+%.1fm");
        	fac = 60.0;
        	while (1) {
        		if (ts > twin) break;
        		hue = 0.0;
        		light = 0.5;
        		sat = 0.0;
        		setfor_ (&hue, &light, &sat);
        		x1 = ts;
        		x2 = ts;
        		line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
        		hue = 0.0;
        		light = 0.0;
        		sat = 0.0;
        		setfor_ (&hue, &light, &sat);
        		xplt = ts;
        		yplt = -0.15;
			sprintf (program, "%.0fm", ts/60.0);
        		text_ (&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
        		ts += 60.0;
        	}
	} else if (twin < 60*150.0) {
        	ts = 0.0;
        	strcpy (display, "+%.1fh");
        	fac = 3600.0;
        	while (1) {
        		if (ts > twin) break;
        		hue = 0.0;
        		light = 0.5;
        		sat = 0.0;
        		setfor_ (&hue, &light, &sat);
        		x1 = ts;
        		x2 = ts;
        		line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
        		hue = 0.0;
        		light = 0.0;
        		sat = 0.0;
        		setfor_ (&hue, &light, &sat);
        		xplt = ts;
        		yplt = -0.15;
			sprintf (program, "%.0fm", ts/60.0);
        		text_ (&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
        		ts += 600.0;
        	}
	} else {
        	ts = 0.0;
        	strcpy (display, "+%.1fh");
        	fac = 3600.0;
        	while (1) {
        		if (ts > twin) break;
        		hue = 0.0;
        		light = 0.5;
        		sat = 0.0;
        		setfor_ (&hue, &light, &sat);
        		x1 = ts;
        		x2 = ts;
        		line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
        		hue = 0.0;
        		light = 0.0;
        		sat = 0.0;
        		setfor_ (&hue, &light, &sat);
        		xplt = ts;
        		yplt = -0.15;
			sprintf (program, "%.0fh", ts/3600.0);
        		text_ (&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
        		ts += 3600.0;
        	}
	}
	ts = 0.0;
	iref = 7;
	for (i=0; i<nl; i++) {
		float yydim, yylow;

 		yydim = ydim/nl;
 		yylow = ylow + ydim - yydim*(i+1);
        	xplt = -0.15;
        	yplt = 0.5;
        	xmin = 0.0;
        	xmax = xdim;
        	ymin = 0.0;
        	ymax = 1.0;
		setdim_ (&xdim, &yydim, &xlow, &yylow);
		setscl_ (&xmin, &xmax, &ymin, &ymax);
		sprintf (program, display, ts/fac);
        	text_ (&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
        	ts += twin;
	}
	xdim=8.0;
	ydim=6.0;
	xlow=1.0;
	ylow=1.0;
	xmin = 0.0;
	xmax = xdim;
	ymin = 0.0;
	ymax = ydim;
	setdim_ (&xdim, &ydim, &xlow, &ylow);
	setscl_ (&xmin, &xmax, &ymin, &ymax);
	x1 = 8.1;
	y1 = 0.0+0.5*ydim/nl;
	x2 = 8.1;
	y2 = 1.0+0.5*ydim/nl;
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	x1 = 8.1;
	y1 = 0.0+0.5*ydim/nl;
	x2 = 8.2;
	y2 = 0.0+0.5*ydim/nl;
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	x1 = 8.1;
	y1 = 1.0+0.5*ydim/nl;
	x2 = 8.2;
	y2 = 1.0+0.5*ydim/nl;
	line_ (&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	iref = 1;
        height = 0.08;
        chrsiz_ (&height, &ratio, &slant);
	xplt = 8.3;
	yplt = 0.0+0.5*ydim/nl;
       	text_ (&xplt, &yplt, &angle, &iref, "0", &iclip, strlen("0"));
	xplt = 8.3;
	yplt = 1.0+0.5*ydim/nl;
	sprintf (program, "%.1f", scal);
        text_ (&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
		
	return (0);
}

void
usage()

{
	fprintf (stderr, "usage: dbheli dbname sta chan tstart twinline nlines scale\n");
	fprintf (stderr, "              [-f filter] [-o dborigin] [-display] [-ps psfile]\n");
	banner (Program_Name, 0) ;
}
