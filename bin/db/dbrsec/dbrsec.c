
#include <stdio.h>
#include <math.h>

#include "db.h"
#include "arrays.h"
#include "scv2.h"

#define	ST_POWER	1
#define	ST_VELOCITY	2
#define	ST_BOTH		3

void *qpbin();

Trace *filter_trace (Trace *trace, char *filter, int save_oldtrace);

main (argc, argv)

int argc;
char **argv;

{
	char *dbname, *sta=NULL, *chan=NULL, *filter=NULL;
	char *sift=NULL;
	long orid;
	double top=0.0,bot=0.0,tstart=0.0,twin=0.0;
	double vred=0.0,gain=1.0,amp=0.0;
	double azmin = 0.0, azmax = 360.0;
	int clip=1;
	int stgrid=0;

	/* Parse command line */

	if (!getargs (argc, argv, &dbname, &sta, &chan, &orid, 
			&filter, &sift, &top, &bot, &tstart, &twin, &vred, 
			&azmin, &azmax,
			&gain, &amp, &clip, &stgrid)) exit (1);

	/* Single station-channel mode */

	if (sta) plotss (dbname, sta, chan, filter, sift, top, bot, tstart, twin, azmin, azmax,
								vred, gain, stgrid);

	/* Single event mode */

	else plotse (dbname, orid, chan, filter, sift, top, bot, tstart, twin, vred, gain, amp, clip, stgrid);

	/* Normal exit */

	exit (0);
}

int 
plotss (dbname, sta, chan, filter, sift, top, bot, tstart, twin, azmin, azmax, vred, gain, stgrid)

char *  dbname;
char *          sta;
char *               chan;
char *                     filter;
char *                             sift;
double                                   top;
double                                        bot;
double                                             tstart;
double                                                     twin;
double                                                           azmin, azmax;
double                                                                         vred;
double                                                                               gain;
int                                                                                        stgrid;

{
	Dbptr db, dbo, dbe;
	static char expr[512];
	int n;
	double slat, slon;
	double elat, elon;
	double etime;
	double t0, t1;
	double dst, az;
	double dmin, dmax;
	Tbl *sc;
	Trace *trace, *trace2, *tr;
	void *qp;
	float xdim, xlow, ydim, ylow;
	float yydim, yylow;
	float ygain=gain;
	int iclip=0;
	int jclip=0;
	int jfont;
	int scale_type=1;
	float ybot, ytop;
	double mb;
	long orid;
	float xplt, yplt, ht, ratio, slant;
	float angle, xmin, xmax, ymin, ymax;
	int iref;

	/* Open database */

	if (dbopen (dbname, "r+", &db) == dbINVALID) {
		clear_register (1);
		fprintf (stderr, "dbrsec: Unable to open database '%s'\n",
							dbname);
		return (0);
	}

	/* Look for station */

	dbo = dblookup (db, 0, "site", 0, 0);
	sprintf (expr, "(sta == \"%s\")", sta);
	dbo = dbsubset (dbo, expr, 0);
	dbquery (dbo, dbRECORD_COUNT, &n);
	if (n < 1) {
		clear_register (1);
		fprintf (stderr, "dbrsec: No site entry for sta '%s'.\n", sta);
		return (0);
	}
	dbo.record = 0;
	dbgetv (dbo, 0, "lat", &slat, "lon", &slon, NULL);
	slat *= M_PI/180.0;
	slon *= M_PI/180.0;

	/* Look for origins */

	dbo = dblookup (db, 0, "origin", 0, 0);
	dbe = dblookup (db, 0, "event", 0, 0);
	dbo = dbjoin (dbo, dbe, 0, 0, 0, 0, 0);
	sprintf (expr, "(event.prefor == origin.orid)");
	dbo = dbsubset (dbo, expr, 0);
	if (sift) dbo = dbsubset (dbo, sift, 0);
	dbquery (dbo, dbRECORD_COUNT, &n);
	if (n < 1) {
		clear_register (1);
		fprintf (stderr, "dbrsec: No events to process.\n");
		return (0);
	}

	/* Make sta-chan */

	t0 = 0.0;
	t1 = 999999999999.0;
	if (!make_scs (db, sta, chan, stgrid, t0, t1, &sc)) {
		clear_register (1);
		fprintf (stderr, "dbrsec: make_scs() error\n");
		return (0);
	}


	/* Set up plot */

	plsetup (0, &xdim, &yydim, &xlow, &yylow, tstart, tstart+twin, top, bot, 
				dbname, filter, sta, chan, vred);

	/* Event loop */

	if (bot > top) {
		dmax = bot;
		dmin = top;
	} else {
		dmax = top;
		dmin = bot;
	}
	for (dbo.record=0; dbo.record<n; dbo.record++) {

		/* Compute time window */

		dbgetv (dbo, 0, "time", &etime,
				"lat", &elat,
				"lon", &elon,
				"orid", &orid,
				"mb", &mb,
				NULL);
		elat *= M_PI/180.0;
		elon *= M_PI/180.0;
		dist (slat, slon, elat, elon, &dst, &az);
		dst *= 180.0/M_PI;
		az *= 180.0/M_PI;
		while (az < 0.0) az += 360.0;
		while (az > 360.0) az -= 360.0;
		if (dst > dmax) continue;
		if (dst < dmin) continue;
		if (az < azmin) continue;
		if (az > azmax) continue;
		if (vred > 0.0) {
			t0 = etime + tstart + dst*111.2/vred;
		} else {
			t0 = etime + tstart;
		}
		t1 = t0 + twin;

		/* Get trace */

		read_sc (gettbl(sc, 0), t0, t1, &trace, &trace2);
		if (trace == NULL) {
			continue;
		}

		/* Filter trace */

		if (filter) trace = filter_trace (trace, filter, 0);
		if (trace == NULL) {
			fprintf (stderr, "dbrsec: filter_trace() error\n");
			continue;
		}

		/* Plot trace */

		angle = 0.0;
		iref = 1;
		jclip = 1;
		xmin = 0.0;
		xmax = xdim;
		ymin = -1.0;
		ymax = 1.0;
		ydim = gain;
		ylow = yylow + yydim*(dst-bot)/(top-bot) - 0.5*ydim;
		setdim_ (&xdim, &ydim, &xlow, &ylow);
		setscl_ (&xmin, &xmax, &ymin, &ymax);
	        jfont = 115;
	        ht = 0.04;
	        ratio = 1.0;
	        slant = 0.0;
                cfont_ (&jfont);
                chrsiz_ (&ht, &ratio, &slant);
                sprintf (expr, "%ld", orid);
		xplt = xdim + 0.1;
		yplt = 0.0;
		text_ (&xplt, &yplt, &angle, &iref, expr, &jclip, strlen(expr));
		if (stgrid == ST_BOTH || stgrid == ST_VELOCITY) {
			draw_both (xdim, ydim, xlow, ylow, t0, twin, trace, trace2, stgrid);
			SCV_free_trace (trace);
			SCV_free_trace (trace2);
		} else {
			for (tr=trace,qp=0; tr!=NULL; tr=tr->next) {
				qp = qpbin (tr->tstart, tr->dt, tr->nsamps, tr->data, t0, twin, 4000, qp);
			}
			switch (stgrid) {
			default:
				break;
			case ST_POWER:
				scale_type = 0;
				ybot = 0.0;
				ytop = 1.0;
				break;
			case ST_VELOCITY:
				scale_type = 0;
				ybot = 2.0;
				ytop = 10.0;
				break;
			}
			ygain = 1.0;
			nqplotsegs (qp, xdim, ydim, xlow, ylow, &ybot, &ytop, scale_type, iclip, ygain);
			qpfree (qp);
			SCV_free_trace (trace);
		}
	}
	finitt_ ();
}

int 
plotse (dbname, orid, chan, filter, sift, top, bot, tstart, twin, vred, gain, amp, clip, stgrid)

char *  dbname;
long            orid;
char *                chan;
char *                      filter;
char *                              sift;
double                                    top;
double                                         bot;
double                                              tstart;
double                                                      twin;
double                                                            vred;
double                                                                  gain;
double                                                                        amp;
int                                                                                clip;
int                                                                                      stgrid;

{
	Dbptr db, dbo, dbs;
	static char expr[512];
	static char sta[32];
	static char chann[32];
	int n;
	double slat, slon;
	double elat, elon;
	double etime;
	double t0, t1;
	double dst, az;
	double dmin, dmax;
	Tbl *sc;
	Trace *trace, *trace2, *tr;
	void *qp;
	float xdim, xlow, ydim, ylow;
	float xmin, xmax, ymin, ymax;
	float yydim, yylow;
	float ygain=gain;
	float xplt, yplt, angle;
	int iref, jclip, jfont;
	int iclip=0;
	int scale_type=1;
	float ybot, ytop;
	double mb;

	/* Open database */

	if (dbopen (dbname, "r+", &db) == dbINVALID) {
		clear_register (1);
		fprintf (stderr, "dbrsec: Unable to open database '%s'\n",
							dbname);
		return (0);
	}
	if (clip) iclip = 0; else iclip = 1;

	/* Look for origin */

	dbo = dblookup (db, 0, "origin", 0, 0);
	sprintf (expr, "(orid == %ld)", orid);
	dbo = dbsubset (dbo, expr, 0);
	dbquery (dbo, dbRECORD_COUNT, &n);
	if (n < 1) {
		clear_register (1);
		fprintf (stderr, "dbrsec: No origin to process.\n");
		return (0);
	}
	dbo.record = 0;
	dbgetv (dbo, 0, "time", &etime,
				"lat", &elat,
				"lon", &elon,
				"mb", &mb,
				NULL);
	elat *= M_PI/180.0;
	elon *= M_PI/180.0;

	/* Look for stations */

	dbs = dblookup (db, 0, "site", 0, 0);
	dbquery (dbs, dbRECORD_COUNT, &n);
	if (n < 1) {
		clear_register (1);
		fprintf (stderr, "dbrsec: No sites to process.\n");
		return (0);
	}

	/* Set up plot */

	sprintf (expr, "%ld", orid);
	plsetup (1, &xdim, &yydim, &xlow, &yylow, tstart, tstart+twin, top, bot, 
				dbname, filter, expr, chan, vred);

	/* Station loop */

	if (bot > top) {
		dmax = bot;
		dmin = top;
	} else {
		dmax = top;
		dmin = bot;
	}
	for (dbs.record=0; dbs.record<n; dbs.record++) {

		/* Compute time window */

		dbgetv (dbs, 0, 
				"lat", &slat,
				"lon", &slon,
				"sta", sta,
				NULL);
		slat *= M_PI/180.0;
		slon *= M_PI/180.0;
		dist (slat, slon, elat, elon, &dst, &az);
		dst *= 180.0/M_PI;
		if (dst > dmax) continue;
		if (dst < dmin) continue;
		if (vred > 0.0) {
			t0 = etime + tstart + dst*111.2/vred;
		} else {
			t0 = etime + tstart;
		}
		t1 = t0 + twin;

		/* Make sta-chan */

		if (!make_scs2 (db, sta, chan, t0, t1, &sc)) {
			clear_register (1);
			continue;
		}
		get_sc_stachan (gettbl(sc, 0), sta, chann);
		sprintf (expr, "%s %s", sta, chann);

		/* Get trace */

		read_sc (gettbl(sc, 0), t0, t1, &trace, &trace2);
		if (trace == NULL) {
			continue;
		}

		/* Filter trace */

		if (filter) trace = filter_trace (trace, filter, 0);
		if (trace == NULL) {
			fprintf (stderr, "dbrsec: filter_trace() error\n");
			continue;
		}

		/* Plot trace */

		xplt = 0.2;
		yplt = 0.0;
		angle = 0.0;
		iref = 1;
		jclip = 1;
		xmin = 0.0;
		xmax = xdim;
		ymin = -1.0;
		ymax = 1.0;
		ydim = 1.0;
		ylow = yylow + yydim*(dst-bot)/(top-bot) - 0.5*ydim;
		setdim_ (&xdim, &ydim, &xlow, &ylow);
		setscl_ (&xmin, &xmax, &ymin, &ymax);
	        jfont = 115;
                cfont_ (&jfont);
		text_ (&xplt, &yplt, &angle, &iref, expr, &jclip, strlen(expr));
		for (tr=trace,qp=0; tr!=NULL; tr=tr->next) {
			qp = qpbin (tr->tstart, tr->dt, tr->nsamps, tr->data, t0, twin, 4000, qp);
		}
		if (amp == 0.0) {
			scale_type = 1;
		} else {
			scale_type = 0;
			ybot = -amp;
			ytop = amp;
		}
		nqplotsegs (qp, xdim, ydim, xlow, ylow, &ybot, &ytop, scale_type, iclip, ygain);
		qpfree (qp);

		/* Free trace */

		SCV_free_trace (trace);
	}
	finitt_ ();
}

int
plsetup (iitran, xdim, ydim, xlow, ylow, tmin, tmax, dtop, dbot, dbname, filter, sta, chan, vred)

int      iitran;
float *  xdim;
float *        ydim;
float *              xlow;
float *                    ylow;
double                           tmin, tmax, dtop, dbot;
char *                                                   dbname;
char *                                                           filter;
char *                                                                  sta;
char *                                                                       chan;
double                                                                            vred;

{
	int itran=0;
	float xxdim=5.5;
	float yydim=8.0;
	float xxlow=1.3;
	float yylow=1.0;
	float xplt, yplt, angle=0.0, height, ratio=1.0, slant=0.0;
	int iref, jfont, iclip=0;
	char string[256];
	float fzero=0.0;
	float xmarg=0.5;
	float ymarg=0.8;
	float xmin=tmin;
	float xmax=tmax;
	float ymin=dbot;
	float ymax=dtop;
	float dxsmal=0.001;
	float dxbig=0.01;
	float dysmal=0.001;
	float dybig=0.01;
	static char fmtx[] = "(*)";
	static char fmty[] = "(*)";
	static char xlabel[126];
	static char ylabel[] = "Distance (deg)";
	static char title[] = " ";
	int iclear=0;
	float scl=0.8;

	itran = iitran;
	if (itran == 0) {
		xxdim=5.5;
		yydim=8.0;
		xxlow=1.3;
		yylow=1.0;
	} else {
		yydim=5.5;
		xxdim=8.0;
		xxlow=1.3;
		yylow=1.0;
	}
	antelope_init_plot (itran, NULL, "dbrsec", dbname, filter);
        sprintf (string, "%s %s", sta, chan);
        xplt = 0.0;
        yplt = 9.8 - 0.35;
        iref = 0;
        jfont = 114;
        cfont_ (&jfont);
        height = 0.15;
        chrsiz_ (&height, &ratio, &slant);
        text_ (&xplt, &yplt, &angle, &iref, string, &iclip, strlen(string));
        if (vred > 0.0) {
        	sprintf (xlabel, "Reduced Time, t - x/%.2f (sec)", vred);
        } else {
        	sprintf (xlabel, "Time (sec)");
        }
        sclsiz_ (&scl);
        axis_ (&xxdim, &yydim, &xmarg, &ymarg, &xxlow, &yylow, &xmax,
        	&xmin, &ymax, &ymin, &dxsmal, &dxbig, &dysmal, &dybig,
        	fmtx, fmty, xlabel, ylabel, title, &iclear,
        	strlen(fmtx), strlen(fmty), strlen(xlabel),
        	strlen(ylabel), strlen(title));

	*xdim = xxdim;
	*ydim = yydim;
	*xlow = xxlow;
	*ylow = yylow;
}

draw_both (xdim, ydim, xlow, ylow, t0, twin, trace, trace2, stgrid)

float      xdim, ydim, xlow, ylow;
double                             t0, twin;
Trace *                                      trace;
Trace *                                             trace2;
int                                                         stgrid;

{
	float xdm, ydm, xlw, ylw;
	float xmin, xmax, ymin, ymax;
	Trace *tr, *tr2;
	int i;
	double time;
	float x1, x2, y1, y2;
	float hue, lit, sat;
	float thick;
	int ithick, iclip;

	xdm = xdim;
	ydm = ydim;
	xlw = xlow;
	ylw = ylow;
	setdim_ (&xdm, &ydm, &xlw, &ylw);
	xmin = 0.0;
	xmax = twin;
	ymin = 0.0;
	ymax = 1.0;
	setscl_ (&xmin, &xmax, &ymin, &ymax);
	switch (stgrid) {
	case ST_BOTH:
		for (tr=trace,tr2=trace2; tr!=NULL; tr=tr->next,tr2=tr2->next) {
			for (i=0; i<tr->nsamps; i++) {
				time = tr->tstart + i*tr->dt;
				if (time < t0) continue;
				if (time > t0+twin) break;
				x1 = time - t0 - 0.5*tr->dt;
				x2 = x1 + tr->dt;
				y1 = 0.0;
				y2 = tr->data[i];
				if (tr2->data[i] > 10.0) {
					hue = 240.0;
				} else if (tr2->data[i] < 5.0) {
					hue = 0.0;
				} else {
					hue = (tr2->data[i]-5.0)*240.0/5.0;
					}
				if (hue < 120.0) {
					hue *= 160.0/120.0;
				} else {
					hue = 160.0 + (hue-120.0)*80.0/120.0;
				}
				lit = 0.5;
				sat = 1.0;
				if (hue <= 60.0) {
					lit = 0.95 - 0.10*hue/60.0;
				} else if (hue <= 120.0) {
					lit = 0.85;
				} else {
					lit = 0.85 + 0.05*(hue-120.0)/120.0;
				}
				setbac_ (&hue, &lit, &sat);
				nfillon_ ();
				thick = -1.0;
				ithick = 0;
				iclip = 0;
				box_ (&x1, &x2, &y1, &y2, &thick, &ithick, &iclip);
				nfilloff_();
			}
		}
		break;
	case ST_VELOCITY:
		for (tr=trace; tr!=NULL; tr=tr->next) {
			for (i=0; i<tr->nsamps; i++) {
				time = tr->tstart + i*tr->dt;
				if (time < t0) continue;
				if (time > t0+twin) break;
				x1 = time - t0 - 0.5*tr->dt;
				x2 = x1 + tr->dt;
				y1 = 0.0;
				y2 = 1.0;
				if (tr->data[i] > 10.0) {
					hue = 240.0;
				} else if (tr->data[i] < 5.0) {
					hue = 0.0;
				} else {
					hue = (tr->data[i]-5.0)*240.0/5.0;
					}
				if (hue < 120.0) {
					hue *= 160.0/120.0;
				} else {
					hue = 160.0 + (hue-120.0)*80.0/120.0;
				}
				lit = 0.5;
				sat = 1.0;
				if (hue <= 60.0) {
					lit = 0.95 - 0.10*hue/60.0;
				} else if (hue <= 120.0) {
					lit = 0.85;
				} else {
					lit = 0.85 + 0.05*(hue-120.0)/120.0;
				}
				setbac_ (&hue, &lit, &sat);
				nfillon_ ();
				thick = -1.0;
				ithick = 0;
				iclip = 0;
				box_ (&x1, &x2, &y1, &y2, &thick, &ithick, &iclip);
				nfilloff_();
			}
		}
		break;
	}
}

int
getargs (argc, argv, dbname, sta, chan, orid, filter, sift, top, bot, 
         tstart, twin, vred, azmin, azmax, gain, amp, clip, stgrid)

int      argc;
char **        argv;
char **              dbname;
char **                      sta;
char **                           chan;
long *                                  orid;
char **                                       filter;
char **                                               sift;
double *                                                    top;
double *                                                         bot;
double * tstart;
double *         twin;
double *               vred;
double *                     azmin;
double *                            azmax;
double *                                   gain;
double *                                         amp;
int *                                                 clip;
int *                                                       stgrid;

{
	int i;

	if (argc < 4) {
		usage();
		return (0);
	}
	*dbname = argv[1];
	if (!strcmp(argv[2], "-sc")) {
		for (i=0; i<strlen(argv[3]); i++) if (argv[3][i] == ':') break;
		if (i < 1 || i >= strlen(argv[3])-1) {
			fprintf (stderr, "dbrsec: Unable to parse -sc arg '%s'.\n", argv[3]);
			usage();
			return (0);
		}
		argv[3][i] = '\0';
		*sta = argv[3];
		*chan = &argv[3][i+1];
	} else if (!strcmp(argv[2], "-orid")) {
		*sta = NULL;
		for (i=0; i<strlen(argv[3]); i++) if (argv[3][i] == ':') break;
		if (i < 1 || i >= strlen(argv[3])-1) {
			fprintf (stderr, "dbrsec: Unable to parse -sc arg '%s'.\n", argv[3]);
			usage();
			return (0);
		}
		argv[3][i] = '\0';
		*orid = atoi(argv[3]);
		*chan = &argv[3][i+1];
	} else {
		fprintf (stderr, "dbrsec: Unrecognized command line arg '%s'\n", argv[2]);
		usage();
		return (0);
	}
	for (argv+=4,argc-=4; argc>0; argv++,argc--) {
		if (!strcmp(*argv, "-top")) {
			argv++;
			argc--;
			if (argc < 1) {
				fprintf (stderr, "dbrsec: No argument for -top option.\n");
				usage();
				return (0);
			}
			*top = atof(*argv);
		} else if (!strcmp(*argv, "-bot")) {
			argv++;
			argc--;
			if (argc < 1) {
				fprintf (stderr, "dbrsec: No argument for -bot option.\n");
				usage();
				return (0);
			}
			*bot = atof(*argv);
		} else if (!strcmp(*argv, "-tstart")) {
			argv++;
			argc--;
			if (argc < 1) {
				fprintf (stderr, "dbrsec: No argument for -tstart option.\n");
				usage();
				return (0);
			}
			*tstart = atof(*argv);
		} else if (!strcmp(*argv, "-twin")) {
			argv++;
			argc--;
			if (argc < 1) {
				fprintf (stderr, "dbrsec: No argument for -twin option.\n");
				usage();
				return (0);
			}
			*twin = atof(*argv);
		} else if (!strcmp(*argv, "-vred")) {
			argv++;
			argc--;
			if (argc < 1) {
				fprintf (stderr, "dbrsec: No argument for -vred option.\n");
				usage();
				return (0);
			}
			*vred = atof(*argv);
		} else if (!strcmp(*argv, "-gain")) {
			argv++;
			argc--;
			if (argc < 1) {
				fprintf (stderr, "dbrsec: No argument for -gain option.\n");
				usage();
				return (0);
			}
			*gain = atof(*argv);
		} else if (!strcmp(*argv, "-amp")) {
			argv++;
			argc--;
			if (argc < 1) {
				fprintf (stderr, "dbrsec: No argument for -amp option.\n");
				usage();
				return (0);
			}
			*amp = atof(*argv);
		} else if (!strcmp(*argv, "-azmin")) {
			argv++;
			argc--;
			if (argc < 1) {
				fprintf (stderr, "dbrsec: No argument for -azmin option.\n");
				usage();
				return (0);
			}
			*azmin = atof(*argv);
		} else if (!strcmp(*argv, "-azmax")) {
			argv++;
			argc--;
			if (argc < 1) {
				fprintf (stderr, "dbrsec: No argument for -azmax option.\n");
				usage();
				return (0);
			}
			*azmax = atof(*argv);
		} else if (!strcmp(*argv, "-clip")) {
			argv++;
			argc--;
			if (argc < 1) {
				fprintf (stderr, "dbrsec: No argument for -clip option.\n");
				usage();
				return (0);
			}
			if (!strcmp(*argv, "on")) {
				*clip = 1;
			} else if (!strcmp(*argv, "off")) {
				*clip = 0;
			} else {
				fprintf (stderr, "dbrsec: Unrecognized -clip argument '%s'.\n",
										*argv);
				usage();
				return (0);
			}
		} else if (!strcmp(*argv, "-stgrid")) {
			argv++;
			argc--;
			if (argc < 1) {
				fprintf (stderr, "dbrsec: No argument for -stgrid option.\n");
				usage();
				return (0);
			}
			if (!strcmp(*argv, "power")) {
				*stgrid = ST_POWER;
			} else if (!strcmp(*argv, "velocity")) {
				*stgrid = ST_VELOCITY;
			} else if (!strcmp(*argv, "both")) {
				*stgrid = ST_BOTH;
			} else {
				fprintf (stderr, "dbrsec: Unrecognized -stgrid argument '%s'.\n",
										*argv);
				usage();
				return (0);
			}
		} else if (!strcmp(*argv, "-filter")) {
			argv++;
			argc--;
			if (argc < 1) {
				fprintf (stderr, "dbrsec: No argument for -filter option.\n");
				usage();
				return (0);
			}
			*filter = *argv;
		} else if (!strcmp(*argv, "-sift")) {
			argv++;
			argc--;
			if (argc < 1) {
				fprintf (stderr, "dbrsec: No argument for -sift option.\n");
				usage();
				return (0);
			}
			*sift = *argv;
		} else {
			fprintf (stderr, "dbrsec: Unrecognized command line arg '%s'\n", *argv);
			usage();
			return (0);
		}
	}
	return (1);
}

int
usage ()

{
	fprintf (stderr, "usage: dbrsec db {-sc sta:chan | -orid orid:chan} [-top top]\n");
	fprintf (stderr, "                 [-bot bot] [-tstart tstart] [-twin twin]\n");
	fprintf (stderr, "                 [-filter filter] [-sift sift] [-clip {on|off}]\n");
	fprintf (stderr, "                 [-vred vred] [-gain gain] [-amp amp]\n");
	/* fprintf (stderr, "                 [-stgrid {power|velocity|both}]\n");
	fprintf (stderr, "                 [-azmin azmin] [-azmax azmax]\n"); */
}

/* $Id$ */
