#include <stdio.h>
#include <math.h>
#include <time.h>

#include "db.h"
#include "Pkt.h"
#include "scv2.h"
#include "arrays.h"
#include "coords.h"
#include "tttaup.h"
#include "polygon.h"

int             usage();
int             init_plot();

typedef struct qpl_ {
	int             npixels;
	int             np;
	int             istart;
	float          *min;
	float          *max;
	float          *in;
	float          *out;
	int            *numb;
	int             overlap;
	int             noverlaps;
	struct qpl_    *overlaps;
}               QPlot;




typedef struct {
	int             n;
	char            sta[6];
	char            chan[8];
	Tbl            *arrivals;
	Tbl            *sc;
}               STACHANSTRUCT;

typedef struct {
	char            sta[6];
	double          time;
	int             arid;
	long            jdate;
	int             stassid;
	int             chanid;
	char            chan[6];
	char            iphase[9];
	char            stype[2];
	double          deltim;
	double          azimuth;
	double          delaz;
	double          slow;
	double          delslo;
	double          ema;
	double          rect;
	double          amp;
	double          per;
	double          logat;
	char            clip[2];
	char            fm[2];
	double          snr;
	char            qual[2];
	char            auth[16];
	int             commid;
	double          lddate;
}               ARRIVAL;

void 
free_arrivals(ARRIVAL * A)
{
	free(A);
}
int 
print_arrival(ARRIVAL * arrival, void *private)
{

	printf("%s:%s %s @ %s\n", arrival->sta, arrival->chan,
	       arrival->iphase, strtime(arrival->time));
	return (0);

}
int 
cmp_arrival(ARRIVAL ** a1, ARRIVAL ** a2, void *private)
{
	/* return (*a1)->time - (*a2)->time; */
	if ((*a1)->time > (*a2)->time)
		return 1;
	if ((*a2)->time > (*a1)->time)
		return -1;
	return 0;
}
int 
cmp_stachan(STACHANSTRUCT ** sc1, STACHANSTRUCT ** sc2, void *private)
{
	ARRIVAL        *a1, *a2;
	int             n1, n2;
	n1 = maxtbl((*sc1)->arrivals);
	n2 = maxtbl((*sc2)->arrivals);
	if (n1 == 0 && n2 == 0) {
		return 0;
	} else if (n1 > 0 && n2 > 0) {
		a1 = gettbl((*sc1)->arrivals, 0);
		a2 = gettbl((*sc2)->arrivals, 0);
		/*
		 * return a1->time - a2->time; printf("%s:%s %s @
		 * %s",a1->sta,a1->chan,a1->iphase,strtime(a1->time));
		 * printf("%s:%s %s @
		 * %s",a2->sta,a2->chan,a2->iphase,strtime(a2->time));
		 */
		if (a1->time > a2->time)
			return 1;
		if (a2->time > a1->time)
			return -1;
		return 0;
	} else if (n1 > 0) {
		return -1;
	} else {
		return 1;
	}

}
int 
print_stachan(STACHANSTRUCT * sc, void *private)
{

	printf("%s:%s\n", sc->sta, sc->chan);

	applytbl((Tbl *) sc->arrivals, print_arrival, private);
	return (0);
}

int
main(argc, argv)
	int             argc;
	char          **argv;

{
	char           *dbname;
	char           *sta;
	char           *chan;
	char           *tstart;
	char           *twinline;
	char           *nlines;
	char           *scale;
	char           *filter;
	char           *psfile;
	double          ts, t0, twin, te, scal;
	double          slat, slon;
	int             nl;


	Dbptr           db;

	Tbl            *sc[100];
	int             i, k, n, r;
	Trace          *trace, *tr;
	int             qpl;
	float           xdim = 8.0;
	float           ydim = 6.0;
	float           xlow = 1.0;
	float           ylow = 1.0;
	float           yydim, yylow, ybot, ytop;
	float           xmin, xmax, ymin, ymax;
	float           hue, light, sat;
	static char     expr[512];
	static char     str[512];
	char           *stachan[100];
	int             display = 0;

	int             allstations = 0;
	Dbptr           _db, _dbg, dba, _dba, dbassoc, dborigin, dborigin_s,
	                dbevent, dbx;
	int             norigs, nevents;
	char           *chan_expression = NULL;
	char           *sta_expression = NULL;
	char           *stachans = NULL;
	Tbl            *sortfields, *groupfields;
	char            sta_s[32], chan_s[32];
	char           *stas[100], *chans[100];
	int             auto_scale = 0;

	void           *private = (void *) NULL;
	Tbl            *pat1, *pat2;
	Tbl            *tbl, *arrtbl;
	STACHANSTRUCT  *thisstachanstruct;
	STACHANSTRUCT  *thiskeystachanstruct;

	double          amp, per, snr;
	char            iphase[9];
	char            dummy[100];
	int             arid;

	ARRIVAL        *arrival;

	QPlot          *ts_;
	int             i_ts;

	int            *numb;
	float          *min, *max, dfmaxtot, dfmintot;
	int             a, b, c, n_;
	char           *polygondb = 0, *placedb = 0;
	char           *extraline = malloc(100);
	char           *str1 = malloc(100);
	char           *name = malloc(80);

	double          latp, lonp, distance, azi;
	char           *originline = malloc(150);
	char           *originline1 = malloc(150);
	char           *otimestr = malloc(45);
	char           *dtype = malloc(5);
	char           *etype = malloc(5);
	char           *wr;
	double          otime, lat, lon, depth, mb, ml, ms;

	int             iclip = 1;
	float           angle = 0.0;
	int             iref = 7;
	float           x1, y1, x2, y2;
	int             ndef, nass, def;
	double          timeres, sdobs;
	char           *timedef = malloc(2);
	float           height = 0.08;
	float           ratio = 1.0;
	float           slant = 0.0;
	int             jfont = 114;
	int             has_latlon = 0;
	char            latchar[2];
	char            lonchar[2];
	char           *sta_chan_line = malloc(80);
	Srcname         parts;
	Tbl            *sctbl;
	Point           p;
	Dbptr           dbinval = dbinvalid();
	originline = strcpy(originline, " ");	/* please don't crash if
						 * nothing is found.. */
	extraline = strcpy(extraline, " ");	/* please don't crash if
						 * nothing is found.. */

	/*
	 * Get command args.
	 */
	Program_Name = argv[0];

	if (argc < 7) {
		usage();
		exit(1);
	}
	dbname = argv[1];
	sta = argv[2];
	chan = argv[3];
	tstart = argv[4];
	twinline = argv[5];
	scale = argv[6];
	filter = NULL;
	psfile = NULL;

	argv += 7;
	argc -= 7;
	for (; argc > 0; argv++, argc--) {
		if (!strcmp(*argv, "-f")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf(stderr, "%s: No -f value.\n", Program_Name);
				usage();
				exit(1);
			}
			filter = *argv;
		} else if (!strcmp(*argv, "-allstations")) {
			/*
			 * argc--; argv++; if (argc < 1) { fprintf (stderr,
			 * "Program_Name: No -a value.\n"); usage(); exit
			 * (1); }
			 */
			if (chan_expression) {
				fprintf(stderr, "either -chan or -allstations!");
				usage();
				exit(1);
			}
			allstations = 1;

		} else if (!strcmp(*argv, "-chan")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf(stderr, "%s: No -chan value.\n", Program_Name);
				usage();
				exit(1);
			}
			chan_expression = *argv;

		} else if (!strcmp(*argv, "-stachan")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf(stderr, "%s: No -stachan value.\n", Program_Name);
				usage();
				exit(1);
			}
			stachans = *argv;


		} else if (!strcmp(*argv, "-sta")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf(stderr, "%s: No -sta value.\n", Program_Name);
				usage();
				exit(1);
			}
			if (allstations) {
				fprintf(stderr, "either -chan or -allstations!");
				usage();
				exit(1);
			}
			sta_expression = *argv;

		} else if (!strcmp(*argv, "-polygons")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf(stderr, "%s: No -polygons value.\n", Program_Name);
				usage();
				exit(1);
			}
			polygondb = *argv;
		} else if (!strcmp(*argv, "-places")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf(stderr, "%s: No -places value.\n", Program_Name);
				usage();
				exit(1);
			}
			placedb = *argv;
		} else if (!strcmp(*argv, "-ps")) {
			argc--;
			argv++;
			if (argc < 1) {
				fprintf(stderr, "%s: No -ps value.\n", Program_Name);
				usage();
				exit(1);
			}
			psfile = *argv;
		} else if (!strcmp(*argv, "-display")) {
			display = 1;
		} else {
			fprintf(stderr, "%s: Illegal command line argument '%s'.\n",
				Program_Name, *argv);
			usage();
			exit(1);
		}
	}

	ts = str2epoch(tstart);
	t0 = ts;
	twin = atof(twinline);
	nl = 1;
	scal = atof(scale);
	if (scal == -1.0) {
		scal = 500.0;
		auto_scale = 1;
	}
	te = t0 + twin * nl;
	/*
	 * Open database.
	 */
	if (dbopen(dbname, "r", &db) == dbINVALID) {
		elog_clear_register(1);
		fprintf(stderr, "%s: Unable to open database.\n", Program_Name);
		exit(1);
	}
	_db = dblookup(db, 0, "wfdisc", 0, 0);
	dbassoc = dblookup(db, 0, "assoc", 0, 0);
	dborigin = dblookup(db, 0, "origin", 0, 0);
	dbevent = dblookup(db, 0, "event", 0, 0);



	strcpy(expr, "");
	if (stachans) {

		sctbl = split(stachans, ',');
		sta_chan_line = poptbl(sctbl);
		split_srcname(sta_chan_line, &parts);
		printf("parts: %s %s\n", parts.src_sta, parts.src_chan);
		sprintf(str, "(((sta=~/%s/ && chan =~ /%s/)", parts.src_sta, parts.src_chan);
		strcat(expr, str);
		while ((sta_chan_line = poptbl(sctbl)) != NULL) {
			split_srcname(sta_chan_line, &parts);
			printf("parts: %s %s\n", parts.src_sta, parts.src_chan);
			sprintf(str, "|| (sta=~/%s/ && chan =~ /%s/)", parts.src_sta, parts.src_chan);
			strcat(expr, str);
			printf("expr: %s\n", expr);
		}
		strcat(expr, ")");
		printf("final expr: %s\n", expr);

	} else {
		if (strncmp(sta, ".*", strlen(sta)) && strncmp(sta, "*", strlen(sta))) {
			strcpy(expr, "( ");
			sprintf(str, "sta =~ /%s/", sta);
			strcat(expr, str);
		}
		if (strncmp(chan, ".*", strlen(chan)) && strncmp(chan, "*", strlen(chan))) {
			if (expr[0])
				strcat(expr, " && ");
			else
				strcpy(expr, "( ");
			sprintf(str, "chan =~ /%s/", chan);
			strcat(expr, str);
		}
	}
	if (t0 != 0.0 || te != 0.0) {
		if (expr[0]) {
			strcat(expr, " && ");

		} else {
			strcpy(expr, "( ");
		}
		sprintf(str, "(time <= %.5f && endtime >= %.5f)", ts, te);
		strcat(expr, str);
	}
	if (expr[0]) {
		strcat(expr, " )");
		_db = dbsubset(_db, expr, 0);
	}
	dbquery(_db, dbRECORD_COUNT, &n);
	if (n < 1) {
		elog_log(0, "grdb_sc_loadcss: No wfdisc rows to process.\n");
		return (-1);
	}
	sortfields = newtbl(3);
	if (sortfields == NULL) {
		elog_log(0, "grdb_sc_loadcss: newtbl() error.\n");
		return (-1);
	}
	settbl(sortfields, 0, strdup("wfdisc.sta"));
	settbl(sortfields, 1, strdup("wfdisc.chan"));
	settbl(sortfields, 2, strdup("wfdisc.time"));
	_db = dbsort(_db, sortfields, 0, 0);
	groupfields = newtbl(2);
	if (groupfields == NULL) {
		elog_log(0, "grdb_sc_loadcss: newtbl() error.\n");
		return (-1);
	}
	settbl(groupfields, 0, strdup("sta"));
	settbl(groupfields, 1, strdup("chan"));
	_dbg = dbgroup(_db, groupfields, 0, 1);
	freetbl(sortfields, free);
	freetbl(groupfields, free);
	dbquery(_dbg, dbRECORD_COUNT, &n);

	_dba = dblookup(db, 0, "arrival", 0, 0);
	sprintf(expr, "( time >= %.5lf && time <= %.5lf && iphase != 'del' )", t0, te);
	_dba = dbsubset(_dba, expr, 0);
	dbassoc = dbjoin(_dba, dbassoc, 0, 0, 0, 0, 0);
	/* search related origin (prefor) */
	if (dbassoc.record != dbINVALID) {
		dborigin_s = dbjoin(dbassoc, dborigin, 0, 0, 0, 0, 0);
		dborigin_s = dbsever(dborigin_s, "assoc", 0);
		dborigin_s = dbsever(dborigin_s, "arrival", 0);
		if (dborigin_s.record != dbINVALID) {
			dbquery(dborigin_s, dbRECORD_COUNT, &norigs);
			if (norigs > 0) {
				dbevent = dbjoin(dborigin_s, dbevent, 0, 0, 0, 0, 0);
				/*
				 * pat1=newtbl(1); pat2=newtbl(1);
				 * settbl(pat1,0,strdup("prefor"));
				 * settbl(pat1,0,strdup("orid"));
				 * dborigin=dbjoin(dbevent,dborigin,&pat1,&pat
				 * 2,0,0,0);
				 * 
				 * freetbl(pat1,free); freetbl(pat2,free);
				 */
				dbevent = dbsubset(dbevent, "orid==prefor", 0);
				dbquery(dbevent, dbRECORD_COUNT, &norigs);
				dbevent.record = 0;
				has_latlon = 1;
				dbgetv(dbevent, 0,
				       "lat", &lat, "lon", &lon,
				       "depth", &depth, "time", &otime,
				       "mb", &mb, "ml", &ml, "ms", &ms,
				       "dtype", dtype, "etype", etype,
				       0);
				otimestr = epoch2str(otime, "%G   %H:%M:%S.%s");
				if (lat > 0.0) {
					strcpy(latchar, "n");
				} else {
					strcpy(latchar, "s");
				}
				if (lon > 0.0) {
					strcpy(lonchar, "e");
				} else {
					strcpy(lonchar, "w");
				}
				sprintf(originline, "%s      %.3f%s %.3f%s d:%.1f", otimestr, lat, latchar, lon, lonchar, depth);
				if (strmatches(dtype, "f|g|r|d", 0)) {
					sprintf(originline1, "   (%s)", dtype);
					originline = strcat(originline, originline1);
				}
				if (!strmatches(etype, "-", 0)) {
					sprintf(originline1, " [%s]", etype);
					originline = strcat(originline, originline1);
				}
				dbassoc = dblookup(db, 0, "assoc", 0, 0);
				dbassoc = dbjoin(dbevent, dbassoc, 0, 0, 0, 0, 0);
				dbquery(dbassoc, dbRECORD_COUNT, &norigs);
				sdobs = 0.0;
				def = 0;
				for (dbassoc.record = 0; dbassoc.record < norigs; dbassoc.record++) {
					dbgetv(dbassoc, 0,
					       "timeres", &timeres,
					       "timedef", timedef,
					       0);
					if (strncmp(timedef, "d", 1) == 0) {
						sdobs += (timeres * timeres);
						def++;
					}
				}
				/*
				 * dbfree(dbassoc); dbfree(dborigin);
				 * dbfree(dborigin_s); dbfree(dbevent);
				 */
				if (sdobs > 0) {
					sdobs = sqrt(sdobs / def);
				} else {
					sdobs = 0.0;
				}
				sprintf(originline1, "   sdobs:%.3f   ", sdobs);
				strcat(originline, originline1);
				if (mb > -900.0) {
					sprintf(originline1, " %.1fmb", mb);
					originline = strcat(originline, originline1);
				}
				if (ml > -900.0) {
					sprintf(originline1, " %.1fml", ml);
					originline = strcat(originline, originline1);
				}
				if (ms > -900.0) {
					sprintf(originline1, " %.1fms", ms);
					originline = strcat(originline, originline1);
				}
				free(originline1);
				if (placedb && has_latlon) {
					dbopen(placedb, "r", &dbx);
					dbx = dblookup(dbx, 0, "places", 0, 0);
					pat1 = newtbl(1);
					sprintf(expr, "distance(lat,lon,%f,%f)", lat, lon);
					pushtbl(pat1, expr);
					dbx = dbsort(dbx, pat1, 0, 0);
					freetbl(pat1, free);
					dbx.record = 0;
					dbgetv(dbx, 0,
					       "place", name,
					       "lat", &latp,
					       "lon", &lonp,
					       0);
					dist(rad(latp), rad(lonp), rad(lat), rad(lon), &distance, &azi);
					wr = windrose(deg(azi));
					distance = deg2km(deg(distance));
					sprintf(extraline, "%.0f km %s of %s", distance, wr, name);
					dbclose(dbx);
				}
				if (polygondb && has_latlon) {
					p.lat = lat;
					p.lon = lon;
					dbopen(polygondb, "r", &dbx);
					dbx = dblookup(dbx, 0, "polygon", 0, 0);
					dbx = inWhichPolygons(dbx, p);
					if (dbx.database != dbinval.database) {
						dbquery(dbx, dbRECORD_COUNT, &norigs);
						if (norigs > 0) {
							pat1 = newtbl(1);
							pushtbl(pat1, "level");
							dbx = dbsort(dbx, pat1, dbSORT_REVERSE, 0);
							dbx.record = 0;
							dbgetv(dbx, 0,
							       "pname", name,
							       0);

							sprintf(str1, " (%s)", name);
							extraline = strcat(extraline, str1);
							dbclose(dbx);
						}
					}
				}
			}
		}
	};

	tbl = newtbl(0);


	for (i = 0; i < n; i++) {
		_dbg.record = i;
		dbgetv(_dbg, 0, "sta", &sta_s, "chan", &chan_s, 0);

		allot(STACHANSTRUCT *, thisstachanstruct, 1);
		strcpy(thisstachanstruct->sta, sta_s);
		strcpy(thisstachanstruct->chan, chan_s);
		thisstachanstruct->n = i;
		thisstachanstruct->arrivals = newtbl(0);

		stas[i] = strdup(sta_s);
		chans[i] = strdup(chan_s);



		sprintf(expr, "( sta == '%s' && chan == '%s' )", stas[i], chans[i]);
		dba = dbsubset(_dba, expr, 0);

		dbquery(dba, dbRECORD_COUNT, &r);

		thisstachanstruct->arrivals = newtbl(0);
		for (k = 0; k < r; k++) {
			dba.record = k;

			allot(ARRIVAL *, arrival, 1);

			dbgetv(dba, 0,
			       "sta", &arrival->sta,
			       "time", &arrival->time,
			       "arid", &arrival->arid,
			       "jdate", &arrival->jdate,
			       "stassid", &arrival->stassid,
			       "chanid", &arrival->chanid,
			       "chan", &arrival->chan,
			       "iphase", &arrival->iphase,
			       "stype", &arrival->stype,
			       "deltim", &arrival->deltim,
			       "azimuth", &arrival->azimuth,
			       "delaz", &arrival->delaz,
			       "slow", &arrival->slow,
			       "delslo", &arrival->delslo,
			       "ema", &arrival->ema,
			       "rect", &arrival->rect,
			       "amp", &arrival->amp,
			       "per", &arrival->per,
			       "logat", &arrival->logat,
			       "clip", &arrival->clip,
			       "fm", &arrival->fm,
			       "snr", &arrival->snr,
			       "qual", &arrival->qual,
			       "auth", &arrival->auth,
			       "commid", &arrival->commid,
			       "lddate", &arrival->lddate,
			       0);
			pushtbl(thisstachanstruct->arrivals, arrival);
		}
		/*
		 * 
		 * applytbl(thisstachanstruct->arrivals,print_arrival,private);
		 * printf("sortiert\n");
		 */
		sorttbl(thisstachanstruct->arrivals, cmp_arrival, private);
		/*
		 * 
		 * applytbl(thisstachanstruct->arrivals,print_arrival,private);
		 * printf("max arivals %i ",maxtbl(arrtbl)); printf("max
		 * arivals %i ",r);
		 */
		/*
		 * freetbl(arrtbl, free_arrivals );
		 */

		/*
		 * pushtbl(tbl,thisstachanstruct);
		 */
		/*
		 * Make sc tables
		 */

		if (!make_scs(db, sta_s, chan_s, ts, te, &sc[i])) {
			elog_clear_register(1);

			fprintf(stderr, "%s: Unable to make sc.\n", Program_Name);
			exit(1);
		}
		thisstachanstruct->sc = sc[i];
		pushtbl(tbl, thisstachanstruct);

	}

	/*
	 * applytbl(tbl,print_stachan,private);
	 */
	sorttbl(tbl, cmp_stachan, private);
	/*
	 * printf("print stachan\n"); applytbl(tbl,print_stachan,private);
	 */
	for (i = 0; i < maxtbl(tbl); i++) {
		thisstachanstruct = gettbl(tbl, i);
		sc[i] = thisstachanstruct->sc;
		strcpy(stas[i], thisstachanstruct->sta);
		strcpy(chans[i], thisstachanstruct->chan);
		stachan[i] = malloc(100);
		strcpy(stachan[i], thisstachanstruct->sta);
		strcat(stachan[i], ":");
		strcat(stachan[i], thisstachanstruct->chan);
	}
	elog_clear_register(1);
	/*
	 * Initialize plot
	 */
	init_plot(psfile, dbname, " ", " ", stachan, ts, twin, n, auto_scale, scal,
		  filter, display);

	xdim = 10.0;
	ydim = 7.5;
	xlow = 0.0;
	ylow = 0.0;
	xmin = 0.0;
	xmax = 1.0;
	ymin = 0.0;
	ymax = 1.0;
	setdim_(&xdim, &ydim, &xlow, &ylow);
	setscl_(&xmin, &xmax, &ymin, &ymax);
	x1 = 0.01;
	y1 = ymax - 0.02;
	iclip = 0;
	iref = 2;

	jfont = 113;
	height = 0.10;
	iref = 0;
	cfont_(&jfont);
	chrsiz_(&height, &ratio, &slant);
	if (has_latlon) {
		text_(&x1, &y1, &angle, &iref, originline, &iclip, strlen(originline));
		y1 = ymax - 0.04;
		strsub(extraline, "ö", "oe", originline);
		strsub(originline, "Ö", "Oe", extraline);
		strsub(extraline, "ü", "ue", originline);
		strsub(originline, "Ü", "Ue", extraline);
		strsub(extraline, "ä", "ae", originline);
		strsub(originline, "Ä", "Ae", extraline);
		strsub(extraline, "ß", "ss", originline);
		text_(&x1, &y1, &angle, &iref, originline, &iclip, strlen(originline));
	}
	/*
	 * sprintf(originline,"Starttime:%s",epoch2str(ts, "%H:%M:%S"));
	 * y1=ymax-0.03; text_ (&x1, &y1, &angle, &iref, originline, &iclip,
	 * strlen(originline));
	 */
	iref = 5;
	jfont = 114;
	cfont_(&jfont);
	height = 0.10;
	chrsiz_(&height, &ratio, &slant);

	xdim = 8.0;
	ydim = 6.0;
	xlow = 1.0;
	ylow = 1.0;
	xmin = 0.0;
	xmax = xdim;
	ymin = 0.0;
	ymax = ydim;
	setdim_(&xdim, &ydim, &xlow, &ylow);
	setscl_(&xmin, &xmax, &ymin, &ymax);

	for (i = 0; i < n; i++) {
		/* te = ts + twin; */
		trace = (Trace *) read_sc(gettbl(sc[i], 0), ts - 20.0, te + 20.0);
		if (trace == NULL)
			continue;
		if (filter) {
			trace = (Trace *) filter_trace(trace, filter, 0);
			if (trace == NULL) {
				fprintf(stderr, "%s: filter_trace() error.\n", Program_Name);
				continue;
			}
		}
		for (tr = trace, qpl = 0; tr != NULL; tr = tr->next) {
			qpl = qpbin(tr->tstart, tr->dt, tr->nsamps, tr->data,
				    ts, twin, 4000, qpl);
		}
		SCV_free_trace(trace);
		yydim = ydim / n;
		yylow = ylow + ydim - yydim * (i + 1);
		ytop = 0.5 * yydim * scal;
		ybot = -ytop;

		nqplotsegs(qpl, xdim, yydim, xlow, yylow, &ybot, &ytop,
			   auto_scale, 1, 1.0);
		qpfree(qpl);
	}


	xdim = 8.0;
	ydim = 6.0;
	yydim = ydim / n;
	xlow = 1.0;
	ylow = 1.0;
	xmin = 0.0;
	xmax = twin;
	ymin = 0;
	ymax = 1;


	/*
	 * Arrival processing.
	 */
	/*
	 * _dba = dblookup (db, 0, "arrival", 0, 0); 	sprintf (expr, "(
	 * time >= %.5lf && time <= %.5lf && iphase!='del')", t0, te); _dba=
	 * dbsubset(_dba,expr,0);
	 */

	for (i = 0; i < n; i++) {

		thisstachanstruct = gettbl(tbl, i);
		r = maxtbl(thisstachanstruct->arrivals);
		if (r < 1)
			continue;
		hue = 0.0;
		light = 0.25;
		sat = 0.0;
		setfor_(&hue, &light, &sat);

		for (k = 0; k < r; k++) {
			ARRIVAL        *arrival = gettbl(thisstachanstruct->arrivals, k);
			double          amp, per, snr;
			char            iphase[9];
			char            fm[3];
			char            dummy[100];
			char           *timestr = malloc(100);
			int             arid;
			double          tm;
			float           thick = 0.005;
			int             ithick = 0;
			float           center;
			strcpy(iphase, arrival->iphase);
			strcpy(fm, arrival->fm);
			tm = arrival->time;
			amp = arrival->amp;
			per = arrival->per;

			tm *= 10.0;
			tm = (double) rint(tm);
			tm /= 10.0;
			timestr = epoch2str(tm, "%T");
			sprintf(timestr, " %10.10s", epoch2str(tm, "%T"));

			sprintf(expr, "%s ", iphase);
			/* print fm only if iphase begins with P */
			if (strlen(fm) > 1 && !strncasecmp(iphase, "p", 1)) {
				strncat(expr, fm, 1);
			}
			strcat(expr, timestr);
			/* sprintf (expr, "%s %s",iphase,timestr); */
			if (amp > 0.0 && per > 0.0) {
				sprintf(dummy, " t %-4.1f a %-4.1f", per, amp);
				strcat(expr, dummy);
			}
			if (tm < t0)
				continue;
			if (tm > te)
				continue;
			tm -= t0;
			/* moving yylow through lines... */
			yylow = ylow + ydim - yydim * (i + 1);
			setdim_(&xdim, &yydim, &xlow, &yylow);
			setscl_(&xmin, &xmax, &ymin, &ymax);
			x1 = tm;
			line_(&x1, &ymin, &x1, &ymax, &thick, &ithick, &iclip);

			/*
			 * will the amp-per parameters be printed above or
			 * below middle
			 */
			y1 = 0.5 + sign(k) * 0.25;

			/*
			 * on which side of line will the arrival-parameters
			 * be printed
			 */

			/*
			 * if (tm > twin/2.0) { x1 = tm - 0.005*twin; iref=7;
			 * } else { x1 = tm + 0.005*twin; iref=1; }
			 */
			x1 = tm + 0.005 * twin;
			iref = 1;
			text_(&x1, &y1, &angle, &iref, expr, &iclip, strlen(expr));
		}
	}

	/*
	 * Normal exit.
	 */
	finitt_();
	exit(0);
}

/*
 * end of main
 */

int 
sign(int zahl)
{
	return 1 - ((zahl % 2) * 2);
}

int
init_plot(fname, dbname, sta, chan, stachan, ts, twin, nl,
	  auto_scale, scal, filter, idisplay)
	char           *fname;
	char           *dbname;
	char           *sta;
	char           *chan;
	char           *stachan[100];
	double          ts;
	double          twin;
	int             nl;
	int             auto_scale;
	double          scal;
	char           *filter;
	int             idisplay;

{
	int             itran = 1;
	float           ssize = 0.9;
	float           xwin = 0.0;
	float           ywin = 0.0;
	char            plotfile[256];
	char            display[16];
	char            program[1024];
	static int      nplot = 1;
	float           xplt, yplt;
	float           angle = 0.0;
	int             iclip = 0;
	int             iref = 5;
	float           height = 0.08;
	float           ratio = 1.0;
	float           slant = 0.0;
	int             jfont = 114;
	float           xdim, ydim, xlow, ylow;
	float           xmin, xmax, ymin, ymax;
	float           thick = 0.0;
	int             ithick = 0;
	float           x1, y1, x2, y2;
	float           hue, light, sat;
	float           fac;
	long            itime;
	int             i;

	if (fname) {
		if (fname[0]) {
			strcpy(plotfile, fname);
		} else {
			sprintf(plotfile, "%s.%s.%d%d.ps",
				Program_Name, dbname, getpid(), nplot++);
		}
	} else {
		sprintf(plotfile, "%s.%s.%d%d.ps",
			Program_Name, dbname, getpid(), nplot++);
	}
	if (idisplay)
		strcpy(display, " ");
	else
		strcpy(display, "none");
	strcpy(program, Program_Name);
	initt_(&itran, plotfile, display, program, &ssize,
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
	setdim_(&xdim, &ydim, &xlow, &ylow);
	setscl_(&xmin, &xmax, &ymin, &ymax);
	iclip = 1;
	box_(&xmin, &xmax, &ymin, &ymax, &thick, &ithick, &iclip);
	xdim = 9.8;
	ydim = 7.3;
	xlow = 0.2;
	ylow = 0.2;
	xmin = 0.0;
	xmax = 9.8;
	ymin = 0.0;
	ymax = 7.3;
	setdim_(&xdim, &ydim, &xlow, &ylow);
	setscl_(&xmin, &xmax, &ymin, &ymax);
	xplt = 0.0;
	yplt = 0.0;
	jfont = 113;
	height = 0.10;
	iref = 0;
	cfont_(&jfont);
	chrsiz_(&height, &ratio, &slant);
	text_(&xplt, &yplt, &angle, &iref, "BRTT,ZAMG",
	      &iclip, strlen("BRTT,ZAMG"));
	jfont = 115;
	cfont_(&jfont);
	itime = time(NULL);
	sprintf(program, "%s: %s %s %s %s",
	      Program_Name, dbname, plotfile, cuserid(NULL), ctime(&itime));
	program[strlen(program) - 1] = '\0';
	xplt = 1;
	text_(&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
	if (filter == NULL) {
		sprintf(program, "Filter: None  Starttime: %s", epoch2str(ts, "%G %H:%M:%S"));
	} else {
		sprintf(program, "Filter: %s  Starttime: %s", filter, epoch2str(ts, "%G %H:%M:%S"));
	}
	xplt = 0.0;
	yplt = 0.18;
	text_(&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
	sprintf(program, "%s %s", sta, chan);
	xplt = 0.0;
	yplt = ydim - 0.35;
	iref = 0;
	jfont = 114;
	cfont_(&jfont);
	height = 0.15;
	chrsiz_(&height, &ratio, &slant);
	text_(&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
	/*
	 * sprintf (program, "Start time: %s", epoch2str(ts, "%Y%j %D
	 * %H:%M:%S"));
	 */
	/*
	 * sprintf (program, "Start time: %s", epoch2str(ts, "%Y-%m-%d
	 * %H:%M:%S"));
	 */

	xplt = 0.5;
	jfont = 115;
	cfont_(&jfont);
	height = 0.12;
	chrsiz_(&height, &ratio, &slant);
	text_(&xplt, &yplt, &angle, &iref, program, &iclip, strlen(program));
	xdim = 8.0;
	ydim = 6.0;
	xlow = 1.0;
	ylow = 1.0;
	xmin = 0.0;
	xmax = twin;
	ymin = 0.0;
	ymax = ydim;
	setdim_(&xdim, &ydim, &xlow, &ylow);
	setscl_(&xmin, &xmax, &ymin, &ymax);
	iref = 5;
	jfont = 114;
	cfont_(&jfont);
	height = 0.10;
	chrsiz_(&height, &ratio, &slant);
	iclip = 1;
	y1 = ymin;
	y2 = ymax;
	if (twin < 30.0) {
		ts = 0.0;
		strcpy(display, "+%.1fs");
		fac = 1.0;
		while (1) {
			if (ts > twin)
				break;
			hue = 0.0;
			light = 0.5;
			sat = 0.0;
			setfor_(&hue, &light, &sat);
			x1 = ts;
			x2 = ts;
			line_(&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
			hue = 0.0;
			light = 0.0;
			sat = 0.0;
			setfor_(&hue, &light, &sat);
			xplt = ts;
			yplt = -0.15;
			sprintf(program, "%.0fs", ts);
			text_(&xplt, &yplt, &angle, &iref, program,
			      &iclip, strlen(program));
			ts += 1.0;
		}
	} else if (twin < 150.0) {
		ts = 0.0;
		strcpy(display, "+%.1fs");
		fac = 1.0;
		while (1) {
			if (ts > twin)
				break;
			hue = 0.0;
			light = 0.5;
			sat = 0.0;
			setfor_(&hue, &light, &sat);
			x1 = ts;
			x2 = ts;
			line_(&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
			hue = 0.0;
			light = 0.0;
			sat = 0.0;
			setfor_(&hue, &light, &sat);
			xplt = ts;
			yplt = -0.15;
			sprintf(program, "%.0fs", ts);
			text_(&xplt, &yplt, &angle, &iref,
			      program, &iclip, strlen(program));
			ts += 10.0;
		}
	} else if (twin < 60 * 30.0) {
		ts = 0.0;
		strcpy(display, "+%.1fm");
		fac = 60.0;
		while (1) {
			if (ts > twin)
				break;
			hue = 0.0;
			light = 0.5;
			sat = 0.0;
			setfor_(&hue, &light, &sat);
			x1 = ts;
			x2 = ts;
			line_(&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
			hue = 0.0;
			light = 0.0;
			sat = 0.0;
			setfor_(&hue, &light, &sat);
			xplt = ts;
			yplt = -0.15;
			sprintf(program, "%.0fm", ts / 60.0);
			text_(&xplt, &yplt, &angle, &iref,
			      program, &iclip, strlen(program));
			ts += 60.0;
		}
	} else if (twin < 60 * 150.0) {
		ts = 0.0;
		strcpy(display, "+%.1fh");
		fac = 3600.0;
		while (1) {
			if (ts > twin)
				break;
			hue = 0.0;
			light = 0.5;
			sat = 0.0;
			setfor_(&hue, &light, &sat);
			x1 = ts;
			x2 = ts;
			line_(&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
			hue = 0.0;
			light = 0.0;
			sat = 0.0;
			setfor_(&hue, &light, &sat);
			xplt = ts;
			yplt = -0.15;
			sprintf(program, "%.0fm", ts / 60.0);
			text_(&xplt, &yplt, &angle, &iref,
			      program, &iclip, strlen(program));
			ts += 600.0;
		}
	} else {
		ts = 0.0;
		strcpy(display, "+%.1fh");
		fac = 3600.0;
		while (1) {
			if (ts > twin)
				break;
			hue = 0.0;
			light = 0.5;
			sat = 0.0;
			setfor_(&hue, &light, &sat);
			x1 = ts;
			x2 = ts;
			line_(&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
			hue = 0.0;
			light = 0.0;
			sat = 0.0;
			setfor_(&hue, &light, &sat);
			xplt = ts;
			yplt = -0.15;
			sprintf(program, "%.0fh", ts / 3600.0);
			text_(&xplt, &yplt, &angle, &iref,
			      program, &iclip, strlen(program));
			ts += 3600.0;
		}
	}
	ts = 0.0;
	iref = 7;
	height = 0.08;

	chrsiz_(&height, &ratio, &slant);
	for (i = 0; i < nl; i++) {
		float           yydim, yylow;

		yydim = ydim / nl;
		yylow = ylow + ydim - yydim * (i + 1);
		xplt = -0.15;
		yplt = 0.5;
		xmin = 0.0;
		xmax = xdim;
		ymin = 0.0;
		ymax = 1.0;
		setdim_(&xdim, &yydim, &xlow, &yylow);
		setscl_(&xmin, &xmax, &ymin, &ymax);
		/* sprintf (program, display, stachan[i]ts/fac */
		sprintf(program, " %s ", stachan[i]);

		text_(&xplt, &yplt, &angle, &iref,
		      program, &iclip, strlen(program));
		ts += twin;
	}
	xdim = 8.0;
	ydim = 6.0;
	xlow = 1.0;
	ylow = 1.0;
	xmin = 0.0;
	xmax = xdim;
	ymin = 0.0;
	ymax = ydim;
	setdim_(&xdim, &ydim, &xlow, &ylow);
	setscl_(&xmin, &xmax, &ymin, &ymax);
	x1 = 8.1;
	y1 = 0.0 + 0.5 * ydim / nl;
	x2 = 8.1;
	y2 = 1.0 + 0.5 * ydim / nl;
	line_(&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	x1 = 8.1;
	y1 = 0.0 + 0.5 * ydim / nl;
	x2 = 8.2;
	y2 = 0.0 + 0.5 * ydim / nl;
	line_(&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	x1 = 8.1;
	y1 = 1.0 + 0.5 * ydim / nl;
	x2 = 8.2;
	y2 = 1.0 + 0.5 * ydim / nl;
	line_(&x1, &y1, &x2, &y2, &thick, &ithick, &iclip);
	iref = 1;
	height = 0.08;
	chrsiz_(&height, &ratio, &slant);
	xplt = 8.3;
	if (auto_scale == 0) {
		yplt = 0.0 + 0.5 * ydim / nl;
		text_(&xplt, &yplt, &angle, &iref, "0", &iclip, strlen("0"));
		xplt = 8.3;
		yplt = 1.0 + 0.5 * ydim / nl;
		sprintf(program, "%.1f", scal);
		text_(&xplt, &yplt, &angle, &iref,
		      program, &iclip, strlen(program));
	} else {
		yplt = 0.5 + 0.5 * ydim / nl;
		sprintf(program, "%s", "Autoscale");
		iref = 4;
		angle = 90;
		text_(&xplt, &yplt, &angle, &iref,
		      program, &iclip, strlen(program));

	}
	return (0);
}

int
usage()
{
	fprintf(stderr, "usage: %s dbname sta chan tstart twinline scale\n",
		Program_Name);
	fprintf(stderr, "              [-f filter] [-display] [-ps psfile]\n");
	fprintf(stderr, "              [-places db] [-polygons db]\n");
	fprintf(stderr, " e.g:  %s db/rtags '.*TA' '.*Z' '7/19/1999 00:00:00' 30 500\n",
		Program_Name);
	cbanner("version 1.1", "runit ", "Niko", "zamg", "horn@zamg.ac.at");
	return (0);
}

/* $Id$ */
