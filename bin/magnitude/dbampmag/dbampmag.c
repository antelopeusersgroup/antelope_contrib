/* Copyright (c) 2002 Boulder Real Time Technologies, Inc. */
 
/* Boulder Real Time Technologies, Inc. grants permission to copy, 
   modify and/or use the source code in this software module as 
   long as the following conditions are satisfied:
   1 - This copyright statement and software agreement appears 
       in all files that contain any of the source code in this 
       software module.
   2 - This software module and any others that are derived from
       this software module must be made freely available for
       copy, modification and use by all others.
   3 - The terms of this software agreement apply only to the
       source code in this software module. */

/* Modified by Nikolaus Horn for ZAMG, Vienna */

#include <stdio.h>
#include <math.h>

#include "db.h"
#include "p_db.h"
#include "brttfilter.h"
#include "brttutil.h"
#include "response.h"
#include "tr.h"
#include "coords.h"
#include "stock.h"
#include "arrays.h"

#define default_v_r 4.0
#define default_minimum_time_window   4.0

#define PF_REVISION_TIME "1086912000"
#define MAX_REASONABLE_MAGNITUDE 9.5

int grdb_sc_getstachan();
int getchannel();
int getsegment();

int verbose=0;
int quiet=0;
static void
usage ()

{
    fprintf (stderr, "usage: dbampmag [-pf pfname] [{-use_if_not_associated|-use_if_not_defining}]\n");
    fprintf (stderr, "                [-make_magtables] [-use_mean] [-use_p2p] [-v] [-quiet] db [orid]\n");
    exit (1) ;
}

struct station_params {
	char sta[32];
	char chan_expr[64];
	char filter[30];
	char filter_type[10];
	double f1,f2;
	int np1,np2;
	char magtype[6];
	int calib_from_db;
	int decon_instr;
	int apply_wa_filter;
	int apply_clip_limits;
	int minclip;
	int maxclip;
	int use;
	Dbptr db;
	Dbptr dbgr;
	Dbptr dbsc;
	double snr_thresh;
	double twin_noise_param;
	double latency;
	double delta;
	double depth;
	double use_hypocentral_distance;
	double parrival;
	double sarrival;
	double t0_signal;
	double twin_signal;
	double t0_noise;
	double twin_noise;
	double mag;
	double signal;
	double signal_time;
	double noise;
	double noise_time;
	int nchannels;
	int use_p2pamp;
	struct channel_ {
		char chan[32];
		double mag;
		double signal;
		double signal_time;
		double noise;
		double noise_time;
	} *channels;
	struct _consts {
		double c0,c1,c2,c3,c4,c5;
	} consts;
};


static int mycompare();
static int myans();
static void mycallback();

int main (int argc, char **argv) {
	char *dbname;
	char pfname[128];
	int orid, evid;
	Dbptr db, dbo, dbs, dba, dbw, dbmw, dbgr, dbsc, dbnm, dbsm;
	int i, n, nn;
	char expr[512];
	struct station_params *sp;
	Tbl *tbl, *proc_tbl;
	double time_window = 0.5;
	double minimum_time_window = default_minimum_time_window;
	double tstart, tend;
	char chan_expr[128];
	char net[32];
	double mean, std ;
	Arr *proc_arr;
	Pf *pf;
	int ngr;
	char missing_stations[STRSZ] ; 
	int too_long = 0 ; 
	int use_if_not_associated = 0;
	int use_if_not_defining = 0;
	int use_mean = 0;
	int use_p2pamp = 0;
	int make_magtables = 0;
	Hook *hook, *hook2, *hookwf;
	Tbl *mtbl, *mtbl2, *wftbl;
	Tbl *wfpat, *tables, *wfsortfields, *groupfields;
	int ret;
	int nsta;
	char auth[32];
	
	char *magtype=NULL;
	char *filter;
	char *time0;
	double mindelta,maxdelta;
	double c0,c1;
	double	v_r=default_v_r;
	char filter_type[40];
	int np1,np2;
	double f1,f2;
	int use_hypocentral_distance=0;



	Program_Name = argv[0];
	if (argc < 2) {
		usage();
	}
	sprintf (auth, "dbamp:%s", cuserid((char *) NULL));

	proc_arr = newarr (0);
	if (proc_arr == NULL) {
		elog_complain(0, "newarr() error.\n");
		exit (1);
	}
	strcpy (pfname, "dbampmag");
	for (argv++,argc--; argc>0; argv++,argc--) {
		if (**argv != '-') break;
		if (!strcmp(*argv, "-")) break;
		if (!strcmp(*argv, "-pf")) {
			argv++; argc--;
			if (argc < 1) {
				elog_complain(0, "Need argument for -pf\n");
				usage();
			}
			strcpy (pfname, *argv);
		} else if (!strcmp(*argv, "-use_if_not_associated")) {
			use_if_not_associated = 1;
		} else if (!strcmp(*argv, "-use_if_not_defining")) {
			use_if_not_defining = 1;
		} else if (!strcmp(*argv, "-use_mean")) {
			use_mean = 1;
		} else if (!strcmp(*argv, "-use_p2p")) {
			use_p2pamp = 1;
		} else if (!strcmp(*argv, "-make_magtables")) {
			make_magtables = 1;
		} else if (!strcmp(*argv, "-v")) {
			verbose = 1;
		} else if (!strcmp(*argv, "-quiet")) {
			quiet = 1;
		} else {
			elog_complain(0, "Unrecognized argument '%s'.\n", *argv);
			usage();
		}
	}

	if (argc < 1) {
		elog_complain(0, "Need dbname argument.\n");
		usage();
	}
	dbname = *argv;

	orid = -1;
	argv++; argc--;
	if (argc > 0) {
		orid = atoi(*argv);
	}

	if (use_if_not_associated + use_if_not_defining > 1) {
		elog_complain(0, "Cannot specify both -use_if_not_associated and -use_if_not_defining at the same time.\n");
		usage();
	}

	if (strcmp(dbname, "-")) {
		if (dbopen (dbname, "r+", &db) == dbINVALID) {
			elog_complain(0, "dbopen(%s) error.\n", dbname);
			usage();
		}
		dbo = dblookup (db, 0, "origin", 0, 0);
	} else {
		if (dbread_view (stdin, &db, NULL) != 0) {
			elog_complain(0, "dbread_view() error.\n");
			usage();
		}
		dbo = db;
	}
	dbs = dblookup (db, 0, "site", 0, 0);
	dba = dblookup (db, 0, "assoc", 0, 0);
	dbw = dblookup (db, 0, "wfdisc", 0, 0);
	dbnm = dblookup (db, 0, "netmag", 0, 0);
	dbsm = dblookup (db, 0, "stamag", 0, 0);
	strcpy (net, "-");

	/* Read orbmag parameter file */

	if (pfrequire(pfname,PF_REVISION_TIME) != 0) {
		elog_die(1,"%s\n");
	}

	if (pfread (pfname, &pf) < 0) {
		elog_complain(0, "pfread(%s) error.\n", pfname);
		exit (1);
	}	

	parse_param (pf, "use_hypocentral_distance", P_BOOL, 0, &use_hypocentral_distance);
	if (parse_param (pf, "v_r", P_DBL, 1, &v_r) < 0) {
		elog_complain(0, "parse_param(v_r) error.\n");
		exit (1);
	}
	if (parse_param (pf, "time_window_factor", P_DBL, 1, &time_window) < 0) {
		elog_complain(0, "parse_param(time_window_factor) error.\n");
		exit (1);
	}
	parse_param (pf, "minimum_time_window", P_DBL, 0, &minimum_time_window);

	if (parse_param (pf, "magtype", P_STR, 1, &magtype) < 0) {
		elog_complain(0, "parse_param(magtype) error.\n");
		exit (1);
	}
	if (strcmp(magtype,"mb")!=0 && strcmp(magtype,"ms")!=0 && strcmp(magtype,"ml")!=0) {
		elog_complain(0, "magtype must be mb,ml or ms instead of %s\n",magtype);
		exit(1);
	}
	if (parse_param (pf, "filter", P_STR, 1, &filter) < 0) {
		elog_complain(0, "parse_param(filter) error.\n");
		exit (1);
	}
	if (sscanf(filter,"%s %lf %d %lf %d",filter_type,&f1,&np1,&f2,&np2)!=5) {
		elog_complain(0,"parse_filter(%s) error.\n",filter);
		exit(1);
	}
	if (parse_param (pf, "c0", P_DBL, 1, &c0) < 0) {
		elog_complain(0, "parse_param(c0) error.\n");
		exit (1);
	}
	if (parse_param (pf, "c1", P_DBL, 1, &c1) < 0) {
		elog_complain(0, "parse_param(c1) error.\n");
		exit (1);
	}
	if (parse_param (pf, "time0", P_STR, 1, &time0) < 0) {
		elog_complain(0, "parse_param(time0) error.\n");
		exit (1);
	}
	if (strcmp(time0,"P")!=0 && strcmp(time0,"S")!=0 && strcmp(time0,"R") !=0) {
		elog_complain(0, "time0 must be P,S or R instead of %s\n",time0);
		exit(1);
	}
	if (parse_param (pf, "mindelta", P_DBL, 1, &mindelta) < 0) {
		elog_complain(0, "parse_param(mindelta) error.\n");
		exit (1);
	}
	if (parse_param (pf, "maxdelta", P_DBL, 1, &maxdelta) < 0) {
		elog_complain(0, "parse_param(maxdelta) error.\n");
		exit (1);
	}
	tbl = NULL;
	if (parse_param (pf, "mag", P_TBL, 1, &tbl) < 0) {
		elog_complain(0, "parse_param(ml) error.\n");
		exit (1);
	}
	for (i=0; i<maxtbl(tbl); i++) {
		char sta[64], calib_ans[8], decon_ans[8], wa_ans[8];
		double snr_thresh;
		char *line;
		int ret;
		int apply_clip_limits, minclip, maxclip;
		double c2,c3,c4,c5,dummy,twin_noise;

		line = (char *) gettbl (tbl, i);
		ret = sscanf (line, "%s %s %s %s %s %lf %lf %lf %lf %lf %lf %lf %lf %lf", 
				sta, chan_expr, calib_ans, decon_ans, wa_ans, 
				&snr_thresh, &twin_noise, &dummy, &c2, &c3 ,&c4 ,&c5, &minclip, &maxclip);
		if (ret != 12 && ret != 14) {
			elog_die(0, "Cannot parse line '%s'\n", line);
		}
		if (ret == 12) {
			apply_clip_limits=0;
			minclip = -2147283648;
			maxclip = 2147283647;
		} else if ( ret == 14) {
			apply_clip_limits=1;
		}
		sp = (struct station_params *) malloc (sizeof (struct station_params));
		if (sp == NULL) {
			elog_complain(1, "malloc() error.\n");
			exit (1);
		}
		memset(sp, 0, sizeof(struct station_params));
		strcpy (sp->sta, sta);
		strcpy (sp->chan_expr, chan_expr);
		sp->calib_from_db = myans(calib_ans);
		sp->decon_instr = myans(decon_ans);
		sp->apply_wa_filter = myans(wa_ans);
		sp->db = db;
		sp->use_p2pamp = use_p2pamp;
		sp->nchannels = 0;
		sp->channels = NULL;
		sp->consts.c0 = c0;
		sp->consts.c1 = c1;
		sp->consts.c2 = c2;
		sp->consts.c3 = c3;
		sp->consts.c4 = c4;
		sp->consts.c5 = c5;
		strcpy (sp->magtype,magtype);
		strcpy (sp->filter,filter);
		setarr (proc_arr, sta, sp);
		sp->snr_thresh = snr_thresh;
		sp->twin_noise_param = twin_noise;
		sp->apply_clip_limits = apply_clip_limits;
		sp->minclip = minclip;
		sp->maxclip = maxclip;
		sp->use_hypocentral_distance = use_hypocentral_distance;
		sp->mag = -999.0;
	}
	proc_tbl = valsarr (proc_arr);
	pf = NULL;

	if (orid > -1) {
		sprintf (expr, "orid == %d", orid);
		dbo = dbsubset (dbo, expr, 0);
	}

	dbquery (dbo, dbRECORD_COUNT, &n);
	if (n < 1) {
		elog_complain(0, "No origins to process.\n");
		exit (1);
	}

	wfpat = strtbl ("sta", "time::endtime", 0);
	tables = strtbl ("wfdisc", 0);
	wfsortfields = strtbl ("sta", "chan", "time", 0);
	groupfields = strtbl ("sta", "chan", 0);

	/* loop over origins */

	hook = NULL;
	hook2 = NULL;
	hookwf = NULL;
	for (dbo.record=0; dbo.record<n; dbo.record++) {
		int nm;
		double otime, olat, olon, odepth;
		char whichid[10],auth[64];

		ret = dbgetv (dbo, 0, 	"orid", &orid, 
					"evid", &evid, 
					"time", &otime, 
					"lat", &olat, 
					"lon", &olon, 
					"depth", &odepth, 
					"auth", auth,
					0);
		if (ret == dbINVALID) {
			elog_die(0, "dbgetv(orid) error.\n");
		}
		if (orid < 0) continue;
		if (verbose && !quiet) {
			char *s;
			elog_debug (0, "\n");
			elog_debug (0, "Processing origin %d at lat %.3f lon %.3f depth %.3f time %s auth %s\n", 
				orid, olat, olon, odepth, s=strtime(otime), auth);
			free(s) ;

		}

		for (i=0; i<maxtbl(proc_tbl); i++) {
			sp = (struct station_params *) gettbl (proc_tbl, i);
			sp->use = 0;
			sp->mag = -999.0;
		}

		/* make join with site or assoc */

		mtbl = NULL;
		if (use_if_not_associated) {
			nm = dbmatches (dbo, dbs, NULL, NULL, &hook, &mtbl);
		} else {
			nm = dbmatches (dbo, dba, NULL, NULL, &hook, &mtbl);
		}
		if (nm == dbINVALID) {
			elog_complain(0, "dbmatches() error...skipping orid %d.\n", orid);
			if (mtbl) freetbl (mtbl, 0);
			continue;
		}

		if (nm < 1) {
			elog_complain(0, "No associations or stations for orid #%d.\n", orid);
			if (mtbl) freetbl (mtbl, 0);
			continue;
		}
		
		tstart = 1.e30;
		tend = -1.e30;
		nn = 0;
		*missing_stations = 0 ; 
		if (use_if_not_associated) {
			for (i=0; i<nm; i++) {
				double delta, parrival, sarrival, twin, twin_noise;
				double t0, t1;
				char sta[32];

				dbs.record = (int) gettbl (mtbl, i);
		
				dbgetv (dbs, 0, "sta", sta, 0);
				sp = (struct station_params *) getarr (proc_arr, sta);
				if (sp == NULL) {
		    			if ( strlen(missing_stations) + strlen(sta) + 2 < STRSZ) { 			
						strcat ( missing_stations, " " ) ;
						strcat ( missing_stations, sta ) ;
		    			} else { 
						too_long = 1 ;
		    			}
		    			continue ;
				}
				sprintf (expr, "distance(%.6f,%.6f,site.lat,site.lon)", olat, olon);
				dbex_evalstr (dbs, expr, dbREAL, &delta);
				sprintf (expr, "ptime(%.6f,%.6f)", delta, odepth);
				dbex_evalstr (dbs, expr, dbREAL, &parrival);
				sprintf (expr, "stime(%.6f,%.6f)", delta, odepth);
				dbex_evalstr (dbs, expr, dbREAL, &sarrival);
				sp->use = 1;		
				sp->delta = delta;
				sp->depth = odepth;
				if (sp->twin_noise_param <= 0.0) {
					twin_noise=0.0;
				} else {
					twin_noise=sp->twin_noise_param;
				}
				twin = time_window*(sarrival - parrival);
				if (twin  < minimum_time_window) twin= minimum_time_window;
				sp->parrival = otime + parrival;
				sp->sarrival = otime + sarrival;
				sp->twin_signal = twin;
				sp->t0_noise = otime + parrival - twin_noise - 10.0;
				sp->twin_noise = twin_noise;
				if ( strncasecmp("P",time0,1)==0) {
					sp->t0_signal = otime + parrival - twin/2.0;
				} else if ( strncasecmp("S",time0,1)==0) { 
					sp->t0_signal = otime + sarrival - twin/2.0;
				} else if (strncasecmp("R",time0,1)==0) {
					sp->t0_signal = otime + deg2km(delta)/v_r - twin/2.0;
				}

				t0 = sp->t0_noise - 100.0;
				t1 = sp->t0_signal + sp->twin_signal;
				if (t0 < tstart) tstart = t0;
				if (t1 > tend) tend = t1;
						
				nn++;
			}
		} else {
			for (i=0; i<nm; i++) {
				double delta, parrival, sarrival, twin, twin_noise;
				double t0, t1;
				char sta[32];
				char timedef[32];
				Dbptr dbsm;

				dba.record = (int) gettbl (mtbl, i);
		
				dbgetv (dba, 0, "sta", sta, "timedef", timedef, 0);
				if (!use_if_not_defining) {
					if (strcmp(timedef, "d")) continue;
				}
				sp = (struct station_params *) getarr (proc_arr, sta);
				if (sp == NULL) {
		    			if ( strlen(missing_stations) + strlen(sta) + 2 < STRSZ) { 			
						strcat ( missing_stations, " " ) ;
						strcat ( missing_stations, sta ) ;
		    			} else { 
						too_long = 1 ;
		    			}
		    			continue ;
				}
				dbsm = dbs;
				dbsm.record = dbSCRATCH;
				dbputv (dbsm, 0, "sta", sta, "ondate", yearday(otime), 0);
				mtbl2 = NULL;
				ret = dbmatches (dbsm, dbs, NULL, NULL, &hook2, &mtbl2);
				if (ret == dbINVALID) {
					elog_complain(0, "dbmatches() error...skipping orid:sta %d:%s.\n", orid, sta);
					if (mtbl2) freetbl (mtbl2, 0);
					continue;
				}
				if (ret == 0) {
					elog_complain(0, "Cannot find sta %s in site table for orid %d.\n", sta, orid);
					if (mtbl2) freetbl (mtbl2, 0);
					continue;
				}
				dbs.record = (int) gettbl (mtbl2, 0);
				freetbl (mtbl2, 0);
				sprintf (expr, "distance(%.6f,%.6f,site.lat,site.lon)", olat, olon);
				dbex_evalstr (dbs, expr, dbREAL, &delta);
				sprintf (expr, "ptime(%.6f,%.6f)", delta, odepth);
				dbex_evalstr (dbs, expr, dbREAL, &parrival);
				sprintf (expr, "stime(%.6f,%.6f)", delta, odepth);
				dbex_evalstr (dbs, expr, dbREAL, &sarrival);
				sp->use = 1;		
				sp->delta = delta;
				sp->depth = odepth;
				if (sp->twin_noise_param <= 0.0) {
					twin_noise=0.0;
				} else {
					twin_noise=sp->twin_noise_param;
				}
				twin = time_window*(sarrival - parrival);
				if (twin  < minimum_time_window) twin= minimum_time_window;
				sp->parrival = otime + parrival;
				sp->sarrival = otime + sarrival;
				sp->twin_signal = twin;
				sp->t0_noise = otime + parrival - twin_noise - 10.0;
				sp->twin_noise = twin_noise;
				if ( strncasecmp("P",time0,1)==0) {
					sp->t0_signal = otime + parrival - twin/2.0;
				} else if ( strncasecmp("S",time0,1)==0) { 
					sp->t0_signal = otime + sarrival - twin/2.0;
				} else if (strncasecmp("R",time0,1)==0) {
					sp->t0_signal = otime + deg2km(delta)/v_r - twin/2.0;
				}
				t0 = sp->t0_noise - 100.0;
				t1 = sp->t0_signal + sp->twin_signal;
						
				if (t0 < tstart) tstart = t0;
				if (t1 > tend) tend = t1;
				nn++;
			}
		}
		freetbl (mtbl, 0);
	
		if ( !quiet) {
			if ( strlen(missing_stations) > 0 ) { 
					elog_complain( 0, "parameter file is missing stations:%s%s\n", 
					missing_stations, too_long ? " and others" : "" ) ; 
			}
		}
	
		if (nn < 1) {
			if (!quiet) {
				elog_complain(0, "No stations to process for orid %d.\n", orid);
			}
			continue;
		}

		for (i=0; i<maxtbl(proc_tbl); i++) {
			int j ;
			char sta[32] ;
			Dbptr dbv;
	
			sp = (struct station_params *) gettbl (proc_tbl, i);
			if (sp->use == 0) continue;
			if (sp->delta < mindelta ||sp->delta > maxdelta) {
					if (verbose) {
						elog_complain(0, "%s - station not in valid distance-range.\n",sta);
					}
					continue;
				}
			dbmw = dbw;
			dbmw.record = dbSCRATCH;
			dbputv (dbmw, 0, 	"sta", sp->sta, 
						"time", sp->t0_noise-100.0, 
						"endtime", sp->t0_signal+sp->twin_signal, 
						0);
			wftbl = NULL;
			ret = dbmatches (dbmw, dbw, &wfpat, &wfpat, &hookwf, &wftbl);
			if (ret == dbINVALID) {
				if (!quiet) {
					elog_complain(0, "dbmatches() error...skipping waveforms for orid:sta %d:%s.\n", orid, sp->sta);
				}
				if (wftbl) freetbl (wftbl, 0);
				continue;
			}
			if (ret == 0) {
				if (!quiet) {
					elog_complain(0, "Cannot find wavforms for orid:sta %d:%s.\n", orid, sp->sta);
				}
				if (wftbl) freetbl (wftbl, 0);
				continue;
			}
			dbv = dbtbl2view (dbw, 0, tables);
			for (j=0; j<maxtbl(wftbl); j++) {
				dbw.record = (int) gettbl (wftbl, j);
				dbadd (dbv, dbref(&dbw));
			}
			freetbl (wftbl, 0);
			dbsc = dbsort (dbv, wfsortfields, 0, 0);
			dbgr = dbgroup (dbsc, groupfields, 0, 1);
			dbquery (dbgr, dbRECORD_COUNT, &ngr); 
	
			sp->dbgr = dbgr;
			sp->dbsc = dbsc;
			for (j=0; j<ngr; j++) {
				mycallback (sp, j);
			} 

			dbfree (dbv);
			dbfree (dbsc);
			dbfree (dbgr);
			
		}
	
		/* Compute network magnitude */
	
		mean = 0.0;
		std = 0.0;
		nn = 0;
		for (i=0; i<maxtbl(proc_tbl); i++) {
			sp = (struct station_params *) gettbl (proc_tbl, i);
			if (sp->use == 0) continue;
			if (sp->mag < -998.0) continue;
			mean += sp->mag;
			std += (sp->mag)*(sp->mag);
			nn++;
		}
		nsta = nn;
	
		sorttbl (proc_tbl, mycompare, NULL);
	
		if (nn > 0) {
			double magnitude, median, lo, hi, unc;
			int magid;
			
	
			magid = -1;
			mean /= nn;
			std /= nn;
			std -= mean*mean;
			if (std > 0.0) {
				std = sqrt(std);
			} else {
				std = 0.0;
			}
			nn = maxtbl(proc_tbl);
			for (i=0; i<nn; i++) {
				sp = (struct station_params *) gettbl (proc_tbl, i);
				if (sp->mag >= -998.0) break;
			}
			nn -= i;
			if (nn%2) {
				sp = (struct station_params *) gettbl (proc_tbl, i+nn/2);
				median = sp->mag;
			} else {
				sp = (struct station_params *) gettbl (proc_tbl, i+nn/2-1);
				median = 0.5*sp->mag;
				sp = (struct station_params *) gettbl (proc_tbl, i+nn/2);
				median += 0.5*sp->mag;
			}
			sp = (struct station_params *) gettbl (proc_tbl, (int) (i+0.1587*nn));
			lo = sp->mag;
			sp = (struct station_params *) gettbl (proc_tbl, (int) (i+0.8413*nn));
			hi = sp->mag;
			printf ("%s mean = %.2f, median = %.2f, uncert = +%.2f/-%.2f\n", 
				sp->magtype,mean, median, hi-median, median-lo);
			fflush (stdout);
			magnitude = median;
			unc = 0.5*(hi-lo);
			if (use_mean) {
				magnitude = mean;
				unc = std;
			}
			
			strcpy(whichid,sp->magtype);
			strcat(whichid,"id");
			
			if (make_magtables) {
				
				
			   magid=dbnextid(dbnm,"magid");
				ret = dbaddv (dbnm, 0,	"magid", magid, 
							"net", net,
							"orid", orid,
							"evid", evid,
							"magtype", sp->magtype,
							"nsta", nsta,
							"magnitude", magnitude,
							"uncertainty", unc,
							"auth", auth,
							0);
				if (ret < 0) {
					if (!quiet) {
						elog_complain(0, "dbaddv(netmag) error.\n(magid=%i net=%s orid=%i evid=%i",magid,net,orid,evid);
					}
				} else {
					dbnm.record = ret;
					dbgetv (dbnm, 0, "magid", &magid, 0);
					for (i=0; i<maxtbl(proc_tbl); i++) {
						sp = (struct station_params *) gettbl (proc_tbl, i);
						if (sp->use == 0) continue;
						if (sp->mag < -998.0) continue;
						ret = dbaddv (dbsm, 0,	"magid", magid,
									"sta", sp->sta,
									"orid", orid,
									"evid", evid,
									"magtype", sp->magtype,
									"magnitude", sp->mag,
									"auth", auth,
									0);
						if (ret < 0) {
							if (!quiet) {
								elog_complain(0, "dbputv(stamag) error.\n");
							}
							continue;
						}
					}
				}
				dbputv (dbo, 0, sp->magtype, magnitude, whichid, magid, 0);
			} else {
				dbputv (dbo, 0, sp->magtype, magnitude, 0);
			}
		} else {
			dbputv (dbo, 0, sp->magtype, -999.0, 0);
		}
	}
	
	return (0) ;
}

static int
mycompare (struct station_params **a, struct station_params **b, void *private)

{
	if ((*a)->mag < (*b)->mag) return (-1);
	if ((*a)->mag > (*b)->mag) return (1);
	return (0);
}
int 
compmag (double c0, double c1, double c2, double c3, double c4, double c5, double delta, double depth, int use_hypocentral_distance, double signal, double *mag)
{
	if (use_hypocentral_distance) {
		double depth_deg;
		depth_deg=km2deg(depth);
		delta=sqrt((depth_deg*depth_deg)+delta*delta);
	}
		
	if (c2 != 0.0 ) {
		*mag=c0 + c5 + log10(signal) + c1*log10(delta) +c2*log10(delta*c3 + c4);
	} else {
		*mag=c0 + c5 + log10(signal) + c1*log10(delta);
	}
	return(0);
}

static int
myans (char *string)

{
	int yes;

	yes = 0;
	if (!strcasecmp (string, "yes")) yes = 1;
	if (!strcasecmp (string, "y")) yes = 1;
	if (!strcasecmp (string, "true")) yes = 1;
	if (!strcasecmp (string, "t")) yes = 1;
	if (!strcmp (string, "1")) yes = 1;

	return (yes);
}

static void
mycallback (struct station_params *sp, int ichan)

{
	char sta[32], chan[32];
	int i, n, nsegs, nsamp;
	float *data;
	Dbptr dbs, dbc, dbi, dbv1, dbv2;
	char expr[512], fname[1024];
	char rsptype[32];
	int ret;
	FILE *file;
	Response *resp=NULL;
	double calib=1.0, tstart, dt, time, endtime, signal, noise, a, signal_time, noise_time, max_a, min_a, max_time, min_time;
	int type;
	void *fil;
	char filspec[512];
	double gain;
	double mag;
	Dbptr trace;
	int getcalib;
	double noise_mean, signal_raw, snr;
	int iseg;
	char filter_specification[120];
	char *t1,*t2;
	char *s1,*s2;

	if (grdb_sc_getstachan (sp->dbgr, ichan, sta, chan, &nsegs, &time, &endtime) < 0) {
		if (!quiet) {
			elog_complain(0, "grdb_sc_getstachan() error for %s.\n", sta);
		}
		return;
	}
	if (strcmp(sta, sp->sta)) return;
	sprintf (expr, "chan =~ /%s/", sp->chan_expr);
	sp->dbgr.record = ichan;
	dbex_evalstr (sp->dbgr, expr, dbBOOLEAN, &i);
	if (i == 0) return;

	if (verbose) {
		printf ("%s %s: ", sta, chan);
		fflush (stdout);
	}

	dbs = dblookup (sp->db, 0, "sensor", 0, 0);
	dbi = dblookup (sp->db, 0, "instrument", 0, 0);
	sprintf (expr, "sta == \"%s\" && chan == \"%s\" && time <= %f && (endtime <= 0.0 || endtime >= %f)",
			sp->sta, chan, sp->t0_noise, sp->t0_noise);
	dbv1 = dbsubset (dbs, expr, 0);
	dbv2 = dbjoin (dbv1, dbi, 0, 0, 0, 0, 0);
	n = 0;
	dbquery (dbv2, dbRECORD_COUNT, &n);
	if (n != 1) {
		dbfree (dbv1);
		dbfree (dbv2);
		if (!quiet) {
			elog_complain(0, "mycallback: cannot find instrument row for %s_%s.\n", sp->sta, chan);
		}
		return;
	}
	dbv2.record = 0;
	if (dbgetv (dbv2, 0, "rsptype", rsptype, 0) < 0) {
		dbfree (dbv1);
		dbfree (dbv2);
		if (!quiet) {
			elog_complain(0, "mycallback: dbgetv(rsptype) error for %s_%s.\n", sp->sta, chan);
		}
		return;
	}

	if (sp->decon_instr) {
		ret = dbextfile (dbv2, "instrument", fname);
		if (ret != 1) {
			dbfree (dbv1);
			dbfree (dbv2);
			if (!quiet) {
				elog_complain(0, "mycallback: dbextfile(instrument) error for %s_%s.\n", sp->sta, chan);
			}
			return;
		}
		file = fopen(fname, "r");
		if (file == NULL) {
			dbfree (dbv1);
			dbfree (dbv2);
			if (!quiet) {
				elog_complain(1, "mycallback: fopen(%s) error for %s_%s.\n", fname, sp->sta, chan);
			}
			return;
		}
		if (read_response (file, &resp) < 0) {
			dbfree (dbv1);
			dbfree (dbv2);
			if (!quiet) {
				elog_complain(1, "mycallback: read_response(%s) error for %s_%s.\n", fname, sp->sta, chan);
			}
			return;
		}
		fclose (file);
	}
	dbfree (dbv1);
	dbfree (dbv2);

	getcalib = 1;
	if (sp->calib_from_db) {
		getcalib = 0;
		dbc = dblookup (sp->db, 0, "calibration", 0, 0);
		dbv1 = dbsubset (dbc, expr, 0);
		n = 0;
		dbquery (dbv1, dbRECORD_COUNT, &n);
		if (n != 1) {
			dbfree (dbv1);
			if (!quiet) {
				elog_complain(0, "mycallback: cannot find calibration row for %s_%s.\n", sp->sta, chan);
			}
			return;
		}
		dbv1.record = 0;
		if (dbgetv (dbv1, 0, "calib", &calib, 0) < 0) {
			dbfree (dbv1);
			if (!quiet) {
				elog_complain(0, "mycallback: dbgetv(calib) error for %s_%s.\n", sp->sta, chan);
			}
			return;
		}
	if (calib == 0.0) {
			dbfree (dbv1);
			if (!quiet) {
				elog_complain(0, "mycallback: calib == 0 for %s_%s.\n", sp->sta, chan);
			}
			return;
		}
		dbfree (dbv1);
	}

	trace.database = dbINVALID;
	trace.table = dbINVALID;
	if (getchannel (sp->dbsc, NULL, sta, chan, sp->t0_noise-100.0, sp->t0_signal+sp->twin_signal,
					"seg", getcalib, NULL, 0.0, 0.0, NULL,
					&nsegs, &trace) < 0) {
		if (!quiet) {
			elog_complain(0, "getchannel() error for %s:%s.\n", sta, chan);
		}
		return;
	}
	if (nsegs < 1) {
		if (!quiet) {
			elog_complain(0, "No data to process for %s.\n", sta);
		}
		trdestroy (&trace);
		return;
	}
	n = 0;
	noise_mean = 0.0;
	noise = 0.0;
	for (iseg = 0; iseg<nsegs; iseg++) {
		if (getsegment (trace, iseg, &tstart, &dt, &nsamp, &data) < 0) {
			if (!quiet) {
				elog_complain(0, "getsegment() error for %s:%s.\n", sta, chan);
			}
			trdestroy (&trace);
			return;
		}
		elog_clear_register(0);
		
		if (sp->apply_clip_limits) {
			float minclip, maxclip;

			minclip = sp->minclip;
			maxclip = sp->maxclip;
			if (getcalib) {
				Dbptr trcalib;
				double calib2;

				trcalib = trace;
				if (iseg >= 0) trcalib.record += iseg;
				if (dbgetv (trcalib, 0, "calib", &calib2, 0) == dbINVALID) {
					elog_complain(0, "getsegment: dbgetv(calib) error.\n");
					calib = 1.0;
				}
				minclip *= calib2;
				maxclip *= calib2;
			}
			for (i=0; i<nsamp; i++) {
				if (data[i] >= 1.e20) continue;
				if (data[i] >= maxclip) {
					if (verbose) {
						printf ("clip limit exceeded\n");
						fflush (stdout);
					}
					trdestroy (&trace);
					return;
				}
				if (data[i] <= minclip) {
					if (verbose) {
						printf ("clip limit exceeded\n");
						fflush (stdout);
					}
					trdestroy (&trace);
					return;
				}
			}
		}
		
		if (sp->calib_from_db) {
			for (i=0; i<nsamp; i++) {
				if (data[i] < 1.e20) {
					data[i] *= calib;
				}
			}
		}
		gain = 1.0;
	
		if (sp->apply_wa_filter && sp->decon_instr) {
			if (!quiet) {
				elog_complain(0, "Cannot decon instrument for %s:%s.\n", sta, chan);
			}
			trdestroy (&trace);
			return;
		} else if (sp->decon_instr) {
			if (!quiet) {
				elog_complain(0, "Cannot decon instrument for %s:%s.\n", sta, chan);
			}
			trdestroy (&trace);
			return;
		} else if (sp->apply_wa_filter) {
			type = BRTTFILTER_NEW;
			fil = NULL;
			if (!strcmp(rsptype, "V")) {
				fil = (void *) wafl_create (NULL, 1, 0, dt);
				if (fil == NULL) {
					if (!quiet) {
						elog_complain(0, "wafl_create() error for %s:%s.\n", sta, chan);
					}
					trdestroy (&trace);
					return;
				}
				strcpy (filspec, "WAV");
			} else if (!strcmp(rsptype, "A")) {
				fil = (void *) wafl_create (NULL, 2, 0, dt);
				if (fil == NULL) {
					if (!quiet) {
						elog_complain(0, "wafl_create() error for %s:%s.\n", sta, chan);
					}
					trdestroy (&trace);
					return;
				}
				strcpy (filspec, "WAA");
			} else if (!strcmp(rsptype, "D")) {
				fil = (void *) wafl_create (NULL, 0, 0, dt);
				if (fil == NULL) {
					if (!quiet) {
						elog_complain(0, "wafl_create() error for %s:%s.\n", sta, chan);
					}
					trdestroy (&trace);
					return;
				}
				strcpy (filspec, "WAD");
			} else {
				if (!quiet) {
					elog_complain(0, "mycallback: Cannot use rsptype '%s'.\n", rsptype);
				}
				return;
			}
			fil_init (fil, (double)data[0]);
			fil_filf (fil, nsamp, data, 1, data);
			fil_free (fil);
		} else {
			if (!strcmp(rsptype, "V")) {
				type = BRTTFILTER_NEW;
				fil = NULL;
				/*if (parse_filter ("BW 0 0 0.3 1", dt, &type, &fil) < 0) {*/
				if (parse_filter (sp->filter, dt, &type, &fil) < 0) {
					if (!quiet) {
						elog_complain(0, "mycallback: parse_filter() error.\n");
					}
					return;
				}
				fil_init (fil, (double)data[0]);
				fil_filf (fil, nsamp, data, 1, data);
				fil_free (fil);
				/*gain = 2080.0 * 1.e-6 / (0.3*2.0*M_PI);*/
				gain = 1.0;
			} else if (!strcmp(rsptype, "A")) {
				type = BRTTFILTER_NEW;
				fil = NULL;
	strcpy(filter_specification,sp->filter);strcat(filter_specification,";");strcat(filter_specification,sp->filter);
				if (parse_filter (filter_specification, dt, &type, &fil) < 0) {
					if (!quiet) {
						elog_complain(0, "mycallback: parse_filter() error.\n");
					}
					return;
				}
				fil_init (fil, (double)data[0]);
				fil_filf (fil, nsamp, data, 1, data);
				fil_free (fil);
				/*gain = 2080.0 * 1.e-6 / ((0.3*2.0*M_PI) * (0.3*2.0*M_PI));*/
				gain = 1/(((sp->f2-sp->f1) * 2.0 * M_PI) * ((sp->f2 - sp->f1) * 2.0*M_PI));
			} else if (!strcmp(rsptype, "D")) {
				gain = 1.0;
			} else {
				if (!quiet) {
					elog_complain(0, "mycallback: Cannot use rsptype '%s'.\n", rsptype);
				}
				return;
			}
		}

		if (sp->twin_noise > 0.0) {
			for (i=0,time=tstart; i<nsamp; i++,time+=dt) {
				if (time < sp->t0_noise) continue;
				if (time > sp->t0_noise+sp->twin_noise) {
			    	char *s ; 
			    	if ( n < 1 && iseg == nsegs-1 ) { 
					elog_complain( 1, "no preceding noise data for %s:%s at time %s", 
						sta, chan, s=strtime(sp->t0_noise)) ; 
					free(s) ;
			    	}
					break;
				}
				if (data[i] >= 1.e20) {
					continue;
				}
				n++;
				noise_mean += data[i];
				noise += data[i]*data[i];
			}
		}
	}
	if (sp->twin_noise > 0.0) {
		if (n < 1) {
			if (verbose) {printf ("\n"); fflush (stdout);}
			return;
		}
		noise_mean = noise_mean/n;
		noise = noise/n;
		noise -= noise_mean*noise_mean;
		noise = sqrt(noise);
	} else {
		double signal_mean;

		signal_mean = 0.0;
		n = 0;
		for (iseg = 0; iseg<nsegs; iseg++) {
			if (getsegment (trace, iseg, &tstart, &dt, &nsamp, &data) < 0) {
				elog_complain(0, "getsegment() error for %s:%s.\n", sta, chan);
				trdestroy (&trace);
				return;
			}
			elog_clear_register(0);
	
			for (i=0,time=tstart,signal=0.0; i<nsamp; i++,time+=dt) {
				if (data[i] >= 1.e20) continue;
				if (time < sp->t0_noise) continue;
				if (time < sp->t0_signal) continue;
				if (time > sp->t0_signal+sp->twin_signal) break;
				signal_mean += data[i];
				n++;
			}
		}
		if (n > 0) noise_mean = signal_mean/n;
	}
	signal = 0.0;
	max_a = -1.0e20;
	min_a =  1.0e20;
	for (iseg = 0; iseg<nsegs; iseg++) {
		if (getsegment (trace, iseg, &tstart, &dt, &nsamp, &data) < 0) {
			if (!quiet) {
				elog_complain(0, "getsegment() error for %s:%s.\n", sta, chan);
			}
			trdestroy (&trace);
			return;
		}
		elog_clear_register(0);

		for (i=0; i<nsamp; i++) {
			if (data[i] < 1.e20) {
				data[i] -= noise_mean;
			}
		}

		for (i=0,time=tstart,signal=0.0; i<nsamp; i++,time+=dt) {
			if (data[i] >= 1.e20) continue;
			if (time < sp->t0_noise) continue;
			if (time < sp->t0_signal) continue;
			if (time > sp->t0_signal+sp->twin_signal) break;
			a = data[i];
			if (a > max_a) {
				max_a = a;
				max_time = time;
			}
			if (a < min_a) {
				min_a = a;
				min_time = time;
			}
		}
	}
	if (sp->use_p2pamp) {
		signal = ABS((max_a - min_a)  / 2.0);
		if (min_time > max_time) {
			signal_time = max_time;
		} else {
			signal_time = min_time;
		}
	} else {
		min_a = ABS(min_a);
		max_a = ABS(max_a);
		if (min_a > max_a) {
			signal = min_a;
			signal_time = min_time;
		} else {
			signal = max_a;
			signal_time = max_time;
		}
	}
	trdestroy (&trace);
	signal_raw = signal;
	signal *= gain;
	noise *= gain;

	mag = -999.0;
	snr = 0.0;
	if (signal > 0.0) {
		if (noise > 0.0) {
			snr = signal/(noise*1.414);
			if (snr > sp->snr_thresh) {
				signal -= noise;
				compmag (sp->consts.c0, sp->consts.c1, sp->consts.c2, sp->consts.c3, sp->consts.c4, sp->consts.c5, sp->delta, sp->depth, sp->use_hypocentral_distance, signal, &mag);
			}
		} else {
			compmag (sp->consts.c0, sp->consts.c1, sp->consts.c2, sp->consts.c3, sp->consts.c4, sp->consts.c5, sp->delta, sp->depth, sp->use_hypocentral_distance, signal, &mag);
		}
		if (mag > MAX_REASONABLE_MAGNITUDE) {
			elog_complain(0,"magnitude %.2f unreasonably high -> ignored\n",mag);
			mag = -999.00;
		}

	}
	if (mag > sp->mag) {
		sp->mag = mag;
		sp->noise = noise;
		sp->noise_time = noise_time;
		sp->signal = signal;
		sp->signal_time = signal_time;
	}


	if (verbose) {
		char *t;
	    printf (
	     	"signal %.3f(%.3f) at %s snr %.3f noise_mean %.3f(%.3f) mag %.2f\n",
			signal, signal_raw/calib, t=strtime(signal_time),
			snr, noise_mean, noise_mean/calib, mag);
		fflush (stdout);
		free(t);
	}
}
