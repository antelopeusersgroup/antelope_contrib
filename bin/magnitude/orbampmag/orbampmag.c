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
#define REVISION_CODE "1.6"
#define REVISION_DATE "2007-03-19 17:00"


#include <stdio.h>
#include <signal.h>
#include <math.h>
#include <unistd.h>

#include "xtra.h"
#include "db.h"
#include "coords.h"
#include "orb.h"
#include "brttpkt.h"
#include "wffil.h"
#include "brttutil.h"
#include "response.h"
#include "stock.h"

#define PF_REVISION_TIME "1086912000"
#define MAX_REASONABLE_MAGNITUDE 9.5

#define	default_time_window	3.0
#define default_minimum_time_window	2.0
#define	AUTH_EXPR	"orbassoc"
#define default_v_r 4.0

void
usage ()

{
	fprintf (stderr, "usage: orbampmag [-v] [-start {pktid|time}]\n");
	fprintf (stderr, "              [-number number] [-nowait] [-state statefile]\n");
	fprintf (stderr, "              [-pf parameter_file] [-auth_expr auth_expr]\n");
	fprintf (stderr, "              [-target_orbmag torbmag] [-next_target_orbmag ntorbmag]\n");
	fprintf (stderr, "              [-make_magtables] [-use_mean] [-use_p2p]\n");
	fprintf (stderr, "              [{-use_if_not_associated|-use_if_not_defining}]\n");
	fprintf (stderr, "              orbwf orbdb dbname\n");
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
	Dbptr db;
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
	struct consts_ {
		double c0,c1,c2,c3,c4,c5;
	} consts;
};

static int mycallback();
static int mycompare();
static int myans ();
static int put_next_target();

char *statefile=NULL;
int lastpktid = -1;
Pf *pf=NULL;
int loop=1;
int verbose=0;

static void myhand();

int main (int argc, char **argv) {
	char *select_wf=NULL;
	char *start=NULL;
	char *target_orbmag=NULL;
	char *orbwf_name, *orbdb_name, *dbname;
	char pfname[128];
	int orbdbin, orbdbout, orbwf;
	int pktid, get;
	int pktidnewest;
	int ret;
	FILE *f;
	Dbptr db, dbo, dbs, dbj, dbnm, dbsm;
	int i, n, nn, num ;
	char expr[2048];
	char chan_expr[512];
	char target[128];
	Tbl *tbl;
	double time_window = default_time_window;
	double minimum_time_window = default_minimum_time_window;
	double tstart, tend, group_latency;
	double mean , std ;
	int numager=0;
	int wait=1;
	Chantraceproc *cp;
	int pktidin, nbytes=0, bufsiz=0;
	char *packet=NULL;
	double twin_noise;
	char *line;
	static Arr *proc_arr;
	static Tbl *proc_tbl;
	struct station_params *sp;
	char auth_expr[1024];
	char *s ;
	int make_magtables = 0;
	int use_mean = 0;
	int use_if_not_associated = 0;
	int use_if_not_defining = 0;
	static char select[8192];
	double maxwaittime = 600.0;
	struct t_consts *consts;
	char *magtype;
	char *filter;
	char *time0;
	double mindelta,maxdelta;
	char filter_type[40];
	int np1,np2;
	double f1,f2;
	
	double c0,c1;
	char *next_target_orbmag=NULL;
	double	v_r=default_v_r;
	int use_p2pamp = 0;
	int use_hypocentral_distance=0;


	elog_init (argc, argv) ;
	elog_notify ( 0, "%s : Revision: %s Date: %s\n", argv[0],REVISION_CODE,REVISION_DATE ) ;

	if (argc < 4) {
		usage();
		exit (1);
	}

	strcpy (pfname, "orbampmag");
	strcpy (auth_expr, AUTH_EXPR);
	proc_arr = newarr (0);
	if (proc_arr == NULL) {
		elog_complain(0, "newarr() error.\n");
		exit (1);
	}
	for (argv++,argc--; argc>0; argv++,argc--) {
		if (**argv != '-') break;
		if (!strcmp(*argv, "-start")) {
			argc--; argv++;
			if (argc < 1) {
				elog_complain(0, "Need -start argument.\n");
				usage();
				exit (1);
			}
			start = *argv;
		} else if (!strcmp(*argv, "-target_orbmag")) {
			argc--; argv++;
			if (argc < 1) {
				elog_complain(0, "Need -target_orbmag argument.\n");
				usage();
				exit (1);
			}
			target_orbmag = *argv;
		} else if (!strcmp(*argv, "-next_target_orbmag")) {
			argc--; argv++;
			if (argc < 1) {
				elog_complain(0, "Need -next_target_orbmag argument.\n");
				usage();
				exit (1);
			}
			next_target_orbmag = *argv;
		} else if (!strcmp(*argv, "-state")) {
			argc--; argv++;
			if (argc < 1) {
				elog_complain(0, "Need -state argument.\n");
				usage();
				exit (1);
			}
			statefile = *argv;
		} else if (!strcmp(*argv, "-pf")) {
			argc--; argv++;
			if (argc < 1) {
				elog_complain(0, "Need -pf argument.\n");
				usage();
				exit (1);
			}
			strcpy (pfname, *argv);
		} else if (!strcmp(*argv, "-auth_expr")) {
			argc--; argv++;
			if (argc < 1) {
				elog_complain(0, "Need -auth_expr argument.\n");
				usage();
				exit (1);
			}
			strcpy (auth_expr, *argv);
		} else if (!strcmp(*argv, "-number")) {
			argc--; argv++;
			if (argc < 1) {
				elog_complain(0, "Need -number argument.\n");
				usage();
				exit (1);
			}
			numager = atoi(*argv);
		} else if (!strcmp(*argv, "-make_magtables")) {
			make_magtables = 1;
		} else if (!strcmp(*argv, "-use_mean")) {
			use_mean = 1;
		} else if (!strcmp(*argv, "-use_p2p")) {
			use_p2pamp = 1;
		} else if (!strcmp(*argv, "-use_if_not_associated")) {
			use_if_not_associated = 1;
		} else if (!strcmp(*argv, "-use_if_not_defining")) {
			use_if_not_defining = 1;
		} else if (!strcmp(*argv, "-nowait")) {
			wait = 0;
		} else if (!strcmp(*argv, "-v")) {
			verbose = 1;
		} else {
			elog_complain(0, "Unrecognized argument '%s'.\n", *argv);
			usage();
			exit (1);
		}
	}

	if (argc < 1) {
		elog_complain(0, "Need orbwf argument.\n");
		usage();
		exit (1);
	}
	orbwf_name = *argv;

	argv++; argc--;
	if (argc < 1) {
		elog_complain(0, "Need orbdb argument.\n");
		usage();
		exit (1);
	}
	orbdb_name = *argv;

	argv++; argc--;
	if (argc < 1) {
		elog_complain(0, "Need dbname argument.\n");
		usage();
		exit (1);
	}
	dbname = *argv;

	if (use_if_not_associated + use_if_not_defining > 1) {
		elog_complain(0, "Cannot specify both -use_if_not_associated and -use_if_not_defining at the same time.\n");
		usage();
		exit (1);
	}

	/* Open database */

	if (dbopen (dbname, "r+", &db) == dbINVALID) {
		elog_complain(0, "dbopen(%s) error.\n", dbname);
		usage();
		exit (1);
	}
	finit_db (db);
	dbs = dblookup (db, 0, "site", 0, 0);
	dbo = dblookup (db, 0, "origin", 0, 0);
	dbnm = dblookup (db, 0, "netmag", 0, 0);
	dbsm = dblookup (db, 0, "stamag", 0, 0);

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
	if (parse_param (pf, "maxwaittime", P_DBL, 1, &maxwaittime) < 0) {
		elog_complain(0, "parse_param(maxwaittime) error.\n");
		exit (1);
	}
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
	if (strcmp(time0,"P")!=0 && strcmp(time0,"S")!=0 && strcmp(time0,"R")!=0) {
		elog_complain(0, "time0 must be P,S  or R instead of %s\n",time0);
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
		elog_complain(0, "parse_param(mag) error.\n");
		exit (1);
	}
	for (i=0; i<maxtbl(tbl); i++) {
		char sta[64], calib_ans[8], decon_ans[8], wa_ans[8];
		double snr_thresh, latency, c2, c3, c4, c5, twin_noise;
		int	apply_clip_limits, minclip, maxclip;

		line = (char *) gettbl (tbl, i);
		ret = sscanf (line, "%s %s %s %s %s %lf %lf %lf %lf %lf %lf %lf", 
				sta, chan_expr, calib_ans, decon_ans, wa_ans, 
				&snr_thresh, &twin_noise, &latency, &c2, &c3, &c4, &c5, &minclip, &maxclip);
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
			elog_die(1, "malloc() error.\n");
		}
		memset (sp, 0, sizeof (struct station_params));
		strcpy (sp->sta, sta);
		strcpy (sp->chan_expr, chan_expr);
		strcpy (sp->magtype,magtype);
		strcpy (sp->filter,filter);
		strcpy(sp->filter_type,filter_type);
		sp->f1=f1; sp->f2=f2;
		sp->np1=np1;sp->np2=np2;
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
		setarr (proc_arr, sta, sp);
		sp->latency = latency;
		sp->snr_thresh = snr_thresh;
		sp->twin_noise_param = twin_noise;
		sp->apply_clip_limits = apply_clip_limits;
		sp->minclip = minclip;
		sp->maxclip = maxclip;
		sp->use_hypocentral_distance = use_hypocentral_distance;
	}
	if (parse_param (pf, "latency", P_DBL, 1, &group_latency) < 0) {
		elog_complain(0, "parse_param(latency) error.\n");
		exit (1);
	}
	proc_tbl = valsarr (proc_arr);
	pf = NULL;

	/* orb setup stuff */

	orbdbin = orbopen (orbdb_name, "r&");
	if (orbdbin < 0) {
		elog_complain(0, "orbopen(%s) error.\n", orbdb_name);
		exit (1);
	}
	orbdbout = orbopen (orbdb_name, "w&");
	if (orbdbout < 0) {
		elog_complain(0, "orbopen(%s) error.\n", orbdb_name);
		exit (1);
	}
	if (target_orbmag) {
		sprintf (target, "/pf/%s", target_orbmag);
	} else {
		sprintf (target, "/pf/orbmag");
	}
	if (orbselect (orbdbin, target) < 0) {
		elog_complain(0, "orbselect(%s) error.\n", orbdb_name);
		exit (1);
	}

	pktid = ORBNEWEST;
	get = 0;
	if (statefile) {
		f = fopen (statefile, "r");
		if (f == NULL) {
			elog_complain(0, "fopen('%s') error...using start parameters\n.", statefile);
		} else {
			pf = NULL;
			if (pfin (f, &pf) < 0) {
				fclose (f);
				elog_complain(0, "pfin('%s') error...using start parameters\n.", statefile);
			} else {
				fclose (f);
				if (parse_param (pf, "lastpktid", P_LINT, 1, &pktid) < 0) {
					elog_complain(0, "parse_param(lastpktid) error....using start parameters\n");
					pktid = ORBNEWEST;
				}
				pktid = orbseek (orbdbin, pktid);
				if (pktid < 0) {
					elog_complain(0, "orbseek(lastpktid) error...using start parameters.\n");
					pktid = ORBNEXT;
				} else {
					elog_complain(0, "resurrection successfull...start following pktid %d.\n", pktid);
				}
			}
			pf = NULL;
		}
	}
	if (!wait) {
		pktidnewest = orbseek (orbdbin, ORBNEWEST);
		if (pktidnewest < 0) {
			elog_complain(0, "No packets.\n");
			exit (1);
		}
	}


	if (pktid < 0) {
		if (start) {
			if (!strcmp(start, "OLDEST")) {
				pktid = ORBOLDEST;
				get = 1;
			} else if (!strcmp(start, "NEWEST")) {
				pktid = ORBNEWEST;
				ret = orbseek (orbdbin, pktid);
				ret = orbseek (orbdbin, ORBPREV);
			} else {
				if (strlen(start) > 10 || strchr(start, ' ') || strchr(start, '.')
							|| strchr(start, ':') || strchr(start, '/')) {
					double time;
	
					time = str2epoch(start);
					orbafter (orbdbin, time);
				} else {
					pktid = atoi(start);
					orbseek (orbdbin, pktid);
					orbseek (orbdbin, ORBPREV);
				}
			}
		} else pktid = ORBNEXT;
	}

	signal (SIGINT, myhand);
	signal (SIGHUP, myhand);
	signal (SIGQUIT, myhand);
	signal (SIGTERM, myhand);

	/* Main processing loop */

	num = 0;
	while (loop) {
		char srcname[64];
		double pkttime;
		char line[STRSZ];
		char *record;
		double olat, olon, otime, odepth;
		char auth[64];
		Pf *pfpkt= NULL;
		int orid, evid, nsta;
		int first;
		int ombid;
		double omb;
		char pfbuf[512];
		Arr *Keep_stass;
		char *Keep_origin;

		if (pfpkt) {
		    pffree (pfpkt);
		    pfpkt = 0 ; 
		}

		if (verbose) {
			elog_debug (0, "Reading next %s packet...\n", target);
		}
		if (get) {
			orbget (orbdbin, pktid, &pktidin, srcname, &pkttime, &packet, &nbytes, &bufsiz);
			get = 0;
			orbseek (orbdbin, ORBOLDEST);
		} else {
			if ( orbreap (orbdbin, &pktidin, srcname, &pkttime, &packet, &nbytes, &bufsiz) ) { 
			    break; 
			}
		}

		if (strcmp(srcname, target)) {
			if (!wait && pktidin == pktidnewest) break;
			continue;
		}

		if (orbpkt2pf (packet, nbytes, &pfpkt) < 0) {
			elog_complain(0, "orbpkt2pf() error.\n");
			if (pfpkt) {
				pffree (pfpkt);
				pfpkt=0;
			}
			if (!wait && pktidin == pktidnewest) break;
			continue;
		}
		if (next_target_orbmag) {
			Keep_stass=NULL;
			if (parse_param (pfpkt, "assocs", P_ARR, 1, &Keep_stass) < 0) {
				elog_complain(0, "parse_param(assocs) error.\n");
				if (pfpkt) {
					pffree (pfpkt);
					pfpkt= 0;
				}
				if (Keep_stass) freearr (Keep_stass, 0);
				continue;
			}
			if (parse_param (pfpkt, "origin", P_STR, 1, &Keep_origin) < 0) {
				elog_complain(0, "parse_param(origin) error\n");
				if (pfpkt) {
					pffree(pfpkt);
					pfpkt= 0;
				}
				if (Keep_origin) free(Keep_origin);
				continue;
			}
			
		}

		record = NULL;
		if (parse_param (pfpkt, "origin", P_STR, 1, &record) < 0) {
			elog_complain(0, "parse_param(origin) error.\n");
			if (pfpkt) {
				pffree (pfpkt);
				pfpkt= 0;
			}
			if (record) free (record);
			if (!wait && pktidin == pktidnewest) break;
			continue;
		}

		dbo.record = dbSCRATCH;
		if (dbput (dbo, record) == dbINVALID) {
			elog_complain(0, "dbput() error.\n");
			if (pfpkt) {
				pffree (pfpkt);
				pfpkt= 0;
			}
			if (record) free (record);
			if (!wait && pktidin == pktidnewest) break;
			continue;
		}
		free (record);

		if (dbgetv (dbo, 0, "lat", &olat, "lon", &olon, "time", &otime, "orid", &orid, "evid", &evid,
							"depth", &odepth, "auth", auth, 0) < 0) {
			elog_complain(0, "dbgetv() error.\n");
			if (pfpkt) {
				pffree (pfpkt);
				pfpkt= 0;
			}
			if (!wait && pktidin == pktidnewest) break;
			continue;
		}
		if (verbose) {
			elog_debug (0, "\n");
			elog_debug (0, "Processing pktid %d event at lat %.3f lon %.3f depth %.3f time %s auth %s\n", 
				pktidin, olat, olon, odepth, s=strtime(otime), auth);
			free(s) ;
		}
		/*sprintf (expr, "auth =~ /%s/", auth_expr);*/
		sprintf (expr, "auth !~ /.*%s.*/", auth_expr);
		
		dbex_evalstr (dbo, expr, dbBOOLEAN, &i);
		if (!i) {
			if (verbose) {
				elog_debug (0, "wrong author.\n");
			}
		   	if (pfpkt) {
				pffree (pfpkt);
				pfpkt= 0;
			}
		    	continue;
		}

		sprintf (expr, "ondate <= %d && (offdate <= 0 || offdate >= %d)", yearday(otime), yearday(otime));
		dbj = dbsubset (dbs, expr, 0);

		dbquery (dbj, dbRECORD_COUNT, &n);
		if (n < 1) {
			dbfree (dbj);
			elog_complain(0, "Nothing to process.\n");
		   	if (pfpkt) {
				pffree (pfpkt);
				pfpkt= 0;
			}
			if (!wait && pktidin == pktidnewest) break;
			continue;
		}

		cp = (Chantraceproc *) chantraceproc_new (group_latency, maxwaittime);
		if (cp == NULL) {
			dbfree (dbj);
			elog_complain(0, "chantraceproc_new() error.\n");
		   	if (pfpkt) {
				pffree (pfpkt);
				pfpkt= 0;
			}
			if (!wait && pktidin == pktidnewest) break;
			continue;
		}

		for (i=0; i<maxtbl(proc_tbl); i++) {
			sp = (struct station_params *) gettbl (proc_tbl, i);
			sp->nchannels = 0;
			if (sp->channels) free (sp->channels);
			sp->channels = NULL;
			sp->mag = -999.0;
		}
	
		if (select_wf) {
			strcpy (select, "(");
			strcat (select, select_wf);
			strcat (select, ") & (");
		} else {
			strcpy (select, "(");
		}
		first = 1;
		if (use_if_not_associated) {
		   	if (pfpkt) {
				pffree (pfpkt);
				pfpkt= 0;
			}
			tstart = 1.e30;
			tend = -1.e30;
			nn = 0;
			for (dbj.record=0; dbj.record<n; dbj.record++) {
				double delta, parrival, sarrival, twin;
				double t0, t1;
				char net[32], sta[32], ssta[32];
				char netstachan[1024];
				char netsta[512];
				int apply_calib;
	
				dbgetv (dbj, 0, "sta", sta, 0);
				sp = (struct station_params *) getarr (proc_arr, sta);
				if (sp == NULL) {
							if (verbose) {
						elog_debug (0, "Selecting %s - station not in parameter file.\n", sta);
					}
					continue;
				}
				sprintf (expr, "distance(%f,%f,lat,lon)", olat, olon);
				dbex_evalstr (dbj, expr, dbREAL, &delta);
				if (delta < mindelta ||delta > maxdelta) {
					if (verbose) {
						elog_debug (0, "Selecting %s - station not in valid distance-range.\n",sta);
					}
					continue;
				}
				if (verbose) {
					elog_debug (0, "Selecting %s - station OK.\n", sta);
				}
				sprintf (expr, "ptime(%f,%f)", delta, odepth);
				dbex_evalstr (dbj, expr, dbREAL, &parrival);
				sprintf (expr, "stime(%f,%f)", delta, odepth);
				dbex_evalstr (dbj, expr, dbREAL, &sarrival);
				
				sp->delta = delta;
				sp->depth= odepth;
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
				seed_net (sta, net, ssta);
				sprintf (netstachan, "%s_%s_%s", net, ssta, sp->chan_expr);
				sprintf (netsta, "%s_%s", net, ssta);
				if (!first) strcat (select, "|");
				first = 0;
				strcat (select, netsta);
				strcat (select, ".*");
				apply_calib = 1;
				if (sp->calib_from_db) apply_calib = 0;
				/*
			    if (sp->input_wf) {
				 */ 
					if (cp == NULL) {
						cp = (Chantraceproc *) chantraceproc_new (group_latency, maxwaittime);
						if (cp == NULL) {
							dbfree (dbj);
							elog_complain(0, "chantraceproc_new() error.\n");
							goto CONTINUE;
						}
					}
					if (chantraceproc_addchan (cp, netstachan, mycallback, sp,
										t0, t1, sp->latency, apply_calib) < 0) {
						elog_complain(0, "chantraceproc_addchan(%s) error.\n", netstachan);
						break;
					}
				/*
					input_wf = 1;
				}
				*/
				nn++;
			}

			if (dbj.record < n || nn == 0) {
				if (verbose) {
					elog_debug (0, "No stations to process.\n");
				}
				if (cp) chantraceproc_free (cp);
				dbfree (dbj);
				if (!wait && pktidin == pktidnewest) break;
				if (next_target_orbmag) {
					put_next_target(Keep_stass,Keep_origin,next_target_orbmag,orbdbout);
					free(Keep_origin);
					freearr(Keep_stass,0);
				}
				continue;
			}
		} else {
			Arr *starr;

			starr = NULL;
			if (parse_param (pfpkt, "assocs", P_ARR, 1, &starr) < 0) {
				elog_complain(0, "parse_param(assocs) error.\n");
				if (cp) chantraceproc_free (cp);
				dbfree (dbj);
				if (pfpkt) {
					pffree (pfpkt);
					pfpkt= 0;
				}
				if (starr) freearr (starr, 0);
				if (!wait && pktidin == pktidnewest) break;
				if (next_target_orbmag) {
					put_next_target(Keep_stass,Keep_origin,next_target_orbmag,orbdbout);
					free(Keep_origin);
					freearr(Keep_stass,0);
				}
				continue;
			}


			tstart = 1.e30;
			tend = -1.e30;
			nn = 0;
			for (dbj.record=0; dbj.record<n; dbj.record++) {
				double delta, parrival, sarrival, twin;
				double t0, t1;
				char net[32], sta[32], ssta[32];
				char timedef[32];
				char netstachan[1024];
				char netsta[512];
				int apply_calib;
				char *ptr;
	
				dbgetv (dbj, 0, "sta", sta, 0);
				sp = (struct station_params *) getarr (proc_arr, sta);
				if (sp == NULL) {
					if (verbose) {
						elog_debug (0, "Selecting %s - station not in parameter file.\n", sta);
					}
					continue;
				}
				ptr = (char *) getarr (starr, sta);
				if (ptr == NULL) {
					if (verbose) {
						elog_debug (0, "Selecting %s - station not associated.\n", sta);
					}
					continue;
				}
				if (!use_if_not_defining) {
					if (sscanf (ptr, "%s", timedef) == 1) {
						if (strcmp(timedef, "d")) {
							if (verbose) {
								elog_debug (0, "Selecting %s - station not defining.\n", sta);
							}
							continue;
						}
					} else {
						if (verbose) {
							elog_debug (0, " - station not defining.\n", sta);
						}
						continue;
					}
				}
				sprintf (expr, "distance(%f,%f,lat,lon)", olat, olon);
				dbex_evalstr (dbj, expr, dbREAL, &delta);
				if (delta < mindelta ||delta > maxdelta) {
					if (verbose) {
						elog_debug (0, "Selecting %s - station not in valid distance-range.\n",sta);
					}
					continue;
				}
				if (verbose) {
					elog_debug (0, "Selecting %s - station OK.\n", sta);
				}
				sprintf (expr, "ptime(%f,%f)", delta, odepth);
				dbex_evalstr (dbj, expr, dbREAL, &parrival);
				sprintf (expr, "stime(%f,%f)", delta, odepth);
				dbex_evalstr (dbj, expr, dbREAL, &sarrival);
				if (sp->twin_noise_param <= 0.0) {
					twin_noise= 0.0;
				} else {
					twin_noise=sp->twin_noise_param;
				}
				sp->delta = delta;
				sp->depth=odepth;
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
				seed_net (sta, net, ssta);
				sprintf (netstachan, "%s_%s_%s", net, ssta, sp->chan_expr);
				sprintf (netsta, "%s_%s", net, ssta);
				if (!first) strcat (select, "|");
				first = 0;
				strcat (select, netsta);
				strcat (select, ".*");
				apply_calib = 1;
				if (sp->calib_from_db) apply_calib = 0;
				/*
				if (sp->input_wf) {
				*/
					if (cp == NULL) {
						cp = (Chantraceproc *) chantraceproc_new (group_latency, maxwaittime);
						if (cp == NULL) {
							dbfree (dbj);
							elog_complain(0, "chantraceproc_new() error.\n");
							goto CONTINUE;
						}
					}
					if (chantraceproc_addchan (cp, netstachan, mycallback, sp,
										t0, t1, sp->latency, apply_calib) < 0) {
						elog_complain(0, "chantraceproc_addchan(%s) error.\n", netstachan);
						break;
					}
				/*	
					input_wf = 1;
				}
				*/
				nn++;
			}
			/*pffree (pfpkt);
			moved to end of loop to be able to read from arr...
			*/
			freearr (starr, 0);
			

			if (dbj.record < n || nn == 0) {
				if (verbose) {
					elog_debug (0, "No stations to process.\n");
				}
				if (cp) chantraceproc_free (cp);
				dbfree (dbj);
				if (!wait && pktidin == pktidnewest) break;
				if (next_target_orbmag) {
					put_next_target(Keep_stass,Keep_origin,next_target_orbmag,orbdbout);
					free(Keep_origin);
					freearr(Keep_stass,0);
				}
				continue;
			}
		}

		while (1) {
			if (verbose) {
				elog_debug (0, "Opening waveform orb...\n");
			}
			orbwf = orbopen (orbwf_name, "r&");
			if (orbwf < 0) {
				elog_complain(0, "orbopen(%s) error.\n", orbwf_name);
				sleep (10);
				if (next_target_orbmag) {
					put_next_target(Keep_stass,Keep_origin,next_target_orbmag,orbdbout);
					free(Keep_origin);
					freearr(Keep_stass,0);
				}
				if (cp) chantraceproc_free (cp);
				dbfree (dbj);
				if (!wait && pktidin == pktidnewest) break;
				continue;
			}
			break;
		}

		strcat (select, ")");
		if (verbose) {
			elog_debug (0, "Processing waveforms...\n");
			elog_debug (0, "select set to %s\n", select);
		}

		ret = orbselect (orbwf, select);
		if (ret < 0) {
			elog_complain(0, "orbselect(%s) error.\n", orbwf_name);
			exit (1);
		}
		if (ret == 0) {
			if (verbose) {
				elog_debug (0, "No stations to process.\n");
			}
			chantraceproc_free (cp);
			dbfree (dbj);
			if (!wait && pktidin == pktidnewest) break;
			if (next_target_orbmag) {
				put_next_target(Keep_stass,Keep_origin,next_target_orbmag,orbdbout);
				free(Keep_origin);
				freearr(Keep_stass,0);
			}
			continue;
		}

		if (verbose) {
			elog_debug (0, "Setting orbafter to %s\n", s=strtime(tstart));
			free (s);
		}
		if (orbafter (orbwf, tstart) < 0) {
			chantraceproc_free (cp);
			dbfree (dbj);
			elog_complain(0, "orbafter() error.\n");
			if (next_target_orbmag) {
				put_next_target(Keep_stass,Keep_origin,next_target_orbmag,orbdbout);
				free(Keep_origin);
				freearr(Keep_stass,0);
			}
			if (!wait && pktidin == pktidnewest) 
			    break;
			continue;
		}

		/* Waveform processing */

		ret = chantraceproc_process (cp, orbwf);
		if (verbose) {
			elog_debug (0, "Closing waveform orb...\n");
		}
		orbclose (orbwf);
		if (ret < 0) {
			chantraceproc_free (cp);
			dbfree (dbj);
			elog_complain(0, "chantraceproc_process() error.\n");
			put_next_target(Keep_stass,Keep_origin,next_target_orbmag,orbdbout);
			free(Keep_origin);
			freearr(Keep_stass,0);
			if (!wait && pktidin == pktidnewest) break;
			continue;
		}
		if (ret == 1 && verbose) {
			elog_debug (0, "Timeout on waveform processing\n");
		}
		chantraceproc_free (cp);
		dbfree (dbj);
		if (verbose) {
			elog_debug (0, "All waveforms processed...\n");
		}

		/* Compute network magnitude */

		mean = 0.0;
		std = 0.0;
		nn = 0;
		for (i=0; i<maxtbl(proc_tbl); i++) {
			sp = (struct station_params *) gettbl (proc_tbl, i);
			if (sp->channels == NULL) continue;
			if (sp->mag < -998.0) continue;
			mean += sp->mag;
			std += (sp->mag)*(sp->mag);
			nn++;
		}
		nsta = nn;

		sorttbl (proc_tbl, mycompare, NULL);

		if (nsta > 0) {
			double magnitude, median, lo, hi, unc;
			int magid;
			char whichid[10];
	
			magid = -1;
			if (make_magtables) {
				magid = dbnextid (db, "magid");
				if (magid < 0) {
					elog_complain(0, "dbnextid(magid) error.\n");
					magid = -1;
				}
			}
			mean /= nsta;
			std /= nsta;
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
			elog_notify (0, "magtype %s mean = %.2f, std = %.2f, median = %.2f, uncert = +%.2f/-%.2f, nsta %d\n", 
				sp->magtype, mean, std, median, hi-median, median-lo, nsta);
			magnitude = median;
			unc = 0.5*(hi-lo);
			if (use_mean) {
				magnitude = mean;
				unc = std;
			}

			strcat (auth, sp->magtype);
			strcpy(whichid,sp->magtype);
			strcat(whichid,"id");
			dbputv (dbo, 0, sp->magtype, magnitude, whichid, magid, "auth", auth, 0);
			while (db2orbpkt (dbo, orbdbout));

			if (magid > -1) {
				dbnm.record = dbNULL;
				dbnm.field = dbALL;
				dbget (dbnm, 0);
				dbnm.record = dbSCRATCH;
				ret = dbputv (dbnm, 0,
							"orid", orid,
							"evid", evid,
							"magtype", sp->magtype,
							"nsta", nsta,
							"magnitude", magnitude,
							"uncertainty", unc,
							"auth", "orbampmag",
							"magid", magid,
							0);
				if (ret < 0) {
					elog_complain(0, "dbputv(netmag) error.\n");
				} else {
					while (db2orbpkt (dbnm, orbdbout));
					for (i=0; i<maxtbl(proc_tbl); i++) {
						sp = (struct station_params *) gettbl (proc_tbl, i);
						if (sp->mag < -998.0) continue;
						dbsm.record = dbNULL;
						dbsm.field = dbALL;
						dbget (dbsm, 0);
						dbsm.record = dbSCRATCH;
						ret = dbputv (dbsm, 0,	"magid", magid,
									"sta", sp->sta,
									"orid", orid,
									"evid", evid,
									"magtype", sp->magtype,
									"magnitude", sp->mag,
									"auth", "orbampmag",
									0);
						if (ret < 0) {
							elog_complain(0, "dbputv(stamag) error.\n");
						} else {
							while (db2orbpkt (dbsm, orbdbout));
						}
					}
				}
			}
		}

		lastpktid = pktidin;
		if (statefile) {
			if (!pf) {
				sprintf (line, "lastpktid	%d\n", lastpktid);
				pfcompile (line, &pf);
			}
			pfput_int (pf, "lastpktid", lastpktid);
			pfwrite (statefile, pf);
			pffree(pf);
			pf = 0;
		}
		
		if (next_target_orbmag) {
			Pf *pf1;
			record=malloc(512);
			dbget(dbo,record);
			strcpy(pfbuf,"origin &Literal{\n");
			strcat(pfbuf,record);
			strcat(pfbuf,"}\n");	
			pf1=NULL;
			if (pfcompile (pfbuf, &pf1) < 0) {
				elog_log(0, "pfcompile() error \n");
				freearr(Keep_stass,0);
				free(record);
				exit (1);
			}
			if (Keep_stass) {
				pfput_arr(pf1,"assocs",Keep_stass);
				while(pf2orbpkt(pf1,next_target_orbmag,orbdbout));
				freearr(Keep_stass,0);
			} else {
				while(pf2orbpkt(pf1,next_target_orbmag,orbdbout));
			}
			pffree(pf1);
			
		}
		
		if (pfpkt) {
			pffree (pfpkt);
			pfpkt= 0;
		}

CONTINUE:	num++;
		if (numager > 0 && num >= numager) 
		    break;
		if (!wait && pktidin == pktidnewest) 
		    break;

	}

	return (0);
}

int 
put_next_target(Arr *arr, char *record, char* target_name, int orb)
{
	char pfbuf[512];
	Pf *pf;
	
	strcpy(pfbuf,"origin &Literal{\n");
	strcat(pfbuf,record);
	strcat(pfbuf,"}\n");	
	pf=NULL;
	if (pfcompile (pfbuf, &pf) < 0) {
		elog_log(0, "pfcompile() error \n");
		return (-1);
	}
	
	pfput_arr(pf,"assocs",arr);
	while(pf2orbpkt(pf,target_name,orb));
	pffree(pf) ;
	return(0);
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
static void 
myhand (int sig)

{
	loop = 0;

	if (pf) {
		elog_complain(0, "saving lastpktid %d in %s.\n", lastpktid, statefile);
		pfwrite (statefile, pf);
	}
	fflush (stdout);
	fflush (stderr);

	exit (0);
}

static int
mycallback (struct station_params *sp, char *netstachan, Chantracebuf *buf)

{
	/*char *chan, *ptr;*/
	char chan[20];
	int i, n;
	Dbptr dbs, dbc, dbi, dbv1, dbv2;
	char expr[512], fname[1024];
	char rsptype[32];
	int ret;
	FILE *file;
	Response *resp=NULL;
	double calib=1.0, time, signal, noise, a, signal_time, noise_time, max_a, min_a, max_time, min_time;
	int type;
	void *fil;
	Chantracebuf *obuf;
	char filspec[512];
	double gain;
	double mag;
	double snr, noise_mean, signal_raw;
	char *s ;
	char msg[128];
	char *s1,*s2;
	char filter_specification[120];
	Srcname	parts;

	if (verbose) {
		sprintf (msg, "Processing %s \n\t%s \n\t%s\n\t", netstachan, s1=strtime(buf->tstart), s2=strtime(buf->tend));
		free(s1);
		free(s2);
	}
	/*
	chan = strchr(netstachan, '_');
	chan++;
	chan = strchr(chan, '_');
	chan++;
	ptr = strchr(chan, '_');
	if (ptr) *ptr ='\0';
	*/
	
	split_srcname(netstachan, &parts);
	strcpy(chan,parts.src_chan);
	if (!blank(parts.src_loc)) {
		strcat(chan,"_");
		strcat(chan,parts.src_loc);
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
		if (verbose) elog_debug (0, "%s - cannot find instrument row\n", msg);
		else elog_complain(0, "mycallback: cannot find instrument row for %s_%s.\n", sp->sta, chan);
		dbfree (dbv1);
		dbfree (dbv2);

		return 0;
	}
	dbv2.record = 0;
	if (dbgetv (dbv2, 0, "rsptype", rsptype, 0) < 0) {
		if (verbose) elog_debug (0, "%s - dbgetv(rsptype) error\n", msg);
		else elog_complain(0, "mycallback: dbgetv(rsptype) error for %s_%s.\n", sp->sta, chan);
		dbfree (dbv1);
		dbfree (dbv2);
		return (0);
	}

	if (sp->decon_instr) {
		ret = dbextfile (dbv2, "instrument", fname);
		if (ret != 1) {
			if (verbose) elog_debug (0, "%s - dbextfile(instrument) error\n", msg);
			else elog_complain(0, "mycallback: dbextfile(instrument) error for %s_%s.\n", sp->sta, chan);
			dbfree (dbv1);
			dbfree (dbv2);
			return (0);
		}
		file = fopen(fname, "r");
		if (file == NULL) {
			if (verbose) elog_debug (1, "%s - fopen(%s) error\n", msg, fname);
			else elog_complain(1, "mycallback: fopen(%s) error for %s_%s.\n", fname, sp->sta, chan);
			dbfree (dbv1);
			dbfree (dbv2);
			return (0);
		}
		if (read_response (file, &resp) < 0) {
			if (verbose) elog_debug (0, "%s - read_response(%s) error\n", msg, fname);
			else elog_complain(0, "mycallback: read_response(%s) error for %s_%s.\n", fname, sp->sta, chan);
			dbfree (dbv1);
			dbfree (dbv2);
			return (0);
		}
		fclose (file);
	}
	dbfree (dbv1);
	dbfree (dbv2);

	if (sp->apply_clip_limits) {
		float minclip, maxclip;

		minclip = sp->minclip;
		maxclip = sp->maxclip;
		if (buf->apply_calib) {
			minclip *= buf->calib;
			maxclip *= buf->calib;
		}
		for (i=0; i<buf->nsamp; i++) {
			if (buf->data[i] >= 1.e20) continue;
			if (buf->data[i] >= maxclip) {
				if (verbose) elog_debug (0, "%s - clip limit exceeded\n", msg);
				return (0);
			}
			if (buf->data[i] <= minclip) {
				if (verbose) elog_debug (0, "%s - clip limit exceeded\n", msg);
				return (0);
			}
		}
	}

	if (sp->calib_from_db) {
		dbc = dblookup (sp->db, 0, "calibration", 0, 0);
		dbv1 = dbsubset (dbc, expr, 0);
		n = 0;
		dbquery (dbv1, dbRECORD_COUNT, &n);
		if (n != 1) {
			if (verbose) elog_debug (0, "%s - cannot find calibration row\n", msg);
			else elog_complain(0, "mycallback: cannot find calibration row for %s_%s.\n", sp->sta, chan);
			dbfree (dbv1);
			return (0);
		}
		dbv1.record = 0;
		if (dbgetv (dbv1, 0, "calib", &calib, 0) < 0) {
			if (verbose) elog_debug (0, "%s - dbgetv(calib) error\n", msg);
			else elog_complain(0, "mycallback: dbgetv(calib) error for %s_%s.\n", sp->sta, chan);
			dbfree (dbv1);
			return (0);
		}
		if (calib == 0.0) {
			if (verbose) elog_debug (0, "%s - calib == 0\n", msg);
			else elog_complain(0, "mycallback: calib == 0 for %s_%s.\n", sp->sta, chan);
			dbfree (dbv1);
			return (0);
		}
		dbfree (dbv1);
		for (i=0; i<buf->nsamp; i++) if (buf->data[i] < 1.e20) buf->data[i] *= calib;
	}

	gain = 1.0;

	if (sp->decon_instr) {
		if (verbose) elog_debug (0, "%s - cannot support instrument deconvolution\n", msg);
		else elog_complain(0, "mycallback: cannot support instrument deconvolution\n");
		return (0);
	} else if (sp->apply_wa_filter) {
		type = WFFILFILTER_NEW;
		fil = NULL;
		obuf = NULL;
		if (!strcmp(rsptype, "V")) {
			strcpy (filspec, "WAV");
		} else if (!strcmp(rsptype, "D")) {
			strcpy (filspec, "WAD");
		} else if (!strcmp(rsptype, "A")) {
			strcpy (filspec, "WAA");
		} else {
			if (verbose) elog_debug (0, "%s - cannot use rsptype '%s'\n", msg, rsptype);
			else elog_complain(0, "mycallback: Cannot use rsptype '%s'.\n", rsptype);
			return (0);
		}
		if (chantracebuf_filter (&fil, &type, buf, &obuf, filspec, 1) < 0) {
			if (verbose) elog_debug (0, "%s - chantracebuf_filter() error\n", msg);
			else elog_complain(0, "mycallback: chantracebuf_filter() error.\n");
			return (0);
		}
		free_hook (&fil);
		memcpy (buf->data, obuf->data, buf->nsamp*sizeof(float));
		chantracebuf_free (obuf);
	} else {
		if (!strcmp(rsptype, "V")) {
			type = WFFILFILTER_NEW;
			fil = NULL;
			obuf = NULL;
			/*if (chantracebuf_filter (&fil, &type, buf, &obuf, "BW 0 0 0.3 1", 1) < 0) {*/
			if (chantracebuf_filter (&fil, &type, buf, &obuf, sp->filter, 1) < 0) {
				if (verbose) elog_debug (0, "%s - chantracebuf_filter() error\n", msg);
				else elog_complain(0, "mycallback: chantracebuf_filter() error.\n");
				return (0);
			}
			free_hook (&fil);
			memcpy (buf->data, obuf->data, buf->nsamp*sizeof(float));
			chantracebuf_free (obuf);
			/*gain = 2080.0 * 1.e-6 / (0.3*2.0*M_PI);*/
			gain=1.0;
		} else if (!strcmp(rsptype, "A")) {
			type = WFFILFILTER_NEW;
			fil = NULL;
			obuf = NULL;
			strcpy(filter_specification,sp->filter);strcat(filter_specification,";");strcat(filter_specification,sp->filter);
			if (chantracebuf_filter (&fil, &type, buf, &obuf, filter_specification, 1) < 0) {
				if (verbose) elog_debug (0, "%s - chantracebuf_filter() error\n", msg);
				else elog_complain(0, "mycallback: chantracebuf_filter() error.\n");
				return (0);
			}
			free_hook (&fil);
			memcpy (buf->data, obuf->data, buf->nsamp*sizeof(float));
			chantracebuf_free (obuf);
			/* gain = 2080.0 * 1.e-6 / ((0.3*2.0*M_PI) * (0.3*2.0*M_PI)); */
			gain = 1/(((sp->f2-sp->f1) * 2.0 * M_PI) * ((sp->f2 - sp->f1) * 2.0*M_PI));
		} else if (!strcmp(rsptype, "D")) {
			/* gain = 2080.0 * 1.e-6; */
			gain = 1.0;
		} else {
			if (verbose) elog_debug (0, "%s - cannot use rsptype '%s'\n", msg, rsptype);
			else elog_complain(0, "mycallback: Cannot use rsptype '%s'.\n", rsptype);
			return (0);
		}
	}

	if (sp->twin_noise > 0.0) {	
		for (i=0,time=buf->tstart,n=0,noise_mean=0.0,noise=0.0; i<buf->nsamp; i++,time+=buf->dt) {
			if (time < sp->t0_noise) continue;
			if (time > sp->t0_noise+sp->twin_noise) 
				break;
			if (buf->data[i] >= 1.e20) continue;
			n++;
			noise_mean += buf->data[i];
			noise += buf->data[i]*buf->data[i];
		}
		if (n < 1) {
			if (verbose) elog_debug (0, "%s - no data samples to process\n", msg);
			return 0;
		}
		noise_mean = noise_mean/n;
		noise = noise/n;
		noise -= noise_mean*noise_mean;
		noise = sqrt(noise);
	} else {
		double signal_mean;

		signal_mean = 0.0;
		n = 0;
		for (i=0,time=buf->tstart,signal=0.0; i<buf->nsamp; i++,time+=buf->dt) {
			if (buf->data[i] >= 1.e20) continue;
			if (time < sp->t0_noise) continue;
			if (time < sp->t0_signal) continue;
			if (time > sp->t0_signal+sp->twin_signal) break;
			signal_mean += buf->data[i];
			n++;
		}
		if (n > 0) noise_mean = signal_mean/n;
	}
	
	signal = 0.0;
	max_a = -1.0e20;
	min_a =  1.0e20;
	for (i=0; i<buf->nsamp; i++) if (buf->data[i] < 1.e20) buf->data[i] -= noise_mean;

	for (i=0,time=buf->tstart,signal=0.0; i<buf->nsamp; i++,time+=buf->dt) {
		if (buf->data[i] >= 1.e20) continue;
		if (time < sp->t0_noise) continue;
		if (time < sp->t0_signal) continue;
		if (time > sp->t0_signal+sp->twin_signal) break;
		a = buf->data[i];
		if (a > max_a) {
			max_a = a;
			max_time = time;
		}
		if (a < min_a) {
			min_a = a;
			min_time = time;
		}
	}
	if (sp->use_p2pamp) {
		signal = ABS( (max_a - min_a)/2.);
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
	signal_raw = signal;
	signal *= gain;
	noise *= gain;

	mag = -999.0;
	snr = 0.0;
	if (signal > 0.0) {
		if (noise > 0.0) {
			snr = signal/(noise*1.414);
			if (sp->snr_thresh < 1.0 || snr > sp->snr_thresh) {
				if (sp->snr_thresh >= 1.0) signal -= noise;
				/*compmag (sp->delta*111.11, signal, &mag);*/
				compmag (sp->consts.c0, sp->consts.c1, sp->consts.c2, sp->consts.c3, sp->consts.c4, sp->consts.c5, sp->delta, sp->depth, sp->use_hypocentral_distance, signal, &mag);
			}
		} else {
			/*compmag (sp->delta*111.11, signal, &mag);*/
			compmag (sp->consts.c0, sp->consts.c1, sp->consts.c2, sp->consts.c3, sp->consts.c4, sp->consts.c5, sp->delta, sp->depth, sp->use_hypocentral_distance, signal, &mag);
		}
		if (mag > MAX_REASONABLE_MAGNITUDE) {
			elog_complain(0,"mycallback: magnitude %.2f unreasonably high -> ignored\n",mag);
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

	if (sp->channels == NULL) {
		sp->channels = (struct channel_ *) malloc (sizeof(struct channel_));
		if (sp->channels == NULL) {
			if (verbose) elog_debug (1, "%s - malloc() error.\n", msg);
			else elog_complain(1, "mycallback: malloc() error.\n");
			free(s1);
			free(s2);
			return 0;
		}
		sp->nchannels = 0;
	} else {
		sp->channels = (struct channel_ *) realloc (sp->channels, (sp->nchannels+1)*sizeof(struct channel_));
		if (sp->channels == NULL) {
			if (verbose) elog_debug (1, "%s - realloc() error.\n", msg);
			else elog_complain(1, "mycallback: realloc() error.\n");
			free(s1);
			free(s2);
			return 0;
		}
	}
	strcpy (sp->channels[sp->nchannels].chan, chan);
	sp->channels[sp->nchannels].mag = mag;
	sp->channels[sp->nchannels].signal = signal;
	sp->channels[sp->nchannels].signal_time = signal_time;
	sp->channels[sp->nchannels].noise = noise;
	sp->channels[sp->nchannels].noise_time = noise_time;
	(sp->nchannels)++;

	if (verbose) {
		elog_debug (0, "%s signal %.3f(%.3f) at %s\n\t\tsnr %.3f noise_mean %.3f(%.3f) mag %.2f\n", 
				msg, signal, signal_raw/calib, 
				s=strtime(signal_time), 
				snr, noise_mean, noise_mean/calib, mag);
		free(s) ;
	}
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
