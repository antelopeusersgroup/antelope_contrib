/* Copyright (c) 2007 Boulder Real Time Technologies, Inc. */
/* All rights reserved */
 
/* This software module is wholly owned by Boulder Real Time 
   Technologies, Inc. This software may be used freely in any 
   way as long as the copyright statement above is not removed. */

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <pthread.h>
#include <signal.h>
#include <math.h>

#include "db.h"
#include "coords.h"
#include "assoc.h"
#include "orb.h"
#include "Pkt.h"
#include "perlembed.h"
#include "brttpkt.h"
#include "brttfilter.h"
#include "brttutil.h"
#include "response.h"
#include "stock.h"

#define    INT2(x,y)        ((x)<0.0?((x)/(y)-0.5):((x)/(y)+0.5))

#define	TYPE_ORB	0
#define	TYPE_DB		1

static double tperl=0.0;

typedef struct process_params_chan_ {
	char chan[16];
	double tlast;
	int done;
} ProcessParamsChan;

typedef struct process_params_ {
	struct process_object_ *po;
	char sta[32];
	char chan_expr[64];
	Arr *channels;
	double tstart;
	double tend;
	double tupdate;
	int done;
} ProcessParams;

typedef struct process_object_ {
	char *name;
	Pf *pf;
	char *perlclass;
	void *perlobj;
	long orid;
	long myevid;
	int done;
	int nplist;
	ProcessParams *plist;
	double expire_time;
	double tperl;
} ProcessObject;

typedef struct event_params_ {
	Dbptr db;
	long orid;
	long myevid;
	double time;
	double lat;
	double lon;
	double depth;
	double tstart;
	double tend;
	char *select_expr;
	int done;
	int halt;
	Tbl *process_tbl;
	Arr *station_arr;
	PktChannel2Trace *pt;
	OrbreapThr *reap;
} EventParams;

typedef struct station_params_ {
	char sta[32];
	Stbl *chan_expr;
	double tstart;
	double tend;
	Tbl *plist;
} StationParams;

static Pf2strucDef processlist_defs[] = {
	{"chan_expr",	P_STRCP,	1,	offsetof(ProcessParams, chan_expr)},
	{"tstart",	P_DBL,		1,	offsetof(ProcessParams, tstart)},
	{"tend",	P_DBL,		1,	offsetof(ProcessParams, tend)},
	{"tupdate",	P_DBL,		1,	offsetof(ProcessParams, tupdate)},
} ;

static int processlist_ndefs = sizeof(processlist_defs)/sizeof(Pf2strucDef) ;

static char *statefile=NULL;
static int lastpktid = -1;
static Pf *pf=NULL;
static int loop=1;
static int verbose=0;
static pthread_t main_thread;
static char *tmpdbdir="/tmp";

void affirm();

void
usage (int type)

{
	if (type == TYPE_ORB) {
		fprintf (stderr, "usage: orbevproc [-start {pktid|time}] [-select select_expr]\n");
		fprintf (stderr, "                 [-number number] [-nowait] [-state statefile]\n");
		fprintf (stderr, "                 [-p parameter_file] [-tmpdbdir dir] [-dbwf dbwf]\n");
		fprintf (stderr, "                 orbwf orbev dbname\n");
	} else {
		fprintf (stderr, "usage: dbevproc [-p parameter_file] [-tmpdbdir dir] [-v]\n");
		fprintf (stderr, "                dbin dbout\n");
	}
}

char *
mystrtime (double epoch)

{
	return (epoch2str (epoch, "%H:%M:%S.%s"));
}

char *
mystrdatetime (double epoch)

{
	return (epoch2str (epoch, "%Y%j:%H:%M:%S.%s"));
}

#include "orbevproc_version.h"

static int myshutdown = 0;

void
startup_banner()

{
	int i, n;
	char *pre1 = "******";
	char *pre2 = "*     ";
	char *post1 = "******";
	char *post2 = "     *";
	char line1[256], line2[256];

	n = strlen(Orbevproc_Version);
	strcpy (line1, "*");
	for (i=1; i<n; i++) {
		strcat (line1, "*");
	}

	elog_notify (0, "\n");
	elog_notify (0, "%s%s%s\n", pre1, line1, post1);
	strcpy (line2, " ");
	for (i=1; i<n; i++) {
		strcat (line2, " ");
	}
	memcpy (line2, "orbevproc startup", strlen("orbevproc startup"));
	elog_notify (0, "%s%s%s\n", pre2, line2, post2);
	elog_notify (0, "%s%s%s\n", pre2, Orbevproc_Version, post2);
	elog_notify (0, "%s%s%s\n", pre1, line1, post1);

}

void ignoreSIGALRM();

static void
haltbysignal (int sig)

{
	static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
	Signal_info *info;
	pthread_t thread;

	pthread_mutex_lock (&mutex);
	if (myshutdown == 1) {
		pthread_mutex_unlock (&mutex);
		return;
	}

	info = signal_bysig (sig);

	if (sig == SIGHUP) {
		pthread_mutex_unlock (&mutex);
		/* elog_notify (0, "haltbysignal: received signal #%d=%s: %s...ignoring\n", sig, 
				info->name, info->description); */
		return;
	}

	thread = pthread_self();

	if (thread != main_thread) {
		pthread_mutex_unlock (&mutex);
		elog_notify (0, "haltbysignal: received signal #%d=%s: %s for reap thread... Shutting down reap thread\n", sig, 
				info->name, info->description);
		pthread_exit (0);
		return;
	}

	myshutdown = 1;
	pthread_mutex_unlock (&mutex);

	elog_notify (0, "haltbysignal: received signal #%d=%s: %s...Shutting down\n", sig, 
				info->name, info->description);

	if (pf) {
		complain (0, "saving lastpktid %d in %s.\n", lastpktid, statefile);
		pfwrite (statefile, pf);
	}

	elog_notify (0, "haltbysignal: halted by signal #%d...exiting\n", sig);

	exit (0);
}

static int
mysetup_signals ()
{
	ignoreSIGPIPE();
	ignoreSIGALRM();
	setup_signal_handlers ( haltbysignal ) ;
	return (0);
}

#include <sys/ucontext.h>

void
signals_ignore(int sig)
{
    return ;
}

void
ignoreSIGALRM()
{
    struct sigaction action;

    memset (&action, 0, sizeof(struct sigaction));
    action.sa_handler = signals_ignore ;
    sigemptyset(&action.sa_mask) ;
    action.sa_flags = SA_RESTART ;

    insist (sigaction(SIGALRM, &action, (struct sigaction *)0) == 0)  ;
}

int
myusleep (double sec)

{
	struct timespec ts;

	ts.tv_sec = sec;
	ts.tv_nsec = (sec - (double) ts.tv_sec) * 1.e9 + 0.5;
	nanosleep (&ts, NULL);

	return (0);
}

int
db2dbtable (Pf *pf, char *pfkey, Dbptr dbin, char *tablename, Dbptr db, int require)

{
	char *ptr=NULL;

	if (pf) {
		char *rec;

		if (parse_param (pf, pfkey, P_STR, require, &ptr) < 0) {
			register_error (0, "db2dbtable: parse_param(%s) error.\n", pfkey);
			return (-1);
		}

		if (ptr == NULL) return (0);

		db = dblookup (db, 0, tablename, 0, 0);
		if (db.table == dbINVALID) {
			register_error (0, "db2dbtable: dblookup(%s) error.\n", tablename);
			return (-1);
		}

		for (rec=strtok(ptr, "\n"); rec != NULL; rec=strtok(NULL, "\n")) {
			if (dbadd (db, rec) < 0) {
				free (ptr);
				register_error (0, "db2dbtable: dbadd(%s) error.\n", rec);
				return (-1);
			}
		}

		free (ptr);
	} else {
		char rec[1024];
		int istart, iend;

		if (dbin.record == dbINVALID) return (0);

		dbin = dblookup (dbin, 0, tablename, 0, 0);
		if (dbin.table == dbINVALID) {
			register_error (0, "db2dbtable: dblookup(%s) error.\n", tablename);
			return (-1);
		}

		db = dblookup (db, 0, tablename, 0, 0);
		if (db.table == dbINVALID) {
			register_error (0, "db2dbtable: dblookup(%s) error.\n", tablename);
			return (-1);
		}

		if (dbin.record != dbALL && dbin.field == dbALL) {
			istart = dbin.record;
			iend = istart+1;
		} else {
			dbget_range (dbin, &istart, &iend);
		}

		for (dbin.record=istart; dbin.record<iend; dbin.record++) {
			if (dbget (dbin, rec) < 0) {
				register_error (0, "db2dbtable: dbget() error.\n");
				return (-1);
			}
			if (dbadd (db, rec) < 0) {
				register_error (0, "db2dbtable: dbadd(%s) error.\n", rec);
				return (-1);
			}
		}
	}

	return (0);
}

int
dbtable2pf (Dbptr db, Pf **pf)

{
	char *tablename, *pfbuf;
	void *vbuf=NULL;
	char key[32];
	char rec[1024];
	int rec0, rec1;

	dbquery (db, dbTABLE_NAME, &tablename);

	dbget_range (db, &rec0, &rec1);

	if (rec1-rec0 > 1) {
		sprintf (key, "%ss", tablename);
	} else {
		sprintf (key, "%s", tablename);
	}

	pushstr (&vbuf, key);
	pushstr (&vbuf, " &Literal{\n");
	for (db.record=rec0; db.record<rec1; db.record++) {
		db.field = dbALL;
		if (dbget (db, rec) < 0) {
			register_error (0, "dbtable2pf: dbget(%s) error.\n", tablename);
			free (popstr(&vbuf, 1));
			return (-1);
		}
		pushstr (&vbuf, rec);
	}
	pushstr (&vbuf, "}\n");

	pfbuf = popstr (&vbuf, 1);

	if (pfcompile (pfbuf, pf) < 0) {
		register_error (0, "dbtable2pf: pfcompile(%s) error.\n", tablename);
		free (pfbuf);
		return (-1);
	}
	free (pfbuf);

	return (0);
}

int
mktmpdb (Pf *pf, Dbptr dbmaster, Dbptr *db, long *myevid)

{
	char tmpdbname[256];
	static int instance=0;
	Dbptr dbo, dba, dbs, dbos, dbon, dbj;
	double time, olat, olon;
	long orid;
	long i, n ; 
	int ret;
	char expr[256];
	Arr *arr;
	Tbl *stas;
	static Hook *hookor=NULL;
	static Hook *hookore=NULL;
	static Hook *hookemd=NULL;
	static Hook *hookpre=NULL;
	static Hook *hookass=NULL;
	static Hook *hookarr=NULL;

	/* setup temporary database */

	*myevid = instance;
	sprintf (tmpdbname, "%s/orbevproc%d_%d", tmpdbdir, getpid(), instance);
	instance++;

	if (dbopen (tmpdbname, "r+", db) == dbINVALID) {
		register_error (0, "mktmpdb: dbopen(%s) error in temp db.\n", tmpdbname);
		return (-1);
	}
	dbdestroy (*db);
	dbclose (*db);
	clear_register (0);
	if (dbopen (tmpdbname, "r+", db) == dbINVALID) {
		register_error (0, "mktmpdb: dbopen(%s) error in temp db.\n", tmpdbname);
		return (-1);
	}

	/* convert origin table */

	if (pf == NULL) {
		Tbl *tbl = NULL;

		dbo = dblookup (dbmaster, 0, "origin", 0, "dbALL");
		ret = dbmatches (dbmaster, dbo, 0, 0, &hookor, &tbl);
		if (ret < 0) {
			register_error (0, "mktmpdb: dbmatches(origin) error.\n");
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
		dbo.record = dbINVALID;
		if (ret > 0) {
			dbo.record = (long) gettbl (tbl, 0);
		}
		freetbl (tbl, 0);
		if (db2dbtable (pf, "origin", dbo, "origin", *db, 0) < 0) {
			register_error (0, "mktmpdb: db2dbtable(%s.origin) error in temp db.\n", tmpdbname);
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
	} else {
		if (db2dbtable (pf, "origin", dbmaster, "origin", *db, 1) < 0) {
			register_error (0, "mktmpdb: db2dbtable(%s.origin) error in temp db.\n", tmpdbname);
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
	}

	/* convert origerr table */

	if (pf == NULL) {
		Tbl *tbl = NULL;

		dbo = dblookup (dbmaster, 0, "origerr", 0, "dbALL");
		ret = dbmatches (dbmaster, dbo, 0, 0, &hookore, &tbl);
		if (ret < 0) {
			register_error (0, "mktmpdb: dbmatches(origerr) error.\n");
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
		dbo.record = dbINVALID;
		if (ret > 0) {
			dbo.record = (long) gettbl (tbl, 0);
		}
		freetbl (tbl, 0);
		if (db2dbtable (pf, "origerr", dbo, "origerr", *db, 0) < 0) {
			register_error (0, "mktmpdb: db2dbtable(%s.origerr) error in temp db.\n", tmpdbname);
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
	} else {
		if (db2dbtable (pf, "origerr", dbmaster, "origerr", *db, 0) < 0) {
			register_error (0, "mktmpdb: db2dbtable(%s.origerr) error in temp db.\n", tmpdbname);
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
	}

	/* convert emodel table */

	if (pf == NULL) {
		Tbl *tbl = NULL;

		dbo = dblookup (dbmaster, 0, "emodel", 0, "dbALL");
		ret = dbmatches (dbmaster, dbo, 0, 0, &hookemd, &tbl);
		if (ret < 0) {
			register_error (0, "mktmpdb: dbmatches(emodel) error.\n");
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
		dbo.record = dbINVALID;
		if (ret > 0) {
			dbo.record = (long) gettbl (tbl, 0);
		}
		freetbl (tbl, 0);
		if (db2dbtable (pf, "emodel", dbo, "emodel", *db, 0) < 0) {
			register_error (0, "mktmpdb: db2dbtable(%s.emodel) error in temp db.\n", tmpdbname);
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
	} else {
		if (db2dbtable (pf, "emodel", dbmaster, "emodel", *db, 0) < 0) {
			register_error (0, "mktmpdb: db2dbtable(%s.emodel) error in temp db.\n", tmpdbname);
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
	}

	/* convert predarr table */

	if (pf == NULL) {
		Tbl *tbl = NULL;

		dbo = dblookup (dbmaster, 0, "predarr", 0, "dbALL");
		ret = dbmatches (dbmaster, dbo, 0, 0, &hookpre, &tbl);
		if (ret < 0) {
			register_error (0, "mktmpdb: dbmatches(predarr) error.\n");
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
		dbo.record = dbINVALID;
		for (i=0; i<maxtbl(tbl); i++) {
			dbo.record = (long) gettbl (tbl, i);
			if (db2dbtable (pf, "predarrs", dbo, "predarr", *db, 0) < 0) {
				register_error (0, "mktmpdb: db2dbtable(%s.predarr) error in temp db.\n", tmpdbname);
				dbdestroy (*db);
				dbclose (*db);
				return (-1);
			}
		}
		freetbl (tbl, 0);
	} else {
		if (db2dbtable (pf, "predarrs", dbmaster, "predarr", *db, 0) < 0) {
			register_error (0, "mktmpdb: db2dbtable(%s.predarr) error in temp db.\n", tmpdbname);
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
	}

	/* convert assoc and arrival tables */

	if (pf == NULL) {
		Tbl *tbl = NULL;

		dbo = dblookup (dbmaster, 0, "assoc", 0, "dbALL");
		dba = dblookup (dbmaster, 0, "arrival", 0, "dbALL");
		ret = dbmatches (dbmaster, dbo, 0, 0, &hookass, &tbl);
		if (ret < 0) {
			register_error (0, "mktmpdb: dbmatches(assoc) error.\n");
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
		dbo.record = dbINVALID;
		for (i=0; i<maxtbl(tbl); i++) {
			Tbl *tbl2=NULL;

			dbo.record = (long) gettbl (tbl, i);
			if (db2dbtable (pf, "assocs", dbo, "assoc", *db, 0) < 0) {
				register_error (0, "mktmpdb: db2dbtable(%s.assoc) error in temp db.\n", tmpdbname);
				dbdestroy (*db);
				dbclose (*db);
				return (-1);
			}
			ret = dbmatches (dbo, dba, 0, 0, &hookarr, &tbl2);
			if (ret < 0) {
				register_error (0, "mktmpdb: dbmatches(arrival) error.\n");
				dbdestroy (*db);
				dbclose (*db);
				return (-1);
			}
			if (ret > 0) {
				dba.record = (long) gettbl (tbl2, 0);
				if (db2dbtable (pf, "arrivals", dba, "arrival", *db, 0) < 0) {
					register_error (0, "mktmpdb: db2dbtable(%s.arrival) error in temp db.\n", tmpdbname);
					dbdestroy (*db);
					dbclose (*db);
					return (-1);
				}
			}
			freetbl (tbl2, 0);
		}
		freetbl (tbl, 0);
	} else {
		if (db2dbtable (pf, "assocs", dbmaster, "assoc", *db, 0) < 0) {
			register_error (0, "mktmpdb: db2dbtable(%s.assoc) error in temp db.\n", tmpdbname);
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
		if (db2dbtable (pf, "arrivals", dbmaster, "arrival", *db, 0) < 0) {
			register_error (0, "mktmpdb: db2dbtable(%s.arrival) error in temp db.\n", tmpdbname);
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
	}

	/* merge in dbmaster stuff */

	dbo = dblookup (*db, 0, "origin", 0, 0);
	dbos = dblookup (*db, 0, "site", 0, 0);
	dbon = dblookup (*db, 0, "snetsta", 0, 0);
	dbo.record = 0;
	if (dbgetv (dbo, "origin", "time", &time, "orid", &orid, "lat", &olat, "lon", &olon, NULL ) < 0) {
		register_error (0, "mktmpdb: dbgetv(origin:time) error.\n");
		dbdestroy (*db);
		dbclose (*db);
		return (-1);
	}
	if (pf == NULL) *myevid = orid;

	/* site and snetsta tables */

	sprintf (expr, "ondate <= %d && (offdate <= 0 || offdate >= %d)", yearday(time), yearday(time));
	dbs = dblookup (dbmaster, 0, "site", 0, 0);
	dbj = dbsubset (dbs, expr, 0);
	dbquery (dbj, dbRECORD_COUNT, &n);
	for (dbj.record = 0; dbj.record<n; dbj.record++) {
		char sta[16], staname[64], statype[8], refsta[16];
		double lat, lon, elev, dnorth, deast, lddate;
		long ondate, offdate;
		Dbptr dbn;
		char rec[256];

		if (dbgetv (dbj, 0, "sta", sta, "ondate", &ondate, "offdate", &offdate,
				"lat", &lat, "lon", &lon, "elev", &elev,
				"staname", staname, "statype", statype,
				"refsta", refsta, "dnorth", &dnorth, "deast", &deast, 
				"lddate", &lddate, NULL ) < 0) {
			register_error (0, "mktmpdb: dbgetv(site) error.\n");
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
		dbos.record = dbSCRATCH;
		if (dbputv (dbos, 0, "sta", sta, "ondate", ondate, "offdate", offdate,
				"lat", lat, "lon", lon, "elev", elev,
				"staname", staname, "statype", statype,
				"refsta", refsta, "dnorth", dnorth, "deast", deast, 
				"lddate", lddate, NULL ) < 0) {
			register_error (0, "mktmpdb: dbputv(site) error.\n");
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
		dbadd (dbos, 0);
		dbn = dblookup (dbmaster, 0, "snetsta", "sta", sta);
		if (dbn.record >= 0) {
			dbn.field = dbALL;
			dbget (dbn, rec);
			dbadd (dbon, rec);
		}
	}

	dbfree (dbj);

	/* look to see if the associations are in an assocs Arr instead of a
	   real database table (ala /pf/orbmag output from orb2dbt) */

	if (pf == NULL) {
		clear_register (0);
		return (0);
	}

	if (parse_param (pf, "assocs", P_ARR, 1, &arr) < 0) {
		clear_register (0);
		return (0);
	}

	stas = keysarr (arr);
	olat *= M_PI/180.0;
	olon *= M_PI/180.0;
	for (i=0; i<maxtbl(stas); i++) {
		char *sta;
		char *line;
		char timedef[32];
		char phase[32];
		double slat, slon, delta, esaz, seaz;
		long arid;

		sta = (char *) gettbl (stas, i);
		line = (char *) getarr (arr, sta);
		if (sscanf (line, "%s %s", timedef, phase) != 2) continue;
		dbos = dblookup (dbos, 0, "site", "sta", sta);
		if (dbos.record < 0) {
			complain (0, "mktmpdb: Cannot find station '%s' in site table - skipping.\n", sta);
			continue;
		}
		if (dbgetv (dbos, 0, "lat", &slat, "lon", &slon, NULL ) < 0) {
			register_error (0, "mktmpdb: dbgetv(site) error.\n");
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
		slat *= M_PI/180.0;
		slon *= M_PI/180.0;
		dist (olat, olon, slat, slon, &delta, &esaz);
		dist (slat, slon, olat, olon, &delta, &seaz);
		delta *= 180.0/M_PI;
		esaz *= 180.0/M_PI;
		seaz *= 180.0/M_PI;
		arid = dbnextid (*db, "arid");
		if (dbaddv (*db, "assoc", "arid", arid, "orid", orid,
				"sta", sta, "phase", phase, "delta", delta,
				"seaz", seaz, "esaz", esaz, "timedef", timedef, NULL ) < 0) {
			register_error (0, "mktmpdb: dbaddv(assoc) error.\n");
			dbdestroy (*db);
			dbclose (*db);
			return (-1);
		}
	}
	freetbl (stas, 0);

	return (0);
}

int
print_logs (Pf *pf, char *class, int event, void *perlobj)

{
	Pf *tblpf;
	Pf *tblpf2;
	long i, m, n;
	double t0;

	if (parse_param (pf, "logs", P_TBLPF, 1, &tblpf) < 0) {
		clear_register (0);
		goto OUTPR ;
	}

	n = 100;
	for (i=0; i<n; i++) {
		char *entry;

		switch (pfget (tblpf, (char *) i, (void *) &tblpf2)) {
		case PFSTRING:
			elog_notify (0, "%d: %s: %s.\n", event, class, tblpf2);
			break;
		case PFTBL:
			m = pfmaxtbl(tblpf2);
			if (m == 2) {
				int v;

				switch (pfget (tblpf2, (char *) 0, (void *) &entry)) {
				case PFSTRING:
					v = atoi(entry);
					if (verbose >= v) {
						switch (pfget (tblpf2, (char *) 1, (void *) &entry)) {
						case PFSTRING:
							elog_notify (0, "%d: %s: %s.\n", event, class, entry);
							break;
						default:
							n = i;
							break;
						}
					}
					break;
				default:
					n = i;
					break;
				}
			} else {
				n = i;
			}
			break;
		default:
			n = i;
			break;
		}
	}

OUTPR:	t0 = now();
	if (perlembed_method (perlobj, "evproc::clearlogs",
				NULL,
				NULL) < 0) {
		tperl += now() - t0;
		register_error (0, "print_logs: perlembed_method(evproc::clearlogs) error.\n");
		return (-1);
	}
	tperl += now() - t0;

	return (0);
}

int
pf2orb (Pf *pf, int orb, char *srcname)

{
	static Packet *pkt=NULL;
	static char *packet=NULL;
	static int nbytes=0;
	static int bufsize=0;
	char srcbuf[128];
	double time;

	if (orb < 0) return (0);

	if (pkt == NULL) {
		pkt = newPkt();
		if (pkt == NULL) {
			register_error (0, "pf2orb: newPkt() error.\n");
			return (-1);
		}
		pkt->pkttype = suffix2pkttype("pf");
	}

	pkt->pf = pf;
	time = now();
	if (stuffPkt (pkt, srcbuf, &time, &packet, &nbytes, &bufsize) < 0) {
		register_error (0, "pf2orb: stuffPkt() error.\n");
		return (-1);
	}

	if (orbput (orb, srcname, time, packet, nbytes) < 0) {
		register_error (0, "pf2orb: orbput() error.\n");
		return (-1);
	}

	return (0);
}

int
process_output (ProcessObject *po, Pf *pf, int type, Dbptr dbout, int orb)

{
	Pf *pfdb, *pftb;
	Pf *pfout=NULL;
	Pf *pfassoc;
	int assoc, i;

	/* output database tables */

	pfdb = NULL;
	if (parse_param (pf, "db", P_ARRPF, 0, &pfdb) < 0) {
		register_error (0, "process_output: parse_param(db) error.\n");
		return (-1);
	}

	if (pfdb == NULL) return (0);

	if (parse_param (pfdb, "tables", P_TBLPF, 1, &pftb) < 0) {
		register_error (0, "process_output: parse_param(db->tables) error.\n");
		return (-1);
	}

	pfassoc = NULL;
	if (parse_param (pfdb, "assoc_params", P_ARRPF, 0, &pfassoc) < 0) {
		register_error (0, "process_output: parse_param(db->assoc_params) error.\n");
		return (-1);
	}

	assoc = 0;
	if (pfassoc) {
		Tbl *keys;

		keys = pfkeys(pfassoc);
		for (i=0; i<maxtbl(keys); i++) {
			char *k;
			char *str;
			char line[512];

			k = (char *) gettbl (keys, i);
			if (!strcmp(k, "smart_assoc")) {
				if (parse_param (pfassoc, k, P_BOOL, 1, &assoc) < 0) {
					register_error (0, "process_output: parse_param(db->assoc_params->%s) error.\n", k);
					freetbl (keys, 0);
					return (-1);
				}
				continue;
			}
			if (parse_param (pfassoc, k, P_STR, 1, &str) < 0) {
				register_error (0, "process_output: parse_param(db->assoc_params->%s) error.\n", k);
				freetbl (keys, 0);
				return (-1);
			}
			sprintf (line, "%s %s\n", k, str);
			free (str);
			if (pfcompile (line, &pfout) < 0) {
				register_error (0, "process_output: pfcompile(%s) error.\n", line);
				freetbl (keys, 0);
				return (-1);
			}
		}
		freetbl (keys, 0);
	}

	for (i=0; i<pfmaxtbl(pftb); i++) {
		char key[512];
		Tbl *tbl;
		Dbptr db;

		sprintf (key, "%d", i);
		if (parse_param (pftb, (char *) i, P_TBL, 1, &tbl) < 0) {
			register_error (0, "process_output: parse_param(db->%s) error.\n", key);
			return (-1);
		}
		if (maxtbl(tbl) != 4) {
			freetbl (tbl, 0);
			register_error (0, "process_output: db->%s does not look like database pointer.\n", key);
			return (-1);
		}
		db.database 	= atoi((char *) gettbl (tbl, 0));
		db.table 	= atoi((char *) gettbl (tbl, 1));
		db.field 	= atoi((char *) gettbl (tbl, 2));
		db.record 	= atoi((char *) gettbl (tbl, 3));
		freetbl (tbl, 0);

		if (assoc) {
			if (dbtable2pf (db, &pfout) < 0) {
				register_error (0, "process_output: dbtable2pf() error.\n", key);
				return (-1);
			}
		} else {
		}
	}

	if (assoc && pfout) {
		if (type == TYPE_ORB) {
			if (verbose) {
				elog_notify (0, "%ld: %s: Outputting /pf/orb2dbt ORB packet\n", po->myevid, po->perlclass);
			}

			if (pf2orb (pfout, orb, "/pf/orb2dbt") < 0) {
				pffree (pfout);
				register_error (0, "process_output: pf2orb() error.\n");
				return (-1);
			}
		}

		if (type == TYPE_DB) {
			if (verbose) {
				elog_notify (0, "%ld: %s: Associating event with output database\n", po->myevid, po->perlclass);
			}
			assoc_process_pf (pfout, dbout, NULL, NULL, NULL, -1, verbose);
			clear_register (1);
		}

		pffree (pfout);
	}

	return (0);
}

int
parse_perlobj_output (Pf *pf, char *disposition, Pf **pfout)

{
	if (parse_param (pf, "disposition", P_STRCP, 1, disposition) < 0) {
		register_error (0, "parse_perlobj_output: Cannot find disposition.\n");
		return (-1);
	}
	if (parse_param (pf, "output", P_ARRPF, 1, pfout) < 0) {
		register_error (0, "parse_perlobj_output: Cannot find output.\n");
		return (-1);
	}

	return (0);
}

int
freepplist (ProcessParams *pp, int npp)

{
	int i;

	if (pp == NULL) return (0);

	for (i=0; i<npp; i++) {
		if (pp[i].channels) {
			freearr (pp[i].channels, free);
		}
	}

	free (pp);

	return (0);
}

int
make_perlobj (Dbptr dbpkt, Dbptr dbmaster, long orid, long myevid, ProcessObject *poin, ProcessObject **poout)

{
	void *sv;
	char sub[256];
	Pf *pf, *pfout, *pfproc;
	char disposition[256];
	int i, n;
	Tbl *stas;
	ProcessParams *proc_list;
	double expire_time;
	double t0;
	double tperl_obj=0.0;

	/* Make a new object instance for each of the perl processing classes */

	sv = NULL;
	sprintf (sub, "%s::new", poin->perlclass);
	t0 = now();
	if (perlembed_call (sub,
			PERLEMBED_TYPE_STR, poin->perlclass,
			PERLEMBED_TYPE_STR, "db", PERLEMBED_TYPE_DB, &dbpkt,
			PERLEMBED_TYPE_STR, "dbm", PERLEMBED_TYPE_DB, &dbmaster,
			PERLEMBED_TYPE_STR, "params", PERLEMBED_TYPE_PF, poin->pf,
			PERLEMBED_TYPE_STR, "event_id", PERLEMBED_TYPE_INT, myevid,
				NULL,
			PERLEMBED_TYPE_SV, &sv,
			PERLEMBED_TYPE_PF, &pf,
				NULL) < 0) {
		tperl += now() - t0;
		register_error (0, "make_perlobj: perlembed_call(%s) error.\n", sub);
		return (-1);
	}
	tperl_obj = now() - t0;
	if (parse_perlobj_output (pf, disposition, &pfout) < 0) {
		pffree (pf);
		perlembed_destroy ( sv ) ;
		register_error (0, "make_perlobj: parse_perlobj_output() error in %s return.\n", sub);
		return (0);
	}
	if (!strcmp(disposition, "skip")) {
		print_logs (pfout, poin->perlclass, myevid, sv);
		pffree (pf);
		perlembed_destroy ( sv ) ;
		register_error (0, "make_perlobj: %s: skipping processing.\n", poin->perlclass);
		return (0);
	}
	if (strcmp(disposition, "ok")) {
		print_logs (pfout, poin->perlclass, myevid, sv);
		pffree (pf);
		perlembed_destroy ( sv ) ;
		register_error (0, "make_perlobj: %s: error processing.\n", poin->perlclass);
		return (0);
	}
	pffree (pf);
	pf = NULL;

	/* get the list of waveform times for processing */

	sprintf (sub, "%s::getwftimes", poin->perlclass);
	t0 = now();
	if (perlembed_method (sv, sub,
				NULL,
			PERLEMBED_TYPE_PF, &pf,
				NULL) < 0) {
		perlembed_destroy ( sv ) ;
		tperl_obj += now() - t0;
		tperl += tperl_obj;
		register_error (0, "make_perlobj: perlembed_method(%s) error.\n", sub);
		return (-1);
	}
	tperl_obj += now() - t0;
	tperl += tperl_obj;
	if (parse_perlobj_output (pf, disposition, &pfout) < 0) {
		pffree (pf);
		perlembed_destroy ( sv ) ;
		register_error (0, "make_perlobj: parse_perlobj_output() error in %s return.\n", sub);
		return (0);
	}
	if (!strcmp(disposition, "skip")) {
		print_logs (pfout, poin->perlclass, myevid, sv);
		pffree (pf);
		perlembed_destroy ( sv ) ;
		register_error (0, "make_perlobj: %s: skipping processing.\n", poin->perlclass);
		return (0);
	}
	if (strcmp(disposition, "ok")) {
		print_logs (pfout, poin->perlclass, myevid, sv);
		pffree (pf);
		perlembed_destroy ( sv ) ;
		register_error (0, "make_perlobj: %s: error processing.\n", poin->perlclass);
		return (0);
	}

	/* str = pf2string (pf);
	printf ("%s\n", str);
	free (str);  */

	print_logs (pfout, poin->perlclass, myevid, sv);

	/* parse out the return processing parameters */

	if (parse_param (pf, "stations", P_ARRPF, 1, &pfproc) < 0) {
		pffree (pf);
		perlembed_destroy ( sv ) ;
		register_error (0, "make_perlobj: Cannot find processing parameters in %s return.\n", sub);
		return (0);
	}
	stas = pfkeys (pfproc) ;
	n = maxtbl(stas) ;
	proc_list = (ProcessParams *) malloc (n*sizeof(ProcessParams));
	if (proc_list == NULL) {
		pffree (pf);
		freetbl (stas, 0);
		perlembed_destroy ( sv ) ;
		register_error (1, "make_perlobj: malloc(proc_list,%ld) error.\n", n*sizeof(ProcessParams));
		return (-1);
	}
	memset (proc_list, 0, n*sizeof(ProcessParams));
	for (i=0; i<n; i++) {
		char *sta;
		Tbl *chans;
		Pf *pflist, *pfchans;
		int nchans, j;

		sta = (char *) gettbl (stas, i);
		if (parse_param (pfproc, sta, P_ARRPF, 1, &pflist) < 0) {
			freepplist (proc_list, n);
			pffree (pf);
			freetbl (stas, 0);
			perlembed_destroy ( sv ) ;
			register_error (0, "make_perlobj: Cannot find processing parameters for %s in %s return.\n", sta, sub);
			return (0);
		}
		if (parse_param (pflist, "channels", P_ARRPF, 1, &pfchans) < 0) {
			freepplist (proc_list, n);
			pffree (pf);
			freetbl (stas, 0);
			perlembed_destroy ( sv ) ;
			register_error (0, "make_perlobj: Cannot find channels parameters for %s in %s return.\n", sta, sub);
			return (0);
		}
		chans = pfkeys (pfchans);
		if (chans == NULL) {
			freepplist (proc_list, n);
			pffree (pf);
			freetbl (stas, 0);
			perlembed_destroy ( sv ) ;
			register_error (0, "make_perlobj: Cannot find any channels for %s in %s return.\n", sta, sub);
			return (0);
		}
		nchans = maxtbl(chans);
		if (nchans < 1) {
			freepplist (proc_list, n);
			pffree (pf);
			freetbl (stas, 0);
			freetbl (chans, 0);
			perlembed_destroy ( sv ) ;
			register_error (0, "make_perlobj: Cannot find any channels for %s in %s return.\n", sta, sub);
			return (0);
		}
		if (pf2struc (pflist, processlist_ndefs, processlist_defs, &(proc_list[i])) < 0) {
			freepplist (proc_list, n);
			pffree (pf);
			freetbl (stas, 0);
			freetbl (chans, 0);
			perlembed_destroy ( sv ) ;
			register_error (0, "make_perlobj: Cannot find processing parameters for %s in %s return.\n", sta, sub);
			return (0);
		}
		strcpy (proc_list[i].sta, sta);
		proc_list[i].channels = newarr (0);
		if (proc_list[i].channels == NULL) {
			freepplist (proc_list, n);
			pffree (pf);
			freetbl (stas, 0);
			freetbl (chans, 0);
			perlembed_destroy ( sv ) ;
			register_error (0, "make_perlobj: newarr(channels) error for %s in %s return.\n", sta, sub);
			return (0);
		}
		for (j=0; j<nchans; j++) {
			char *chan;
			ProcessParamsChan *ppc;

			chan = (char *) gettbl (chans, j);
			ppc = (ProcessParamsChan *) malloc (sizeof(ProcessParamsChan));
			if (ppc == NULL) {
				freepplist (proc_list, n);
				pffree (pf);
				freetbl (stas, 0);
				freetbl (chans, 0);
				perlembed_destroy ( sv ) ;
				register_error (1, "make_perlobj: malloc(chan=%s,%ld) error.\n", chan, sizeof(ProcessParamsChan));
				return (-1);
			}
			memset (ppc, 0, sizeof(ProcessParamsChan));
			strcpy (ppc->chan, chan);
			setarr (proc_list[i].channels, chan, ppc);
		}
		freetbl (chans, 0);
	}
	freetbl (stas, 0);
	expire_time = 0.0;
	if (parse_param (pf, "expire_time", P_DBL, 0, &expire_time) < 0) {
		complain (0, "make_perlobj: parse_param error for expire_time.\n");
	}

	pffree (pf);

	/* create and populate the process object instance structure */

	*poout = (ProcessObject *) malloc (sizeof(ProcessObject));
	if (*poout == NULL) {
		free (proc_list);
		perlembed_destroy ( sv ) ;
		register_error (1, "make_perlobj: malloc(%s,%ld) error.\n", poin->perlclass, sizeof(ProcessObject));
		return (-1);
	}
	memset (*poout, 0, sizeof(ProcessObject));
	(*poout)->name = strdup(poin->name);
	(*poout)->perlclass = strdup(poin->perlclass);
	(*poout)->pf = poin->pf;
	(*poout)->perlobj = sv;
	(*poout)->nplist = n;
	(*poout)->plist = proc_list;
	(*poout)->orid = orid;
	(*poout)->myevid = myevid;
	(*poout)->expire_time = expire_time;
	(*poout)->tperl = tperl_obj;
	for (i=0; i<n; i++) {
		(*poout)->plist[i].po = *poout ;
	}

	return (1);
}

int
process_channel_callback (ProcessParams *pp, Dbptr dbtrace, int flush)

{
	char sub[256];
	char disposition[256];
	Pf *pf, *pfout;
	int ret = 0;
	double t0, tp;

	sprintf (sub, "%s::process_channel", pp->po->perlclass);
	t0 = now();
	if (perlembed_method (pp->po->perlobj, sub,
			PERLEMBED_TYPE_DB, &dbtrace,
			PERLEMBED_TYPE_INT, flush,
				NULL,
			PERLEMBED_TYPE_PF, &pf,
				NULL) < 0) {
		tp = now() - t0;
		tperl += tp;
		pp->po->tperl += tp;
		register_error (0, "process_channel_callback: perlembed_method(%s) error.\n", sub);
		return (-1);
	}
	tp = now() - t0;
	tperl += tp;
	pp->po->tperl += tp;

	/* check the return disposition */

	if (parse_perlobj_output (pf, disposition, &pfout) < 0) {
		pffree (pf);
		register_error (0, "process_channel_callback: parse_perlobj_output() error in %s return.\n", sub);
		return (-1);
	}
	if (!strcmp(disposition, "channeldone")) {
		ret = 1;
	}
	if (!strcmp(disposition, "stationdone")) {
		ret = 2;
	}
	if (!strcmp(disposition, "processdone")) {
		ret = 3;
	}
	if (!strcmp(disposition, "notneeded")) {
		ret = 4;
	}
	print_logs (pfout, pp->po->perlclass, pp->po->myevid, pp->po->perlobj);
	pffree (pf);

	return (ret);
}

int
process_station_callback (ProcessParams *pp, int flush)

{
	char sub[256];
	char disposition[256];
	Pf *pf, *pfout;
	int ret = 0;
	double t0, tp;

	sprintf (sub, "%s::process_station", pp->po->perlclass);
	t0 = now();
	if (perlembed_method (pp->po->perlobj, sub,
			PERLEMBED_TYPE_STR, pp->sta,
			PERLEMBED_TYPE_INT, flush,
				NULL,
			PERLEMBED_TYPE_PF, &pf,
				NULL) < 0) {
		tp = now() - t0;
		tperl += tp;
		pp->po->tperl += tp;
		register_error (0, "process_station_callback: perlembed_method(%s) error.\n", sub);
		return (-1);
	}
	tp = now() - t0;
	tperl += tp;
	pp->po->tperl += tp;

	/* check the return disposition */

	if (parse_perlobj_output (pf, disposition, &pfout) < 0) {
		pffree (pf);
		register_error (0, "process_station_callback: parse_perlobj_output() error in %s return.\n", sub);
		return (-1);
	}
	print_logs (pfout, pp->po->perlclass, pp->po->myevid, pp->po->perlobj);
	pffree (pf);

	return (ret);
}

int
process_network_callback (ProcessObject *po, int flush, int type, Dbptr dbout, int orb)

{
	char sub[256];
	char disposition[256];
	Pf *pf, *pfout;
	int ret = 0;
	double t0, tp;

	sprintf (sub, "%s::process_network", po->perlclass);
	t0 = now();
	if (perlembed_method (po->perlobj, sub,
			PERLEMBED_TYPE_INT, flush,
				NULL,
			PERLEMBED_TYPE_PF, &pf,
				NULL) < 0) {
		tp = now() - t0;
		tperl += tp;
		po->tperl += tp;
		register_error (0, "process_network_callback: perlembed_method(%s) error.\n", sub);
		return (-1);
	}
	tp = now() - t0;
	tperl += tp;
	po->tperl += tp;

	/* check the return disposition */

	if (parse_perlobj_output (pf, disposition, &pfout) < 0) {
		pffree (pf);
		register_error (0, "process_network_callback: parse_perlobj_output() error in %s return.\n", sub);
		return (-1);
	}
	print_logs (pfout, po->perlclass, po->myevid, po->perlobj);

	/* if (verbose) {
		char *s;
		s = pf2string (pfout);
		printf ("%s", s);
		free (s);
	} */

	/* output the results */

	if (process_output (po, pfout, type, dbout, orb) < 0) {
		pffree (pf);
		register_error (0, "process_network_callback: process_output() error.\n", sub);
		return (-1);
	}

	pffree (pf);

	return (ret);
}

int
event_destroy (EventParams *ep)

{
	if (ep->reap) orbreapthr_destroy (ep->reap);
	if (dbdestroy (ep->db) == dbINVALID) {
		complain (0, "%s: event_destroy: dbdestroy() error.\n",
						ep->myevid);
	}
	if (ep->process_tbl) {
		int i;

		for (i=0; i<maxtbl(ep->process_tbl); i++) {
			ProcessObject *po;

			po = (ProcessObject *) gettbl (ep->process_tbl, i);
			if (verbose) {
				elog_notify (0, "%ld: %s: %.3f seconds spent in perl processing\n", 
						po->myevid, po->name, po->tperl);
			}
			perlembed_destroy ( po->perlobj ) ;
			if (po->plist) freepplist (po->plist, po->nplist);
			if (po->name) free (po->name);
			if (po->perlclass) free (po->perlclass);
		}
		freetbl (ep->process_tbl, 0);
	}
	if (ep->station_arr) {
		Tbl *keys;
		int i;

		keys = keysarr (ep->station_arr);
		for (i=0; i<maxtbl(keys); i++) {
			StationParams *sp;

			sp = (StationParams *) getarr (ep->station_arr, (char *) gettbl (keys, i));
			freetbl (sp->plist, 0);
			if (sp->chan_expr) freestbl (sp->chan_expr, 0);
			free (sp);
		}
		freetbl (keys, 0);
		freearr (ep->station_arr, 0);
	} 
	if (ep->pt) {
		if (ep->pt->dbt.database != dbINVALID) trdestroy (&(ep->pt->dbt));
		pktchannel2trace_free (ep->pt); 
	} 
	if (ep->select_expr) free (ep->select_expr); 
	free (ep);

	return (0);
}

int
setup_station_params (EventParams *ep)

{
	int i;
	double tstart, tend;
	void *vst = NULL;
	char *select_expr;
	int first = 1;

	tstart = 1.e20;
	tend = -1.e20;
	ep->station_arr = newarr (0);
	if (ep->station_arr == NULL) {
		register_error (0, "setup_station_params: newarr(ep->station_arr) error.\n");
		return (-1);
	}
	for (i=0; i<maxtbl(ep->process_tbl); i++) {
		ProcessObject *po;
		int j;
		double tendpo;

		tendpo = -1.e20;
		po = (ProcessObject *) gettbl (ep->process_tbl, i);
		for (j=0; j<po->nplist; j++) {
			char snet[32], ssta[32], netsta[64];
			StationParams *sp;
			Tbl *sl;

			if (seed_net (po->plist[j].sta, snet, ssta) < 0) {
				complain (0, "setup_station_params: seed_net(%s) error.\n", po->plist[j].sta);
				continue;
			}
			sprintf (netsta, "%s_%s", snet, ssta);
			sp = (StationParams *) getarr (ep->station_arr, netsta);
			if (sp == NULL) {
				sp = (StationParams *) malloc (sizeof(StationParams));
				if (sp == NULL) {
					register_error (1, "setup_station_params: malloc(StationParams,%ld) error.\n", 
										sizeof(StationParams));
					return (-1);
				}
				memset (sp, 0, sizeof(StationParams));
				sl = newtbl (1);
				if (sl == NULL) {
					register_error (0, "setup_station_params: newtbl(ep->station_arr(%s)->tbl) error.\n", netsta);
					return (-1);
				}
				sp->chan_expr = newstbl (0);
				sp->plist = sl;
				strcpy (sp->sta, po->plist[j].sta);
				sp->tstart = 1.e30;
				sp->tend = -1.e30;
				setarr (ep->station_arr, netsta, sp);
			}
			settbl (sp->plist, -1, &(po->plist[j]));
			addstbl (sp->chan_expr, po->plist[j].chan_expr);
			if (po->plist[j].tstart < tstart) tstart = po->plist[j].tstart;
			if (po->plist[j].tend > tend) tend = po->plist[j].tend;
			if (po->plist[j].tend > tendpo) tendpo = po->plist[j].tend;
			if (po->plist[j].tstart < sp->tstart) sp->tstart = po->plist[j].tstart;
			if (po->plist[j].tend > sp->tend) sp->tend = po->plist[j].tend;
			if (first) {
				first = 0;
			} else {
				if (pushstr (&vst, "|") < 0) {
					register_error (0, "setup_station_params: pushstr(%s) error.\n", "|");
					return (-1);
				}
			}
			if (pushstr (&vst, netsta) < 0) {
				register_error (0, "setup_station_params: pushstr(%s) error.\n", netsta);
				return (-1);
			}
			if (pushstr (&vst, "[_/].*") < 0) {
				register_error (0, "setup_station_params: pushstr(%s) error.\n", "[_/].*");
				return (-1);
			}
		}
	}
	ep->tstart = tstart;
	ep->tend = tend;
	ep->select_expr = popstr (&vst, 1);

	return (0);
}

int
revise_station_params (EventParams *ep)

{
	int i;
	double tstart, tend;
	void *vst = NULL;
	char *select_expr;
	int first = 1;
	Tbl *stas;

	tstart = 1.e20;
	tend = -1.e20;
	stas = keysarr (ep->station_arr);
	for (i=0; i<maxtbl(stas); i++) {
		char *netsta;
		StationParams *sp;
		int j, use;

		netsta = (char *) gettbl (stas, i);
		sp = (StationParams *) getarr (ep->station_arr, netsta);
		for (j=0,use=0; j<maxtbl(sp->plist); j++) {
			ProcessParams *pp;

			pp = (ProcessParams *) gettbl (sp->plist, j);
			if (pp->done || pp->po->done) continue;
			use = 1;
			if (pp->tstart < tstart) tstart = pp->tstart;
			if (pp->tend > tend) tend = pp->tend;
		}
		if (!use) continue;
		if (first) {
			first = 0;
		} else {
			if (pushstr (&vst, "|") < 0) {
				register_error (0, "revise_station_params: pushstr(%s) error.\n", "|");
				freetbl (stas, 0);
				return (-1);
			}
		}
		if (pushstr (&vst, netsta) < 0) {
			register_error (0, "revise_station_params: pushstr(%s) error.\n", netsta);
			freetbl (stas, 0);
			return (-1);
		}
		if (pushstr (&vst, "[_/].*") < 0) {
			register_error (0, "revise_station_params: pushstr(%s) error.\n", "[_/].*");
			freetbl (stas, 0);
			return (-1);
		}
	}
	freetbl (stas, 0);

	ep->tstart = tstart;
	ep->tend = tend;
	if (ep->select_expr) free (ep->select_expr);
	ep->select_expr = popstr (&vst, 1);

	return (0);
}

int
setup_wfthread (EventParams *ep, char *orbname)

{
	if (verbose) {
		char *s;

		s = strtime(ep->tstart - 60.0);
		elog_notify (0, "%ld: Starting wf read from '%s' with tafter = %s\n", ep->myevid, orbname, s);
		elog_notify (0, "%ld: and select = %s\n", ep->myevid, ep->select_expr);
		free (s);
	}

	ep->reap = orbreapthr_new2 (orbname, ep->select_expr, NULL, ep->tstart - 60.0, 0.0, 50);
	if (ep->reap == NULL) {
		register_error (0, "orbreapthr_news(%s) error.\n", orbname);
		return (-1);
	}

	return (0);
}

int
make_trrow_copy (Dbptr trrow, double tstart, double tend, Dbptr *dbtr)

{
	char rec[1024];
	double time, samprate, dt, t0;
	long nsamp;
	float *data, *datacp;
	long i, ioff, istart, iend, nbad, ns;

	if (dbget (trrow, rec) == dbINVALID) {
		register_error (0, "make_trrow_copy: dbget() error.\n");
		return (-1);
	}
	if (dbgetv (trrow, 0,	"time", &time,
				"samprate", &samprate,
				"nsamp", &nsamp,
				"data", &data,
				NULL ) < 0) {
		register_error (0, "make_trrow_copy: dbget() error.\n");
		return (-1);
	}
	dt = 1.0/samprate;

	istart = INT2 (tstart - time, dt);
	iend = INT2 (tend - time, dt);
	t0 = time + iend*dt ;
	while ( t0 > tend ) { 
		t0 -= dt ;
		iend--; 
	}
	t0 = time + istart*dt ;
	while ( t0 < tstart ) { 
		t0 += dt ;
		istart++; 
	}
	ioff = -istart;

	nbad = 0;
	ns = iend - istart + 1 ;
	if (ns < 1) {
		return ( 0 ) ;
	}

	datacp = (float *) malloc (ns*sizeof(float));
	if (datacp == NULL) {
		register_error (1, "make_trrow_copy: malloc(datacp,%ld) error.\n", ns*sizeof(float));
		return (-1);
	}
	trfill_gap (datacp, ns);

	if (istart < 0) { 
		if (iend < 0) {
			return ( 0 ) ;
		}
		nbad = -istart ; 
		istart = 0 ;
	}
	if (iend >= nsamp) {
		if (istart >= nsamp) {
			return ( 0 ) ;
		}
		nbad += (iend - nsamp) + 1 ;
		iend = nsamp - 1;
	}

	for (i=istart; i<=iend; i++) {
		datacp[i+ioff] = data[i];
		if (data[i] > 1.e30) nbad++;
	}

	if ((*dbtr).database == dbINVALID) {
		*dbtr = dblookup (trnew(0, 0), 0, "trace", 0, 0);
		if ((*dbtr).database == dbINVALID) {
			register_error (0, "make_trrow_copy: trnew() error.\n");
			free (datacp);
			return (-1);
		}
	}

	if ((i=dbadd (*dbtr, rec)) == dbINVALID) {
		register_error (0, "make_trrow_copy: dbput(%s) error.\n", rec);
		free (datacp);
		trdestroy (dbtr);
		return (-1);
	}

	(*dbtr).record = i;
	if (dbputv (*dbtr, 0,	"data", datacp,
				"nsamp", ns,
				"time", t0,
				"m0", (double) nbad,
				NULL ) < 0) {
		register_error (0, "make_trrow_copy: dbputv() error.\n");
		free (datacp);
		trdestroy (dbtr);
		return (-1);
	}

	return (ns);
}

int
isit_ready (Dbptr trrow, ProcessParams *pp)

{
	char chan[16];
	double time, samprate, dt, t0;
	long nsamp;
	float *data;
	ProcessParamsChan *ppc;
	long i, ioff, istart, iend, nbad, ns;

	if (dbgetv (trrow, 0,	"chan", chan,
				"time", &time,
				"samprate", &samprate,
				"nsamp", &nsamp,
				"data", &data,
				NULL ) < 0) {
		register_error (0, "isit_ready: dbgetv() error.\n");
		return (-1);
	}
	dt = 1.0/samprate;

	ppc = (ProcessParamsChan *) getarr (pp->channels, chan) ;
	if (ppc == NULL) {
		return (0);
	}

	if (pp->tupdate > 0.0) {
		if (now() - ppc->tlast > pp->tupdate) {
			ppc->tlast = now();
			return (1); 
		}
	}

	istart = INT2 (pp->tstart - time, dt);
	iend = INT2 (pp->tend - time, dt);
	t0 = time + iend*dt ;
	while ( t0 > pp->tend ) { 
		t0 -= dt ;
		iend--; 
	}
	t0 = time + istart*dt ;
	while ( t0 < pp->tstart ) { 
		t0 += dt ;
		istart++; 
	}
	ioff = -istart;

	nbad = 0;
	ns = iend - istart + 1 ;
	if (ns < 1) {
		return ( 0 ) ;
	}

	if (istart < 0) { 
		if (iend < 0) {
			return ( 0 ) ;
		}
		nbad = -istart ; 
		istart = 0 ;
	}
	if (iend >= nsamp) {
		if (istart >= nsamp) {
			return ( 0 ) ;
		}
		nbad += (iend - nsamp) + 1 ;
		iend = nsamp - 1;
	}

	for (i=istart; i<=iend; i++) {
		if (data[i] > 1.e30) nbad++;
	}

	if (nbad == 0) {
		ppc->tlast = now();
		return (1); 
	}

	return (0);
}

int
flush_processing (ProcessObject *po, Dbptr dbtrace_cache, int type, Dbptr dbout, int orb)

{
	int i, ret;

	if (po->done) return (0);
	for (i=0; i<po->nplist; i++) {
		int j;
		Tbl *channels;
		ProcessParams *pp;
		char expr[128];

		pp = &(po->plist[i]);
		if (pp->done) continue;

		channels = keysarr(pp->channels);

		for (j=0; j<maxtbl(channels); j++) {
			char *chan;
			Dbptr dbtr;
			ProcessParamsChan *ppc;

			chan = (char *) gettbl (channels, j);
			ppc = (ProcessParamsChan *) getarr (pp->channels, chan);
			if (ppc->done) continue;

			sprintf (expr, "sta == \"%s\" && chan == \"%s\"", pp->sta, chan);
			dbtrace_cache.record = -1;
			ret = dbfind (dbtrace_cache, expr, 0, 0);
			if (ret < 0) continue;
			dbtrace_cache.record = ret;

			dbtr = dbinvalid() ;
			ret = make_trrow_copy (dbtrace_cache, pp->tstart-10.0, pp->tend+10.0, &dbtr);
			if (ret < 0) {
				die (0, "make_trrow_copy() error.\n");
			}
			ret = process_channel_callback (pp, dbtr, 1);
			if (ret < 0) {
				die (0, "process_channel_callback() error.\n");
			}
			trdestroy (&dbtr);
			ppc->done = 1;
		}
		freetbl (channels, 0);
		ret = process_station_callback (pp, 1);
		if (ret < 0) {
			die (0, "process_station_callback() error.\n");
		}
		pp->done = 1;
	}
	ret = process_network_callback (po, 1, type, dbout, orb);
	if (ret < 0) {
		die (0, "process_network_callback() error.\n");
	}
	po->done = 1;

	return (0);
}

void
main (int argc, char **argv)

{
	char *start=NULL;
	char *select="/pf/orb2dbt";
	char *orbwf_name, *orbev_name, *dbname, *dbwfname=NULL;
	char *dbin_name, *dbout_name;
	char pfname[128];
	long orbevin, orbevout, orbwf;
	int pktid, get;
	int pktidnewest;
	int ret, i;
	int number=0;
	int wait=1;
	Dbptr db, dbwf, dbin, dbout;
	long ndbin;
	int num;
	Pf *pf;
	Tbl *evproc_tbl;
	Tbl *process_tbl;
	Tbl *script_tbl;
	void *vst=NULL;
	void *pi=NULL;
	char *perl_script;
	OrbreapThr *event_thr=NULL;
	Tbl *wfthread_tbl;
	int sleepset ;
	int max_events_to_thread = 10;
	int sleepev, sleepwf;
	char dir[FILENAME_MAX];
	char name[FILENAME_MAX];
	int type;
	char *antenv;

	elog_init (argc, argv) ;

	antenv = getenv ("ANTELOPE");

	dirbase (*argv, dir, name);
	if (!strcmp(name, "dbevproc")) {
		type = TYPE_DB;
	} else {
		type = TYPE_ORB;
	}
	if (type == TYPE_ORB) {
		if (argc < 4) {
			usage(type);
			banner ("orbevproc", "$Revision$ $Date$\n") ;
			exit (1);
		}
		startup_banner();
		strcpy (pfname, "orbevproc");

		for (argv++,argc--; argc>0; argv++,argc--) {
			if (**argv != '-') break;
			if (!strcmp(*argv, "-start")) {
				argc--; argv++;
				if (argc < 1) {
					complain (0, "Need -start argument.\n");
					usage(type);
					exit (1);
				}
				start = *argv;
			} else if (!strcmp(*argv, "-select")) {
				argc--; argv++;
				if (argc < 1) {
					complain (0, "Need -select argument.\n");
					usage(type);
					exit (1);
				}
				select = *argv;
			} else if (!strcmp(*argv, "-state")) {
				argc--; argv++;
				if (argc < 1) {
					complain (0, "Need -state argument.\n");
					usage(type);
					exit (1);
				}
				statefile = *argv;
			} else if (!strcmp(*argv, "-p")) {
				argc--; argv++;
				if (argc < 1) {
					complain (0, "Need -p argument.\n");
					usage(type);
					exit (1);
				}
				strcpy (pfname, *argv);
			} else if (!strcmp(*argv, "-tmpdbdir")) {
				argc--; argv++;
				if (argc < 1) {
					complain (0, "Need -tmpdbdir argument.\n");
					usage(type);
					exit (1);
				}
				tmpdbdir = *argv;
			} else if (!strcmp(*argv, "-dbwf")) {
				argc--; argv++;
				if (argc < 1) {
					complain (0, "Need -dbwf argument.\n");
					usage(type);
					exit (1);
				}
				dbwfname = *argv;
			} else if (!strcmp(*argv, "-number")) {
				argc--; argv++;
				if (argc < 1) {
					complain (0, "Need -number argument.\n");
					usage(type);
					exit (1);
				}
				number = atoi(*argv);
			} else if (!strcmp(*argv, "-nowait")) {
				wait = 0;
			} else if (!strcmp(*argv, "-v")) {
				verbose = 1;
			} else if (!strcmp(*argv, "-vv")) {
				verbose = 2;
			} else if (!strcmp(*argv, "-vvv")) {
				verbose = 3;
			} else {
				complain (0, "Unrecognized argument '%s'.\n", *argv);
				usage(type);
				exit (1);
			}
		}
	
		if (argc < 1) {
			complain (0, "Need orbwf argument.\n");
			usage(type);
			exit (1);
		}
		orbwf_name = *argv;
	
		argv++; argc--;
		if (argc < 1) {
			complain (0, "Need orbev argument.\n");
			usage(type);
			exit (1);
		}
		orbev_name = *argv;
	
		argv++; argc--;
		if (argc < 1) {
			complain (0, "Need dbname argument.\n");
			usage(type);
			exit (1);
		}
		dbname = *argv;
	} else {
		if (argc < 3) {
			usage(type);
			banner ("dbevproc", "$Revision$ $Date$\n") ;
			exit (1);
		}
		strcpy (pfname, "dbevproc");

		for (argv++,argc--; argc>0; argv++,argc--) {
			if (**argv != '-') break;
			if (!strcmp(*argv, "-")) {
				break;
			} else if (!strcmp(*argv, "-p")) {
				argc--; argv++;
				if (argc < 1) {
					complain (0, "Need -p argument.\n");
					usage(type);
					exit (1);
				}
				strcpy (pfname, *argv);
			} else if (!strcmp(*argv, "-tmpdbdir")) {
				argc--; argv++;
				if (argc < 1) {
					complain (0, "Need -tmpdbdir argument.\n");
					usage(type);
					exit (1);
				}
				tmpdbdir = *argv;
			} else if (!strcmp(*argv, "-v")) {
				verbose = 1;
			} else if (!strcmp(*argv, "-vv")) {
				verbose = 2;
			} else if (!strcmp(*argv, "-vvv")) {
				verbose = 3;
			} else {
				complain (0, "Unrecognized argument '%s'.\n", *argv);
				usage(type);
				exit (1);
			}
		}
	
		if (argc < 1) {
			complain (0, "dbin argument.\n");
			usage(type);
			exit (1);
		}
		dbin_name = *argv;
	
		argv++; argc--;
		if (argc < 1) {
			complain (0, "Need dbout argument.\n");
			usage(type);
			exit (1);
		}
		dbout_name = *argv;
	}

	/* Read orbevproc parameter file */

	if (pfread (pfname, &pf) < 0) {
		die (0, "pfread(%s) error.\n", pfname);
	}
	if (parse_param (pf, "event_processes", P_TBL, 1, &evproc_tbl) < 0) {
		die (0, "parse_param(event_processes) error.\n");
	}
	if (parse_param (pf, "max_events_to_thread", P_LINT, 1, &max_events_to_thread) < 0) {
		die (0, "parse_param(max_events_to_thread) error.\n");
	}
	if (type == TYPE_DB) {
		if (assoc_parse_pf (pf) < 0) {
			die (0, "assoc_parse_pf(%s) error.\n", pfname);
		}
	}

	/* Parse the event processes table */

	script_tbl = newtbl( 0 );
	process_tbl = newtbl (0);
	for (i=0; i<maxtbl(evproc_tbl); i++) {
		char *line;
		char script[128], class[128], params[128];
		Pf *paramspf;
		ProcessObject *po;

		line = (char *) gettbl (evproc_tbl, i);
		ret = sscanf (line, "%s %s %s", script, class, params);
		if (ret == 1) {
			settbl (script_tbl, -1, strdup(script));
			continue;
		}
		if (ret != 3) continue;
		if (parse_param (pf, params, P_ARRPF, 1, &paramspf) < 0) {
			die (0, "parse_param(params %s) error.\n", params);
		}
		settbl (script_tbl, -1, strdup(script));
		po = (ProcessObject *) malloc (sizeof(ProcessObject));
		if (po == NULL) {
			die (0, "malloc(%s,%ld) error.\n", script, sizeof(ProcessObject));
		}
		memset (po, 0, sizeof(ProcessObject));
		settbl (process_tbl, -1, po);
		po->name = strdup(script);
		po->perlclass = strdup(class);
		po->pf = paramspf;
	}

	/* Initialize embedded perl engine */

	for (i=0,vst=NULL; i<maxtbl(script_tbl); i++) {
		char *script_name;
		char *script;
		char file_name[FILENAME_MAX];
		char line[8192];
		FILE *f;

		script_name = (char *) gettbl (script_tbl, i);
		script = NULL;
		if (strchr(script_name, '/')) {
			strcpy (file_name, script_name);
		} else {
			sprintf (file_name, "%s/data/evproc/%s", antenv, script_name);
		}
		elog_notify (0, "Adding perl module '%s'\n", file_name);
		f = fopen (file_name, "r");
		if (f == NULL) {
			die (0, "fopen(%s) error.\n", file_name);
		}
		while (fgets (line, 8191, f)) {
			pushstr (&vst, line);
		}
		fclose (f);
	}
	perl_script = popstr (&vst, 1);
	elog_notify (0, "Initializing perl modules\n");
	pi = perlembed_init ( perl_script );
	if (pi == NULL) {
		die (0, "perlembed_init() error.\n");
	}

	if (type == TYPE_ORB) {

		/* Open database */

		if (dbopen (dbname, "r+", &db) == dbINVALID) {
			complain (0, "dbopen(%s) error.\n", dbname);
			usage(type);
			exit (1);
		}
		finit_db (db);

		dbwf.database = dbINVALID ;
		if (dbwfname) {
			if (dbopen (dbwfname, "r+", &dbwf) == dbINVALID) {
				complain (0, "dbopen(%s) error.\n", dbwfname);
				usage(type);
				exit (1);
			}
		}

		/* orb setup stuff */

		orbevin = orbopen (orbev_name, "r&");
		if (orbevin < 0) {
			die (0, "orbopen(%s) error.\n", orbev_name);
		}
		orbevout = orbopen (orbev_name, "w&");
		if (orbevout < 0) {
			die (0, "orbopen(%s) error.\n", orbev_name);
		}
		if (orbselect (orbevin, select) < 0) {
			die (0, "orbselect(%s,%s) error.\n", orbev_name, select);
		}

		pktid = ORBNEWEST;
		get = 0;
		if (statefile) {
			FILE *f;

			f = fopen (statefile, "r");
			if (f == NULL) {
				complain (0, "fopen('%s') error...using start parameters\n.", statefile);
			} else {
				pf = NULL;
				if (pfin (f, &pf) < 0) {
					fclose (f);
					complain (0, "pfin('%s') error...using start parameters\n.", statefile);
				} else {
					fclose (f);
					if (parse_param (pf, "lastpktid", P_LINT, 1, &pktid) < 0) {
						complain (0, "parse_param(lastpktid) error....using start parameters\n");
						pktid = ORBNEWEST;
					}
					pktid = orbseek (orbevin, pktid);
					if (pktid < 0) {
						complain (0, "orbseek(lastpktid) error...using start parameters.\n");
						pktid = ORBNEXT;
					} else {
						complain (0, "resurrection successfull...start following pktid %d.\n", pktid);
					}
				}
				pffree(pf) ;
				pf = NULL;
			}
		}
	
		if (!wait) {
			pktidnewest = orbseek (orbevin, ORBNEWEST);
			if (pktidnewest < 0) {
				complain (0, "No packets.\n");
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
					ret = orbseek (orbevin, pktid);
					ret = orbseek (orbevin, ORBPREV);
				} else {
					if (strlen(start) > 10 || strchr(start, ' ') || strchr(start, '.')
								|| strchr(start, ':') || strchr(start, '/')) {
						double time;
		
						time = str2epoch(start);
						orbafter (orbevin, time);
					} else {
						pktid = atoi(start);
						orbseek (orbevin, pktid);
						orbseek (orbevin, ORBPREV);
					}
				}
			} else pktid = ORBNEXT;
		}

		main_thread = pthread_self ();
	}

	dbin = dbinvalid();
	dbout = dbinvalid();
	if (type == TYPE_DB) {
		if (!strcmp(dbin_name, "-")) {
			if (dbread_view (stdin, &dbin, NULL) == dbINVALID) {
				complain (0, "dbread_view(stdin) error.\n");
				usage(type);
				exit (1);
			}
		} else {
			if (dbopen(dbin_name, "r+", &dbin) == dbINVALID) {
				complain (0, "dbopen(%s) error.\n", dbin_name);
				usage(type);
				exit (1);
			}
			dbin = dblookup (dbin, 0, "origin", 0, 0);
			if (dbin.table == dbINVALID) {
				complain (0, "Cannot lookup origin table for %s.\n", dbin_name);
				usage(type);
				exit (1);
			}
		}
		dbquery (dbin, dbRECORD_COUNT, &ndbin);
		if (ndbin < 1) {
			complain (0, "Nothing to process in %s.\n", dbin_name);
			usage(type);
			exit (1);
		}
		if (dbopen(dbout_name, "r+", &dbout) == dbINVALID) {
			complain (0, "dbopen(%s) error.\n", dbout_name);
			usage(type);
			exit (1);
		}
		db = dbin;
		dbwf = dbin ;
		finit_db (db);
		dbin.record = 0;
		if (verbose) {
			elog_notify (0, "Processing %ld origins from %s\n", ndbin, dbin_name);
		}
	}

	wfthread_tbl = newtbl (0);

	mysetup_signals () ;

	/* Main processing loop */

	num = 0;
	sleepset = 0;

	sleepev = 0;
	sleepwf = 0;
	
	while (loop) {
		int pktidin;
		char srcname[64];
		double pkttime;
		static char *packet=NULL;
		static int nbytes=0;
		static int bufsiz=0;
		static Packet *pkt=NULL;
		Dbptr dbpkt, dbo;
		double olat, olon, otime, odepth;
		long orid, evid, n;
		char *s;
		char auth[64];
		char expr[256];
		char rec[512];
		EventParams *ep;
		int nodata = 1;
		long myevid;
		double last_output_time = 0.0;

		if (type == TYPE_ORB) {

			/* Get the next packet */

			if (sleepev) myusleep (0.1);

			if (get) {
				orbget (orbevin, pktid, &pktidin, srcname, &pkttime, &packet, &nbytes, &bufsiz);
				get = 0;
				orbseek (orbevin, ORBOLDEST);
			} else {
				if (event_thr == NULL) {
					event_thr = orbreapthr_new (orbevin, 0.0, 5);
					if (event_thr == NULL) {
						die (0, "orbreapthr_new() error for main event reap thread.\n");
					}
				}
				ret = orbreapthr_get (event_thr, &pktidin, srcname, &pkttime, &packet, &nbytes, &bufsiz);
				if (ret < 0) {
					die (0, "fatal orbreapthr_get() error for main event reap thread.\n");
				}
				switch (ret) {
				case ORBREAPTHR_STOPPED:
					die (0, "fatal orbreapthr_get()  main event reap thread stopped.\n");
				case ORBREAPTHR_NODATA:
					sleepev = 1;
					goto PROCESS_WFTHREADS;
				default:
					sleepev = 0;
					break; 
				}
			}

			/* unstuff the packet */

			ret = unstuffPkt (srcname, pkttime, packet, nbytes, &pkt);
			if (ret < 0) {
				complain (0, "unstuffPkt(%s) error.\n", srcname);
				goto PROCESS_WFTHREADS;
			}

			/* Skip if not a pf packet */
	
			if (ret != Pkt_pf) {
				goto PROCESS_WFTHREADS;
			}

			/* Convert pf packet into a temporary local database */
	
			if (mktmpdb (pkt->pf, db, &dbpkt, &myevid) < 0) {
				complain (0, "mktmpdb(%s) error.\n", srcname);
				goto PROCESS_WFTHREADS;
			}
			myevid = pktidin;
		}

		if (type == TYPE_DB) {
			if (mktmpdb (NULL, dbin, &dbpkt, &myevid) < 0) {
				complain (0, "mktmpdb(%s) error.\n", dbin_name);
				goto PROCESS_WFTHREADS;
			}
		}

		/* Extract some of the origin info */

		dbo = dblookup (dbpkt, 0, "origin", 0, 0);
		dbo.record = 0;
		if (dbgetv (dbo, 0, "lat", &olat, "lon", &olon, "time", &otime, "orid", &orid, "evid", &evid,
							"depth", &odepth, "auth", auth, NULL ) < 0) {
			complain (0, "dbgetv() error.\n");
			dbdestroy (dbpkt); 
			goto PROCESS_WFTHREADS;
		}
		if (verbose) {
			if (type == TYPE_DB) {
				elog_notify (0, "\n%ld: Processing orid %ld at %.3f %.3f %.3f %s auth %s\n", 
					myevid, orid, olat, olon, odepth, s=strtime(otime), auth);
			} else {
				elog_notify (0, "\n%ld: Processing pktid %ld at %.3f %.3f %.3f %s auth %s\n", 
					myevid, pktidin, olat, olon, odepth, s=strtime(otime), auth);
			}
			free(s) ;
		}

		/* Make a new object instance for each of the perl processing classes */

		ep = NULL;
		for (i=0; i<maxtbl(process_tbl); i++) {
			ProcessObject *poin, *poout;

			poin = (ProcessObject *) gettbl (process_tbl, i);
			ret = make_perlobj (dbpkt, db, orid, myevid, poin, &poout);
			if (ret < 0) {
				die (0, "make_perlobj() error.\n");
			}
			if (ret == 0) {
				clear_register (1);
				continue;
			}
			if (ep == NULL) {
				Dbptr dbtrace;

				ep = (EventParams *) malloc (sizeof(EventParams));
				if (ep == NULL) {
					die (1, "malloc(EventParams,%ld) error.\n", sizeof(EventParams));
				}
				memset (ep, 0, sizeof(EventParams));
				ep->db = dbpkt;
				ep->orid = orid;
				ep->myevid = myevid;
				ep->time = otime;
				ep->lat = olat;
				ep->lon = olon;
				ep->depth = odepth;
				ep->process_tbl = newtbl (1);
				if (ep->process_tbl == NULL) {
					die (0, "newtbl(ep->process_tbl) error.\n");
				}

				/* close and re-open the waveform database if this is orbevproc */

				if (dbwf.database != dbINVALID && type == TYPE_ORB) {
					if (dbclose (dbwf)  == dbINVALID) {
						complain (0, "dbclose(dbwf) error.\n");
					}
					if (strcmp(dbname, dbwfname)) {
						if (dbclose (db)  == dbINVALID) {
							complain (0, "dbclose(db) error.\n");
						}
					}
					if (dbopen (dbwfname, "r+", &dbwf) == dbINVALID) {
						die (0, "dbopen(%s) error.\n", dbwfname);
					}
					if (dbopen (dbname, "r+", &db) == dbINVALID) {
						die (0, "dbopen(%s) error.\n", dbname);
					}
					finit_db (db);
				}

				dbtrace = trnew ( 0, 0 ) ;
				if (dbtrace.database == dbINVALID) {
					die (0, "trnew(dbtrace) error\n");
				}
				ep->pt = pktchannel2trace_new (db, dbtrace, dbwf, 1, 0, 0);
				if (ep->pt == NULL) {
					die (0, "pktchannel2trace_new(ep->pt) error\n");
				}
			}
			settbl (ep->process_tbl, -1, poout);
			if (poout->expire_time <= 0.0) {
				complain (0, "%ld: %s: WARNING - No expiration time\n", ep->myevid, poout->perlclass);
			} else if (verbose) {
				char *s1;

				s1 = mystrdatetime(poout->expire_time);
				elog_notify (0, "%ld: %s: Expire time set to %s\n", ep->myevid, poout->perlclass, s1);
				free (s1);
			}
		}

		if (ep == NULL) {
			complain (0, "Nothing to process for event %ld\n", myevid);
			dbdestroy (dbpkt); 
			goto PROCESS_WFTHREADS;
		}
		num++;

		/* setup station parameters for this event */

		if (setup_station_params (ep) < 0) {
			die (0, "setup_station_params() error.\n");
		}

		/* get waveforms out of database for this event */

		if (dbwf.database != dbINVALID) {
			Tbl *netstas;
			int j, done;

			netstas = keysarr (ep->station_arr);
			for (j=0; j<maxtbl(netstas); j++) {
				char *netsta;
				StationParams *sp;
				char *chan_expr;
				int k, nrec, irec;

				netsta = (char *) gettbl (netstas, j);
				sp = (StationParams *) getarr (ep->station_arr, netsta);
				chan_expr = jointbl(tblstbl(sp->chan_expr), "|");

				nrec = pktchannel2trace_put_from_db (ep->pt, sp->sta, chan_expr, 
								1, sp->tstart-10.0, sp->tend+10.0);
				if (nrec < 0) {
					die (0, "pktchannel2trace_put_from_db(%s,%s) error.\n", sp->sta, sp->chan_expr);
				}
				free (chan_expr);

				if (nrec == 0) {
					if (verbose > 2) {
						char *s1, *s2;
						s1 = mystrtime(sp->tstart-10.0);
						s2 = mystrtime(sp->tend+10.0);
						elog_notify (0, "%ld: %s: %s: Processing db - no data from %s to %s\n", ep->myevid, sp->sta, sp->chan_expr, s1, s2);
						free (s1);
						free (s2);
					}
					continue;
				}

				irec = ep->pt->dbt.record;

				for (k=0; k<maxtbl(sp->plist); k++) {
					ProcessParams *pp;
					char expr[128];

					pp = (ProcessParams *) gettbl (sp->plist, k);

					if (pp->done) continue;

					sprintf (expr, "chan =~ /%s/", pp->chan_expr);

					for (ep->pt->dbt.record=irec; ep->pt->dbt.record<irec+nrec; (ep->pt->dbt.record)++) {
						long ival;
						char chan[32];
						ProcessParamsChan *ppc;
						Dbptr dbtr;

						dbex_evalstr (ep->pt->dbt, expr, dbBOOLEAN, &ival);
						if (ival == 0) continue;

						dbgetv (ep->pt->dbt, 0, "chan", chan, NULL );

						ppc = (ProcessParamsChan *) getarr (pp->channels, chan);
						if (ppc == NULL) continue;

						if (ppc->done) continue;

						dbtr = dbinvalid() ;
						ret = make_trrow_copy (ep->pt->dbt, pp->tstart-10.0, pp->tend+10.0, &dbtr);
						if (ret < 0) {
							die (0, "make_trrow_copy() error.\n");
						}

						if (ret == 0) {
							if (verbose > 2) {
								elog_notify (0, "%ld: %s: %s: %s: Processing db - no data\n", ep->myevid, pp->po->perlclass, pp->sta, chan);
							}
							continue;
						}

						if (verbose > 2) {
							elog_notify (0, "%ld: %s: %s: %s: Processing db\n", ep->myevid, pp->po->perlclass, pp->sta, chan);
						}

						/* make perl callbacks for interested processing objects */
	
						ret = process_channel_callback (pp, dbtr, 0);
						if (ret < 0) {
							die (0, "process_channel_callback() error.\n");
						}
						trdestroy (&dbtr);
	
						/* return of 0 means that data still needed for particular event-process-station-channel */
	
						if (ret == 0) {
						}
	
						/* return of 1 means that the particular event-process-station-channel is done */
	
						if (ret == 1) {
							ppc->done = 1;
						}
	
						/* return of 2 means that the particular event-process-station is done */
	
						if (ret == 2) {
							ppc->done = 1;
							pp->done = 1;
							ret = process_station_callback (pp, 0);
							if (ret < 0) {
								die (0, "process_station_callback() error.\n");
							}
						}
	
						/* return of 3 means that the particular event-process is done */
		
						if (ret == 3) {
							ppc->done = 1;
							pp->po->done = 1;
							pp->done = 1;
							ret = process_station_callback (pp, 0);
							if (ret < 0) {
								die (0, "process_station_callback() error.\n");
							}
							ret = process_network_callback (pp->po, 0, type, dbout, orbevout);
							if (ret < 0) {
								die (0, "process_network_callback() error.\n");
							}
						}
	
						/* return of 4 means that data not needed for the particular event-process */
		
						if (ret == 4) {
						}
					}
				}
			}
			freetbl (netstas, 0);

			/* If dbevproc, then flush and continue */

			if (type == TYPE_DB) {
				for (j=0; j<maxtbl(ep->process_tbl); j++) {
					ProcessObject *po;

					po = (ProcessObject *) gettbl (ep->process_tbl, j);
					if (flush_processing (po, ep->pt->dbt, type, dbout, orbevout) < 0) {
						complain (0, "flush_processing() error.\n");
					}
				}
				if (verbose) {
					elog_notify (0, "%ld: Processing done - deleting event\n", ep->myevid);
				}
				event_destroy (ep);
				goto PROCESS_WFTHREADS;
			}

			/* cull out processing objects and stations */

			for (j=0,done=1; j<maxtbl(ep->process_tbl); j++) {
				ProcessObject *po;

				po = (ProcessObject *) gettbl (ep->process_tbl, j);
				if (po->done == 0 && done == 1) done = 0;
			}
			if (done) {
				if (verbose) {
					elog_notify (0, "%ld: Processing done - deleting event\n", ep->myevid);
				}
				event_destroy (ep);
				goto PROCESS_WFTHREADS;
			}

			/* revise station parameters for this event */

			if (revise_station_params (ep) < 0) {
				die (0, "revise_station_params() error.\n");
			}
		}

		/* setup the wf thread for this event */

		if (setup_wfthread (ep, orbwf_name) < 0) {
			die (0, "setup_wfthread() error.\n");
		}
		settbl (wfthread_tbl, -1, ep);

		/* wf thread processing */

PROCESS_WFTHREADS:
		sleepwf = 1;

		/* loop through the wfthread table */

		for (i=0,nodata=1; i<maxtbl(wfthread_tbl); i++) {
			int j, done;

			/* read a packet */

			ep = (EventParams *) gettbl (wfthread_tbl, i);
			ret = orbreapthr_get (ep->reap, &pktidin, srcname, &pkttime, &packet, &nbytes, &bufsiz);
			if (ret < 0) {
				die (0, "fatal orbreapthr_get() error for wf reap thread.\n");
			}
			if (ret == ORBREAPTHR_NODATA) goto CHECKEXPIRED;
			if (ret == ORBREAPTHR_STOPPED) {
				if (verbose) {
					elog_notify (0, "%ld: Deleting event\n", ep->myevid);
				}
				event_destroy (ep); 
				deltbl (wfthread_tbl, i);
				i--;
				continue;
			}
			if (ep->halt) continue;

			sleepwf = 0;

			/* unstuff the packet */

			ret = unstuffPkt (srcname, pkttime, packet, nbytes, &pkt);
			if (ret < 0) {
				complain (0, "unstuffPkt(%s) error.\n", srcname);
				continue;
			}

			/* Skip if not a wf packet */
	
			if (ret != Pkt_wf) {
				continue;
			}

			nodata = 0;

			if (verbose > 2) {
				char *s;

				s = mystrtime(pkt->time);
				elog_notify (0, "%ld: Reaped packet %s, %.3f at %s\n", ep->myevid, srcname, now()-pkt->time, s);
				free (s);
			}

			/* loop through the packet channels and match against the station array */

			for (j=0; j<pkt->nchannels; j++) {
				PktChannel *pchan;
				char netsta[64];
				StationParams *sp;
				int k, used;

				pchan = (PktChannel *) gettbl (pkt->channels, j);
				sprintf (netsta, "%s_%s", pchan->net, pchan->sta);
				sp = (StationParams *) getarr (ep->station_arr, netsta);
				if (sp == NULL) continue;
				for (k=0,used=0; k<maxtbl(sp->plist); k++) {
					ProcessParams *pp;
					ProcessParamsChan *ppc;
					char chan[32];
					char *s;
					int l;
					Dbptr dbtr;

					pp = (ProcessParams *) gettbl (sp->plist, k);

					if (pp->done) continue;

					/* now match against the chan expression */

					if (k == 0) map_seed_chanloc (pp->sta, pchan->chan, pchan->loc, chan);
					if (strmatches (chan, pp->chan_expr, 0) != 1) continue;

					ppc = (ProcessParamsChan *) getarr (pp->channels, chan);
					if (ppc == NULL) continue;
					if (ppc->done) continue;

					/* channel is ok - dispatch it to the cache */

					if (used == 0) {
						used = 1;

						if (verbose > 2) {
							char *s, *s1, *s2;

							s1 = mystrtime(sp->tstart-10.0);
							s2 = mystrtime(sp->tend+10.0);
							s = mystrtime(pchan->time);
							elog_notify (0, "%ld: Adding data to %s %s %s %s at %s\n", ep->myevid, pp->sta, s1, s2, pchan->chan, s);
							free (s);
							free (s1);
							free (s2);
						}
						ret = pktchannel2trace_put (ep->pt, pchan, 1, sp->tstart-10.0, sp->tend+10.0) ;
						if (ret < 0) {
							complain (0, "pktchannel2trace_put(%s) error.\n", srcname);
						}
					}

					/* for this particular event-process-station-channel, see if it is time to
					   make the perl callback */

					ret = isit_ready (ep->pt->dbt, pp);
					if (ret < 0) {
						die (0, "isit_ready() error.\n");
					}

					if (ret == 0) continue;

					dbtr = dbinvalid() ;
					ret = make_trrow_copy (ep->pt->dbt, pp->tstart-10.0, pp->tend+10.0, &dbtr);
					if (ret < 0) {
						die (0, "make_trrow_copy() error.\n");
					}
					if (ret == 0) continue;

					if (verbose > 2) {
						char *s;

						s = mystrtime(pchan->time);
						elog_notify (0, "%ld: Processing %s %s %s at %s\n", ep->myevid, pchan->sta, pp->sta, pchan->chan, s);
						free (s);
					}
				
					/* make perl callbacks for interested processing objects */

					ret = process_channel_callback (pp, dbtr, 0);
					if (ret < 0) {
						die (0, "process_channel_callback() error.\n");
					}
					trdestroy (&dbtr);

					/* return of 0 means that data still needed for particular event-process-station-channel */

					if (ret == 0) {
					}

					/* return of 1 means that the particular event-process-station-channel is done */

					if (ret == 1) {
						ppc->done = 1;
					}

					/* return of 2 means that the particular event-process-station is done */

					if (ret == 2) {
						ppc->done = 1;
						pp->done = 1;
						ret = process_station_callback (pp, 0);
						if (ret < 0) {
							die (0, "process_station_callback() error.\n");
						}
					}

					/* return of 3 means that the particular event-process is done */

					if (ret == 3) {
						ppc->done = 1;
						pp->po->done = 1;
						pp->done = 1;
						ret = process_station_callback (pp, 0);
						if (ret < 0) {
							die (0, "process_station_callback() error.\n");
						}
						ret = process_network_callback (pp->po, 0, type, dbout, orbevout);
						if (ret < 0) {
							die (0, "process_network_callback() error.\n");
						}
					} 

					/* return of 4 means that data not needed for the particular event-process */

					if (ret == 4) {
					}
				}
			}


			/* check to see if this event is done and if any 
			   of its process objects have expired */

CHECKEXPIRED:		for (j=0,done=1; j<maxtbl(ep->process_tbl); j++) {
				ProcessObject *po;

				po = (ProcessObject *) gettbl (ep->process_tbl, j);

				if (po->done) continue;

				if (po->expire_time > 0.0 && now() > po->expire_time) {

					if (verbose) {
						elog_notify (0, "%ld: %s: Maximum wait time expired - flushing processing\n", ep->myevid, po->perlclass);
					}

					if (flush_processing (po, ep->pt->dbt, type, dbout, orbevout) < 0) {
						complain (0, "flush_processing() error.\n");
					}
				}

				if (po->done == 0) {
					done = 0;
				} 
			}
			if (done) {
				if (ep->halt == 1) continue;
				if (verbose) {
					elog_notify (0, "%ld: Stopping wf reap thread\n", ep->myevid);
				}
				if (orbreapthr_set_to_stop (ep->reap) < 0) {
					complain (0, "orbreapthr_set_to_stop() error.\n");
				}
				if (verbose) {
					elog_notify (0, "%ld: Processing done for event\n", ep->myevid);
				}
				ep->halt = 1;
				continue;
			}
		}

		if (!sleepwf) goto PROCESS_WFTHREADS;

		if (maxtbl(wfthread_tbl) >= max_events_to_thread) {
			myusleep (0.1);
			if (verbose && now()-last_output_time > 60.0) {
				last_output_time = now();
				for (i=0; i<maxtbl(wfthread_tbl); i++) {
					int j;
		
					ep = (EventParams *) gettbl (wfthread_tbl, i);
					for (j=0; j<maxtbl(ep->process_tbl); j++) {
						ProcessObject *po;

						po = (ProcessObject *) gettbl (ep->process_tbl, j);
						if (po->done) continue;
						if (po->expire_time > 0.0) {
							elog_notify (0, "%ld: %s: Waiting...expires in %.3f seconds\n", ep->myevid, po->perlclass, po->expire_time-now());
						} else {
							elog_notify (0, "%ld: %s: Waiting...\n", ep->myevid, po->perlclass);
						}
					}
				}
			}
			goto PROCESS_WFTHREADS;
		}

		if (type == TYPE_DB) {
			(dbin.record)++;
			if (dbin.record >= ndbin) break;
		}

		if (number > 0 && num >= number) break;
		if (!wait && pktidin == pktidnewest) break;
	}

	exit (0);
}

#ifdef linux

/*
 * ctype_b.c
 *
 * This file has been added to compensate buggy object files
 * using __ctype_b, __ctype_tolower and __ctype_toupper.
 *
 * This file only allows the linking of this buggy object files,
 * there is no gurantee, that it will work correct. Linking of
 * object files is only possible with the same glibc as they are
 * compiled with.
 *
 * Compile this file with -fPIC and add it to the link stage of
 * your application.
 *
 */

#include <stdint.h>

#define CTYPE_EXTERN_INLINE /* Define real functions for accessors.  */
#include <ctype.h>

static const char _nl_C_LC_CTYPE_class[768] =
  /* 0x80 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x86 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x8c */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x92 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x98 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x9e */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xa4 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xaa */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xb0 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xb6 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xbc */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xc2 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xc8 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xce */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xd4 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xda */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xe0 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xe6 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xec */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xf2 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xf8 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xfe */ "\000\000" "\000\000" "\002\000" "\002\000" "\002\000" "\002\000"
  /* 0x04 */ "\002\000" "\002\000" "\002\000" "\002\000" "\002\000" "\003\040"
  /* 0x0a */ "\002\040" "\002\040" "\002\040" "\002\040" "\002\000" "\002\000"
  /* 0x10 */ "\002\000" "\002\000" "\002\000" "\002\000" "\002\000" "\002\000"
  /* 0x16 */ "\002\000" "\002\000" "\002\000" "\002\000" "\002\000" "\002\000"
  /* 0x1c */ "\002\000" "\002\000" "\002\000" "\002\000" "\001\140" "\004\300"
  /* 0x22 */ "\004\300" "\004\300" "\004\300" "\004\300" "\004\300" "\004\300"
  /* 0x28 */ "\004\300" "\004\300" "\004\300" "\004\300" "\004\300" "\004\300"
  /* 0x2e */ "\004\300" "\004\300" "\010\330" "\010\330" "\010\330" "\010\330"
  /* 0x34 */ "\010\330" "\010\330" "\010\330" "\010\330" "\010\330" "\010\330"
  /* 0x3a */ "\004\300" "\004\300" "\004\300" "\004\300" "\004\300" "\004\300"
  /* 0x40 */ "\004\300" "\010\325" "\010\325" "\010\325" "\010\325" "\010\325"
  /* 0x46 */ "\010\325" "\010\305" "\010\305" "\010\305" "\010\305" "\010\305"
  /* 0x4c */ "\010\305" "\010\305" "\010\305" "\010\305" "\010\305" "\010\305"
  /* 0x52 */ "\010\305" "\010\305" "\010\305" "\010\305" "\010\305" "\010\305"
  /* 0x58 */ "\010\305" "\010\305" "\010\305" "\004\300" "\004\300" "\004\300"
  /* 0x5e */ "\004\300" "\004\300" "\004\300" "\010\326" "\010\326" "\010\326"
  /* 0x64 */ "\010\326" "\010\326" "\010\326" "\010\306" "\010\306" "\010\306"
  /* 0x6a */ "\010\306" "\010\306" "\010\306" "\010\306" "\010\306" "\010\306"
  /* 0x70 */ "\010\306" "\010\306" "\010\306" "\010\306" "\010\306" "\010\306"
  /* 0x76 */ "\010\306" "\010\306" "\010\306" "\010\306" "\010\306" "\004\300"
  /* 0x7c */ "\004\300" "\004\300" "\004\300" "\002\000" "\000\000" "\000\000"
  /* 0x82 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x88 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x8e */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x94 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0x9a */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xa0 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xa6 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xac */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xb2 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xb8 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xbe */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xc4 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xca */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xd0 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xd6 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xdc */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xe2 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xe8 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xee */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xf4 */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
  /* 0xfa */ "\000\000" "\000\000" "\000\000" "\000\000" "\000\000" "\000\000"
;

static const uint32_t _nl_C_LC_CTYPE_toupper[384] =
{
  /* 0x80 */ 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
  /* 0x88 */ 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
  /* 0x90 */ 0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
  /* 0x98 */ 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
  /* 0xa0 */ 0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
  /* 0xa8 */ 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
  /* 0xb0 */ 0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
  /* 0xb8 */ 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
  /* 0xc0 */ 0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
  /* 0xc8 */ 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
  /* 0xd0 */ 0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
  /* 0xd8 */ 0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
  /* 0xe0 */ 0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
  /* 0xe8 */ 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
  /* 0xf0 */ 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
  /* 0xf8 */ 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xffffffff,
  /* 0x00 */ 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  /* 0x08 */ 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  /* 0x10 */ 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  /* 0x18 */ 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
  /* 0x20 */ 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
  /* 0x28 */ 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
  /* 0x30 */ 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
  /* 0x38 */ 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
  /* 0x40 */ 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
  /* 0x48 */ 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
  /* 0x50 */ 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
  /* 0x58 */ 0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
  /* 0x60 */ 0x60, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
  /* 0x68 */ 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
  /* 0x70 */ 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
  /* 0x78 */ 0x58, 0x59, 0x5a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f,
  /* 0x80 */ 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
  /* 0x88 */ 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
  /* 0x90 */ 0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
  /* 0x98 */ 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
  /* 0xa0 */ 0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
  /* 0xa8 */ 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
  /* 0xb0 */ 0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
  /* 0xb8 */ 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
  /* 0xc0 */ 0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
  /* 0xc8 */ 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
  /* 0xd0 */ 0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
  /* 0xd8 */ 0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
  /* 0xe0 */ 0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
  /* 0xe8 */ 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
  /* 0xf0 */ 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
  /* 0xf8 */ 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff
};

static const uint32_t _nl_C_LC_CTYPE_tolower[384] =
{
  /* 0x80 */ 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
  /* 0x88 */ 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
  /* 0x90 */ 0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
  /* 0x98 */ 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
  /* 0xa0 */ 0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
  /* 0xa8 */ 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
  /* 0xb0 */ 0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
  /* 0xb8 */ 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
  /* 0xc0 */ 0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
  /* 0xc8 */ 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
  /* 0xd0 */ 0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
  /* 0xd8 */ 0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
  /* 0xe0 */ 0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
  /* 0xe8 */ 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
  /* 0xf0 */ 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
  /* 0xf8 */ 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xffffffff,
  /* 0x00 */ 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
  /* 0x08 */ 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
  /* 0x10 */ 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
  /* 0x18 */ 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
  /* 0x20 */ 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
  /* 0x28 */ 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
  /* 0x30 */ 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
  /* 0x38 */ 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
  /* 0x40 */ 0x40, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
  /* 0x48 */ 0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
  /* 0x50 */ 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
  /* 0x58 */ 0x78, 0x79, 0x7a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
  /* 0x60 */ 0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
  /* 0x68 */ 0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
  /* 0x70 */ 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
  /* 0x78 */ 0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f,
  /* 0x80 */ 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
  /* 0x88 */ 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
  /* 0x90 */ 0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
  /* 0x98 */ 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
  /* 0xa0 */ 0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
  /* 0xa8 */ 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
  /* 0xb0 */ 0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
  /* 0xb8 */ 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
  /* 0xc0 */ 0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
  /* 0xc8 */ 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
  /* 0xd0 */ 0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
  /* 0xd8 */ 0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
  /* 0xe0 */ 0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
  /* 0xe8 */ 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
  /* 0xf0 */ 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
  /* 0xf8 */ 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff
};

#define b(t,x,o) (((const t *) _nl_C_LC_CTYPE_##x) + o)
const unsigned short int *__ctype_b = b (unsigned short int, class, 128);
const __int32_t *__ctype_tolower = b (__int32_t, tolower, 128);
const __int32_t *__ctype_toupper = b (__int32_t, toupper, 128);

#endif
