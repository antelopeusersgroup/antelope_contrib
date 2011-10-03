/*
 * Nikolaus Horn, ZAMG / Vienna 2005-02-16
 * 
 */


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "Pkt.h"
#include "brttutil.h"
#include "bury.h"
#define NEW_TABLE 0
#define TABLE_SEEN 1
#define MAX_TABLES_IN_DB	200

int             Stop = 0;
char           *dbname;
double         *bury_times;
int            *static_flags;
long            ntables;
int             verbose = 0;

void
mortician()
{
	if (verbose > 1)
		elog_notify(0, "saving state\n");
}
/* poor man's searchtbl */
static int
findtbl(char *string, Tbl * table)
{
	int             i, found = 0;
	char           *str;

	for (i = 0; i < maxtbl(table); i++) {
		str = gettbl(table, i);
		if (strcmp(string, str) == 0) {
			found = 1;
			break;
		}
	}
	return (found);
}
static void
usage()
{
	cbanner("$Date$",
		"[-sleep seconds] [-pf pfname] [-state statefile]\n\t\t\t[-prefix prefix] [-modified_after time] [-v] db orb",
		"Nikolaus Horn",
		"ZAMG / Vienna",
		"nikolaus.horn@zamg.ac.at");
	exit(1);
}
static int
dbrows2orb(Dbptr db, int orb, char *prefix)
{
	Packet         *pkt;
	char            srcname[ORBSRCNAME_SIZE];
	double          time;
	char           *packet;
	int             nbytes, packetsize = 0;
	Dbptr           tmpdb;
	long            t, nrecords, r, ntables;
	Arr            *records = NULL;
	Tbl            *tables = NULL;
	char           *thistablename;
	Stbl           *stbl;
	char           *s;

	dbuntangle(db, &records);
	tables = keysarr(records);
	ntables = maxtbl(tables);
	if (ntables > 0)
		pkt = newPkt();
	if (prefix)
		strncpy(pkt->parts.src_net, prefix, PKT_TYPESIZE);
	pkt->pkttype = suffix2pkttype("db");
	for (t = 0; t < ntables; t++) {
		thistablename = gettbl(tables, t);
		tmpdb = dblookup(db, 0, thistablename, 0, 0);
		stbl = (Stbl *) getarr(records, thistablename);
		nrecords = maxstbl(stbl);
		if (nrecords > 0) {
			for (r = 0; r < nrecords; r++) {
				tmpdb.record = (long) getstbl(stbl, r);
				pkt->db = tmpdb;
				if (stuffPkt(pkt, srcname, &time, &packet, &nbytes, &packetsize) < 0) {
					elog_complain(0, "stuffPkt fails for pf packet");
					return (-1);
				}
				if (orbput(orb, srcname, time, packet, nbytes) < 0) {
					elog_complain(0, "Couldn't send packet to orb\n");
					return (-1);
				}
			}
		}
	}
	freetbl(tables, 0);
	dbfree_untangle(records);
	freePkt(pkt);
	if (verbose) {
		elog_notify(0, "%s: %ld patcket(s) sent with sourcename: %s\n", s = strtime(now()), nrecords, srcname);
		free(s);
	}
	return (0);




}
int
main(int argc, char **argv)
{
	double          modified_after =now() , last_lddate, last_mtime, mtime;
	char           *orbname = NULL;
	char           *dbname = NULL;
	int             orb;
	int             naptime = -1, check_lddate_interval = -1;
	Dbptr           db, dbt, dbs;
	char           *prefix = NULL;
	struct stat     filestat;
	int             i;
	Tbl            *tablenames, *tables_containing_dfile, *check_tables = NULL,
	               *ignore_tables = NULL;
	long            table_present, recc, is_view;
	char           *tablename, *schemaname;
	char           *filename;
	int             counter = 0, force_check = 0;
	char            expr[512];
	char           *statefilename = NULL, *pfname = "dbnew2orb";
	Pf             *pf = NULL;
	double          lastburytime;
	Relic           relic;
	char           *s;
	Expression     *expr_lddate;
	double         *mtimes;
	double         *lddates;

	elog_init(argc, argv);

	if (argc < 2) {
		usage();
		exit(1);
	}
	for (argc--, argv++; argc > 0; argc--, argv++) {
		if (!strcmp(*argv, "-modified_after")) {
			argc--;
			argv++;
			if (argc < 1) {
				elog_complain(0, "Need -modified_after argument.\n");
				usage();
				exit(1);
			}
			modified_after = str2epoch(*argv);
		} else if (!strcmp(*argv, "-prefix")) {
			argc--;
			argv++;
			if (argc < 1) {
				elog_complain(0, "Need -prefix argument.\n");
				usage();
				exit(1);
			}
			prefix = *argv;
		} else if (!strcmp(*argv, "-pf")) {
			argc--;
			argv++;
			if (argc < 1) {
				elog_complain(0, "Need -pf argument.\n");
				usage();
				exit(1);
			}
			pfname = *argv;
		} else if (!strcmp(*argv, "-state")) {
			argc--;
			argv++;
			if (argc < 1) {
				elog_complain(0, "Need -state argument.\n");
				usage();
				exit(1);
			}
			statefilename = *argv;
		} else if (!strcmp(*argv, "-sleep")) {
			argc--;
			argv++;
			if (argc < 1) {
				elog_complain(0, "Need -sleep argument.\n");
				usage();
				exit(1);
			}
			naptime = atoi(*argv);
		} else if (!strcmp(*argv, "-check_lddate_interval")) {
			argc--;
			argv++;
			if (argc < 1) {
				elog_complain(0, "Need -check_lddate_interval argument.\n");
				usage();
				exit(1);
			}
			check_lddate_interval = atoi(*argv);
		} else if (!strcmp(*argv, "-v")) {
			verbose++;
		} else if (**argv != '-') {
			break;
		} else {
			elog_complain(0, "Unrecognized argument '%s'.\n", *argv);
			usage();
			exit(1);
		}
	}


	if (pfread(pfname, &pf)) {
		elog_die(0, "parse_pf: pfread('%s') error.\n", pfname);
	}
	if (check_lddate_interval < 1) {
		if (parse_param(pf, "check_lddate_interval", P_LINT, 1, &check_lddate_interval) < 0) {
			elog_die(1, "parse_pf: sleep check_lddate_interval needed!\n");
		} else {
			if (check_lddate_interval < 0) {
				check_lddate_interval = 1;
			}
		}
	}
	if (naptime < 1) {
		if (parse_param(pf, "sleep", P_LINT, 1, &naptime) < 0) {
			elog_die(1, "parse_pf: sleep value needed!\n");
		} else {
			if (naptime < 0) {
				naptime = 1;
			}
		}
	}
	if (!prefix) {
		if (parse_param(pf, "prefix", P_STR, 0, &prefix) < 0) {
			printf("NO PREFIX!\n");
			prefix = NULL;
		}
	}
	parse_param(pf, "check_tables", P_TBL, 0, &check_tables);
	if (check_tables) {
		if (maxtbl(check_tables) < 1) {
			freetbl(check_tables, 0);
			check_tables = NULL;
		}
	}
	parse_param(pf, "ignore_tables", P_TBL, 0, &ignore_tables);
	if (ignore_tables) {
		if (maxtbl(ignore_tables) < 1) {
			freetbl(ignore_tables, 0);
			ignore_tables = NULL;
		}
	}
	/*
	 * no good here, would erase the table above pffree(pf);
	 */

	if (argc < 1) {
		elog_complain(0, "Need db argument.\n");
		usage();
		exit(1);
	}
	dbname = *argv;

	argc--;
	argv++;
	if (argc < 1) {
		elog_complain(0, "Need orb argument.\n");
		usage();
		exit(1);
	}
	orbname = *argv;
	argc--;
	argv++;
	if (argc > 0) {
		elog_complain(0, "Unrecognized argument '%s'.\n", *argv);
		usage();
		exit(1);
	}
	if (dbopen(dbname, "r", &db) < 0) {
		elog_complain(0, "Can't open database");
		exit(1);
	}
	dbquery(db, dbSCHEMA_NAME, &schemaname);
	orb = orbopen(orbname, "w&");
	if (orb < 0) {
		elog_die(0, "orbopen(%s) error\n", orbname);
	}
	/*
	 * prepare for later call to dbquery(dbFIELD_TABLES) to find only
	 * tables containing lddate
	 */

	/*
	 * dbtables is much better, does not require the existence of table
	 * origin dbf = dblookup(db, 0, "origin", "lddate", "dbNULL");
	 * dbquery(dbf, dbFIELD_TABLES, &tablenames);
	 */

	dbex_compile(db, "max(lddate)", &expr_lddate, dbTIME);
	tablenames = dbtables(db, "lddate");
	tables_containing_dfile = dbtables(db, "dfile");

	/* waste a few bytes... */
	ntables = maxtbl(tablenames);
	mtimes = malloc(ntables * sizeof(double));
	lddates = malloc(ntables * sizeof(double));
	bury_times = malloc(ntables * sizeof(double));
	static_flags = malloc(ntables * sizeof(long));
	if (statefilename) {
		if (exhume(statefilename, &Stop, 10, mortician)) {
			elog_notify(0, "read old state file\n");
		} else {
			elog_complain(0, "could not read old statefile\n");
		}
	}
	for (i = 0; i < ntables; i++) {
		/*
		 * mtimes[i] = modified_after; lddates[i] = modified_after;
		 */
		static_flags[i] = NEW_TABLE;
	}
	for (;;) {
		tablenames = dbtables(db, "lddate");

		for (i = 0; i < ntables; i++) {
			tablename = gettbl(tablenames, i);
			if (!tablename) {
				continue;
			}
			dbt = dblookup(db, 0, tablename, 0, 0);
			dbquery(dbt, dbTABLE_PRESENT, &table_present);
			if (!table_present) {
				continue;
			}
			dbquery(dbt, dbTABLE_IS_VIEW, &is_view);
			if (is_view) {
				continue;
			}
			/* lastid is not a good idea (my personal choice)... */
			if (strcmp(tablename, "lastid") == 0) {
				continue;
			}
			/* remove after Dan fixed the bug with remark */
			if (strcmp(tablename, "remark") == 0) {
				continue;
			}
			if (findtbl(tablename, tables_containing_dfile)) {
				continue;
			}
			if (check_tables) {
				if (!findtbl(tablename,check_tables)) {
					if (verbose > 1 && static_flags[i]==NEW_TABLE) elog_notify(0,"ignoring table %s because it's NOT in 'check_tables'\n",tablename);
					continue;
				}
			}
			if (ignore_tables) {
				if (findtbl(tablename,ignore_tables)) {
					if (verbose > 1 && static_flags[i]==NEW_TABLE) elog_notify(0,"ignoring table %s because it's in 'ignore_tables'\n",tablename);
					continue;
				}
			}
			dbquery(dbt, dbRECORD_COUNT, &recc);
			if (recc < 1) {
				continue;
			}
			if (statefilename) {
			if (static_flags[i] == NEW_TABLE) {
				relic.dp = &bury_times[i];
				if (resurrect(tablename, relic, TIME_RELIC) == 0) {
					mtimes[i] = bury_times[i];
					lddates[i] = bury_times[i];
					if (verbose > 1) {
						elog_notify(0, "resurrection successful: check %s after %s\n", tablename, s = strtime(bury_times[i]));
						free(s);
					}
				} else {
					bury_times[i] = modified_after;
					mtimes[i] = modified_after;
					lddates[i] = modified_after;
					if (verbose > 1) {
						elog_notify(0, "resurrection unsuccessful: check %s after %s\n", tablename, s = strtime(modified_after));
						free(s);
					}
				}
				static_flags[i] = TABLE_SEEN;
			}
			} else {
				if (static_flags[i] == NEW_TABLE) {
					bury_times[i] = modified_after;
					mtimes[i] = modified_after;
					lddates[i] = modified_after;
					static_flags[i] = TABLE_SEEN;
				}
			}
			dbquery(dbt, dbTABLE_FILENAME, &filename);
			if (stat(filename, &filestat) < 0) {
				elog_die(1, "stat(%s) error.\n", filename);
			}
			last_mtime = mtimes[i];
			last_lddate = lddates[i];

			mtime = filestat.st_mtime;
			/*
			 * the whole mtime stuff is not soo good: mtime is
			 * typically > lddate, so setting modified_after to
			 * mtime will certainly ignore the last value. To get
			 * everything, I will have to keep 2 arrays: mtimes
			 * to detect file modifications and lddates to get
			 * the actual entries...
			 */
			if (force_check || mtime > last_mtime) {
				sprintf(expr, "lddate > %f", last_lddate);
				dbs = dbsubset(dbt, expr, 0);
				dbquery(dbs, dbRECORD_COUNT, &recc);
				if (recc > 0) {
					if (dbrows2orb(dbs, orb, prefix) == 0) {
						/*
						 * dbex_evalstr(dbs,
						 * "max(lddate)", dbTIME,
						 * &lddates[i]);
						 */
						dbex_eval(dbs, expr_lddate, 0, &lddates[i]);
						mtimes[i] = mtime;
						bury_times[i] = lddates[i];
					}
				}
				dbfree(dbs);
			}
			/*
			 * a call to dbfree(dbt) would remove it from the
			 * list of tablenames, all later calls to tablename
			 * would return NIL...
			 */
			if (Stop) {
				bury();
				return (0);
			}
		}
		sleep(naptime);
		if ((counter + 1) >= check_lddate_interval) {
			counter = 0;
			force_check = 1;
		} else {
			force_check = 0;
			counter++;
		}
		if (statefilename) {
			double          nowtime;

			nowtime = now();
			if (nowtime - lastburytime > 600.0) {
				lastburytime = nowtime;
				bury();
			}
		}
	}
}
