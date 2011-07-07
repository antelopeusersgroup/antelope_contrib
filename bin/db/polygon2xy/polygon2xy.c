/*
 * Nikolaus Horn, ZAMG / Vienna 2005-02-16
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>

#include "coords.h"
#include "db.h"
#include "stock.h"
#include "polygon.h"

static void
usage()
{
	char           *usage = "[-subset expr] db";
	char           *version = "1.0";
	char           *author = "Nikolaus Horn";
	char           *location = "ZAMG / Vienna";
	char           *email = "Nikolaus.Horn@zamg.ac.at";
	cbanner(version, usage, author, location, email);
	exit(1);
}

int
main(int argc, char **argv)
{
	int             c, verbose = 0, errflg = 0;


	char           *dbname= NULL;
	Point          *poly;
	long             nvertices;

	Dbptr           db;
	long             i;
	long 		 	nrecs;
	char           *subset_expr=NULL;
	char            pname[STRSZ], closed[STRSZ];

	elog_init(argc, argv);

	for (argc--, argv++; argc > 1; argc--, argv++) {
		if (!strcmp(*argv, "-subset")) {
			argc--;
			argv++;
			if (argc < 1) {
				elog_complain(0, "Need -subset argument.\n");
				usage();
				exit(1);
			}
			subset_expr = *argv;
		} else if (!strcmp(*argv, "-v")) {
			verbose++;
		} else if (**argv != '-') {
			break;
		} else {
			 elog_complain(0, "Unrecognized argument '%s'.\n",
			 *argv); usage(); exit(1);
				usage();
				exit(1);
		}
	}
	if (argc < 1) {
		elog_complain(0, "Need db argument.\n");
		usage();
		exit(1);
	}
	dbname = *argv;
	argc--;
	argv++;

	dbopen_database(dbname, "r", &db);
	if (db.table <0) {
		db=dblookup(db,0,"polygon",0,0);
	}
	if (subset_expr) {
		db=dbsubset(db,subset_expr,0);
	}

	dbquery(db, dbRECORD_COUNT, &nrecs);
	for (db.record = 0; db.record < nrecs; db.record++) {
		nvertices = readPolygon(db, &poly);
		if (nvertices > 0) {
			dbgetv(db, 0, "pname", &pname, "closed", &closed, NULL );
			printf(">\n");
			for (i = 0; i < nvertices; i++) {
				printf("%.4f %.4f\n", poly[i].lon, poly[i].lat);
			}
			if (yesno(closed) == -1 &&
			    ((poly[0].lat != poly[nvertices - 1].lat) ||
			     (poly[0].lon != poly[nvertices - 1].lon))) {
				printf("%.4f %.4f\n", poly[0].lon, poly[0].lat);
			}
		}
	}
	dbclose(db);
	return (0);
}
