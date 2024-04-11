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
	char           *usage = "[-v] [-n] [-close_polygons] [-ptype ts] [-pname name]\n\t\t[-level level] [-auth author]\n\t\t[-dir dir] [-dfile dfile] datafile db";
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
	int             verbose = 0;
	int             names_from_headers = 0;


	char           dbname[STRSZ];
	char           datafilename[STRSZ];
	char           name[STRSZ];
	char           line[STRSZ];
	char            str1[20], str2[20], *str3 = malloc(1024);
	int             level = 0;
	int             closed = 0, close = 0;
	int             maxpoints = 50000;
	Point          *poly = malloc(2 * maxpoints * sizeof(double));
	double          lat, lon;

	long             npoints;
	char           auth[STRSZ];
	char            ptype[STRSZ];
	char           dir[STRSZ], dfile[STRSZ], tname[STRSZ], tstr[STRSZ], a[2], b[STRSZ],
	                numstr[10];
	int             ftype = polyFLOAT;
	Dbptr           db;
	FILE           *fh;
	int             putit;
	int             polycounter = 0;

	elog_init(argc, argv);


	my_username(auth);
	strcat(auth, ":gmt");
	strcpy(name, "-");
	strcpy(dir, ".");
	strcpy(dfile, "gmt.bin");
	strcpy(ptype, "-");
	for (argc--, argv++; argc > 0; argc--, argv++) {
		if (!strcmp(*argv, "-auth")) {
			argc--;
			argv++;
			if (argc < 3) {
				elog_complain(0, "Need -auth argument.\n");
				usage();
				exit(1);
			}
			strcpy(auth,*argv);
		} else if (!strcmp(*argv, "-dir")) {
			argc--;
			argv++;
			if (argc < 3) {
				elog_complain(0, "Need -dir argument.\n");
				usage();
				exit(1);
			}
			strcpy(dir, *argv);
            printf("dir: %s\n",dir);
		} else if (!strcmp(*argv, "-dfile")) {
			argc--;
			argv++;
			if (argc < 3) {
				elog_complain(0, "Need -dfile argument.\n");
				usage();
				exit(1);
			}
			strcpy(dfile, *argv);
		} else if (!strcmp(*argv, "-pname")) {
			argc--;
			argv++;
			if (argc < 3) {
				elog_complain(0, "Need -pname argument.\n");
				usage();
				exit(1);
			}
			strcpy(name, *argv);
		} else if (!strcmp(*argv, "-ptype")) {
			argc--;
			argv++;
			if (argc < 3) {
				elog_complain(0, "Need -ptype argument.\n");
				usage();
				exit(1);
			}
			strncpy(ptype, *argv, 2);
		} else if (!strcmp(*argv, "-level")) {
			argc--;
			argv++;
			if (argc < 3) {
				elog_complain(0, "Need -level argument.\n");
				usage();
				exit(1);
			}
			level = atoi(*argv);
		} else if (!strcmp(*argv, "-close_polygons")) {
			close = 1;
		} else if (!strcmp(*argv, "-v")) {
			verbose++;
		} else if (!strcmp(*argv, "-n")) {
			names_from_headers = 1;
		} else if (**argv != '-') {
			break;
		} else {
			elog_complain(0, "unrecognized argument '%s'\n", *argv);
			usage();
			exit(1);
		}
	}
	if (argc < 2) {
		elog_complain(0, "Need datfile and db arguments.\n");
		usage();
		exit(1);
	}
	strcpy(datafilename, *argv);
	argc--;
	argv++;
	strcpy(dbname, *argv);
	if (access(datafilename, 4)) {
		elog_die(0, "datafile %s should be READABLE!\n", datafilename);
	}
	if (dbcreate(dbname, "polygon1.2", 0, 0, 0, 0, 0)) {
		elog_die(1, "can't create database %s\n", dbname);
	}
	if (!strcmp(name, "-")) {
		strncpy(name, datafilename, strnlen(datafilename,20));
	}
	dbopen(dbname, "r+", &db);
	db = dblookup(db, 0, "polygon", 0, 0);
	fh = fopen(datafilename, "r");
	putit = 0;
	npoints = 0;
	polycounter = 0;
	while (fgets(line, 80, fh) != 0) {
		if (line[0] == '>') {
            strsub(line,"> ","",tname);
			if (putit) {
                if (!names_from_headers) {
                    strcpy(tname, name);
                    if (polycounter > 0) {
                        sprintf(numstr, ".%d", polycounter);
                        strcat(tname, numstr);
                    }
                }
                printf("tname: %s\n",tname);
				if (verbose) {
					elog_notify(0, "%s: (%ld points to file %s/%s)\n", tname, npoints, dir, dfile);
				}
				if (npoints > 2 && (close || (
				 (poly[0].lat == poly[npoints - 1].lat) &&
				(poly[0].lon == poly[npoints - 1].lon)))) {
					closed = 1;
				}
                if (names_from_headers) {
                    writePolygonData(db, poly, npoints, line, closed, level, ptype, auth, dir, dfile, ftype);
                } else {
                    writePolygonData(db, poly, npoints, tname, closed, level, ptype, auth, dir, dfile, ftype);
                }
				closed = 0;
				polycounter++;
			}
			strtrim(line);
			npoints = 0;
			putit = 1;
		} else {
			sscanf(line, "%s %s %s", str1, str2, str3);
			lon = atof(str1);
			lat = atof(str2);
			if (npoints > maxpoints - 2) {
				maxpoints += 5000;
				poly = realloc(poly, 2 * maxpoints);
			}
			poly[npoints].lat = lat;
			poly[npoints].lon = lon;
			npoints++;
		}
	}
	if (npoints > 0) {
        if (!names_from_headers) {
            strcpy(tname, name);
            if (polycounter > 0) {
                sprintf(numstr, ".%d", polycounter);
                strcat(tname, numstr);
            }
        }
        printf("tname: %s\n",tname);
		if (verbose) {
			elog_notify(0, "%s: (%ld points n file %s/%s)\n", tname, npoints, dir, dfile);
		}
		if (npoints > 2 && (close || (
				 (poly[0].lat == poly[npoints - 1].lat) &&
			       (poly[0].lon == poly[npoints - 1].lon)))) {
			closed = 1;
		}
		writePolygonData(db, poly, npoints, tname, closed, level, ptype, auth, dir, dfile, ftype);
		closed = 0;
	}
	fclose(fh);
	elog_print(0, 0);
	return 0;
}
