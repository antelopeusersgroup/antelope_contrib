
#include <stdio.h>

#include "db.h"
#include "stock.h"

extern int chksac ( Dbptr db );

static void
usage ()
{
    fprintf (stderr, "\nUsage: %s ", Program_Name);
    fprintf (stderr, " database\n");
    banner (Program_Name, 0) ;
    exit (1);
}


int
main (int argc, char **argv)
{
    int             c,
                    errflg = 0;
    char           *database_name;
    long            n;
    Dbptr           db,
                    db2;

    Program_Name = argv[0];

    while ((c = getopt (argc, argv, "hvV")) != -1) {
	switch (c) {
	  case 'h':
	    usage ();
	    break;

	  case 'V':
	    banner (Program_Name, 0) ;
	    exit (0);

	  default:
	    errflg++;
	}
    }
    if (errflg || argc - optind != 1)
	usage ();

    database_name = argv[optind];

    dbopen (database_name, "r", &db);
    db = dblookup (db, 0, "wfdisc", 0, 0);
    db2 = dblookup (db, 0, "sensor", 0, 0);
    db = dbjoin (db, db2, 0, 0, 0, 0, 0);

    db2 = dblookup (db, 0, "sitechan", 0, 0);
    db = dbjoin (db, db2, 0, 0, 0, 0, 0);

    db2 = dblookup (db, 0, "site", 0, 0);
    db = dbjoin (db, db2, 0, 0, 0, 0, 0);

    db2 = dblookup (db, 0, "wftag", 0, 0);
    db = dbjoin (db, db2, 0, 0, 0, 0, 0);

    db2 = dblookup (db, 0, "origin", 0, 0);
    db = dbtheta (db, db2, "tagid==orid", 0, 0);

    dbquery (db, dbRECORD_COUNT, &n);
    for (db.record = 0; db.record < n; db.record++)
	chksac (db);

    return 0;
}
