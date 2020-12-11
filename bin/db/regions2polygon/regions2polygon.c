#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>

#include "coords.h"
#include "db.h"
#include "stock.h"
#include "polygon.h"

static void usage ()
{
    char *usage = "[-v] dbin dbout";
    char *version = "1.1";
    char *author = "Nikolaus Horn";
    char *location = "ZAMG / Vienna";
    char *email = "Nikolaus.Horn@zamg.ac.at";
    cbanner (version, usage, author, location, email);
    exit (1);
}

int main (int argc, char **argv)
{
    int c, verbose = 0, errflg = 0;


    char *dbinname = malloc (1024);
    char *dboutname = malloc (1024);
    Point *poly;
    double lat, lon;
    char *auth = strdup ("regions2polygon");
    char *ptype = strdup ("rp");
    char *dir = strdup (".");
    char *dfile = strdup ("polygons");
    int ftype = polyFLOAT;
    char *name = malloc (100);
    long nregions, nvertices;
    Tbl *sortkeys, *groupkeys;

    Dbptr dbin, dbout, dbi, dbo, dbg, dbs, dbb;
    long i, from, to, nv;

    elog_init (argc, argv);
    while ((c = getopt (argc, argv, "vV")) != -1) {
        switch (c) {

        case 'v':
            verbose++;
            break;

        case 'V':
            usage ();
            break;

        case '?':
            errflg++;
            break;
        }
    }

    if ((errflg) || argc < 3)
        usage ();

    dbinname = argv[optind++];
    dboutname = argv[optind++];

    if (dbopen (dbinname, "r", &dbin)) {
        elog_die (1, "cannot open database %s", dbinname);
    }
    dbi = dblookup (dbin, 0, "regions", 0, 0);



    sortkeys = newtbl (2);
    pushtbl (sortkeys, "regname");
    pushtbl (sortkeys, "vertex");
    groupkeys = newtbl (1);
    pushtbl (groupkeys, "regname");

    dbs = dbsort (dbi, sortkeys, 0, "regions.sorted");
    dbg = dbgroup (dbs, groupkeys, 0, 0);
    dbquery (dbg, dbRECORD_COUNT, &nregions);
    if (nregions < 1) {
        elog_die (0, "table regions seems to be empty (or not present)");
    }

    if (verbose)
        elog_notify (0, "creating database descriptor %s", dboutname);

    if (dbcreate (dboutname, "polygon1.2", 0, 0, 0)) {
        elog_die (1, "cannot create database %s", dboutname);
    }
    dbopen (dboutname, "r+", &dbout);
    dbo = dblookup (dbout, 0, "polygon", 0, 0);

    for (i = 0; i < nregions; i++) {
        dbg.record = i;
        dbgetv (dbg, 0, "regname", name, "bundle", &dbb, NULL);
        dbget_range (dbb, &from, &to);
        nvertices = to - from;
        if (verbose)
            elog_notify (0, "%s (%li nvertices)", name, nvertices);
        poly = malloc (2 * nvertices * sizeof (double));
        nv = 0;

        for (dbs.record = from; dbs.record < to; dbs.record++) {
            dbgetv (dbs, 0, "regname", name,
                    //"vertex",&vertex,
                    "lat", &lat, "lon", &lon, NULL);
            poly[nv].lat = lat;
            poly[nv].lon = lon;
            nv++;
        }
        writePolygonData (dbo, poly, nv, name, 1, 0, ptype, auth, dir, dfile,
                          ftype);
        free (poly);
    }
    /*
     */
    return 0;
}
