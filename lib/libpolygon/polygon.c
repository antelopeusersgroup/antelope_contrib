#include <limits.h>
#include <math.h>
#include "stock.h"
#include "coords.h"
#include "polygon.h"
#include "swapbytes.h"

static int      signed_crossing_number (Point P1, Point P2)
{
    double          xintercept, slope, direction;

    if ((P1.lon * P2.lon) > 0.0) {
        /* no crossing, both points are on same side of y axis */
        return 0;
    }
    /* horizontal lines */
    if ((P1.lon == 0.0) && (P2.lon == 0.0)) {
        if ((P1.lat * P2.lat) > 0.0) {
            /* no crossing */
            return 0;
        } else {
            /* segment crosses or touches origin */
            return 4;
        }
    }
    if (P2.lat == P1.lat) {
        /* vertical lines */
        xintercept = P1.lat;
    } else {
        slope = (P2.lon - P1.lon) / (P2.lat - P1.lat);
        xintercept = P1.lat - (P1.lon / slope);
    }
    if (P2.lon > P1.lon) {
        direction = 2;
    } else {
        direction = -2;
    }

    if (xintercept > 0.0) {
        return 0;
    } else if (xintercept == 0.0) {
        return 4;
    } else if ((P1.lon == 0.0) || (P2.lon == 0.0)) {
        return (int)(direction / 2.0);
    } else {
        return direction;
    }
}
static int      winding_number (Point * polygon, long n)
{
    int             wn = 0;
    int             scn;
    long            i;
    Point           P1, P2;

    for (i = 0; i < n; i++) {
        P1.lat = polygon[i].lat;
        P1.lon = polygon[i].lon;
        P2.lat = polygon[i + 1].lat;
        P2.lon = polygon[i + 1].lon;
        scn = signed_crossing_number (P1, P2);
        if (scn == 4) {
            /* point is ON polygon */
            return 2;
        } else {
            wn += scn;
        }

    }
    return abs (wn / 2);

}
static Point   *shift_polygon (Point P, Point * polygon, long n)
{
    Point          *shifted = malloc (sizeof (Point) * n);
    long            i;

    for (i = 0; i < n; i++) {
        shifted[i].lat = polygon[i].lat - P.lat;
        shifted[i].lon = polygon[i].lon - P.lon;
    }
    return shifted;
}
static int      is_inside_polygon (Point P, Point * polygon, long n)
{
    int             wn;
    Point          *copy = malloc ((n + 1) * sizeof (Point));

    memcpy (copy, polygon, n * sizeof (Point));
    /* as we are searching for inclusion, close the polygon... */
    if ((copy[0].lat == copy[n - 1].lat) && (copy[0].lon == copy[n - 1].lon)) {
        n--;
    } else {
        copy[n].lat = copy[0].lat;
        copy[n].lon = copy[0].lon;
    }

    if ((P.lat != 0.0) || (P.lon != 0.0)) {
        copy = shift_polygon (P, copy, n);
    }
    wn = winding_number (copy, n);
    free (copy);
    if (wn == 0) {
        return 0;
    } else {
        return 1;
    }
}

int             isGeographicallyInside (Point P, Point * polygon, long n)
{

    long            i;
    double          distance, azi;
    Point           P_null;
    Point          *np = malloc (n * sizeof (Point));
    int             retval;

    P_null.lat = 0.0;
    P_null.lon = 0.0;

    for (i = 0; i < n; i++) {
        dist (P.lat, P.lon, polygon[i].lat, polygon[i].lon, &distance, &azi);
        if (distance >= 90.0) {
            elog_log (0, "is_geopgraphically_inside: Polygon and test point must be entirely within the same hemisphere!\n");
            free (np);
            return 0;
        }
        /*
         * both versions are not really needed, the simplest projection ever
         * also works (and saves time)...   polygon[i].lat=distance * sin(azi *
         * Pi / 180.0); polygon[i].lon=distance * cos(azi * Pi / 180.0);
         * latlon(0.0,0.0,distance,azi,&(polygon[i].lat),&(polygon[i].lon));
         */
        np[i].lat = polygon[i].lat - P.lat;
        np[i].lon = polygon[i].lon - P.lon;

    }
    retval = is_inside_polygon (P_null, np, n);
    free (np);
    return retval;
}

typedef struct PolyCodeType {
    char            type[3];
    int             code;
}               PolyCodeType;
static PolyCodeType PolyCodes[] = {
    {"s4", polyINT},
    {"i4", polyINTELINT},
    {"gs", polyGSHHS},
    {"t4", polyFLOAT},
    {"u4", polyINTELFLOAT}
};

static int      nPolyTypes = sizeof (PolyCodes) / sizeof (PolyCodeType);

int             polycode (char *type)
{
    int             i;

    for (i = 0; i < nPolyTypes; i++) {
        if (strcmp (type, PolyCodes[i].type) == 0)
            return PolyCodes[i].code;
    }
    return -1;
}

char           *polytype (int code)
{
    int             i;

    for (i = 0; i < nPolyTypes; i++) {
        if (code == PolyCodes[i].code)
            return PolyCodes[i].type;
    }
    return "-";
}

typedef union {
    char           *c;
    short          *s;
    int            *i;
    float          *f;
    double         *d;
}               Data;

long            readPolygon (Dbptr db, Point ** Poly)
{
    char            fname[PATH_MAX];
    char            ftype[6];
    long            npoints;
    long            foff;
    int             pcode;
    FILE           *df;

    Data            dp;         /* union of pointers, a pointer */
    long            i;

    if (dbextfile (db, 0, fname) < 1) {
        elog_log (0, "readPolygon: problem finding data in %s (are dir and dfile set properly?)", fname);
        return -1;
    }
    if (is_file (fname) != 1) {
        elog_log (0, "readPolygon: file %s not found!", fname);
        return -1;
    }
    dbgetv (db, 0, "ftype", &ftype, "npoints", &npoints, "foff", &foff, NULL);


    if ((pcode = polycode (ftype)) == -1) {
        elog_log (0, "readPolygon: unknown storage format %s", ftype);
        return -1;
    }
    *Poly = malloc ((npoints + 2) * sizeof (Point));
    dp.c = malloc (8 * 2 * (npoints + 2));
    df = fopen (fname, "r");
    fseek (df, foff, 0);

    switch (pcode) {
    case polyGSHHS:
        break;
    case polyINT:
        /*
         * fread( rawdata,1,2 * sizeof(int) * npoints,df);
         * N2H4(rawdata,dp.i,2 * npoints);
         */
        fread (dp.i, sizeof (int), npoints * 2, df);
        N2H4 ((char *)dp.i, (char *)dp.i, 2 * npoints);
        for (i = 0; i < npoints; i++) {
            (*Poly)[i].lon = dp.i[i * 2] / 1.0e6;
            (*Poly)[i].lat = dp.i[i * 2 + 1] / 1.0e6;
        }

        break;
    case polyINTELINT:
        fread (dp.i, sizeof (int), 2 * npoints, df);
#ifdef WORDS_BIGENDIAN
        swap4 (dp.i, dp.i, 2 * npoints);
#endif
        for (i = 0; i < npoints; i++) {
            (*Poly)[i].lon = dp.i[i * 2] / 1.0e6;
            (*Poly)[i].lat = dp.i[i * 2 + 1] / 1.0e6;
        }

        break;
    case polyFLOAT:
        fread (dp.f, sizeof (float), 2 * npoints, df);
        N2H4 ((char *)dp.f, (char *)dp.f, 2 * npoints);
        for (i = 0; i < npoints; i++) {
            (*Poly)[i].lon = dp.f[i * 2];
            (*Poly)[i].lat = dp.f[i * 2 + 1];
        }
        break;
    case polyINTELFLOAT:
        fread (dp.f, sizeof (float), 2 * npoints, df);
#ifdef WORDS_BIGENDIAN
        swap4 ((char *)dp.f, (char *)dp.f, 2 * npoints);
#endif
        for (i = 0; i < npoints; i++) {
            (*Poly)[i].lon = dp.f[i * 2];
            (*Poly)[i].lat = dp.f[i * 2 + 1];
        }
        break;
    default:
        elog_log (0, "readPolygon: no code for storage format %s", ftype);
        free (dp.c);
        fclose (df);
        return -1;
    }
    free (dp.c);
    fclose (df);
    return npoints;
}

long            writePolygonData (Dbptr db, Point * poly, long npoints, char *pname, int closed, int level, char *ptype, char *auth, char *dir, char *dfile, int pcode)
{
    /*
     * writes poygon data into file if dir and o dfile are NULL, use defaults
     * returns record number if successful,dbINVALID else
     */
    long            i;
    char            datafilename[PATH_MAX];
    FILE           *dfh;
    int             foff;
    char           *ftype;
    int             int1, int2;
    float           float1, float2;
    double          north = -360.0, east = -360.0, west = 360.0, south = 360.0;
    double          lat, lon;
    int             pid;
    int             free_dir = 0;
    int             free_dfile = 0;
    char            closed_flag[2];

    if (closed) {
        strcpy (closed_flag, "y");
    } else {
        strcpy (closed_flag, "n");
    }

    pid = dbnextid (db, "pid");

    db = dblookup (db, 0, "polygon", 0, 0);
    if ((db.record = dbaddnull (db)) == dbINVALID) {
        elog_log (0, "writePolygonData: problem adding row...");
        return dbINVALID;
    }
    if (dir == NULL) {
        dir = strdup (default_dir);
        free_dir = 1;
    }
    if (dfile == NULL) {
        dfile = strdup (default_dfile);
        free_dfile = 0;
    }
    if (dbputv (db, 0, "pid", pid, "dir", dir, "dfile", dfile, NULL) == dbINVALID) {
        elog_log (0, "writePolygonData: error putting dir %s & dfile %s", dir, dfile);
        if (free_dir)
            free (dir);
        if (free_dfile)
            free (dfile);
        return dbINVALID;
    }
    /* some fix needed here when in "relative" directories... */
    switch (dbfilename (db, datafilename)) {
    case -1:
        if (makedir (dir) < 0) {
            elog_log (0, "writePolygonData: dir %s is NOT writable!", dir);
            if (free_dir)
                free (dir);
            if (free_dfile)
                free (dfile);
            return dbINVALID;
        }
        if ((dfh = fopen (datafilename, "w")) == NULL) {
            elog_log (0, "writePolygondata: error crating %s/%s", dir, dfile);
            if (free_dir)
                free (dir);
            if (free_dfile)
                free (dfile);
            return dbINVALID;
        }
        foff = 0;
        break;
    case 0:
        /* make file */
        if (makedir (dir) < 0) {
            elog_log (0, "problem creating directory %s", dir);
            if (free_dir)
                free (dir);
            if (free_dfile)
                free (dfile);
            return dbINVALID;
        }
        if ((dfh = fopen (datafilename, "w")) == NULL) {
            elog_log (0, "problem creating file %s", dir);
            if (free_dir)
                free (dir);
            if (free_dfile)
                free (dfile);
            return dbINVALID;
        }
        foff = 0;
        break;
    case 1:
        /* append */
        dfh = fopen (datafilename, "a");
        fseek (dfh, 0, 2);
        foff = ftell (dfh);
        break;
    case 2:
        /* .Z exists.... */
        elog_log (0, "writePolygonData: compression not yet supported...");
        if (free_dir)
            free (dir);
        if (free_dfile)
            free (dfile);
        return dbINVALID;
        break;
    default:
        elog_log (0, "writePolygondata: unknown return value from dbfilename, giving up...");
        if (free_dir)
            free (dir);
        if (free_dfile)
            free (dfile);
        return dbINVALID;

    }
    ftype = polytype (pcode);

    switch (pcode) {
    case polyINT:
        for (i = 0; i < npoints; i++) {
            lon = poly[i].lon;
            west = (lon < west) ? lon : west;
            east = (lon > east) ? lon : east;
            int1 = lon * 1e6;
            H2N4 ((char *)(&int2), (char *)(&int1), 1);
            fwrite (&int2, sizeof (int), 1, dfh);

            lat = poly[i].lat;
            south = (lat < south) ? lat : south;
            north = (lat > north) ? lat : north;
            int1 = lat * 1e6;
            H2N4 ((char *)(&int2), (char *)(&int1), 1);
            fwrite (&int2, sizeof (int), 1, dfh);
        }
        fclose (dfh);
        break;
    case polyINTELINT:
        for (i = 0; i < npoints; i++) {
            lon = poly[i].lon;
            west = (lon < west) ? lon : west;
            east = (lon > east) ? lon : east;
            int1 = lon * 1e6;
#ifdef WORDS_BIGENDIAN
            swap4 (&int1, &int2, 1);
#else
            int2 = int1;
#endif
            fwrite (&int2, sizeof (int), 1, dfh);

            lat = poly[i].lat;
            south = (lat < south) ? lat : south;
            north = (lat > north) ? lat : north;
            int1 = lat * 1e6;
#ifdef WORDS_BIGENDIAN
            swap4 (&int1, &int2, 1);
#else
            int2 = int1;
#endif
            fwrite (&int2, sizeof (int), 1, dfh);
        }
        fclose (dfh);
        break;
    case polyFLOAT:
        for (i = 0; i < npoints; i++) {
            lon = poly[i].lon;
            west = (lon < west) ? lon : west;
            east = (lon > east) ? lon : east;
            float1 = lon;
            H2N4 ((char *)(&float2), (char *)(&float1), 1);
            fwrite (&float2, sizeof (float), 1, dfh);

            lat = poly[i].lat;
            south = (lat < south) ? lat : south;
            north = (lat > north) ? lat : north;
            float1 = lat;
            H2N4 ((char *)(&float2), (char *)(&float1), 1);
            fwrite (&float2, sizeof (float), 1, dfh);
        }
        fclose (dfh);
        break;
    case polyINTELFLOAT:
        for (i = 0; i < npoints; i++) {
            lon = poly[i].lon;
            west = (lon < west) ? lon : west;
            east = (lon > east) ? lon : east;
            float1 = lon;
#ifdef WORDS_BIGENDIAN
            swap4 (&float1, &float2, 1);
#else
            float2 = float1;
#endif
            fwrite (&float2, sizeof (float), 1, dfh);
            lat = poly[i].lat;
            south = (lat < south) ? lat : south;
            north = (lat > north) ? lat : north;
            float1 = lat;
#ifdef WORDS_BIGENDIAN
            swap4 (&float1, &float2, 1);
#else
            float2 = float1;
#endif
            fwrite (&float2, sizeof (float), 1, dfh);
        }
        fclose (dfh);
        break;
    case polyGSHHS:
        elog_log (0, "writePolygonData: storage type %s not yet implemented... ", ftype);
        if (free_dir)
            free (dir);
        if (free_dfile)
            free (dfile);
        return dbINVALID;
        break;
    default:
        elog_log (0, "writePolygonData: unknown storage type pcode, giving up");
        if (free_dir)
            free (dir);
        if (free_dfile)
            free (dfile);
        return dbINVALID;
    }
    if (dbputv (db, 0, "north", north, "east", east, "south", south, "west", west,
                "pname", pname, "closed", closed_flag, "level", level, "ptype", ptype, "auth", auth, "npoints", npoints, "ftype", ftype, "foff", foff, NULL) == dbINVALID) {
        elog_log (0, "writePolygonData: error putting values!");
        //free (ftype);
        if (free_dir)
            free (dir);
        if (free_dfile)
            free (dfile);
        return dbINVALID;
    } else {
        //free (ftype);
        if (free_dir)
            free (dir);
        if (free_dfile)
            free (dfile);
        return db.record;
    }
}


Dbptr           inWhichPolygons (Dbptr db, Point P)
{
    Point          *poly;
    long            i, firstrecord;
    long            nrec;
    Dbptr           dbr;        /* = dblookup(db,0,"polygon",0,0); */
    double          lat, lon, north, south, east, west;
    Dbptr           dbs;
    long            npoints;
    char            expr[STRSZ];
    char            temp[STRSZ];
    long            pid;
    int             first;
    int             found = 0;
    int             free_dbs = 0;
    char            name[STRSZ];

    lat = P.lat;
    lon = P.lon;

    if (db.table < 0) {
        dbs = dblookup (db, 0, "polygon", 0, 0);
    } else {
        dbs = db;
    }
    /*
     * would be better to check only on closed polygons..., but breaks too many
     * programs (older versions of the polygon-schema)
     * sprintf(expr,"closed=~/y/ && north >= %f && south <= %f && east >= %f && west <= %f",
     */
    if (db.record < 0) {
        sprintf (expr, "north >= %f && south <= %f && east >= %f && west <= %f",
                 lat, lat, lon, lon);
        dbs = dbsubset (dbs, expr, 0);
        free_dbs = 1;
        dbquery (dbs, dbRECORD_COUNT, &nrec);
        firstrecord = 0;
        if (nrec < 1) {
            /*
             * elog_log(0,"inWhichPolygons: initial check of Bounding Box
             * returns 0!");
             */
            dbr = dbinvalid ();
            return dbr;
        }
    } else {
        /*
         * make sure we ca use the for-loop below both for one record and a
         * whole series...
         */
        dbgetv (dbs, 0, "north", &north, "east", &east, "west", &west, "south", &south, NULL);
        if (north >= lat && south <= lat && east >= lon && west <= lon) {
            firstrecord = db.record;
            nrec = firstrecord + 1;
        } else {
            dbr = dbinvalid ();
            return dbr;
        }
    }
    first = 1;
    for (i = firstrecord; i < nrec; i++) {
        dbs.record = i;
        dbgetv (dbs, 0, "pname", &name, "pid", &pid, NULL);

        if ((npoints = readPolygon (dbs, &poly)) > 0) {
            if (isGeographicallyInside (P, poly, npoints)) {
                found = 1;
                if (first) {
                    sprintf (expr, "pid==%ld", pid);
                    first = 0;
                } else {
                    sprintf (temp, "|| pid==%ld", pid);
                    strcat (expr, temp);
                }
            }
            free (poly);
        }
    }

    /* dbr=dblist2subset(dbs,list); */
    if (found) {
        dbr = dbsubset (dbs, expr, 0);
    } else {
        dbr = dbinvalid ();
    }
    if (free_dbs) {
        dbfree (dbs);
    }
    return dbr;
}

static char    *Directions[] = {
    "N",
    "NNE",
    "NE",
    "ENE",
    "E",
    "ESE",
    "SE",
    "SSE",
    "S",
    "SSW",
    "SW",
    "WSW",
    "W",
    "WNW",
    "NW",
    "NNW",
    "N"
};

char           *windrose (double azimuth)
{
    /**
      returns direction like on a compass
    */

    int             index;
    char           *ret;
    double          iaz;
    double          frac;

    iaz = floor (azimuth);
    frac = azimuth - iaz;
    iaz = (int)iaz % 360;
    if (iaz < 0)
        iaz += 360;
    iaz += frac;
    index = floor ((iaz + 11.25) / 22.5);
    ret = strdup (Directions[index]);
    return ret;
}








/*
 * // orientation2D_Polygon(): tests the orientation of a simple polygon //
 * Input:  int n = the number of vertices in the polygon //
 * Pofoint* V = an array of n+1 vertices with V[n]=V[0] //    Return: >0 for
 * counterclockwise //            =0 for none (degenerate) //            <0
 * for clockwise //    Note: this algorithm is faster than computing the
 * signed area. int orientation2D_Polygon( int n, Point* V ) { // first find
 * rightmost lowest vertex of the polygon int rmin = 0; int xmin = V[0].lon;
 * int ymin = V[0].lat;
 * 
 * for (int i=1; i<n; i++) { if (V[i].lat > ymin) continue; if (V[i].lat ==
 * ymin) {    // just as low if (V[i].lon < xmin)   // and to left continue;
 * } rmin = i;          // a new rightmost lowest vertex xmin = V[i].lon;
 * ymin = V[i].lat; }
 * 
 * // test orientation at this rmin vertex // ccw <=> the edge leaving is left
 * of the entering edge if (rmin == 0) return isLeft( V[n-1], V[0], V[1] );
 * else return isLeft( V[rmin-1], V[rmin], V[rmin+1] ); }
 * 
 * // area2D_Polygon(): computes the area of a 2D polygon //    Input:  int n =
 * the number of vertices in the polygon //            Point* V = an array of
 * n+2 vertices //                       with V[n]=V[0] and V[n+1]=V[1] //
 * Return: the (float) area of the polygon float area2D_Polygon( int n, Point*
 * V ) { float area = 0; int   i, j, k;     // indices
 * 
 * for (i=1, j=2, k=0; i<=n; i++, j++, k++) { area += V[i].lon * (V[j].lat -
 * V[k].lat); } return area / 2.0; }
 * //===================================================================
 * 
 */


static double   distance_to_line (Point P, Point L1, Point L2)
{
    double          latdist, londist, distance, dlat, dlon, u;
    Point           closest;
    if ((P.lat == L1.lat) && (P.lon == L1.lon)) {
        distance = 0;
        return distance;
    }
    if ((P.lat == L2.lat) && (P.lon == L2.lon)) {
        distance = 0;
        return distance;
    }
    dlat = L2.lat - L1.lat;
    dlon = L2.lon - L1.lon;

    if ((dlat == 0) && (dlon == 0)) {
        latdist = P.lat - L1.lat;
        londist = P.lon - L1.lon;

        distance = sqrt (latdist * latdist + londist * londist);
        return distance;
    }
    u = ((P.lat - L1.lat) * dlat + (P.lon - L1.lon) * dlon) / (dlat * dlat + dlon * dlon);


    if (u < 0) {
        closest = L1;
    } else if (u > 1) {
        closest = L2;
    } else {
        closest.lat = L1.lat + u * dlat;
        closest.lon = L1.lon + u * dlon;
    }
    latdist = P.lat - closest.lat;
    londist = P.lon - closest.lon;

    distance = sqrt (latdist * latdist + londist * londist);
    return distance;
}

double          distance_to_polygon (Point P, Point * polygon, long npoints)
{
    long            i;
    double          mindist, dist;
    Point           L1, L2;
    mindist = 1e99;
    if (npoints == 1) {
        L1.lat = polygon[0].lat;
        L1.lon = polygon[0].lon;

        L2.lat = P.lat - L1.lat;
        L2.lon = P.lon - L1.lon;
        mindist = sqrt (L2.lat * L2.lat + L2.lon * L2.lon);
    } else {
        for (i = 0; i < (npoints - 1); i++) {
            L1.lat = polygon[i].lat;
            L1.lon = polygon[i].lon;
            L2.lat = polygon[i + 1].lat;
            L2.lon = polygon[i + 1].lon;
            dist = distance_to_line (P, L1, L2);
            if (dist < mindist) {
                mindist = dist;
            }
        }
    }
    return mindist;
}

double          distanceToPolygon (Dbptr db, Point P)
{
    Point          *poly;
    long            i, nrec, npoints;
    double          dist, mindist;
    mindist = 1.0e99;

    if (db.table <0) {
        db=dblookup(db,0,"polygon",0,0);
    }
    if (db.record < 0) {
        dbquery (db, dbRECORD_COUNT, &nrec);
        if (nrec < 1) {
            return -1.0;
        }
        for (i = 0; i < nrec; i++) {
            db.record = i;
            if ((npoints = readPolygon (db, &poly)) > 0) {
                dist = distance_to_polygon (P, poly, npoints);
                if (dist < mindist) {
                    mindist = dist;
                }
            }
        }
    } else {
        if ((npoints = readPolygon (db, &poly)) > 0) {
            mindist = distance_to_polygon (P, poly, npoints);
            free (poly);
        } else {
            return -1.0;
        }
    }
    return mindist;

}
