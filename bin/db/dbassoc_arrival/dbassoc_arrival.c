
#include <stdio.h>
#include <math.h>
#include "db.h"
#include "tttaup.h"
#include "stock.h"
#include "coords.h"

void
usage()
{
    fprintf(stderr, "Usage: %s [-p pdelta ]  [-s sdelta] arrival_db origin_db \n",
	    Program_Name);
    banner ( Program_Name, 0) ;
    exit(1);
}


int
main(int argc, char **argv)
{
    char           *arrival_dbname,
                   *origin_dbname;
    int             c;
    double          max_pres = 3.0;
    double          max_sres = 10.0;
    double          seaz,
                    esaz,
                    distance;
    Dbptr           dbarr,
                    dborg,
                    dborg_rcd,
                    dbdummy,
                    dbsite,
                    dbneworg;
    double          lat,
                    lon,
                    depth,
                    stalat,
                    stalon,
                    rlat,
                    rlon;
    long            narr,
                    norigin,
		    narrival,
                    nsite;
    double          orgtime,
                    arrtime,
                    parrival,
                    sarrival,
                    timeres;
    Tbl            *arrkeys, *wfkeys;
    long	    i, n ;
    Hook	   *hook=0 ;
    Tbl	           *matches ;

    long            arid,
                    orid;
    char            phase[10];
    char            record[STRSZ];
    char            sta[25];
    double          min_delta,
                    max_delta,
                    min_orgtime,
                    max_orgtime,
                    sta_delta,
                    pmin,
                    smax;
    long            sta_index;
    int             addflag;
    Arr            *stations;
    double         *longitude,
                   *latitude,
                   *delta;
    long  	    nass;

    double          min_atime,
                    max_atime;

    Program_Name = argv[0];

    while ((c = getopt(argc, argv, "p:s:")) != -1)
      {
	switch (c)
	  {
	case 'p':
	    max_pres = atof(optarg);
	    break;
	case 's':
	    max_sres = atof(optarg);
	    break;
	default:
	    usage();
	  }
      }
    if (argc - optind < 2)
	usage();
    arrival_dbname = argv[optind];
    origin_dbname = argv[optind + 1];

    arrkeys = newtbl(1);
    pushtbl(arrkeys, "time");
    wfkeys = newtbl(1);
    pushtbl(wfkeys, "time::endtime");

    if ( dbopen(origin_dbname, "r", &dborg) ) 
	die ( 0, "Can't open database %s\n", origin_dbname ) ;

    dborg = dblookup(dborg, 0, "origin", 0, 0);
    dborg = dbsort(dborg, arrkeys, 0, "origin.sorted");
    dbquery(dborg, dbRECORD_COUNT, &norigin);
    if ( norigin < 1 ) 
	die ( 0, "Origin table has no records.\n" ) ; 

    dbopen(arrival_dbname, "r+", &dbarr);
    dbarr = dblookup(dbarr, 0, "arrival", 0, 0);
    dbarr = dbsort(dbarr, arrkeys, 0, "arrival.sorted");
    dbquery(dbarr, dbRECORD_COUNT, &narrival);
    if ( narrival < 1 ) 
	die ( 0, "Arrival table has no records.\n" ) ; 

    dbsite = dblookup(dbarr, 0, "site", 0, 0);
    dbquery(dbsite, dbRECORD_COUNT, &nsite);
    if ( narrival < 1 ) 
	die ( 0, "Site table has no records.\n" ) ; 

    dbneworg = dblookup(dbarr, 0, "origin", 0, 0);
    dbdummy = dblookup(dbarr, 0, "wfdisc", 0, 0);
    dbdummy.record = dbSCRATCH;

    dbquery(dbarr, dbRECORD_COUNT, &narr);
    dbarr.record = 0;
    dbgetv(dbarr, 0, "time", &min_atime, NULL );

    /* find a minimum origin time to consider -- 30 minutes before first
     * arrival corresponds to event on the other side of the world. */
    min_orgtime = min_atime - 30.0 * 60.0;

    dbarr.record = narr - 1;
    dbgetv(dbarr, 0, "time", &max_atime, NULL );
    max_orgtime = max_atime + max_sres;

    stations = newarr(0);
    allot(double *, latitude, nsite);
    allot(double *, longitude, nsite);
    allot(double *, delta, nsite);
    for (sta_index = 0; sta_index < nsite; sta_index++)
      {
	dbsite.record = sta_index;
	dbgetv(dbsite, 0,
	       "sta", sta,
	       "lat", &stalat,
	       "lon", &stalon,
	       NULL );
	latitude[sta_index] = rad(stalat);
	longitude[sta_index] = rad(stalon);
	setarr(stations, sta, (char *) sta_index);
      }

    for (dborg.record = 0; dborg.record < norigin; dborg.record++)
      {
	dbgetv(dborg, 0, "time", &orgtime, NULL );
	if (orgtime < min_orgtime)
	    continue;
	if (orgtime > max_orgtime)
	    break;

	dbgetv(dborg, 0, "lat", &lat, "lon", &lon,
	       "time", &orgtime, "depth", &depth, NULL );
	rlat = rad(lat);
	rlon = rad(lon);

	max_delta = 0.0;
	min_delta = 190.0;
	for (sta_index = 0; sta_index < nsite; sta_index++)
	  {
	    stalat = latitude[sta_index];
	    stalon = longitude[sta_index];
	    dist(rlat, rlon, stalat, stalon, &distance, &esaz);
	    sta_delta = deg(distance);
	    max_delta = MAX(sta_delta, max_delta);
	    min_delta = MIN(sta_delta, min_delta);
	    delta[sta_index] = sta_delta ;
	  }

	smax = orgtime + stime(max_delta, depth);
	pmin = orgtime + ptime(min_delta, depth);

	dbputv(dbdummy, 0, "time", pmin - max_pres,
	       "endtime", smax + max_sres, NULL );
	dbmatches(dbdummy, dbarr, &wfkeys, &arrkeys, &hook, &matches ) ; 

	orid = 0;

	n = maxtbl(matches) ; 
	for (i=0 ; i<n ; i++ ) 
	  {
	    dbarr.record = (long) gettbl(matches, i) ; 
	    dbgetv(dbarr, 0,
		   "sta", sta,
		   "arid", &arid,
		   "time", &arrtime,
		   "iphase", phase,
		   NULL );

	    sta_index = (long) getarr(stations, sta);
	    sta_delta = delta[sta_index];

	    addflag = 0;
	    if (*phase == 'P')
	      {
		parrival = orgtime + ptime(sta_delta, depth);
		if (ABS(timeres = arrtime - parrival) < max_pres)
		    addflag = 1;
	      }
	    else if (*phase == 'S')
	      {
		sarrival = orgtime + stime(sta_delta, depth);
		if (ABS(timeres = arrtime - sarrival) < max_sres)
		    addflag = 1;
	      }
	    if (addflag)
	      {
		if (orid == 0)
		  {
		    orid = dbnextid(dbarr, "orid");
		    if ( orid < 1 ) 
			die ( 0, "Can't get new orid id" ) ; 
		    dbget(dborg, (char *) &dborg_rcd);
		    dbget(dborg_rcd, record);
		    dbneworg.record = dbadd(dbneworg, record);
		    if ( dbneworg.record < 0 ) 
			die ( 0, "Can't add to origin table" ) ; 
		    dbputv(dbneworg, 0, "orid", orid, NULL );
		    nass = 0;
		  }
		stalat = latitude[sta_index];
		stalon = longitude[sta_index];
		dist(stalat, stalon, rlat, rlon, &distance, &seaz);
		dist(rlat, rlon, stalat, stalon, &distance, &esaz);
		nass++;
		dbputv(dbneworg, 0, "nass", nass, NULL );
		if ( dbaddv(dbarr, "assoc",
		       "arid", arid,
		       "orid", orid,
		       "sta", sta,
		       "phase", phase,
		       "delta", sta_delta,
		       "seaz", deg(seaz),
		       "esaz", deg(esaz),
		       "timeres", timeres,
		       NULL ) < 0) 
		    die ( 0, "Can't add to assoc table.\n" ) ;
	      }
	  }
	  freetbl(matches, 0) ; 
      }

    return 0;
}

/* $Id$ */
