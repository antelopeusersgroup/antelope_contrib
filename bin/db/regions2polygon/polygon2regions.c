#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>

#include "coords.h"
#include "db.h"
#include "stock.h"
#include "polygon.h"

static void
usage ()
{
	char	*usage="[-v] [-s subset] dbin dbout";
	char 	*version= "1.1";
	char	*author= "Nikolaus Horn";
	char	*location="ZAMG / Vienna";
	char	*email = "Nikolaus.Horn@zamg.ac.at";
    cbanner (version,usage,author,location,email);
    exit (1);
}

int
main (int argc, char **argv)
{
    int             c,
                    verbose = 0,
		    errflg = 0;


	char *dbinname=malloc(1024);
	char *dboutname=malloc(1024);
	Point *poly;
	double lat,lon;
	char *subset_expr=NULL;
	char *name=malloc(100);
	long nregions, nvertices;
	Tbl *sortkeys;
			
	Dbptr dbin,dbout,dbi,dbo,dbs;
	long i,from,to,nv;
	long vertex;
	
    elog_init ( argc, argv ) ; 
    while ((c = getopt (argc, argv, "s:vV")) != -1) {
	switch (c) {

	case 's':
	    subset_expr=optarg;
	    break;
	case 'v':
	    verbose++ ;
	    break;

	case 'V':
	    usage ();
	    break;

	case '?':
	    errflg++;
	    break ;
	}
    }

    if ((errflg) || argc < 3)
	usage ();

	dbinname = argv[optind++];
	dboutname= argv[optind++];

	if (dbopen(dbinname,"r",&dbin)) {
		elog_die(1,"cannot open database %s",dbinname);
	}
	dbi=dblookup(dbin,0,"polygon",0,0);
	if (subset_expr) {
		dbi=dbsubset(dbi,subset_expr,NULL);
	}

	

	sortkeys=newtbl(1);
	pushtbl(sortkeys,"pname");
	
	dbs=dbsort(dbi,sortkeys,0,"sorted");
	dbquery(dbs,dbRECORD_COUNT,&nregions);
	if (nregions <1) {
		elog_die(0,"table regions seems to be empty (or not present)");
	}
	
	if (verbose) elog_notify(0,"creating database descriptor %s",dboutname);
	
	if (dbcreate(dboutname,"places1.2",0,0,0,0,0)) {
		elog_die(1,"cannot create database %s",dboutname);
	}
	dbopen(dboutname,"r+",&dbout);
	dbo=dblookup(dbout,0,"regions",0,0);
	
	for (i=0; i< nregions; i++) {
		dbs.record=i;
		dbgetv(dbs,0,"pname",name,NULL );
		nvertices=readPolygon(dbs,&poly);
		for (nv=0;nv < nvertices;nv++) {
			lat=poly[nv].lat;
			lon=poly[nv].lon;
			dbaddv(dbo,0,
					"regname",name,
					"vertex",nv,
					"lat",lat,"lon",lon,
					NULL );
		}
		free(poly);
	}
	dbclose(dbo);
	/*
				*/
    return 0;
}
