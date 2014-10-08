
#include <stdio.h>
#include "stock.h"
#include "db.h"
#include "polygon.h"
Dbptr db;
int verbose=0;

void mydist(double lat,double lon) {
	Dbptr dbs;
	long nrecords, ninside, pid;
	char name[1024], bname[1024];	
	double mindist, dist;
	Point p;
	

	p.lat=lat;p.lon=lon;
	if (db.table <0) {
		db=dblookup(db, 0, "polygon", 0, 0);
	}
	mindist = 1e99;
	if (db.database >= 0) {
		if (verbose >0) {
			dbquery(db,dbRECORD_COUNT,&nrecords);
			//printf("in %ld zeilen wird gesucht\n",nrecords);
			for (db.record=0; db.record < nrecords; db.record++) {
				int inside;

				dbgetv(db,0,"pname",name,"pid",&pid,NULL);
					
				dist = distanceToPolygon(db, p);
				if (dist > 0 && dist < mindist) {
					mindist=dist;
					strlcpy(bname,name,1024 - 1);
				}
				dbs=inWhichPolygons(db,p);
				if (dbs.table == dbINVALID) {
					inside=0;
					dbfree(dbs);
				} else {
					inside=1;
					dbfree(dbs);
				}
				if (inside) {
					printf("\t%8.3f deg inside %6ld  %s\n",dist, pid, name);
				} else {
					printf("\t%8.3f deg from   %6ld  %s\n",dist, pid, name);
				}
			}
			printf("mindist: %8.3f deg to %s\n", mindist,bname);
		} else {
			dist = distanceToPolygon(db, p);
			printf("%8.3f\n", dist);
		}
	} else {
		printf("no polygons in database\n");
	}

}

int main(int argc, char **argv) {
    double          lat, lon;
    char            line[100];
	char			*dbname=malloc(1024);

	if (argc < 2) {
		
		printf("usage: %s [-v] db [lat lon]\n",argv[0]);
		exit(1);
	}
	 
	if (!strcmp(argv[1], "-v")) {
		argc--; argv++; 
		verbose++;
	}

	if (argc == 2) {
		dbname=argv[1];
		if (dbopen_database(dbname,"r",&db) <0) {
			elog_die(1,"cannot open database %s",dbname);
		}
		printf("Enter lat, lon: ");
		while (gets(line) != NULL) {
			sscanf(line, "%lf %lf", &lat, &lon);
			mydist(lat, lon);
			printf("Enter lat lon: ");
		}
		printf("\n" ) ; 
	} else if (argc == 4) {
		dbname=argv[1];
		if (dbopen_database(dbname,"r",&db)<0) {
			elog_die(1,"cannot open database %s",dbname);
		}
		sscanf(argv[2], "%lf", &lat);
		sscanf(argv[3], "%lf", &lon);
		mydist(lat, lon);
	} else {
		printf("Usage: %s dbname [ lat lon ]\n", argv[0]);
		exit(1);
	}
	dbclose(db);

    return 0;
}
