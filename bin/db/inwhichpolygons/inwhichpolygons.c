
#include <stdio.h>
#include "stock.h"
#include "db.h"
#include "polygon.h"
Dbptr db;

void where(double lat,double lon) {
	Dbptr dbs;
	long nrecords;
	char name[1024];	
	Point p;
	

	p.lat=lat;p.lon=lon;
	dbs=inWhichPolygons(db,p);
	if (dbs.database >= 0) {
		dbquery(dbs,dbRECORD_COUNT,&nrecords);
		for (dbs.record=0; dbs.record < nrecords; dbs.record++) {
			dbgetv(dbs,0,"pname",name,NULL);
			printf("%s\n",name);
		}
		dbfree(dbs);
		
	} else {
		printf("nothing found\n");
	}

}

int main(int argc, char **argv) {
    double          lat, lon;
    char            line[100];
	char			*dbname=malloc(1024);

	if (argc == 2) {
		dbname=argv[1];
		if (dbopen_database(dbname,"r",&db) < 0 ) {
			elog_die(1,"cannot open database %s",dbname);
		}
		printf("Enter lat, lon: ");
		while (gets(line) != NULL) {
			sscanf(line, "%lf %lf", &lat, &lon);
			where(lat, lon);
			printf("Enter lat lon: ");
		}
		printf("\n" ) ; 
	} else if (argc == 4) {
		dbname=argv[1];
		if (dbopen_database(dbname,"r",&db) < 0 ) {
			elog_die(1,"cannot open database %s",dbname);
		}
		sscanf(argv[2], "%lf", &lat);
		sscanf(argv[3], "%lf", &lon);
		where(lat, lon);
	} else {
		printf("Usage: %s dbname [ lat lon ]\n", argv[0]);
		exit(1);
	}
	dbclose(db);

    return 0;
}
