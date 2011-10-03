#include <stdio.h>
#include <math.h>
#include "elog.h"
#include "coords.h"
#include "db.h"
#define RADIUS_EARTH 6378.164
/* For plane wave moveout computations a local cartesian coordinate
system is used wrt to a particular origin.  This approximation is
reasonable until the distance of a station from the origin 
becomes a significant fraction of the epicentral distance.  
In pseudostation stacking we can minimize the impact of this approximation
by continually translating the origin to the pseudostation point and 
computing distances to all stations wrt to that point.  Since large
distance stations receive a low weight, time alignments at large
distances become unimportant.  this function is used to computer 
vector distances in this context.

Arguments:
	nsta - number of stations to process = length of lat and lon vectors
			(see below).
	lat0, lon0 - origin to compute dnorth deast from
	lat, lon - vectors of length nsta of station coordinates to be 
		converted.
	dnorth, deast - vectors of length nsta to contain the results 
		(These are geographic dirctions +north and + east
		respectively)

Author:  G Pavlis
Written:  June 2000
*/
void geographic_to_dne(double lat0, double lon0,
	double lat, double lon, double *dnorth, double *deast)
{
	double azimuth;
	double d; 
	dist(rad(lat0),rad(lon0),rad(lat),rad(lon),&d,&azimuth);
	d *= RADIUS_EARTH;
	*deast = d*sin(azimuth);
	*dnorth = d*cos(azimuth);
}
main(int argc, char **argv)
{
	Dbptr db;
	char *dbname, *refsta;
	double lat0,lon0,lat,lon,dnorth,deast;
	int nsta;
	char sta[12];

	if(argc!=3) elog_die(0,"Usage:  setdne db refsta\n");
	
	dbname=argv[1];
	refsta=argv[2];

	if(dbopen(dbname,"r+",&db)==dbINVALID)
		elog_die(0,"Cannot open database %s\n",dbname);
	db = dblookup(db,0,"site",0,0);
	dbquery(db,dbRECORD_COUNT,&nsta);
	if(nsta<=0)elog_die(0,"Empty site table for db %s\n",dbname);
	db = dblookup(db,0,"site","sta",refsta);
	
	if(dbgetv(db,0,"lat",&lat0,"lon",&lon0,0)==dbINVALID)
		elog_die(0,"dbgetv error reading reference station %s on row %d\n",
			refsta,db.record);
	db = dblookup(db,0,"site",0,0);
	for(db.record=0;db.record<nsta;++db.record)
	{
		dbgetv(db,0,"sta",sta,"lat",&lat,"lon",&lon,0);
		if(strcmp(sta,refsta))
			geographic_to_dne(lat0,lon0,lat,lon,&dnorth,&deast);
		else
		{
			dnorth=0.0;
			deast = 0.0;
		}
		dbputv(db,0,"dnorth",dnorth,"deast",deast,"refsta",refsta,0);
	}
	return(0);
}
		
		

	
	
