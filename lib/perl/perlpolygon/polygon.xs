#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "polygon.h"
#include "errlog2string.c"

MODULE = polygon		PACKAGE = polygon		


void
inWhichPolygons(idatabase, itable, ifield, irecord, lat, lon)
	int		idatabase
	int 	itable
	int 	ifield
	int 	irecord
	double	lat
	double	lon
	PPCODE:
	{ 
	Dbptr db;
	Dbptr dbr;
	Point P;
	db.database= idatabase;
	db.table= itable;
	db.field= ifield;
	db.record= irecord;
	P.lat= lat;
	P.lon= lon;
	dbr= inWhichPolygons(db, P);
 # for some reasons, the following does NOT work
 # if ( dbr.database < 0 ) {
 #		SV *sv;
 #		sv= errlog2string(1);
 #		croak("%s",SvPV_nolen(sv));
 #		}
	EXTEND(sp,4);
	PUSHs(sv_2mortal(newSViv(dbr.database)));	
	PUSHs(sv_2mortal(newSViv(dbr.table)));	
	PUSHs(sv_2mortal(newSViv(dbr.field)));	
	PUSHs(sv_2mortal(newSViv(dbr.record)));	
	}

char *
windrose(azimuth)
	double azimuth

void
readPolygon(idatabase, itable, ifield, irecord)
	int		idatabase
	int 	itable
	int 	ifield
	int 	irecord
	PPCODE:
	{
	Dbptr 	db;
	int		nrecords;
	int 	i;
	Point 	*poly;	
	
	db.database= idatabase;
	db.table= itable;
	db.field= ifield;
	db.record= irecord;
	nrecords= readPolygon(db,&poly);
	EXTEND(sp,nrecords * 2);

	for (i= 0; i < nrecords; i++) {
 		PUSHs(sv_2mortal(newSVnv(poly[i].lat)));
 		PUSHs(sv_2mortal(newSVnv(poly[i].lon)));
 	}
	free(poly);
	}

# void
# writePolygondata(idatabase, itable, ifield, irecord,)
# 	int		idatabase
# 	int 	itable
# 	int 	ifield
# 	int 	irecord
# 	
# 	PPCODE:
# 	{
# 	Dbptr 	db;
# 	int		record;
# 	
# 	int 	i;
# 	Point 	*poly;	
# 	
# 	db.database= idatabase;
# 	db.table= itable;
# 	db.field= ifield;
# 	db.record= irecord;
# 	record= writePolygonData(db,poly,npoints,pname,ptype,auth,dir,dfile,pcode);
# 	EXTEND(sp,nrecords * 2);
# 
# 	for (i= 0; i < nrecords; i++) {
#  		PUSHs(sv_2mortal(newSVnv(poly[i].lat)));
#  		PUSHs(sv_2mortal(newSVnv(poly[i].lon)));
#  	}
# 	}
# 
