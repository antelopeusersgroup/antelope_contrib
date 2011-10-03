#include <math.h>
#include <strings.h>
#include "stock.h"
#include "arrays.h"
#include "coords.h"
#include "db.h"
#include "pf.h"
#include "elog.h"
#include "location.h"
#include "pmel.h"
#define RADIUS_EARTH 6370.8
#define KMPERDEG 111.19

/*  Companion to below.  Because lat and lon are stored in site
with fewer digits that it is technically possible to measure there
are cases where one wants to use dnorth and deast as offsets from
a reference position to compute a very accurate latitude and longitude
instead of just using them as offsets (case in all array processing).
Note this should NOT be called for a large aperture array were dnorth
and deast may be used for computing plane wave time shifts.  
I use a very simple formula that should be more than sufficient for
the presumed case of small offsets.

Arguments:
	dnorth,deast - fields read from site table
	lat, lon - lat and lon to apply correction to (assumed in 
		units of degrees)

Author:  Gary Pavlis
Written:  October 2000
*/
void apply_dnde(double dnorth,double deast,double *lat,double *lon)
{
	double lon_km_per_rad,lon_deg_per_km,lat_deg_per_km;

	lon_km_per_rad = RADIUS_EARTH*sin(rad(90.0-(*lat)));
	/* overkill, but the following would fail at the pole otherwise*/
	if((*lat)==90.0)
	{
		lon_deg_per_km = 0.0;
		lat_deg_per_km = 0.0;
	}
	else
	{
		lon_deg_per_km = 1.0/lon_km_per_rad;
		lon_deg_per_km = deg(lon_deg_per_km);
		lat_deg_per_km = deg(1.0/RADIUS_EARTH);
	}
	*lat += lat_deg_per_km*dnorth;
	*lon += lon_deg_per_km*deast;
	return;
}
/* This routine loads the associative array used by genloc from 
the css3.0 site table.  It is a hybrid of a routine of similar
function found in libmultiwavelet and one called dbload_station_table
in libgenloc.  Functionally this module loads stations from a database
site table, but keeping only those defined in a list defined in the
parameter file.  This allows the use of an all encompassing site
table but running with a more restricted constellation of stations.
It is IMPORTANT to realize something this algorithm does NOT do.
That is, technically the site table has an ondate:offdate field
that could allow a station to move around.  This is evil in the 
context of multiple event location as a station's position could,
in principle, be time variable.  This would be very difficult to 
deal with and would require extension tables or some major 
changes to css3.0 to handle the complications.  Thus, it should
be viewed as a requirement that stations with a given name must
remain at a fixed spot.  Consequently, this routine scans the
site table and dies if it finds any station has moved.  

After the moving station scan, the basic algorithm is reading
the site table matching against the pf list keeping only 
entries that match.  

Arguments:
	db - open input database.  Can point anywhere as step
		one is a call to dblookup on site
	pf - parameter file object created by pfread

Normal return is an associative array keyed by station of 
Station objects (defined in location.h).  Serious problems
will cause this function to die under an assumption it is called
in initialization.

Author:  Gary Pavlis
Written:  October 2000
*/
Arr *pmel_dbload_stations(Dbptr db, Pf *pf)
{
        Dbptr dbs;
        long nrows;
        Tbl *sortkeys;
	Arr *a,*out;
 	char staname[12];
	double lat,lon,elev,dnorth,deast;
	int useall,usednde;
	Station *s;
	char laststa[12];
	double lastlon,lastlat,lastelev,lastdn,lastde;

	/*We first scan the site table for moving stations*/
	sortkeys = strtbl("sta","ondate::offdate",NULL );
	dbs = dblookup(db,0,"site",0,0);
	dbs = dbsort(dbs,sortkeys,0,0);
	dbquery(dbs,dbRECORD_COUNT,&nrows);
	if(nrows <=0) elog_die(0,"Site table is empty\nFix database\n");

	out = newarr(0);

	useall = pfget_boolean(pf,"use_all_stations");
	usednde = pfget_boolean(pf,"use_dnorth_deast");
	/* The associate array a uses only the keys here */
	a = pfget_arr(pf,"pmel_stations");
	dbs.record=0;
	dbgetv(dbs,0,"sta",laststa,
		"lon",&lastlon,
		"lat",&lastlat,
		"elev",&lastelev,
		"dnorth",&lastdn,
		"deast",&lastde,NULL );

	for(dbs.record=0;dbs.record<nrows;++dbs.record)
	{
		if(dbgetv(dbs,0,"sta",staname,
			"lon",&lon,
			"lat",&lat,
			"elev",&elev,
			"dnorth",&dnorth,
			"deast",&deast,NULL ) == dbINVALID)
			elog_die(0,"pmel_dbload_stations:  dbgetv error scaning site table at row %ld of working view\n",dbs.record);

		if(usednde) apply_dnde(dnorth,deast,&lat,&lon);
		if(strcmp(staname,laststa) )
		{
			if(useall || (getarr(a,laststa)!=NULL) )
			{
				allot(Station *,s,1);
				strcpy(s->name,laststa);
				s->lat = lastlat;
				s->lon = lastlon;
				s->elev = lastelev;
				setarr(out,laststa,s);
			}
			strcpy(laststa,staname);
			lastlon = lon;
			lastlat = lat;
			lastelev = elev;
		}
		else
		{
			if( (lastlat != lat) || (lastlon != lon) )
			  elog_die(0,"Fatal(pmel_dbload_stations):  station location for %s is not constant in site table\nFound lat,lon pairs of (%lf,%lf) and (%lf,%lf)\n",
				staname,lat,lon,lastlat,lastlon);
		}
	}
	/* We drop the last station without this.  There is 
	probably a cleaner logic for this, but this function
	is called only once per run so I won't be picky */
	if(useall || (getarr(a,laststa)!=NULL) )
	{
		allot(Station *,s,1);
		strcpy(s->name,laststa);
		s->lat = lastlat;
		s->lon = lastlon;
		s->elev = lastelev;
		setarr(out,laststa,s);
	}
	dbfree(dbs);
	freearr(a,0);
	return(out);
}
