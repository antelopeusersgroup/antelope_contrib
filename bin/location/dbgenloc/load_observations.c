#include "dbgenloc.h"

int
load_observations ( Pf *pf, Dbptr db, Arr *arr_phase, 
	    Arr **stationsp, Arr **arraysp, Tbl **tap, Tbl **tup ) 
{ 
    Station *station ;
    Seismic_Array *array ;
    Arrival *a ;
    Tbl *ta, *tu ;
    Dbptr dbarr, dbsite ;
    static void *hook = 0 ; 
    static Tbl *matching ;
    Tbl *arrival_tbl ;
    Arr *stations ;
    Arr *arrays ;
    int i, narr ;
    char sta[32], iphase[32], timedef[16], azdef[16], slodef[16] ;
    double time, deltim, azimuth, delaz, slow, delslo ; 
    int arid ;
    Phase_handle *phase ;
    Slowness_vector *u ;
    int nobs = 0 ;

    if ( *stationsp == 0 ) {
	*stationsp = newarr(0) ; 
	*arraysp = newarr(0) ; 
    }
    stations = *stationsp ; 
    arrays = *arraysp ; 

    ta = *tap = newtbl(0) ;
    tu = *tup = newtbl(0) ;

    dbarr = dblookup ( db, 0, "arrival", 0, "dbSCRATCH" ) ; 
    dbsite = dblookup ( db, 0, "site", 0, 0 ) ; 

    arrival_tbl = pfget_tbl(pf, "arrival_table");
    narr = maxtbl(arrival_tbl);
    for (i = 0; i < narr; i++) {
	sscanf(gettbl(arrival_tbl, i),
	       "%d %s %s %lf %lf %s %lf %lf %s %lf %lf %s",
	       &arid,
	       sta,
	       iphase,
	       &time,
	       &deltim,
	       timedef,
	       &azimuth,
	       &delaz,
	       azdef,
	       &slow,
	       &delslo,
	       slodef
	    );

	if ( (station = getarr ( stations, sta ) ) == 0 ) {
	    dbputv ( dbarr, 0, "sta", sta, "time", time, 0 ) ; 
	    dbmatches ( dbarr, dbsite, 0, 0, &hook, &matching ) ;
	    if ( maxtbl(matching) < 1 ) {
		char *t ;
		complain ( 0, "No site table record for %s at time %s\n", 
		    sta, t=strtime(time));
		free(t) ;
		continue ;
	    }

	    dbsite.record = (int) gettbl(matching, 0) ;
	    freetbl(matching,0) ;
	    allot ( Station *, station, 1 ) ; 
	    allot ( Seismic_Array *, array, 1 ) ; 
	    dbgetv ( dbsite, 0, 
		"sta", station->name, "lat", &station->lat, "lon", &station->lon, "elev", &station->elev, 0 ) ;
	    dbgetv ( dbsite, 0, 
		"sta", array->name, "lat", &array->lat, "lon", &array->lon, "elev", &array->elev, 0 ) ;
	    setarr (stations, sta, station ) ; 
	    setarr (arrays, sta, array ) ; 
	} else {
	    array = getarr(arrays, sta) ;
	}
      
	if ( (phase = (Phase_handle *) getarr(arr_phase,iphase)) == 0 ) {
	    complain ( 0, "Can't compute travel time for phase %s\n", iphase ) ;
	} else {

	    if ( time > 0.0 && *timedef == 'd' ) {
		allot(Arrival *, a, 1) ;
		a->arid = arid ;
		a->sta = station ;
		a->time = time;
		a->deltat = deltim ;
		a->phase = phase ; 
		if( (a->deltat) <= 0.0 ) 
		    a->deltat = (double)a->phase->deltat0;
		pushtbl(ta,a);
		nobs ++ ;
	    }

	    if (   slow >= 0.0    && azimuth >= 0.0 
		&& *slodef == 'd' && *azdef == 'd')  {

		allot ( Slowness_vector *, u, 1 ) ; 
		u->arid = arid ;
		u->array = array ;
		u->phase = phase ;
			
	       /* CSS3.0 records slowness vectors in polar form with
		* the magnitude measured in second/degree and azimuth
		* in degrees.  genloc uses only components internally
		* in units of seconds/km.  We now convert the slow,
		* azimuth values to ux and uy in s/km.  Oh, another
		* possible confusion.  For all the data I've seen,
		* azimuth is really the receiver backazimuth which
		* points 180 degrees away from the propagation
		* direction.  Internally I use the azimuth, so this
		* formula makes that conversion too through the
		* negative sign on each component.
		*/

		slow *= km2deg(1.0);
		u->ux = -slow*sin(rad(azimuth));
		u->uy = -slow*cos(rad(azimuth));
		u->deltaux = u->phase->deltau0;
		u->deltauy = u->phase->deltau0;
		pushtbl(tu,u);
		nobs++ ;
	    }

	}
    }
    freetbl(arrival_tbl, 0) ;

    return nobs ;
}

/* $Id$ */
