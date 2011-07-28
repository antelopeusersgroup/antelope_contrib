/* This group of functions are used to implement a variety of methods 
of calculating an initial hypocenter location.  

Author:  Gary L. Pavlis
Written:  November 1996
*/

#include "stock.h"
#include "arrays.h"
#include "coords.h" 
#include "stock.h"
#include "arrays.h"
#include "location.h" 
#include "coords.h" 
#include <string.h>
/* These are used to simplify logic of various methodologies for 
doing an initial location */
#define MANUAL 0
#define NEAREST_STATION 1
#define S_PTIME 2
#define RECTANGLE_GRIDSEARCH 3
#define RADIAL_GRIDSEARCH 4

#define DEFAULT_TRIALZ 5.0
#define VELOCITY 6.0  /* Velocity used in origin time for nearest station
			method from trial z (ot correction = z/VELOCITY) */
#define DELTA_FUDGE 0.0001  /* fudge factor added to prevent odd 
			singularity that showed up at times in nearest 
			station measurement */
#define DEFAULT_SPVEL  9.0 /* default pseudovelocity used for scaling S-P time
				to a distance in km */
#define DEFAULT_SPTIME 5.0  /* Used when no S wave data are available. */
#define DEFAULT_SPANGLES 180 /* number of points around a circle used in s-p 
				grid circular search pattern */


/* This function implements the nearest station method.  It scans 
the arrival table, a,  and returns the table index for the first
arrival station */
int find_first_arrival(Tbl *t)
{
	Arrival *a;
	int i,n, ifirst;
	double time;

	a = (Arrival *) gettbl(t,0);
	time = a->time;
	ifirst = 0;
	n = maxtbl(t) ;
	for(i=1 ; i<n ; ++i)
	{
		a = (Arrival *)gettbl(t,i);
                /* Correction by JN. Earliest arrival was incorrecly determined. */
		if( (a->time) < time) {
                        time = a->time;
			ifirst = i;
                }
	}
	return(ifirst);
}
int getpstime (Tbl *t, double *ptime, double *stime, double *lat, double *lon)
{
	Arrival *a,*atest;
	int ip,is;

	for(ip=0;ip<maxtbl(t);++ip)
	{
		a = (Arrival *) gettbl(t,ip);
		if(!strcmp(a->phase->name,"P"))
		{
			for(is=0;is<maxtbl(t);++is)
			{
				if(is == ip )continue;
				atest = (Arrival *) gettbl(t,is);
				if(!strcmp(atest->phase->name,"S")
				&&  !strcmp(a->sta->name,atest->sta->name) )
				/* station - phase match lands here */
				{
					*lat = a->sta->lat;
					*lon = a->sta->lon;
					*ptime = a->time;
					*stime = atest->time;
					return(0);
				}
			}
		}
	}
	return(1);
}

Hypocenter initial_locate(Tbl *attbl, Tbl *utbl, 
			Location_options options, Pf *pf)
{
	char *s;
	int method;
	Hypocenter h;
	Point *p;
	int istat;
	int use_raw;
	int ifirst;
	int nangles,ngrid;
	Arrival *a;
	double stime, ptime, sptime,spvel,dist;
	double clat, clon;

	if((s=pfget_string(pf,"initial_location_method"))==NULL)
	{
		elog_log(0,"warning(initial_locate):  no method specified.  using default of nearest station method\n");
		method = NEAREST_STATION;
	}

	if(!strcasecmp(s,"manual"))
		method = MANUAL;
	else if(!strcasecmp(s,"nearest_station"))
		method = NEAREST_STATION;
	else if(!strcasecmp(s,"S-Ptime"))
		method = S_PTIME;
	else if(!strcasecmp(s,"rectangle_gridsearch"))
		method = RECTANGLE_GRIDSEARCH;
	else if(!strcasecmp(s,"radial_gridsearch"))
		method = RADIAL_GRIDSEARCH;
	else
	{
		elog_log(0,"warning(initial_locate): unrecognized method keyword %s\nUsing default of nearest station method\n",
			s);
		method = NEAREST_STATION;
	}
	/* This initializes parts of the hypocenter stucture that define
	this as an initial location.  Some are reset below in some
	routines, but initialization is always prudent. */
        h.dz = 0.0;
        h.dx = 0.0;
        h.dy = 0.0;
        h.dt = 0.0;
        h.rms_raw = -1.0;
        h.rms_weighted = -1.0;
        h.interquartile = -1.0;
        h.number_data = 0;
        h.degrees_of_freedom = 0;
	h.used = 1;
	if((method == RECTANGLE_GRIDSEARCH) || (method == RADIAL_GRIDSEARCH) )
		s = pfget_string(pf,"gridsearch_norm");
		if(s== NULL)
			use_raw = 1;
		else if(!strcmp(s,"weighted_rms"))
			use_raw = 0;
		else
			use_raw = 1;

	switch (method)
	{
	case MANUAL:
        	h.lat = pfget_double(pf,"initial_latitude");
        	h.lon = pfget_double(pf,"initial_longitude");
        	h.z = pfget_double(pf,"initial_depth");
        	h.time = pfget_double(pf,"initial_origin_time");
		break;
	case S_PTIME:
		if(getpstime(attbl, &ptime, &stime, &clat, &clon))
		{
			a = (Arrival *) gettbl(attbl,0);
			clat = a->sta->lat;
			clon = a->sta->lon;
			ptime = a->time;
			sptime = DEFAULT_SPTIME;
		}
		else
			sptime = stime - ptime;
		h.z = pfget_double_wdef(pf,"initial_depth",DEFAULT_TRIALZ);
		spvel = pfget_double_wdef(pf,"S-P_velocity",DEFAULT_SPVEL);
		dist = sptime*spvel;
		nangles = pfget_int_wdef(pf,"number_angles",DEFAULT_SPANGLES);
		/* We now trick the radial_grid_setup routine by 
		adding a set of parameters to the parameter space that it
		requires and then calling it with only one increment in 
		radial distance determined from the S-P time */
		pfput_double(pf,"center_latitude",clat);
		pfput_double(pf,"center_longitude",clon);
		pfput_double(pf,"center_depth",h.z);
		pfput_double(pf,"maximum_distance",dist);
		pfput_double(pf,"minimum_distance",dist);
		pfput_double(pf,"minimum_azimuth",0.0);
		pfput_double(pf,"maximum_azimuth",360.0);
		pfput_int(pf,"number_points_r",1);
		pfput_int(pf,"number_points_azimuth",nangles);
		pfput_int(pf,"ndepths",1);
		istat = radial_grid_setup(pf,&p,&ngrid);	
		/* This really should not happen, but we need to trap it if 
		it does */
		if(istat != 0)
		{
			h.lat = clat;
			h.lon = clon;
			h.time = ptime - sptime;
		}
		else
		{
			h = gridloc(attbl, utbl, p, ngrid, use_raw, options);
                        if((h.lat == 0.0) && (h.lon == 0.0) && (h.time == 0.0))
			{
                                elog_complain(1,"error in gridloc during radial grid scan\nSetting initial location to S-P station location\n");
				h.lat = clat;
                        	h.lon = clon;
                        	h.time = ptime - sptime;
                	}
                        free(p);
                }
		break;
/* Note a tricky logic used in these two cases.  If errors occur in 
any of the higher order functions (e.g. gridloc), the break statements are 
bypassed and the default is executed.  Note exploitation of parallel 
format of the two setup routines.*/

	case RECTANGLE_GRIDSEARCH:
		istat = lat_lon_grid_setup(pf, &p,&ngrid);
	case RADIAL_GRIDSEARCH:
		if(method == RADIAL_GRIDSEARCH)
			istat = radial_grid_setup(pf, &p,&ngrid);
		if(istat == 0)
		{
			h = gridloc(attbl, utbl, p, ngrid, use_raw, options);
			if((h.lat == 0.0) && (h.lon == 0.0) && (h.time == 0.0))
				elog_complain(1,"gridloc failure\nReverting to nearest station metho\n");
			else
			{
				free(p);
				break;
			}
		}
		else
		{
			elog_complain(1,"inital location setup problem\nReverting to default of nearest station method\n");
		}		
	default:
		ifirst = find_first_arrival(attbl);
		a = (Arrival *) gettbl(attbl,ifirst);
		h.lat = a->sta->lat;
		h.lon = a->sta->lon;
		h.lat += DELTA_FUDGE;
		h.lon += DELTA_FUDGE;
		h.time = a->time;
		if((s=pfget_string(pf,"initial_depth"))==NULL)
			h.z = DEFAULT_TRIALZ;
		else
			h.z = pfget_double_wdef(pf,"initial_depth",
							DEFAULT_TRIALZ);
		h.time -= h.z/VELOCITY;
	}

	h.lat0 = h.lat;
        h.lon0 = h.lon;
        h.z0 = h.z;
        h.t0 = h.time;
	return(h);
}

/* $Id$ */
