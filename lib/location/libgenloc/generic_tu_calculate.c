#include "stock.h"
#include "arrays.h"
#include "location.h"
/* calculate_travel_time is a generic function that takes an arrival time
object (a) and a current hypocenter estimate (stored in h) and calls the
externally set travel time function ttcalc with generic arguments x and 
mode.  (x holds source and receiver coordinates while mode defines how
much information the ttcalc function should calculate (see location.h) )

This function is generic because it know nothing about what the arrival 
is.  It will produce garbage or seg fault if the phase handle portion of
the arrival structure (argument a) is not set up properly 

Author:  Gary L Pavlis
Written:  August 1996
*/
Travel_Time_Function_Output calculate_travel_time(Arrival a, Hypocenter h, int mode)
{
	Travel_Time_Function_Output dt;
	Ray_Endpoints x;
	double *station_correction;
	x.sta = a.sta->name;
	x.slat = h.lat;
	x.slon = h.lon;
	x.sz = h.z;  /* Don't get confused about z0 in hypo structure here.
			see source for adjust_hypocenter */
	x.rlat = a.sta->lat;
	x.rlon = a.sta->lon;
	x.rz = -(a.sta->elev);

	dt = a.phase->ttcalc(x, a.phase->name, mode);
	/* Trap errors returned by ttcalc and return immediately */
	if(dt.time == TIME_INVALID) return(dt);

	/* Now we need to hunt for a station correction and add that term.
	Uses associative array routine getarr */
	station_correction = (double *)getarr(a.phase->time_station_corrections,
				a.sta->name);
	if(station_correction != NULL) dt.time += *station_correction;
	return(dt);
}
/* This is the equivalent routine for slowness vectors.  The inputs 
are identical to the travel time calculator, but the output structure
is, of course, different.  

This is a generic interface routine just like the travel time version.
The function pointer in the Arrival structure must be set or all hell
can break loose.

Author:  Gary L Pavlis
Written:  August 1996
*/

Slowness_Function_Output calculate_slowness_vector(Slowness_vector u, Hypocenter h, int mode)
{
	Slowness_Function_Output du;
	Ray_Endpoints x;
	double *ux_sc, *uy_sc;

        x.sta = u.array->name;
	x.slat = h.lat;
	x.slon = h.lon;
	x.sz = h.z;  /* Don't get confused about z0 in hypo structure here.
			see source for adjust_hypocenter */
	x.rlat = u.array->lat;
	x.rlon = u.array->lon;
	x.rz = -(u.array->elev);

	du = u.phase->ucalc(x, u.phase->name, mode);
	if( (du.ux == SLOWNESS_INVALID) || (du.uy == SLOWNESS_INVALID) )
		return(du);

	/* Now we need to hunt for a station correction and add that term.
	Uses associative array routine getarr */
	ux_sc = (double *)getarr(u.phase->ux_sc,
				u.array->name);
	if(ux_sc != NULL) du.ux += *ux_sc;
	uy_sc = (double *)getarr(u.phase->uy_sc,
				u.array->name);
	if(uy_sc != NULL) du.uy += *uy_sc;

	return(du);
}
/* A simple interface routine to utilize phase handles and return only 
a travel time.  Useful for routines other than the location code that
only care about travel times, or some location algorithms that do 
not require time derivatives*/
	
double generictt(Arrival a, Ray_Endpoints x)
{
	Travel_Time_Function_Output tt;
	Hypocenter h;
	/* We could get by without this initialization, but 
	it is good programming to assure things like this are
	properly initialized. */
	h.dx = 0.0;
	h.dy = 0.0;
	h.dz = 0.0;
	h.dt = 0.0;
	h.lat = x.slat;
	h.lon = x.slon;
	h.z = x.sz;
	h.lat0 = 0.0;
	h.lon0 = 0.0;
	h.z0 = 0.0;
	h.t0 = 0.0;
	h.rms_raw = 0.0;
	h.rms_weighted = 0.0;
	h.interquartile = 0.0;
	h.number_data = 0;
	h.degrees_of_freedom = 0;
	tt = calculate_travel_time(a, h, RESIDUALS_ONLY);
	return(tt.time);
}

/* $Id$ */
