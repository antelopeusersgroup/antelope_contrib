/* General distance weighting function using linear interpolation of 
arbitrary function specified on an irregular grid.  Returns 
weight derived from the weighting function for epicentral distance delta.
Is slightly cautious of inputs in this sense.  If delta <= delta of first
point, then function returns the weight assigned to first point.  
If delta > 360, returns 0.0 
Inputs:
	 wfunction   - pointer to weighting function structure.  
	delta - epicentral distance (in degrees). 
Author:
Gary L. Pavlis
Indiana University

Written:  February 1996
*/
#include "stock.h"
#include "coords.h"
#include "arrays.h"
#include "location.h"
float distance_weight(Distance_weight_function *wfunction, float delta)
{
	int i;
	Distance_weight_function *h;
	float w;  /* temporary */
	/* We just do a linear search being careful of endpoints */
	if(delta <= (wfunction->delta) ) return (wfunction->weight);
	if(delta >= 360.0) return (0.0);

	/* because of the above test, we can start at 1, not 0 */
	i = 1;
	h = wfunction;  /* Use the pointer h as a shorthand */
	while( ( h[i].delta < delta ) && ( h[i].delta < 360.0 ) ) ++i;
	
	w = (h[i-1].weight) + (delta - (h[i-1].delta))*(h[i-1].slope);
	if(w < 0.0) return(0.0);
	return(w);
}

/* Specific distance weighting function for arrival times */
float distance_weight_time(Arrival a, Hypocenter x)
{
	double delta, az;

	dist(rad(a.sta->lat), rad(a.sta->lon), 
			rad(x.lat), rad(x.lon), &delta, &az);	
	return( distance_weight(a.phase->arrival_time, (float)delta));
}
float distance_weight_ux(Slowness_vector s, Hypocenter x)
{
	double delta, az;

	dist(rad(s.array->lat), rad(s.array->lon), 
			rad(x.lat), rad(x.lon), &delta, &az);	
	return(distance_weight(s.phase->ux, (float)delta));
}
float distance_weight_uy(Slowness_vector s, Hypocenter x)
{
	double delta, az;

	dist(rad(s.array->lat), rad(s.array->lon), 
			rad(x.lat), rad(x.lon), &delta, &az);	
	return(distance_weight(s.phase->uy, (float)delta));
}

/* $Id$ */
