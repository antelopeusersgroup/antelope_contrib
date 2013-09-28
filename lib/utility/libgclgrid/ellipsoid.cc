#include <math.h>
#include "gclgrid.h"
/* Standize functions to handle ellipticity corrections */

/* Earth ellipticity constants from Turcotte and Schubert */
#define R_EQUATOR 6378.139
#define FLATTENING 0.00335282
/* r0_ellipse returns the sea level geoid radius at latitute
lat in radians */
double r0_ellipse(double lat)
{
        double r, sinlat;
        sinlat = sin(lat);
        r = R_EQUATOR*(1.0 - sinlat*sinlat*FLATTENING);
        return(r);
}
/* Converts from a radius to a depth for a standard ellipse */
double r_to_depth(double r, double lat)
{
	double r0;
	r0=r0_ellipse(lat);
	return(r0-r);
}
double GCLgrid::depth(int i, int j)
{
	return(r0_ellipse(lat(i,j))-r(i,j));
}
double GCLgrid3d::depth(int i, int j, int k)
{
	return(r0_ellipse(lat(i,j,k))-r(i,j,k));
}
double BasicGCLgrid::depth(Cartesian_point p)
{
	Geographic_point gp;
	gp=ctog(p);
	return(r0_ellipse(gp.lat) - gp.r);
}
double BasicGCLgrid::depth(Geographic_point p)
{
	return(r0_ellipse(p.lat) - p.r);
}
