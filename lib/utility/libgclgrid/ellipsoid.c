#include <math.h>
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
