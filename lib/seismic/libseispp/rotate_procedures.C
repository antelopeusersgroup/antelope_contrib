#include "seispp.h"
/* This function uses the analytic formula from Aki and Richards for
particle motion emergence angle for a P wave with a particular slowness
emergent at a half space.  The formula is not just Snell's law because
it include the effects of the P to S conversion at the free surface.

Arguments:
	vp0, vs0 - P and S velocities of the half space = surface 
	ux,uy - slowness vector components in standard geographical
		coordinates (x=+east, y=+north)

Returns a unit vector expressed as spherical coordinate angles
theta and phi.

Author:  G Pavlis
Written:  August 2000
*/
Spherical_Coordinate pm_halfspace_model(
		double vp0,
		double vs0,
		double ux,
		double uy)
{
	Spherical_Coordinate s;
	double sin_i,vpvs2,sini2;

	s.radius = 1.0;
	s.phi = atan2(uy,ux);
	sin_i = hypot(ux,uy)*vp0;
	sini2 = sin_i*sin_i;
	vpvs2 = vp0/vs0;
	vpvs2 *= vpvs2;
	s.theta = atan2( 2.0*sin_i*sqrt(vpvs2 - sini2),vpvs2 - 2.0*sini2);
	return(s);
}

/* This function defines a new set of angles for a ray based coordinate
system for the slowness vector ux, uy.  It does this in a somewhat 
odd way because particle motions can be strongly skewed from the 
theoretical value.  Thus we use angles relative to to a reference
ray direction not a pure model based estimate.  Said another way, 
we compute from a model the difference in emergence angle and 
azimuth for input slowness vectors uxref,uyref and ux,uy.  These
angles are added to the angles in rayref.  The angle manipulations
are done be assure the results are in the correct range for spherical
coordinates (theta 0 to pi/2 and -pi<phi<pi).
Author:  Gary Pavlis
Written:  August 2000
*/
Spherical_Coordinate compute_ray_coordinates(
	Spherical_Coordinate rayref,
	double vp0,
	double vs0,
	double uxref,
	double uyref,
	double ux,
	double uy)
{
	Spherical_Coordinate sref,snew;
	double dphi, dtheta;

	sref = pm_halfspace_model(vp0,vs0,uxref,uyref);
	snew = pm_halfspace_model(vp0,vs0,ux,uy);
	dtheta = snew.theta - sref.theta;
	dphi = snew.phi = sref.phi;
	rayref.theta += dtheta;
	rayref.phi += dphi;
	/* When the zonal angles goes negative, we assume it means we jumped
	across the z axis.  We fix this by adding 180 degrees to the azimuth
	and making the zonal angle positive*/
	if(rayref.theta < 0.0)
	{
		rayref.theta = - rayref.theta;
		rayref.phi += M_PI;
	}
	/* Make sure the azimuth is between -Pi and PI*/
	while(rayref.phi > M_PI) rayref.phi -= M_PI;
	while(rayref.phi < -M_PI) rayref.phi += M_PI;
	return(rayref);
}
