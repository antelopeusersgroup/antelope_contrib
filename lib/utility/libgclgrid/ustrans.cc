#include "gclgrid.h"
#include "dmatrix.h"
/* Sometimes it is useful to work in unit spherical coordinates.  That is,
 * local coordinates at a given latitude and longitude with x1 local host,
 * x2 = local north, and x3 local up.  The orientation of this coordinate
 * system is different at every point on the earth.  This function computes
 * the transformation matrix from this reference frame TO that used in
 * the GCLgrid object.  Note this is ONLY the rotational component, not
 * the translation vector which is stored with the GCLgrid object.  
 * Note also the returned transformation from coordinates in a local
 * frame at lat,lon to the GCLgrid coordinate frame.  The inverse transform
 * is just the transpose of this matrix because the basis vectors are
 * orthogonal.
 */

dmatrix ustrans(GCLgrid& g, double lat, double lon)
{
	double colat=M_PI_2-lat;
	dmatrix usph(3,3);
	dmatrix grm(3,3);
	// load u_phi, u_theta, and u_r into successive columns of usph
	// using equation (4), p. 463 of Stein and Wysession
	usph(0,0)=-sin(lon);
	usph(1,0)=cos(lon);
	usph(2,0)=0.0;
	usph(1,0)=cos(colat)*cos(lon);
	usph(1,1)=cos(colat)*sin(lon);
	usph(1,2)=-sin(colat);
	usph(2,0)=sin(colat)*cos(lon);
	usph(2,1)=sin(colat)*sin(lon);
	usph(2,2)=cos(colat);
	grm=g.fetch_transformation_matrix();
	dmatrix Ut(3,3);
	Ut=grm*usph;
	dmatrix U(3,3);
	U=tr(Ut);
	return(U);
}

