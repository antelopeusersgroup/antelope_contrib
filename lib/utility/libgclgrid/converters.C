#include <stdio.h>
#include <math.h>
#include "gclgrid.h"


void GCLgrid::set_transformation_matrix()
{
	double pole_lat, pole_lon;
	double x0[3], xpole[3], xcros[3];

	latlon(lat0, lon0, M_PI_2,azimuth_y, &pole_lat, &pole_lon);

	dsphcar(lon0,lat0,x0);
	dsphcar(pole_lon, pole_lat, xpole);
	dr3cros(xpole, x0, xcros);

	//
	//these unit vectors are stored in rows of transformation matrix
	//yielding the matrix from geo to rotated cartesian of GCLgrid object
	//
	dcopy(3,x0,1,gtoc_rmatrix[2],1);
	dcopy(3,xpole,1,gtoc_rmatrix[1],1);
	dcopy(3,xcros,1,gtoc_rmatrix[0],1);
	dcopy(3,x0,1,translation_vector,1);
	dscal(3,r0,translation_vector,1);
}
Cartesian_point GCLgrid::gtoc(double plat, double plon, double pr)
{
	Cartesian_point p;
	double xp[3],dxp[3];
	int i;
	//dsphcar computes a unit vector.  We then scale it by radius
	dsphcar(plon, plat, xp);
	for(i=0;i<3;++i) xp[i]*=pr;
	//
	//Remove the origin translation vector
	//
	dr3sub(xp,translation_vector,dxp);
	//
	//Rotate by transformation matrix and that is it
	//
	p.x1=ddot(3,gtoc_rmatrix[0],1,dxp,1);
	p.x2=ddot(3,gtoc_rmatrix[1],1,dxp,1);
	p.x3=ddot(3,gtoc_rmatrix[2],1,dxp,1);
	return(p);
}
Geographic_point GCLgrid::ctog(double x1p, double x2p, double x3p)
{
	Geographic_point p;
	double dxp[3], x[3];
	int i;

	//
	//First apply rotation matrix to put this point in the
	//standard geographic reference frame.  We use a safe
	//BLAS algorithm for the multiply of the transpose 
	//
	for(i=0;i<3;++i) dxp[i]=0.0;
	daxpy(3,x1p,gtoc_rmatrix[0],1,dxp,1);
	daxpy(3,x2p,gtoc_rmatrix[1],1,dxp,1);
	daxpy(3,x3p,gtoc_rmatrix[2],1,dxp,1);
	//
	//dxp now contains the relative location vector in standard
	//coordinates.  We have to apply a translation vector
	//relative to the center of the earth before we can convert
	//to correct latitude and longitude
	//
	for(i=0;i<3;++i) x[i]=dxp[i]+translation_vector[i];
	dcarsph(x,&(p.lon),&(p.lat));
	p.r = dnrm2(3,x,1);
	return(p);
}
