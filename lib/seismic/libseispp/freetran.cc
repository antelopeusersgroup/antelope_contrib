#include "seispp.h"
using namespace SEISPP;
namespace SEISPP {
/* This function computes and applies the free surface tranformaton
matrix described by Kennett 1991.  The result is a ray coordinate
transformation with x1=transverse, x2=radial, and x3=longitudinal.
Note this transformation is into a nonorthogonal system.  

Algorithm first applies a rotation of horizontal coordinates to 
horizonal radial and transverse, then applies free surface 
transformation to the radial-vertical plane.

The free surface transformation code segment is a direct 
translation of m file from Michael Bostock.  

Author:  Gary Pavlis
*/
void Three_Component_Seismogram::free_surface_transformation(Slowness_vector uvec,
		 double a0, double b0) 
{
	double a02,b02,pslow,p2;
	double qa,qb,vpz,vpr,vsr,vsz;
	double umag=uvec.mag();
	pslow=uvec.mag();
	// silently do nothing if magnitude of the slowness vector is 0
	// (vertical incidence)
	if(pslow<DBL_EPSILON) return;
	// Can't handle evanescent waves with this operator
	double vapparent=1.0/pslow;
	if(vapparent<a0 || vapparent<b0)
		throw seispp_error(string("free_surface_transformation:  ")
		+string("cannot handle evanescent waves.\n")
		+string("Surface velocities must be smaller than apparent velocity"));

	// First the horizonal rotation
	Spherical_Coordinate scor;
	scor.phi = M_PI_2 - uvec.azimuth(); // geo azimuth != phi of scor
	scor.theta=0.0;
	scor.radius=1.0;
	// after this transformation x1=transverse horizontal
	// x2=radial horizonal, and x3 is still vertical
	this->rotate(scor);

	a02=a0*a0;
	b02=b0*b0;
	p2=pslow*pslow;
	qa=sqrt((1.0/a02)-p2);
	qb=sqrt((1.0/b02)-p2);
	vpz=-(1.0-2.0*b02*p2)/(2.0*a0*qa);
	vpr=pslow*b02/a0;
	vsr=(1.0-2.0*b02*p2)/(2.0*b0*qb);
	vsz=pslow*b0;
	// Now construct the transformation matrix
	// This is different from Bostock's original code
	// in sign and order.  
	double fstran[3][3];
	fstran[0][0]=0.5;  fstran[0][1]=0.0;  fstran[0][2]=0.0;
	fstran[1][0]=0.0;  fstran[1][1]=vsr;  fstran[1][2]=vpr;
	fstran[2][0]=0.0;  fstran[2][1]=vsz;  fstran[2][2]=vpz;
	this->apply_transformation_matrix(fstran);

	components_are_cardinal=false;
	components_are_orthogonal=false;
}
} // end namespace SEISPP 
