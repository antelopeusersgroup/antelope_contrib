#include "seispp.h"
#include "perf.h"
void Three_Component_Seismogram::rotate_to_standard()
{
	double *work[3];
	int i,j;
	if(components_are_cardinal) return;
	for(j=0;j<3;++j) work[j]=new double[ns];
	if(components_are_orthogonal)
	{
		//
		//Use a daxpy algorithm.  tmatrix stores the
		//forward transformation used to get current
		//Use the transpose to get back
		//
		for(i=0;i<3;++i)
		{
			dcopy(ns,x[0].s,1,work[i],1);
			dscal(ns,tmatrix[0][i],work[i],1);
			daxpy(ns,tmatrix[1][i],x[1].s,1,work[i],1);
			daxpy(ns,tmatrix[2][i],x[2].s,1,work[i],1);
		}
		for(i=0;i<3;++i) dcopy(ns,work[i],1,x[i].s,1);
	}
	else
	{
		//
		//Enter here only when the transformation matrix is
		//not orthogonal.  We have to construct a fortran 
		//order matrix a to use LINPACK routine in sunperf/perf
		//This could be done with the matrix template library 
		//but the overhead ain't worth it
		//
		double a[9];
		int ipivot[3];
		int info;
		double det;
		int asize=3;
		double awork[10];
		int ldwork=10;
		a[0] = tmatrix[0][0];
		a[1] = tmatrix[1][0];
		a[2] = tmatrix[2][0];
		a[3] = tmatrix[0][1];
		a[4] = tmatrix[1][1];
		a[5] = tmatrix[2][1];
		a[6] = tmatrix[0][2];
		a[7] = tmatrix[1][2];
		a[8] = tmatrix[2][2];
		//Perf lib matrix inversion routine using LU factorizatoin
		// Note this is changed from parent code.  Untested.
		dgetrf_(&asize,&asize,a,&asize,ipivot,&info);
		//Probably should throw an exception info != 0
		dgetri_(&asize,a,&asize,ipivot,awork,&ldwork,&info);
		
		tmatrix[0][0] = a[0];
		tmatrix[1][0] = a[1];
		tmatrix[2][0] = a[2];
		tmatrix[0][1] = a[3];
		tmatrix[1][1] = a[4];
		tmatrix[2][1] = a[5];
		tmatrix[0][2] = a[6];
		tmatrix[1][2] = a[7];
		tmatrix[2][2] = a[8];
		//
		//Yes these two blocks of code are different
		//Above multiplies with a transpose without building
		//it.  Here we have the transformation matrix
		//
		for(i=0;i<3;++i)
		{
			dcopy(ns,x[0].s,1,work[i],1);
			dscal(ns,tmatrix[i][0],work[i],1);
			daxpy(ns,tmatrix[i][1],x[1].s,1,work[i],1);
			daxpy(ns,tmatrix[i][2],x[2].s,1,work[i],1);
		}
		for(i=0;i<3;++i) dcopy(ns,work[i],1,x[i].s,1);
		components_are_orthogonal = true;
	}
	//
	//Have to set the transformation matrix to an identity now
	//
	for(i=0;i<3;++i)
		for(j=0;j<3;++j)
			if(i==j)
				tmatrix[i][i]=1.0;
			else
				tmatrix[i][j]=0.0;

	components_are_cardinal=true;
	for(i=0;i<3;++i) delete [] work[i];
}


/* This routine takes a spherical coordinate vector that defines
a given direction in space and returns a transformation matrix that
should be viewed as a transformation to ray coordinates under an 
assumption that this vector points in the direction of P wave 
particle motion.  If the theta angle is greater than PI/2 it 
switches the azimuth by 180 degrees so that the direction of the
transformed x1 axis will be pointing upward in space.  This removes
ambiguities in the transformation that make it easier to sort out
handedness of the transformation.  

The transformation produced for a P wave will be true ray coordinates
with X1 = transverse, X2 = radial, and X3 = longitudinal.  
The best way to understand the transformation is as a pair of 
rotations:  (1) rotate North to radial about z, (2) rotate z to
transverse around X1 (transverse).  Note this leaves X1 (transverse)
always as a purely horizontal direction.  It should also work for a 
principal component direction determined for an S phase, but the 
appropriate the only component that will make any sense after the
transformation, in that case, is the X3 direction = direction of 
inferred peak particle motion.  

One special case has to be dealt with.  If the direction passed into
the program is purely vertical (up or down), the function can only 
return an identity matrix because there is no way to determine a 
horizontal rotation direction.  

Arguments:
	xsc - spherical coordinate structure defining unit vector used
		to define the transform (radius is ignored).  Angles
		are assumed in radians.

Author:  Gary L. Pavlis
Written:  Sept. 1999
Modified:  Feb 2003
Original was plain C.  Adapted to C++ for seismic processing
*/
void Three_Component_Seismogram::rotate(Spherical_Coordinate xsc)
{
	int i;
	double theta, phi;  /* corrected angles after dealing with signs */
	double a,b,c,d;

	//
	//Undo any previous transformations
	//
	this->rotate_to_standard();
	// This maybe should generate an exception but assuming
	// this implies return to standard coordinates it should
	// be ok.  Note implicit transformation to cardinal directions
	//
	if(xsc.theta == 0.0)return;
       	if(xsc.theta == M_PI) 
	{
		//This will be left handed
		tmatrix[2][2] = -1.0;
		return;
	}

	if(xsc.theta < 0.0) 
	{
		theta = -(xsc.theta);
		phi = xsc.phi + M_PI;
		if(phi > M_PI) phi -= (2.0*M_PI);
	}
	else if(xsc.theta > M_PI_2)
	{
		theta = xsc.theta - M_PI_2;
		phi = xsc.phi + M_PI;
		if(phi > M_PI) phi -= (2.0*M_PI);
	}
	else
	{
		theta = xsc.theta;
		phi = xsc.phi;
	}
        a = cos(phi);
        b = sin(phi);
        c = cos(theta);
        d = sin(theta);

	tmatrix[0][0] = a*c;
	tmatrix[1][0] = b*c;
	tmatrix[2][0] = d;
	tmatrix[0][1] = -b;
	tmatrix[1][1] = a;
	tmatrix[2][1] = 0.0;
	tmatrix[0][2] = -a*d;
	tmatrix[1][2] = -b*d;
	tmatrix[2][2] = c;

	double *work[3];
	for(i=0;i<3;++i)work[i] = new double[ns];
	for(i=0;i<3;++i)
	{
		dcopy(ns,x[0].s,1,work[i],1);
		dscal(ns,tmatrix[i][0],work[i],1);
		daxpy(ns,tmatrix[i][1],x[1].s,1,work[i],1);
		daxpy(ns,tmatrix[i][2],x[2].s,1,work[i],1);
	}
	for(i=0;i<3;++i) dcopy(ns,work[i],1,x[i].s,1);
	//
	//If they weren't before they are now
	//
	components_are_orthogonal=true;
	components_are_cardinal=false;
	for(i=0;i<3;++i) delete [] work[i];
}
/* This routine takes a 3-d unit vector, nu, and converts it
to a Spherical_Coordinate structure which is returned.  The 
input coordinates are assume to be standard, right handed
cartesian coordinates in 1,2,3 order */
Spherical_Coordinate unit_vector_to_spherical(double nu[3])
{
	Spherical_Coordinate xsc;

	xsc.radius = 1.0;
	xsc.theta = acos(nu[2]);
	xsc.phi = atan2(nu[1],nu[0]);
	return(xsc);
}
void Three_Component_Seismogram::rotate(double nu[3])
{
	Spherical_Coordinate xsc=unit_vector_to_spherical(nu);
	this->rotate(xsc);
}
void Three_Component_Seismogram::apply_transformation_matrix(double a[3][3])
{
	int i;
	double *work[3];
	for(i=0;i<3;++i) work[i] = new double[3];
	double twork[3];
	for(i=0;i<3;++i)
	{
		dcopy(ns,x[0].s,1,work[i],1);
		dscal(ns,a[i][0],work[i],1);
		daxpy(ns,a[i][1],x[1].s,1,work[i],1);
		daxpy(ns,a[i][2],x[2].s,1,work[i],1);
	}
	for(i=0;i<3;++i) dcopy(ns,work[i],1,x[i].s,1);
	// update tmatrix -- note this is a matrix multiply
	// so this accumulates transformations
	for(i=0;i<3;++i)
	{
		twork[0]=tmatrix[0][i];
		twork[1]=tmatrix[1][i];
		twork[2]=tmatrix[2][i];
		tmatrix[0][i]=ddot(3,a[0],1,twork,1);
		tmatrix[1][i]=ddot(3,a[1],1,twork,1);
		tmatrix[2][i]=ddot(3,a[2],1,twork,1);
	}
	 for(i=0;i<3;++i) delete [] work[i];
}
