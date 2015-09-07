#include <math.h>
#include <float.h>
#include "stock.h"
#include "perf.h"
#include "arrays.h"
#include "coords.h"
#include "elog.h"
#include "multiwavelet.h"

/* This function takes complex numbers x, y, and z defined by an 
eigenvector for a multiwavelet (would work for Fourier transforms
too, however) and returns a pointer to a structure that defines
the major and minor axes of the particle motion vectors defined
by those three complex numbers.  The up vector (assumed to be
three element vector) defines the direction used to resolve
the sign ambiguity inherent in defining an ellipse.  That is,
both the major and minor component directions are required
to have a positive projection in the up direction.  If they 
aren't the sign is flipped before returning.  Normally up 
would point [0,0,1] or in the up radial direction for P waves.
For S, it becomes more ambiguous and should be sorted out 
by a more complicated method.

The polarization information (defined by the Particle_Motion_Ellipse 
structure) is allocated within this routine. 

Author:  G. L. Pavlis
Written:  October 1999
*/
Particle_Motion_Ellipse compute_particle_motion(complex x, 
						complex y, 
						complex z,
						double *up)
{
	double rx,ry,rz,thetax,thetay,thetaz;  /* polar forms of x,y,z*/
	double a,b;
	double phi1,phi2;
	double x1[3],x2[3];
	double nrmx1,nrmx2;
	Particle_Motion_Ellipse e;


	rx = hypot((double)x.r,(double)x.i);
	ry = hypot((double)y.r,(double)y.i);
	rz = hypot((double)z.r,(double)z.i);
	thetax = atan2((double)x.i,(double)x.r);
	thetay = atan2((double)y.i,(double)y.r);
	thetaz = atan2((double)z.i,(double)z.r);

	a = rx*rx*cos(2.0*thetax) 
		+ ry*ry*cos(2.0*thetay) 
		+ rz*rz*cos(2.0*thetaz);
	b = rx*rx*sin(2.0*thetax) 
		+ ry*ry*sin(2.0*thetay) 
		+ rz*rz*sin(2.0*thetaz);

	phi1 = atan2(-b,a)/2.0;
	phi2 = phi1 + M_PI_2;

	x1[0] = rx*cos(phi1+thetax);
	x1[1] = ry*cos(phi1+thetay);
	x1[2] = rz*cos(phi1+thetaz);
	x2[0] = rx*cos(phi2+thetax);
	x2[1] = ry*cos(phi2+thetay);
	x2[2] = rz*cos(phi2+thetaz);

	nrmx1 = dnrm2(3,x1,1);
	nrmx2 = dnrm2(3,x2,1);
	/* normalize to unit vectors */
	dscal(3,1.0/nrmx1,x1,1);
	dscal(3,1.0/nrmx2,x2,1);

	if(nrmx1>nrmx2)
	{
		dcopy(3,x1,1,e.major,1);
		dcopy(3,x2,1,e.minor,1);
		e.rectilinearity = (1.0 - nrmx2/nrmx1);
	}
	else
	{
		dcopy(3,x2,1,e.major,1);
		dcopy(3,x1,1,e.minor,1);
		e.rectilinearity = (1.0 - nrmx1/nrmx2);
	}
	/* Choose the positive sign direction */
	if(ddot(3,up,1,e.major,1) < 0.0)
		dscal(3,-1.0,e.major,1);
	if(ddot(3,up,1,e.minor,1) < 0.0)
		dscal(3,-1.0,e.minor,1);
	return(e);
}
	
/* this is a blas like function analogous to scopy, dcopy, etc
for a matrix of pointers to Particle_Motion_Vector objects.  I could
have made this a general matrix function, I suppose, but I decided that
would be a bit opaque, and would promote on of C's most evil features.
The pointers are blindly copied and it assumed the output vector 
bounds are not violated.

Arguments:
	n - number of elements in input and output vectors]
	x - input vector of pointers
	incx - storage increment of x ala blas
	y - output vector
	incy - storage increment of y ala blas.
Written:  February 2000
Author:  G Pavlis
*/
void pmvector_copy(int n, Particle_Motion_Ellipse *x, int incx,
			Particle_Motion_Ellipse *y, int incy)
{
	int i,ix,iy;

	for(i=0,ix=0,iy=0;i<n;++i,ix+=incx,iy+=incy)
	{
		y[iy] = x[ix];
	}
}
#define PM_MINSCALE_MAJOR 0.2  /* This needs to be pretty large compared to 
				good data because if the errors get much
				larger than this the results are trash anyway */
#define PM_MINSCALE_MINOR 1.0 /* Minor axis can easily be totally random.  
				Nearly always happens for pure linear pm.
				This essentially turns off robust weighting */
void pmvector_average(Particle_Motion_Ellipse *pmv, int n,
	Particle_Motion_Ellipse *pmavg, Particle_Motion_Error *pmerr)
{
	int i,j,ii;
	double *v;  /* work space used to store coordinates passed
			to m-estimator routine */
	double avg[3];
	double *weight;
	double nrm_major, nrm_minor;  
	Spherical_Coordinate scoor;
	double U[9];   /* transformation matrix*/
	double work[3];
	double *workn;
	double dotprod;
	double sumsq,sumwt;
	int ndgf;
	MW_scalar_statistics stats;
	double nrmtest;

	allot(double *,v,3*n);
	allot(double *,weight,n);
	allot(double *,workn,n);
	for(i=0,ii=0;i<n;++i,ii+=3)
	{
		/* This could be done with the blas, but it would
		be more obscure and no faster */
		v[ii] = pmv[i].major[0];
		v[ii+1] = pmv[i].major[1];
		v[ii+2] = pmv[i].major[2];
	}
	/* We use relative scaling here because the pm vectors are 
	not normalized.  We could use absolute scaling if we normalized
	them above.  This is a modification that might actually give
	better results.  */
	M_estimator_double_n_vector(v,3,n,
		IQ_SCALE_RELATIVE,PM_MINSCALE_MAJOR,avg,weight);
	nrm_major = dnrm2(3,avg,1);
	for(i=0;i<3;++i) 
	{
	    /* Needed to avoid random NaN */
	    if(nrm_major<FLT_EPSILON)
	    	pmavg->major[i] = avg[i];
	    else
		pmavg->major[i] = avg[i]/nrm_major;
	}

	/* Error estimates are computed completely differently here from
	that described in Bear and Pavlis (1999).  Rather than use a 
	jackknife on individual angles, here I've chosen to use a simple
	standard deviation measure using weighted residuals.  The residuals,
	however, are computed from total angular separation computed using
	a dot product.  This allows us to avoid wraparound errors that 
	are inevitable with angles.  

	First step is to compute a vector of angle residuals.  */
	for(i=0,ii=0;i<n;++i,ii+=3)
	{
		dotprod = ddot(3,v+ii,1,pmavg->major,1);
		dotprod /= dnrm2(3,v+ii,1);
		workn[i] = acos(dotprod);
	}
	/* weighted mean formula for error */
	for(i=0,sumwt=0.0,sumsq=0.0;i<n;++i)
	{
		sumsq += workn[i]*workn[i]*weight[i]*weight[i];
		sumwt += weight[i];
	}
	ndgf = nint(sumwt) - 3;
	if(ndgf<1)
	{
		elog_notify(0,"pmvector_average:  sum of weights = %lf in major axis average implies degrees of freedom less than 1\nUsing 1 degree of freedom\n",sumwt);
		ndgf = 1;
	}
	pmerr->ndgf_major = ndgf;
	pmerr->dtheta_major =  sqrt(sumsq/((double)ndgf));
	/* We scale the azimuthal error by 1/sin(theta) to get a stable
	error estimate that correctly goes to infinitity when theta -> 0*/
	scoor = unit_vector_to_spherical(pmavg->major);
	pmerr->dphi_major = (pmerr->dtheta_major)/sin(scoor.theta);

	/* We first project the minor axis vectors onto the plane
	perpendicular to the average major axis.  This reduces the
	degrees of freedom in a way that I consider reasonable and
	is in line with with Lorie Bear did */
	for(i=0,ii=0;i<n;++i,ii+=3)
	{
		double minor_scale;
		/* Intentionally ignore error return of null project because
		the only error condition in current code cannot happen 
		with this call.  null_project writes result in the
		last argument, so this step is functionally like the 
		v[ii]=pmv[i].major, etc. loop above, but combines
		the projection operation . */
		null_project(pmavg->major,3,1,pmv[i].minor,v+ii);
		/* We also want to scale the vector by a factor
		that is determinable from rectilinearity to keep
		the axis length consistent to allow a refined
		rectilinearity average below */
		minor_scale = 1.0 - pmv[i].rectilinearity;
		dscal(3,minor_scale,v+ii,1);
	}
	/* This constructs a rotational tranformation to a coordinate
	system where x1 and x2 are in the desired projection plane.
	Actually, the null projection above is redundant, but for now
	the extra work is largely irrelevant and is a good cross check
	for debugging. */
	ray_coordinate_trans(scoor,U);
	for(i=0;i<n;++i)
	{
		for(j=0;j<3;++j) 
		{
			work[j] = ddot(3,v+j+3*i,1,U+j,3);
		}
		dcopy(3,work,1,v+3*i,1);
	}
	/* Note the change from above to a 2-d space now.  The above 
	transformations zero the x3 direction after the transformation */
	M_estimator_double_n_vector(v,2,n,
		IQ_SCALE_RELATIVE,PM_MINSCALE_MINOR,avg,weight);
	avg[2] = 0.0;
	nrm_minor = hypot(avg[0],avg[1]);
	/* This is the inverse tranformation -- u is orthogonal */
	for(j=0;j<3;++j)
		work[j] = ddot(3,U+j*3,1,avg,1);

	/* This is similar to above, but, perhaps incorrectly, the
	degrees of freedom are larger by one because we reduce the
	space to 2d */
	for(i=0,ii=0;i<n;++i,ii+=3)
	{
		dotprod = ddot(2,v+ii,1,avg,1);
		nrmtest = dnrm2(2,v+ii,1);
		if(nrmtest<=0.0)
		{
			elog_notify(0,"pmvector_average:  minor axis estimate %d of %d estimates has 0 projection perpendicular to major\nArtificially set to average\n",
				i,n);
			workn[0] = 0.0;
		}
		else
		{
			dotprod/= nrmtest;
		/* because avg wasn't normalized we have divide by norm */
			dotprod /= nrm_minor;
			workn[i] = acos(dotprod);
		}
	}
	/* We want the final result normalized to a unit vector length */
	for(i=0;i<3;++i) pmavg->minor[i] = work[i]/nrm_minor;

	/* weighted mean formula again */
	for(i=0,sumwt=0.0,sumsq=0.0;i<n;++i)
	{
		sumsq += workn[i]*workn[i]*weight[i]*weight[i];
		sumwt += weight[i];
	}
	ndgf = nint(sumwt) - 2;
	if(ndgf<1)
	{
		elog_notify(0,"pmvector_average:  sum of weights = %lf in minor axis average implies degrees of freedom less than 1\nUsing 1 degree of freedom\n",sumwt);
		ndgf = 1;
	}
	pmerr->ndgf_minor = ndgf;
	pmerr->dtheta_minor =  sqrt(sumsq/((double)ndgf));
	/* We cast the minor axis in spherical coordinates like
	the major axis.  This differ's from Lorie's skew measures, but
	it is simpler to deal with in a database output as it treats
	the two vector in a common way */
	scoor = unit_vector_to_spherical(pmavg->minor);
	pmerr->dphi_minor = (pmerr->dtheta_major)/sin(scoor.theta);

	/* Finally, we deal with rectilinearity.
	We use the contents of v which are the projected
	minor axis values rather than the raw minor
	axes.  This estimator will tend to give slightly
	better rectilinearity using the raw vectors
	because a projection is always <= original */
	for(i=0;i<n;++i)
	{
		double minor_nrm;
		minor_nrm = dnrm2(3,v+i*3,1);
		/* Not needed because the major axis vector
		was previously normalized to unit length 
		major_nrm = dnrm2(3,pmv[i].major,1);
		*/

		workn[i] = 1.0 - minor_nrm;
	}
	stats = MW_calc_statistics_double(workn,n);
	pmavg->rectilinearity = stats.median;
	pmerr->ndgf_rect = n - 1;
	/* Assume a simple normal distribution to convert interquartiles
	to standard deviation */
	pmerr->delta_rect = NORMAL_IQSCALE*((stats.q3_4)-(stats.q1_4));

	free(weight);
	free(workn);
	free(v);
}
/* simple function to copy a polarization vector described in
spherical coordinates.  Arguments are obvious */
void copy_polarization(Spherical_Coordinate *from, Spherical_Coordinate *to)
{
	to->radius = from->radius;
	to->theta = from->theta;
	to->phi = from->phi;
}
