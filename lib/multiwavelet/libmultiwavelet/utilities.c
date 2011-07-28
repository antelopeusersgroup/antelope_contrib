#include <math.h>
#include <float.h>
#include "multiwavelet.h"
#include "perf.h"
/* This function is used to check a list of pf names to verify they
are in the parameter space.  It is useful for any program that uses
parameter files that cracks the pf space anywhere except up front.
That is, the pf space is a convenient place to store all the run time
parameters of any program and is a convenient way to pass a large
control structure.  However, because flow can be complex it is possible
to have parameters that are only accessed deep within a program and 
in these situations it is desirable to run a check at program initialization
to verify the required parameters will be there when required.  

Normally a string variable will not cause pfget to die, but it will
here if it is listed as required

Parameters to be checked are grouped by type.  Each numerical fields
can contain an optional range check.  This is not allowed for strings.
Code below only checks int, double, boolean, and string variables.  Because of
the complexity of Tbl and Arrs I thought this not worth messing with.
Furthermore, it is not unusual to have an empty Tbl list or Arr that
the program should handle correctly.  

Function will die on the first occurence of a problem parameter.
It is void because it only returns if everything checks out.

Author:  Gary L. Pavlis
*/
void check_required_pf(Pf *pf)
{
	Tbl *t,*testtbl;
	Pf *pf_required;
	char *key;
	int i,j,nitems;
	int itest, ilowcheck, ihighcheck;
	double dtest, dlowcheck, dhighcheck;
	int bool;
	char *line;
	char *ctest;
	char name[50];

	/* This cracks the "required" &Arr dies if it isn't present 
	at all.  This assumes you wouldn't call this routine if you
	weren't serious about checking */
	if(pfget(pf,"require",(void **)&pf_required) != PFARR)
		elog_die(0,"Arr of required parameters (require &Arr) missing from parameter space\nMust be present to execute this program\n");
	t=pfkeys(pf_required);
	for(i=0;i<maxtbl(t);++i)
	{
		key = gettbl(t,i);
		if(!strcmp(key,"int"))
		{
			testtbl = pfget_tbl(pf_required,"int");
			for(j=0;j<maxtbl(testtbl);++j)
			{
				line = gettbl(testtbl,i);
				nitems = sscanf(line,"%s%d%d",name,&ilowcheck,
							&ihighcheck);
				itest = pfget_int(pf,name);
				if(nitems == 3)
				{
					if( (itest < ilowcheck)
					  || (itest > ihighcheck) ) elog_die(0,
						"Parameter %s has value %d which is outside required range of %d to %d\n",
						   name,itest,ilowcheck,ihighcheck);
				}
			}
		}
		else if(!strcmp(key,"double"))
		{
			testtbl = pfget_tbl(pf_required,"double");
			for(j=0;j<maxtbl(testtbl);++j)
			{
				line = gettbl(testtbl,i);
				nitems = sscanf(line,"%s%lg%lg",name,&dlowcheck,
							&dhighcheck);
				dtest = pfget_double(pf,name);
				if(nitems == 3)
				{
					if( (dtest < dlowcheck)
					  || (dtest > dhighcheck) ) elog_die(0,
						"Parameter %s has value %lg which is outside required range of %lg to %lg\n",
						   name,dtest,dlowcheck,dhighcheck);
				}
			}
		}
		else if(!strcmp(key,"boolean"))
		{
			testtbl = pfget_tbl(pf_required,"boolean");
			for(j=0;j<maxtbl(testtbl);++j)
			{
				line = gettbl(testtbl,i);
				itest = pfget_boolean(pf,line);
			}
		}
		else if(!strcmp(key,"string"))
		{
			testtbl=pfget_tbl(pf_required,"string");
			for(j=0;j<maxtbl(testtbl);++j)
			{
				line = gettbl(testtbl,i);
				ctest = pfget_string(pf,line);
				if(ctest == NULL)
					elog_die(0,"Missing required string variable = %s\n",
						line);
			}
		}
		else
		{
			elog_notify(0,"Unknown required type name = %s\nRequired parameters under this heading will not be checked\n",
				key);
		}
	}
	freetbl(t,0);
}
/* NOTE:  the following two routines work with "spherical coordinate"
angles.  It bears repeating the these angles are in radians and
the phi angle always means an azimuth relative to north ala compass
bearings -- this is not the same as textbook spherical coordinates.
*/

/* This routine computes initial ray coordinate direction definition
based on slowness vector u0 and surface velocity tagged to the 
reference station.  It returns the spherical coordinates of the 
propagation vector in 3-space.  Note this is theoretical particle
motion direction for a P wave and orthogonal to theoretical motion
of an S phase.  Note also algorithm does try to use correct angle
for P or S phases.
*/
Spherical_Coordinate estimate_initial_polarization(MWSlowness_vector u0,
			Arr *stations,
			char *refsta,
			char *phase)
{
	Spherical_Coordinate x;
	MWstation *s;
	double slow;
	double pv;

	x.radius = 1.0;
	s = getarr(stations,refsta);
	if(s == NULL)
	{
		elog_complain(0,"estimate_initial_polarization function could not locate reference station %s\nSetting initial emergence angle to vertical\n",
			refsta);
		x.theta = 0.0;
	}
	else
	{
		slow = hypot(u0.ux,u0.uy);
		if(is_S(phase))
			pv = slow*(s->vs0);
		else
			pv = slow*(s->vp0);
		x.theta = asin(pv);
	}
	x.phi = atan2(u0.ux,u0.uy);
	return(x);
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
	x - spherical coordinate structure defining unit vector used
		to define the transform (radius is ignored).  Angles
		are assumed in radians.
	u - vector to hold output transformation.  Transformation matrix
		is stored in fortran order with column 1 of the
		transformation matrix being u[0], u[1], and u[2];
		column 2 starts with u[3], etc.  

Author:  Gary L. Pavlis
Written:  Sept. 1999
*/
void ray_coordinate_trans(Spherical_Coordinate x,double *u)
{
	int i;
	double theta, phi;  /* corrected angles after dealing with signs */
	double a,b,c,d;

	for(i=0;i<9;++i) u[i] = 0.0;
	if( (x.theta == 0.0) || (x.theta == M_PI) )
	{
		elog_notify(0,"ray_coordinate_trans cannot compute azimuth for a vertical vector.  Setting transformation to an identity\n");
		u[0] = 1.0;
		u[4] = 1.0;
		u[8] = 1.0;
		return;
	}

	if(x.theta < 0.0) 
	{
		theta = -(x.theta);
		phi = x.phi + M_PI;
		if(phi > M_PI) phi -= (2.0*M_PI);
	}
	else if(x.theta > M_PI_2)
	{
		theta = x.theta - M_PI_2;
		phi = x.phi + M_PI;
		if(phi > M_PI) phi -= (2.0*M_PI);
	}
	else
	{
		theta = x.theta;
		phi = x.phi;
	}
        a = cos(phi);
        b = sin(phi);
        c = cos(theta);
        d = sin(theta);

        u[0] = a*c;
        u[1] = b*c;
        u[2] = d;
        u[3] = -b;
        u[4] = a;
        u[5] = 0.0;
        u[6] = -a*d;
        u[7] =  -b*d;
        u[8] = c;

}
/* This routine takes a 3-d unit vector, nu, and converts it
to a Spherical_Coordinate structure which is returned.  The 
input coordinates are assume to be standard, right handed
cartesian coordinates in 1,2,3 order */
Spherical_Coordinate unit_vector_to_spherical(double *nu)
{
	Spherical_Coordinate x;

	x.radius = 1.0;
	x.theta = acos(nu[2]);
	x.phi = atan2(nu[1],nu[0]);
	return(x);
}
/* This small function uses parallel vectors w and x removing entries of 
x for which w[i] is 0.0 and overwriting x with these null entries removed.
Function returns a count of the number of entries transferred. 

n is the length of x and w while incx and incw are storage increment
ala BLAS.

Author:  G Pavlis
Written:  March 2000
*/
int remove_null_complex(int n, float *w, int incw, complex *z, int incz)
{
	complex *ztmp;
	int i,iw,iz,iztmp;

	allot(complex *,ztmp,n);

	for(i=0,iw=0,iz=0,iztmp=0;i<n;++i,iw+=incz,iz+=incz)
	{
		/* This is a safe test agaist 0 for a float */
		if(((w[iw]+1.0)-1.0)>FLT_EPSILON)
		{
			ztmp[iztmp].r = z[iz].r;
			ztmp[iztmp].i = z[iz].r;
			++iztmp;
		}
	}
	ccopy(iztmp,ztmp,1,z,incz);
	free(ztmp);
	return(iztmp);
}
/* This is an identical function for floats */
int remove_null_float(int n, float *w, int incw, float *x, int incx)
{
	float *xtmp;
	int i,iw,ix,ixtmp;

	allot(float *,xtmp,n);

	for(i=0,iw=0,ix=0,ixtmp=0;i<n;++i,iw+=incx,ix+=incx)
	{
		/* This is a safe test agaist 0 for a float */
		if(((w[iw]+1.0)-1.0)>FLT_EPSILON)
		{
			xtmp[ixtmp] = x[ix];
			++ixtmp;
		}
	}
	scopy(ixtmp,xtmp,1,x,incx);
	free(xtmp);
	return(ixtmp);
}

/* Fairly general interpolation routine to take a function specified on an irregular
grid of points defined by x1 and y1 and interpolate them onto a regular grid, y,
with a simple linear interpolation formula between (x1,y1) pairs.  
This algorithm was written with an implicit assumption that x1,y1 pairs were
widely spaced compared to dx so this was mainly a fill opertion.  I think it will
handle vertical discontinuities correctly, but I'm not sure it will deal with
a highly oversampled x1,y1 relative to dx.  Unpredictable behaviour is guaranteed
if the x1,y1 pairs are not in order of increasing x1.  

arguments:
	x1 - vector of length n1 of abscissa values of irregular grid to be 
		interpolated.
	y1 - parallel vector to x1 of function values.
	n1 - length of x1 and y1
	y - output function of regularly sampled values
	x0 - abscissa value of first sample of y
	dx - sample interval of y.
	ny - length of y.

Author:  Gary Pavlis
Written:  December 2001
Modified:  May 2002
Changed error codes.  Now returns an error if elements of y are set.
*/
int irregular_to_regular_interpolate(double *x1, double *y1, int n1,
	double *y, double x0, double dx, int ny)
{
	int i1,i;
	int istart,i1start;
	double grad;
	double x;
	int ny_set=0;

	/* first zero y*/
	for(i=0;i<ny;++i) y[i] = 0.0;
	/* silently do nothing if n1 is invalid */
	if(n1<=0) return(-1);

	/* find the starting point */
	i1start = 0;
	istart = nint((x1[0]-x0)/dx);  
	if(istart<0) 
	{
		do
		{
			++i1start;
			istart = nint((x1[i1start]-x0)/dx);
		}
		while (istart<0);
	}
	for(i=istart,i1=i1start;i<ny,i1<n1-1;++i1)
	{
		if(x1[i1+1]==x1[i1])continue;
		grad = (y1[i1+1]-y1[i1])/(x1[i1+1]-x1[i1]);
		x = x0 + dx*((double)i);
		while((x<=x1[i1+1]) && (i<ny) )
		{
			y[i] = y1[i1] + grad*(x-x1[i1]);
			++i;
			++ny_set;
			x += dx;
		}
	}
	if(ny_set<=0)
		return(-2);
	else
		return(0);
}

