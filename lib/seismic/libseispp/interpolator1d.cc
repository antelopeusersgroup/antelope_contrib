#include <float.h>
#include "interpolator1d.h"
#include "perf.h"
#include "dmatrix.h"
/* This is a collection of interpolators for 1D scalar or vector
functions of one variable.  I have stolen portions from Igor Morozov, but
changed the wrapper completely.  The collection is wrapped in a namespace
called INTERPOLATOR1D with the idea that normal use of these functions
would use the scope resolution operator to make it's relation to these
functions crystal clear.  e.g. INTERPOLATOR1D::linear_scalar() instead of
linear_scale() or you are likely to have a name conflict.  This 
library could have been fully objectized, but I chose to keep it 
procedural because these operations are commonly done on primitives.*/
#include <math.h>
namespace INTERPOLATOR1D
{
/* primitive for 2 point linear interpolation.  Uses a weight formula
instead of an explicit point-slope to reduce operation counts.  
Probably a trivial detail, but it uses it anyway.  

Arguments:
	x - abscissa value to interpolate at
	(x1,y1) and (x2,y2) are endpoints to interpolate x between
	(assumes x is between x1 and x2)
*/
double linear_scalar ( double x, double x1, double y1,
                                        double x2, double y2 )
{
	double a,dx,adx;
	if(fabs((x1-x2)/x1)<=DBL_EPSILON)
		return((y1+y2)/2.0);
	else
	{
		a = 1.0/(x2-x1);
		dx = x - x1;
		adx = a*dx;
		return( (1.0-adx)*y1 + adx*y2);
	}
}
/* Primitive for 2 point linear interpolation of a vector function.
Arguments are the same as above but y1 and y2 are assumed to be
vectors of length nv.  Result is returned in y.  I considered
this subroutine like behaviour less error prone that the potential
memory leak from allocating y internally and returning it.  
Note carefully that no checking on array bounds is done for 
efficiency.  We use the blas, which will be slower for small
vectors but makes this more general.  A quadrature weight is used
instead of the explicit point slope form to allow the vector 
operations.
*/
void linear_vector (double x, double x1, double *y1,
	double x2, double *y2, int nv, double *y)
{
	double a,dx,adx;
	double w1, w2;
	if(fabs((x1-x2)/x1)<=DBL_EPSILON)
	{
		dcopy(nv,y1,1,y,1);
		daxpy(nv,1.0,y2,1,y,1);
	}
	else
	{
		a = 1.0/(x2-x1);
		dx = x - x1;
		adx = a*dx;
		w1 = 1.0 - adx;
		w2 = adx;
		dcopy(nv,y1,1,y,1);
		dscal(nv,w1,y,1);
		daxpy(nv,w2,y2,1,y,1);
	}
	
}

/* lookup function for a regular grid.  Returns index for point at xp in grid
with 0 value of min_x.  It simply returns a number and assumes caller will
handle negative indices or numbers too large. */
int regular_lookup(double xp, double min_x, double dx)
{
	return( (int) floor ( ( xp - min_x ) / dx ) );
}
int irregular_lookup(double xp, double *x,int nx)
{
	int i;
	for(i=-1;i<nx;++i)
		if(x[i+1]>xp) return(i);
	if(fabs(x[nx-1]-xp)<FLT_EPSILON)
		return(nx-1);
	else
		return(nx);
}
/* Interpolate 1 point at x for a regular grid of scalars.
*/

double linear_scalar_regular ( double	xp,	/* argument */
			double	x0,	/* minimum x value */
			double	dx,	/* grid increment */
			double	*y,	/* function values */
			int	nx	/* number of values */ )
{
	int	i;
	double yout;
	double x1;
	i = INTERPOLATOR1D::regular_lookup(xp,x0,dx);
	if(i<0) return y[0];
	if(i>(nx-2)) return(y[nx-1]);
	x1 = x0 + ((double)i)*dx;
	yout = INTERPOLATOR1D::linear_scalar(xp, x1, y [ i ], x1 + dx, y [ i + 1 ] );
	return(yout);
}

/* Same for an irregular grid */
double linear_scalar_irregular(double xp,double *x, double *y, int nx)
{
	int i;
	double yout;

	i = INTERPOLATOR1D::irregular_lookup(xp,x,nx);
	if(i<0) return y[0];
	if(i>(nx-2)) return(y[nx-1]);
	yout = INTERPOLATOR1D::linear_scalar(xp,x[i],y[i],x[i+1],y[i+1]);
	return(yout);
}
/* top level functions that call the above primitives many times to 
interpolate one mesh onto another */
void linear_scalar_regular_to_regular(int nin, double x0in, double dxin, double *yin,
		int nout, double x0out, double dxout, double *yout)
{
	for(int i=0;i<nout;++i)
	{
		double xp;
		xp = x0out + ((double)i)*dxout;
		yout[i] = INTERPOLATOR1D::linear_scalar_regular(xp,x0in,dxin,yin,nin);
	}
}
void linear_scalar_irregular_to_regular(int nin, double *xin, double *yin,
		int nout, double x0out, double dxout, double *yout)
{
	for(int i=0;i<nout;++i)
	{
		double xp;
		xp = x0out + ((double)i)*dxout;
		yout[i] = INTERPOLATOR1D::linear_scalar_irregular(xp,xin,yin,nin);
	}
}

void linear_scalar_regular_to_irregular(int nin, double x0in, double dxin, double *yin,
		int nout, double *xout, double *yout)
{
	for(int i=0;i<nout;++i)
	{
		yout[i] = INTERPOLATOR1D::linear_scalar_regular(xout[i],x0in,dxin,yin,nin);
	}
}
void linear_scalar_irregular_to_irregular(int nin, double *xin, double *yin,
		int nout, double *xout, double *yout)
{
	for(int i=0;i<nout;++i)
	{
		yout[i] = INTERPOLATOR1D::linear_scalar_irregular(xout[i],xin,yin,nin);
	}
}

/* Now the comparable family for vector functions of 1d.  Here the function
is assumed stored in a matrix with the vectors in the columns.  Because
we use a matrix object we don't have to pass all the size variables.  Allocates
and returns a vector of doubles that is the interpolated vector. */
double *linear_vector_regular ( double	xp, double x0, double dx,dmatrix& y)
{
	int	i;
	int nx = y.columns();
	int nv = y.rows();
	double *yout = new double[nv];
	double x1;
	i = INTERPOLATOR1D::regular_lookup(xp,x0,dx);
	if(i<0)
		dcopy(nv,&y(0,0),1,yout,1);
	else if(i>(nx-2)) 
		dcopy(nv,&y(0,nx-1),1,yout,1);
	else
	{
		x1 = x0 + ((double)i)*dx;
		INTERPOLATOR1D::linear_vector(xp, x1,&y(0,i),
					x1+dx, &y(0,i+1),nv,yout);
	}
	return(yout);
}

/* Same for an irregular grid */
double *linear_vector_irregular(double xp,double *x, dmatrix& y)
{
	int i;
	int nx = y.columns();
	int nv = y.rows();
	double *yout = new double[nv];

	i = INTERPOLATOR1D::irregular_lookup(xp,x,nx);
	if(i<0)
		dcopy(nv,&y(0,0),1,yout,1);
	else if(i>(nx-2)) 
		dcopy(nv,&y(0,nx-1),1,yout,1);
	else
	{
		INTERPOLATOR1D::linear_vector(xp, x[i],&y(0,i),
					x[i+1], &y(0,i+1),nv,yout);
	}
	return(yout);
}
/* Now for top level functions like the scalar versions */
void linear_vector_regular_to_regular(double x0in, double dxin, dmatrix& yin,
		double x0out, double dxout, dmatrix& yout)
{
	int nin = yin.columns();
	int nout = yout.columns();
	int nv = yin.rows();
	double *ytmp;

	for(int i=0;i<nout;++i)
	{
		double xp;
		xp = x0out + ((double)i)*dxout;
		ytmp = INTERPOLATOR1D::linear_vector_regular(xp,x0in,dxin,yin);
		dcopy(nv,ytmp,1,&yout(0,i),1);
		delete [] ytmp;
	}
}
void linear_vector_irregular_to_regular(double *xin, dmatrix& yin,
		double x0out, double dxout, dmatrix& yout)
{
	int nin = yin.columns();
	int nout = yout.columns();
	int nv = yin.rows();
	double *ytmp;
	for(int i=0;i<nout;++i)
	{
		double xp;
		xp = x0out + ((double)i)*dxout;
		ytmp = INTERPOLATOR1D::linear_vector_irregular(xp,xin,yin);
		dcopy(nv,ytmp,1,&yout(0,i),1);
		delete [] ytmp;
	}
}

void linear_vector_regular_to_irregular(double x0in, double dxin, dmatrix& yin,
		double *xout, dmatrix& yout)
{
	int nin = yin.columns();
	int nout = yout.columns();
	int nv = yin.rows();
	double *ytmp;

	for(int i=0;i<nout;++i)
	{
		ytmp = INTERPOLATOR1D::linear_vector_regular(xout[i],x0in,dxin,yin);
		dcopy(nv,ytmp,1,&yout(0,i),1);
		delete [] ytmp;
	}
}
void linear_vector_irregular_to_irregular(double *xin, dmatrix& yin,
		double *xout, dmatrix& yout)
{
	int nin = yin.columns();
	int nout = yout.columns();
	int nv = yin.rows();
	double *ytmp;

	for(int i=0;i<nout;++i)
	{
		ytmp = INTERPOLATOR1D::linear_vector_irregular(xout[i],xin,yin);
		dcopy(nv,ytmp,1,&yout(0,i),1);
		delete [] ytmp;
	}
}

// end of namespace
}
