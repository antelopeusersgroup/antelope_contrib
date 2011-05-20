/* This is a collection of routines used in mwap that do various matrix
manipulations and inversions.  It is heavily dependent upon LAPACK routines
as implemented in sunperf.  It should be possible to link this without
using sunperf and using LAPACK instead, but this is an untested concept. */

#include <stdlib.h>
#include <math.h>
#include "multiwavelet.h"
/* Basic double pseudoinverse equation solver based on dgesvd routine
in sunperf that assumes more equations than unknowns.   Uses the 
output of dgesvd in the computation (i.e. dgesvd has to have been 
called first with a request to compute both right and left singular
vectors.  Example:
	dgesvd('o','a',m,n,G,m,s,NULL,m,vt,n,&info);

Arguments:
	U -- matrix of left singular vectors from dgesvd
	Vt -- matrix of right singular vectors (transpose) from dgesvd
	s - vector of singular values (assumed in descending order
		as produced by dgesvd)
	m - number of rows in original system of equations 
	n - number of columns in original system of equations 
	b - right hand side vector of length n
	maxcon - maximum condition number.  This is used to define
		SVD cutoff criteria by taking the largest singular
		value and dividing by this number.
	x - computed solution vector (returned).

Note: is is assume x has sufficient storage allocated.

The function returns the number of singular values kept in
computing the solution.

Author:  Gary pavlis
Written:  May 1999 as modification of similar routines in libgenloc.
*/

int pseudo_inv_solver(double *U, 
	double *Vt,
	double *s,
	int m, 
	int n, 
	double *b, 
	double maxcon, 
	double *x)
{
	double sv_cutoff;
	double *work;
	int nsvused;
	int i, j;

	work = (double *)calloc(n,sizeof(double));
	if(work == NULL) elog_die(0,"pseudoinverse solver cannot alloc array of %d doubles\n",
		n);

	/*dgesvd returns singular values in descending order so
	finding the largest is trivial.  We use this to establish
	the sv cutuff */
	sv_cutoff = s[0]/maxcon;  

	/* multiply by S-1 * UT */
	nsvused = 0;
	for(j=0;j<n;++j)
	{
		if(s[j]<sv_cutoff) break;
		work[j] = ddot(m,(U+j*m),1,b,1);
		work[j] /= s[j];
		++nsvused;
	}
	for(i=0;i<n;++i) x[i]=0.0;
	/* This is the right form because of Vt */
	for(j=0;j<nsvused;++j)
	{
		daxpy(n,work[j],(Vt+j),n,x,1);
	}
	free(work);
	return(nsvused);
}
/* This function compute a projection of a vector b onto the 
null space of a matrix using orthogonal matrices computed from
an svd of an original matrix using the sunperf routine 
dgesvd.  

Arguments:
	U - matrix of singular vectors used to compute the projector.
		The projector is computed as I - UU^T 
		Note:  vectors are assumed stored in columns ala FORTRAN
	m - number of rows in U
	n - number of columns of U to use in forming the projector.
		(the output is a copy of the input if n>= m)
	b - vector to be projected (length m)
	bp - vector to hold projection  (length m)

Normal return is 0.   Nonzero return indicates and error has been
posted with elog_log.  

Author:  Gary Pavlis
*/
int null_project(double *U,int m, int n, double *b, double *bp)
{
	int i;
	double val;

	dcopy(m,b,1,bp,1);
	if(n>=m)
	{
		elog_log(0,"null_project passed illegal U matrix of size %d by %d\nProjection request discarded\n",
			m,n);
		return(1);
	}
	for(i=0;i<n;i++)
	{
		val = ddot(m,(U+i*m),1,b,1);
		daxpy(m,-val,(U+i*m),1,bp,1);
	}
	return(0);
}

/* This function computes a covariance estimate for slowness 
vector estimates computed by mwap.  It is very specialized
in that it contains hard wired 3 dimensions for the number
of unknowns in this problem.  This estimate is not at all the same
as that proposed in Bear and Pavlis (1997).  They used variations in
estimates made in semblance/slowness space.  Here we use the estimated
uncertainties in the static estimates as estimates of the data covariance
that is scaled by the inverse of the slowness estimation matrix to 
produce a covariance estimate for the slowness vector.  

This routine is confused greatly by being forced to use FORTRAN
indexing to mesh with sunperf.  This leads to some very messy
indexing in a somewhat tricky algorithm I use to compute the
covariance with the SVD components. 

Arguments:
	stations - associative array of station objects
	statics - associative array of MWstatic objects
	c - 3x3 covariance estimate (result) in order of
		ux, uy, dt

Normal return is 0.  Postive returns mean a nonfatal problem
occurred that will be posted to elog.  The routine dies only
from malloc errors.  

Author: G Pavlis
Written:  March 2000
Modified:  March 2002
Removed the sample interval floor on the error.  Previously this
function did not allow the error for a single station to drop
below the one sample lever.  This was done to be conservative
but the new algorithm seems capable of resolving subsample
timing.  Hence, I removed this feature.
*/
int compute_slowness_covariance(Arr *stations,Arr *statics,
				 double *c)
{
	double *A;
	double vt[9];
	double svalue[3];
	double work[9];
	double *Cd1_2;  /* holds vector of diagonal elements of 
		data covariance to 1/2 power (useful for scaling)*/
	int nsta;  /* number of stations = rows in A */
	int nsta_used;  /*actual value when problems happen */
	MWstatic *mws;
	MWstation *s;
	Tbl *t;  /* tbl of keys used to parse statics arr */
	char *sta;
	int i,j,ii;
	int errcount=0;
	int info;

	for(i=0;i<9;++i) c[i] = 0.0;

	t = keysarr(statics);
	nsta = maxtbl(t);

	allot(double *,A,3*nsta);
	allot(double *,Cd1_2,nsta);

	for(i=0,ii=0;i<nsta;++i)
	{
		sta = gettbl(t,i);
		mws = (MWstatic *)getarr(statics,sta);
		s = (MWstation *)getarr(stations,sta);
		if(s==NULL)
		{
			elog_notify(0,"Station %s has a computed MWstatic but is not in station table\nStation array may be corrupted\n",sta);
			++errcount;
		}
		else
		{
			A[ii] = s->deast;
			A[ii+nsta] = s->dnorth;
			A[ii+2*nsta] = 1.0;
			Cd1_2[ii] = (mws->sigma_t);
			++ii;
		}
	}
	nsta_used = ii;
	dgesvd('o','a',nsta,3,A,nsta_used,svalue,NULL,nsta,vt,3,&info);
	/* Now we just compute covariance as C=A+(Cd)(A+)T 
	A+ = VS+UT.  We first replace A by U*S+ */
	for(i=0;i<3;++i)
	{
		dscal(nsta_used,1.0/svalue[i],A+i*nsta,1);
	}
	/* Another devious trick -- row scaling by Cd1_2 elements 
	forms proto form of V [ S+UT]Cd[US+T] VT */
	for(i=0;i<nsta_used;++i) dscal(3,Cd1_2[i],A+i,nsta);

	/* Now compute the term in brackets above = [ S+UT]Cd[US+T] */
	for(i=0;i<3;++i)
	{
		for(j=0;j<3;++j)
			c[i+3*j] = ddot(nsta_used,A+i,1,A+j,1);	
	}
	/* Now we have to complete the products with V and VT.  
	First VT */
	for(i=0;i<3;++i)
		for(j=0;j<3;++j)
			work[i+3*j] = ddot(3,c+i,3,vt+j*3,1);
	/* then V */
	for(i=0;i<3;++i)
		for(j=0;j<3;++j)
			c[i+3*j] = ddot(3,vt+i*3,1,work+j*3,1);	

	free(A);
	free(Cd1_2);
	return(errcount);
}
	
