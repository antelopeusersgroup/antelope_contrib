#include <sunperf.h>
#include "stock.h"
/* Equation solver for pseudoinverse solutions using output of lapack
double precision svd function.

None of the input vectors is altered to allowed repeated solutions with
the same matrix.  Only x and an internal work vector are altered.

Arguments:
	m,n - original matrix A is assumed to have been of dimension mxn
	U - matrix of left singular vectors returned by dgesvd
	ldu - fortran leading dimension of U.
	s - vector of singular values (starts at 0)
	Vt - vector of right singular vectors stored by rows in 
		Vt.
	ldv - fortran leading dimension of Vt.
	b - rhs vector for solution
	m,n - original matrix A is assumed to have been of dimension mxn
	x - solution vector (length n)
	rsvc - key parameter used to determined sv 
		cutoff.  Cutoff is set as max (s) * rsvc

Author:  Gary Pavlis
Written:  October 2000 patterned after similar routine called
pseudoinverse_solver in libgenloc
*/
int dpinv_solver(int m, int n, 
	double *U, int ldu, 
	double *s, 
	double *Vt, int ldv,
	double *b,
	double *x,
	double rsvc)
{
	int i,j;  /* counters*/
	int nsvused;  /* number of nonzero singular values used in solution*/
	int nsize;
	double sv_cutoff, smax;
	double *work;  /* work space */

	allot(double *,work,n);

	/* The lapack dgesvd routine sorts the singular values in
	descending order so we get the largest from the first element of s.
	We then compute the cutoff relative to this value with the rsvc
	scale factor.*/
	smax = s[0];
	sv_cutoff = smax*rsvc;
	nsize = MIN(m,n);


	/* multiply by S-1 * UT */
	nsvused = 0;
	for(j=0;j<nsize;++j)
	{
		if(s[j]<sv_cutoff)break;
		/* dgesvd stores left singular vectors in fortran columns */
		work[j] = ddot(m,U+ldu*j,1,b,1);
		work[j] /= s[j];
		++nsvused;
	}
	/* multiply by Vt */
	for(j=0;j<n;++j) x[j] = 0.0;
	for(j=0;j<nsvused;++j)
		daxpy(n,work[j],Vt+j,ldv,x,1);
	free(work);
	return(nsvused);
}
/* This function compute a projection of a vector x onto the 
null space of a matrix using orthogonal matrices computed from
an svd of an original matrix using the sunperf routine 
dgesvd.  This version is a project for the model space
side constructed from the right singular vectors (V^T).

Arguments:
	Vt - matrix of right singular vectors used to compute the projector.
		The projector is computed as I - V*Vt
		Note:  dgesvd returns the tranpose of V which means the 
		singular vectors are stored in the rows of the input
		matrix Vt.  Storage is sunperf fortran form meaning that
		the singular vectors are accessed in the blas 
		with a stride of ldvt
	ldvt - leading dimension of Vt ala fortran
	nsv - actual number of rows in Vt to use in forming the projector
		which is defined by the number of singular vectors used
		in a pseudoinverse solution.  
		(the output is a copy of the input if nsv>= m)
	n - number of columns of Vt 
	x - vector to be projected (length m)
	xp - vector to hold projection  (length m) Note we blindly copy
		to this work area assuming it is large enough.  

Normal return is 0.   Nonzero return indicates and error has been
posted with elog_log.  Only xp is altered by the function.

Author:  Gary Pavlis
Written:  October 2000, variant of similar routine for data space in 
	libmultiwavelet. 

Note:  This function and the following could be generalized to work
on either data or model space, but I prefer making it explicit and
adding to name space than have the confusion of one function do 
as many as four related but different operations.  
*/
int model_space_null_project(double *Vt, int ldvt, int nsv, int n, 
	double *x, double *xp)
{
	int i;
	double val;

	dcopy(n,x,1,xp,1);
	if(nsv>=n)
	{
		elog_log(0,"model_space_null_project passed illegal Vt matrix of size %d by %d\nProjection request ignored.  Output=input\n",
			nsv,n);
		return(1);
	}
	for(i=0;i<nsv;i++)
	{
		val = ddot(n,Vt+i,ldvt,x,1);
		daxpy(n,-val,Vt+i,ldvt,xp,1);
	}
	return(0);
}
/* Complementary projector to above that projects onto range of the
operator defined by Vt.  That is, here we compute xp<-V*V^T*x.
The argument list is identical with the same caveats */

int model_space_range_project(double *Vt, int ldvt, int nsv, int n, 
		double *x, double *xp)
{
	int i;
	double val;

	if(nsv>=n)
	{
		elog_log(0,"model_space_range_project passed illegal Vt matrix of size %d by %d\nProjection request ignored.  Setting output=input\n",
			nsv,n);
		dcopy(n,x,1,xp,1);
		return(1);
	}

	for(i=0;i<n;i++) xp[i]=0.0;
	for(i=0;i<nsv;i++)
	{
		val = ddot(n,Vt+i,ldvt,x,1);
		daxpy(n,val,Vt+i,ldvt,xp,1);
	}
	return(0);
}
/* This function compute a projection of a vector x onto the 
null space of a matrix using orthogonal matrices computed from
an svd of an original matrix using the sunperf routine 
dgesvd.  This version is a project for the data space
side constructed from the left singular vectors (U).

Arguments:
	U - matrix of right singular vectors used to compute the projector.
		The projector is computed as I - U*Ut
		Note:  dgesvd returns the singular vectors in the column
		of a FORTRAN storage array.  This means the vectors are 
		assume of length m with ldu (ldu>=m) between vectors.
	ldu - leading dimension of Vt ala fortran
	nsv - actual number of rows in Vt to use in forming the projector
		which is defined by the number of singular vectors used
		in a pseudoinverse solution.  
		(the output is a copy of the input if nsv>= m)
	m - number of columns of Vt 
	x - vector to be projected (length m)
	xp - vector to hold projection  (length m) Note we blindly copy
		to this work area assuming it is large enough.  

Normal return is 0.   Nonzero return indicates and error has been
posted with elog_log.  Only xp is altered by the function.

Author:  Gary Pavlis
Written:  November 2000, variant of similar routine for data space in 
	libmultiwavelet. 
Note:  This is a close sibling to the related projector above.
The difference is significant in that here the singular vectors
are stored in the columns, not in the rows.   
*/
int data_space_null_project(double *U, int ldu, int nsv, int m, 
	double *x, double *xp)
{
	int i;
	double val;

	dcopy(m,x,1,xp,1);
	if(nsv>=m)
	{
		elog_log(0,"data_space_null_project passed illegal U matrix of size %d by %d\nProjection request ignored.  Output=input\n",
			nsv,m);
		return(1);
	}
	for(i=0;i<nsv;i++)
	{
		val = ddot(m,U+i*ldu,1,x,1);
		daxpy(m,-val,U+i*ldu,1,xp,1);
	}
	return(0);
}

