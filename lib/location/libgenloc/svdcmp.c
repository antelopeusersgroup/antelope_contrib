#include <stdlib.h>
#include <stdio.h>

#include "stock.h"
#include "elog.h"

#define SUNPERF 1
#include "perf.h"

/* This is a plug in interface module that replaces the svdcmp
routine from numerical recipes and calls the sunperf singular
value decomposition routine.  

Arguments:

A - m by n input "matrix" in numerical recipes double indexed C format.
m,n - input dimensions of A
s - vector to hold min(m,n) singular values of A
V - n by n matrix to hold right singular vectors of A.

Note that A and V are doubly indexed C float objects.  The
lapack routines in the sunpref library use the fortran storage 
convention that in C syntax is a float * with an internal 
structure of FORTRAN.  That is the float * vector stores an
m by n matrix like A by columns. This is not an efficient way
to do this process, but it makes a plug compatible interface to 
svdcmp.

Routine will die only malloc errors. 

Author:  Gary Pavlis
*/

int svdcmp (float **A, int m, int n, float *s, float **V)
{
	float *afort, *vfort;  /*Vector format work spaces */
	int i;
	int info=0;
#ifndef SUNPERF
	int one=1;
	float *swork;
	int ldwork;
#endif

	/*Alloc work spaces and copy from numerical recipes matrix
	form to the form sunperf wants */

	afort = (float *) calloc(m*MAX(m,n),sizeof(float));
	vfort = (float *) calloc(n*n,sizeof(float));
	if( (afort == NULL) || (vfort == NULL))
		elog_die(0,"svdcmp could not alloc work arrays\n");
#ifdef SUNPERF
	for(i=0;i<m;++i)
		scopy(n,A[i],1,(afort+i),m);
#else
        for(i=0;i<m;++i)
                scopy_(&n,A[i],&one,(afort+i),&m);
#endif
	/* old rule:  always initialize things like this */
	for(i=0;i<n*n;++i) vfort[i] = 0.0;

#ifdef SUNPERF
	sgesvd('o','s',m,n,afort,m,s,NULL,m,vfort,n,&info);
#else
	ldwork = 12+2*n;  /* This is slightly larger than the minimum
			required for safety.  Space required is never
			large by modern standards  */
	swork = calloc(ldwork,sizeof(float));
	if(n == NULL) elog_die(0,"Cannot alloc work space for SVD routine\n");
	sgesvd_("o","s",&m,&n,afort,&m,s,NULL,&m,vfort,&n,
		swork, &ldwork,
		&info);
	free(swork);
#endif
	if(info!=0)
	{
		if(info > 0)
                	elog_notify(0,"Convergence failure in svd routine\n");
		else
			elog_notify(0,"Illegal value for argument %d passed to sgesvd\nNo solution possible\n",
			-info);
		free(afort);
		free(vfort);
		return(info);
	}
#ifdef SUNPERF
	/* note we do return something even when convergence failed.
	Must handle negative return as junk is returned then. */
	for(i=0;i<m;++i)
		scopy(n,(afort+i),m,A[i],1);
	/* Note this copies V transpose in vfort to V in the matrix V.  
	Tricky BLAS code I know*/
	for(i=0;i<n;++i)
		scopy(n,(vfort+i*n),1,V[i],1);
#else
	for(i=0;i<m;++i)
                scopy_(&n,(afort+i),&m,A[i],&one);
        for(i=0;i<n;++i)
                scopy_(&n,(vfort+i*n),&one,V[i],&one);
#endif


	free(afort);
	free(vfort);
	return(0);
}
