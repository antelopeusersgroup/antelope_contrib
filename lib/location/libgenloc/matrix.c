#include <stdlib.h>
#include "stock.h"
/* Modified version of numerical recipes utility to alloc a float 
matrix.  The routine allocs a vector of pointers that can be referenced as
as the range A[nrl] to A[nrh].  Columns are similarly indexed by biasing
the memory blocks allocated for each row through the range of ncl to nch.

For example, to alloc a normal FORTRAN array A(1:20,1:50) use
A = matrix(1,20,1,50)

For something stranger, like A(5:30,-4:60) use
A = matrix(5,30,-4,60)

This function uses Dan Quinlan's register error code, and will never
exit.  The function returns a NULL if any of the allocs fail.  Normally
this highly recommends the calling program call "die".

Author:  Gary L. Pavlis
Written:  May 1995 using numerical recipe code as a pattern
*/

float **matrix(int nrl, int nrh, int ncl,int nch)
{
	int i;
	float **m;
	/* This allocs pointers for rows */
	m=(float**) calloc(nrh-nrl+1,sizeof(float*));
	if(m == NULL) 
	{
		elog_log(1,"matrix:  Cannot alloc matrix vector of pointers\n");
		return (NULL);
	}
	m-=nrl;
	for(i=nrl;i<=nrh;i++)
	{
		m[i] = (float *) calloc(nch-ncl+1,sizeof(float));
		if(m[i] == NULL)
		{
			elog_log(1,"matrix:  Cannot alloc row %d of matrix of length %d\n",
				i,nch-ncl+1);
			/* cleanup in case you want to continue*/
			if(i!=nrl)
			{
				int j;
				for(j=nrl;j<i;++j) free(m[j]);
			}
			return (NULL);
		}
		m[i]-=ncl;
	}
	return (m);
}
/* Same for double matrix */

double **dmatrix(int nrl, int nrh, int ncl,int nch)
{
	int i;
	double **m;
	/* This allocs pointers for rows */
	m=(double**) calloc(nrh-nrl+1,sizeof(double*));
	if(m == NULL) 
	{
		elog_log(1,"dmatrix:  Cannot alloc matrix vector of pointers\n");
		return (NULL);
	}
	m-=nrl;
	for(i=nrl;i<=nrh;i++)
	{
		m[i] = (double *) calloc(nch-ncl+1,sizeof(double));
		if(m[i] == NULL)
		{
			elog_log(1,"dmatrix:  Cannot alloc row %d of matrix of length %d\n",
				i,nch-ncl+1);
			/* cleanup in case you want to continue*/
			if(i!=nrl)
			{
				int j;
				for(j=nrl;j<i;++j) free(m[j]);
			}
			return (NULL);
		}
		m[i]-=ncl;
	}
	return (m);
}
/* Same for char matrix */

char **cmatrix(int nrl, int nrh, int ncl,int nch)
{
	int i;
	char **m;
	/* This allocs pointers for rows */
	m=(char**) calloc(nrh-nrl+1,sizeof(char*));
	if(m == NULL) 
	{
		elog_log(1,"cmatrix:  Cannot alloc matrix vector of pointers\n");
		return (NULL);
	}
	m-=nrl;
	for(i=nrl;i<=nrh;i++)
	{
/* For non char types the following would be the correct line:
		m[i] = (char *) calloc(nch-ncl+1,sizeof(char));
However, because a char* is synonymous with a string, that in a unix system
is required to be terminated by a \0, there are innocent circumstances 
where one can stuff char entries into this matrix, and write over memory
that was alloced because of the requirement of the terminating null string.
Hence, we use the following almost trivial modification.  It is not
always required, but is safer. */
                m[i] = (char *) calloc(nch-ncl+2,sizeof(char));
		if(m[i] == NULL)
		{
			elog_log(1,"cmatrix:  Cannot alloc row %d of matrix of length %d\n",
				i,nch-ncl+1);
			/* cleanup in case you want to continue*/
			if(i!=nrl)
			{
				int j;
				for(j=nrl;j<i;++j) free(m[j]);
			}
			return (NULL);
		}
		m[i]-=ncl;
	}
	return (m);
}
/* Free routine:  will work for either float, double, or char */

void free_matrix(float **m, int nrl, int nrh, int ncl) /*do not pass nch*/
 {
 int i;
 for(i=nrh;i>=nrl;i--) free((char*)(m[i]+ncl));
 free((char*)(m+nrl));
 }

/* $Id$ */
