#include "stock.h"
#include "gclgrid.h"
/* These are create and free routines for 2d and 3d GCLgrid objects.
The create routines load an named object from a database allocating
memory as needed.  The free routines release this space. */


/* Many gcl functions work with 3d C grids.  C implements multidimensioned
arrays pointer arrays, which makes this kind of ugly.  Anyway, this
function will create the F90 equivalent of  x[0:n1-1][0:n2-1][0:n3-1]
and return a ***double pointer for the 3d array.  The approach 
used here allocates the full block of memory in one single call to 
guarantee the result is in a contiguous block of memory. 
This is useful for reading and writing this type of entity because
then a single call to fread and fwrite can be used.  
Note, however, that the actual order in memory remains the C convention
not the FOTRAN form.  i.e. the rightmost index defines members contingous
in memory.
Author:  GAry Pavlis
Written:  June 2000
*/

double ***create_3dgrid_contiguous(int n1, int n2, int n3)
{
	double ***ptr3d;
	double **ptr2ptr;
	double *ptr;
	int i,j;

	allot(double *,ptr,n1*n2*n3);
	allot(double ***,ptr3d,n1);
	for(i=0;i<n1;++i)
	{
		allot(double **,ptr2ptr,n2);
		ptr3d[i] = ptr2ptr;
	}
	for(i=0;i<n1;++i)
	{
		for(j=0;j<n2;++j)
		{
			ptr3d[i][j] = ptr + n2*n3*i + n3*j;
		}
	}
	return(ptr3d);
}		
/* Free routine for a 3d array.  pointers arrays require exra attention
to avoid a memory leak.   This function assumes standard C indexing
*/
void free_3dgrid_contiguous(double ***x,int n1, int n2)
{
	int i;
	double *ptr;

	/* this clears the work space*/
	ptr = x[0][0];
	free((void *)ptr);

	/* The pointer arrays are still then and have to be freed 
	seperately */
	for(i=0;i<n1;++i)  free((void *)x[i]);
	free((void *)x);
}
/* parallel routines to the above for 2d */
double **create_2dgrid_contiguous(int n1, int n2)
{
	double **ptr2ptr;
	double *ptr;
	int i;

	allot(double *,ptr,n1*n2);
	allot(double **,ptr2ptr,n1);
	for(i=0;i<n1;++i)
		ptr2ptr[i] = ptr + n2*i;
	return(ptr2ptr);
}
void free_2dgrid_contiguous(double **x,int n1)
{
	double *ptr;

	ptr = x[0];
	free((void *)ptr);
	free((void *)x);
}
