#include "gclgrid.h"
/* This set of functions implement methods on GCLgrid field objects
to initialize all field variables to zero.  This is necessary in
some algorithms.  The organization of the same function for 
multiple objects in the same file is nonstandard, but was done
because I added this functionality when most of the library was
already completed. */
void GCLscalarfield::zero()
{
	int i,j;
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) val[i][j]=0.0;
}
void GCLscalarfield3d::zero()
{
	int i,j,k;
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) val[i][j][k]=0.0;
}
void GCLvectorfield::zero()
{
	int i,j,k;
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<nv;++k) val[i][j][k]=0.0;
}
void GCLvectorfield3d::zero()
{
	int i,j,k,l;
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) 
				for(l=0;l<nv;++l) val[i][j][k][l]=0.0;
}
