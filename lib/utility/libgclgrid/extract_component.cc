#include "gclgrid.h"
/* This file contains a pair of functions that extract a single component 
from a vector field and return the result as a scalar field defined
on the same base grid.  There is a 2d and 3d version.
Both return pointers to the potentially large objects so the normal
warnings about freeing the output in the caller is in order.
Both throw an exception if the number defining the requested component
is outside the valid range of the input field.
*/
GCLscalarfield *extract_component(GCLvectorfield& f,int component)
{
	int i,j;
	if( (component<0) || (component>=f.nv) ) 
	{
		elog_notify(0,
		 "extract_component:  requested component %d is not consistent with field dimension %d\n",
		 component, f.nv);
		throw 1;
	}
	GCLscalarfield *sf = new GCLscalarfield(dynamic_cast<GCLgrid&>(f));
	for(i=0;i<f.n1;++i)
		for(j=0;j<f.n2;++j)
		{
			sf->val[i][j]=f.val[i][j][component];
		}
	return(sf);
}
GCLscalarfield3d *extract_component(GCLvectorfield3d& f,int component)
{
	int i,j,k;
	if( (component<0) || (component>=f.nv) ) 
	{
		elog_notify(0,
		 "extract_component:  requested component %d is not consistent with field3d dimension %d\n",
		 component, f.nv);
		throw 1;
	}
	GCLscalarfield3d *sf = new GCLscalarfield3d(dynamic_cast<GCLgrid3d&>(f));
	for(i=0;i<f.n1;++i)
	    for(j=0;j<f.n2;++j)
		for(k=0;k<f.n3;++k)
		{
			sf->val[i][j][k]=f.val[i][j][k][component];
		}
	return(sf);
}

	
