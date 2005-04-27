#include "gclgrid.h"
/*  This set of functions take the input grid g and remap the
  internal coordinates to be consistent with the coordinate system
  in pattern.  The same function exists for both 2d and 3d grid.
  The field objects can use this function using a dynamic cast
  to the parent grid because they do not alter the field variable
  only the grid cartesian coordinate system.

  All return an object rather than a pointer under and assumption
  that this process will not be done a lot.  There is a large overhead
  in returning the object instead of a pointer to the object.  
  If that need arises a pointer version of these functions would be
  easy to create from these.
*/
GCLgrid remap_grid(GCLgrid& g, BasicGCLgrid& pattern)
{
	// return immediately if these grids are congruent
	if(g==pattern) return(g);
	// First copy g
	GCLgrid newgrid(g);
	// since newgrid is a copy we can now alter the 
	// transformation variables
	// This requires only setting the origin and azimuth_y 
	// followed by use of the set_transsformation_matrix function
	newgrid.lat0=pattern.lat0;
	newgrid.lon0=pattern.lon0;
	newgrid.r0=pattern.r0;
	newgrid.azimuth_y=pattern.azimuth_y;
	newgrid.set_transformation_matrix();

	// Now loop through the grid converting all the points
	// to the coordinate system of parent
	int i,j;
	Geographic_point geo;
	Cartesian_point p;
	for(i=0;i<newgrid.n1;++i)
		for(j=0;j<newgrid.n2;++j)
		{
			geo.lat=g.lat(i,j);
			geo.lon=g.lon(i,j);
			geo.r=g.r(i,j);
			p=pattern.gtoc(geo);
			newgrid.x1[i][j]=p.x1;
			newgrid.x2[i][j]=p.x2;
			newgrid.x3[i][j]=p.x3;
		}
	return(newgrid);
}
GCLgrid3d remap_grid(GCLgrid3d& g, BasicGCLgrid& pattern)
{
	// return immediately if these grids are congruent
	if(g==pattern) return(g);
	// First copy g
	GCLgrid3d newgrid(g);
	// since newgrid is a copy we can now alter the 
	// transformation variables
	// This requires only setting the origin and azimuth_y 
	// followed by use of the set_transsformation_matrix function
	newgrid.lat0=pattern.lat0;
	newgrid.lon0=pattern.lon0;
	newgrid.r0=pattern.r0;
	newgrid.azimuth_y=pattern.azimuth_y;
	newgrid.set_transformation_matrix();

	// Now loop through the grid converting all the points
	// to the coordinate system of parent
	int i,j,k;
	Geographic_point geo;
	Cartesian_point p;
	for(i=0;i<newgrid.n1;++i)
	    for(j=0;j<newgrid.n2;++j)
		for(k=0;k<newgrid.n3;++k)
		{
			geo.lat=g.lat(i,j,k);
			geo.lon=g.lon(i,j,k);
			geo.r=g.r(i,j,k);
			p=pattern.gtoc(geo);
			newgrid.x1[i][j][k]=p.x1;
			newgrid.x2[i][j][k]=p.x2;
			newgrid.x3[i][j][k]=p.x3;
		}
	return(newgrid);
}
			
			
