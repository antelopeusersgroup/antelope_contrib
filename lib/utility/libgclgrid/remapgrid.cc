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

  Major modification May 2005
Original functions did indeed return a new object.  I realized thought
it worked better to modify the object in place.  The reason for this is
that the same algorithm can be applied to field objects using a dynamic_cast
back to the parent grid.  The reason this is possible is that remap_grid 
does not alter the grid geometry but only the coordinate system used to 
reference each grid point.  This feature MUST be recognized by the caller,
however, this alters the parent object.
*/
void remap_grid(GCLgrid& g, BasicGCLgrid& pattern)
{
	// return immediately if these grids are congruent
	if(g==pattern) return;
	// First copy g
	GCLgrid oldgrid(g);
	// We have a copy of the original in oldgrid so now
	// modify g in place.
	// This requires only setting the origin and azimuth_y 
	// followed by use of the set_transsformation_matrix function
	g.lat0=pattern.lat0;
	g.lon0=pattern.lon0;
	g.r0=pattern.r0;
	g.azimuth_y=pattern.azimuth_y;
	g.set_transformation_matrix();

	// Now loop through the grid converting all the points
	// to the coordinate system of parent
	int i,j;
	Geographic_point geo;
	Cartesian_point p;
	for(i=0;i<oldgrid.n1;++i)
		for(j=0;j<oldgrid.n2;++j)
		{
			geo.lat=oldgrid.lat(i,j);
			geo.lon=oldgrid.lon(i,j);
			geo.r=oldgrid.r(i,j);
			p=pattern.gtoc(geo);
			g.x1[i][j]=p.x1;
			g.x2[i][j]=p.x2;
			g.x3[i][j]=p.x3;
		}
	g.compute_extents();
	return;
}
void remap_grid(GCLgrid3d& g, BasicGCLgrid& pattern)
{
	// return immediately if these grids are congruent
	if(g==pattern) return;
	// First copy g
	GCLgrid3d oldgrid(g);
	g.lat0=pattern.lat0;
	g.lon0=pattern.lon0;
	g.r0=pattern.r0;
	g.azimuth_y=pattern.azimuth_y;
	g.set_transformation_matrix();

	// Now loop through the grid converting all the points
	// to the coordinate system of parent
	int i,j,k;
	Geographic_point geo;
	Cartesian_point p;
	for(i=0;i<oldgrid.n1;++i)
	    for(j=0;j<oldgrid.n2;++j)
		for(k=0;k<oldgrid.n3;++k)
		{
			geo.lat=oldgrid.lat(i,j,k);
			geo.lon=oldgrid.lon(i,j,k);
			geo.r=oldgrid.r(i,j,k);
			p=pattern.gtoc(geo);
			g.x1[i][j][k]=p.x1;
			g.x2[i][j][k]=p.x2;
			g.x3[i][j][k]=p.x3;
		}
	g.compute_extents();
	return;
}
/* This procedure is a wrapper that uses an inheritance trick.
   It builds a minimal sized and then will use one of the above
   procedures to do the actual work.   Inheritance sorts out the
   2d or 3d form. */
void remap_grid(BasicGCLgrid *g, 
        double olat, double olon, double oradius, double azn)
{
    GCLgrid rmg(2,2,string("dummy"),olat,olon,oradius,azn,1.0,1.0,0,0);
    GCLgrid *g2d;
    GCLgrid3d *g3d;
    g2d=dynamic_cast<GCLgrid *>(g);
    if(g2d!=NULL)
        remap_grid(*g2d,rmg);
    else
    {
        g3d=dynamic_cast<GCLgrid3d *>(g);
        if(g3d==NULL) throw GCLgridError("remap_grid - downcast failed from BasicGCLgrid pointer");
    }
}

			
			
