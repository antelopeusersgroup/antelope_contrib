#include <float.h>
#include <limits.h>
#include <iostream>
#include <list>
#include "ray1d.h"
#include "dmatrix.h"
#include "gclgrid.h"
#include "SeisppError.h"
namespace SEISPP
{


/*
 * Full constructor that traces a ray to yield a RayPathSphere object
 * for velocity model vmod and ray parameter u.  vmod is
 * assumed to have units of km/s and u is assumed to have
 * units of s/km.  Internally s/radians are used, but 
 * to use s/rad externally would be very confusing.
 * When mode=="z" aims for equal z intervals of size del.
 * When mode=="t" aims for equal time intervals of size del.
 * Ray is traced to depth zmax.  
 *
 * Note that we have to handle the common situation of a turning
 * ray.  That is, if zmax is below the turning point of a ray
 * with the requested ray parameter we handle this specially because
 * if we did not the travel time formulae contain sqrt of negative
 * numbers.  This is handled in a simple way here by simply stopping
 * the ray tracing if the next point down goes singular.  Whether 
 * this is important to the caller depends on the use.  Rather than
 * throw an exception, which isn't necessary really, the user is 
 * simply warned that they should test the actual output's maximum
 * depth compared to zmax when this is a problem.
 *
 * A similar feature is implemented when running in equal time
 * step mode.  This could do nasty things too if the requested
 * zmax was very close to a turning point.  It is possible in
 * this situation that the function could try to compute a vector
 * that is unbounded because the dz step length for each dt 
 * gets tiny.  To avoid this we also include the tmax parameter.
 * The ray is truncated when either z>zmax or t>tmax.  If you 
 * are sure the ray won't bottom for the requested p simply make
 * tmax large.
 */

RayPathSphere::RayPathSphere(VelocityModel_1d& vmod,
		double u, double zmax, double tmax,
		double del, const string mode)
{
	list <double> ddlist;
	list <double> dzlist;
	list <double> dtlist;
	list <double>::iterator id, iz, it;
	double tnow,z,x;  // accumulated time, depth and distance
	double rnow;  // radius now = R0-z
	const double R0=6378.17;  
	double ddelta,dz,dt;
	double vel;
	int npoints;
	int i;

	p = u*R0; // this ray parameter will have units of s/radian

	tnow=0.0;
	z=0.0;

	while(z<zmax && tnow<tmax)
	{
		// see Lay and Wallace section on spherical earth
		// travel time claculations for formulae used here
		double eta;  
		double root_term;  
		vel=vmod.getv(z);
		rnow=R0-z;
		eta=rnow/vel; //
		if(p>eta)break;  // exit loop to avoid a negative sqrt
		root_term=sqrt(eta*eta-p*p);
		// mode switches between equal depth steps and equal
		// time steps
		if(mode=="z")
		{
			dz=del;
			dt=eta*eta*dz/(rnow*root_term);
		}
		else
		{
			dt=del;
			dz=rnow*root_term*dt/(eta*eta);
		}
		// Note this is in radians
		ddelta=p*dz/(rnow*root_term);
		tnow+=dt;
		z+=dz;
		ddlist.push_back(ddelta);
		dtlist.push_back(dt);
		dzlist.push_back(dz);
	}
	// all the lists have to be the same size so we randomly
	// choose one to compute the size of the output vector
	// Is +1 because this is segments versus points
	npoints=ddlist.size()+1;
	// Now we build the output structure by simple accumulation
	// The lists will be delted on exit and provided a useful
	// alternative to realloc.  Because this lists are parallel
	// we loop over one and just passively increment the iterators
	// on the others.  This could have been done with a list of a
	// temporary structure, but that's a judgment call on memory
	// use versus clarity.
	npts = npoints;
	r.resize(npoints);
	t.resize(npoints);
	delta.resize(npoints);
	r[0]=R0;
	t[0]=0.0;
	delta[0]=0.0;
	for(i=1,it=dtlist.begin(),id=ddlist.begin(),iz=dzlist.begin();
			it!=dtlist.end();++it,++id,++iz,++i) 
	{
		r[i]=r[i-1] - (*iz);
		t[i]=t[i-1] + (*it);
		delta[i]=delta[i-1] + (*id);
	}
}
// Standard copy constructor
RayPathSphere::RayPathSphere(const  RayPathSphere& other)
{
	npts=other.npts;
	p=other.p;
	r=other.r;
	t=other.t;
	delta=other.delta;
}

/* basic assignment operator for this object*/
RayPathSphere& RayPathSphere::operator=(const RayPathSphere& other)
{
	if(this!=&other)
	{
		npts=other.npts;
		p=other.p;
		r=other.r;
		t=other.t;
		delta=other.delta;
	}
	return(*this);
}
double RayPathSphere::depth(int ip)
{
	const double R0=6378.17;  
	if( (ip>=npts) || (ip<0) )
		throw SeisppError("RayPathSphere::depth index out of range");
	else
		return(R0-r[ip]);
}
/* This function takes an input ray path defined by a RayPathSphere
object (path) and returns a 3xN_points matrix of points defining
the projection of this point in GLCgrid3d cartesian coordinates.
The path returned is always directed downward.  The first point 
will be the coordinates of the point at ix1,ix2,ix3 in the 
GCLgrid3d object (grid).  Successive points will be the projection
of the ray (path) beginning at the depth defined by this point 
and continuing to the end of path.  Note the first interval may 
commonly be shortened in length unless the path happens to mesh
exactly with the grid.  In short, that point is subject to numerical
instability if on tries to divide by that interval.  

The function returns a pointer to a newly allocated dmatrix
to avoid the copying overhead of this potentially fairly large
object.  

The function throws an exception if ix1,ix2, or ix3 are outside
the bounds of the grid.

Author:  Gary L. Pavlis
*/
dmatrix *GCLgrid_Ray_project_down(GCLgrid3d& grid, RayPathSphere& path,
		double theta, int ix1, int ix2, int ix3)
					throw(GCLgrid_error)
{
	Cartesian_point this_point;
	double radius;  // Radius corrected for ellipticity
	double lat,lon;
	double lat0,lon0;
	if(ix1>=grid.n1 || ix2>=grid.n2 || ix3>=grid.n3
			|| ix1<0 || ix2<0 || ix3<0)
		throw GCLgrid_error("GCLgrid_Ray_project_down was passed an illegal index");

	int np;
	int i0,iextra;
	double r0,delta0;
	int i,ii;
	double depth = grid.depth(ix1,ix2,ix3);
	r0=grid.r(ix1,ix2,ix3);
	if((path.r[0]-path.r[path.npts-1])<depth)
		throw GCLgrid_error("GCLgrid_Ray_project_down:  Given ray path does not reach requested depth");
	//Search for point just below depth of ix3 point
	if(depth<0.0)
	{
		i0=1;
		iextra=1;
	}
	else if(fabs(depth)<FLT_EPSILON) // A conservative test for zero since depth is double
	{
		i0=0;
		iextra=0;
	}
	else
	{
		iextra=1;
		for(i0=0;i0<path.npts;++i0)
			if((path.r[0]-path.r[i0])>depth) break;
	}
	np = path.npts - i0 + iextra;  
	dmatrix *pathptr = new dmatrix(3,np);
	dmatrix& pathout=*pathptr;
	// In all cases we start from the specified grid point
	pathout(0,0) = grid.x1[ix1][ix2][ix3];
	pathout(1,0) = grid.x2[ix1][ix2][ix3];
	pathout(2,0) = grid.x3[ix1][ix2][ix3];
	lat0=grid.lat(ix1,ix2,ix3);
	lon0=grid.lon(ix1,ix2,ix3);
	// Get the distance correction to subtract for this point
	if(depth<0.0)
	{
		//
		// Land here if first point in path is above datum.
		// Project up from first point in RayPathSphere (delta,r)
		//
		delta0 = (r0-path.r[0])
			*(path.delta[1]-path.delta[0])
			   /(path.r[0]-path.r[1]);  
		// Project one point to datum and add to path in
		// this special case
		latlon(lat0,lon0,delta0,theta,&lat,&lon);
		radius = r0_ellipse(lat);  // This forces point to datum
		this_point = grid.gtoc(lat,lon,radius);
		pathout(0,1)=this_point.x1;
		pathout(1,1)=this_point.x2;
		pathout(2,1)=this_point.x3;
		ii=2;
		delta0 = 0.0;  // Reset to zero to get distance correct.
	}
	else if(i0==0)
	{
		// simple case for exact match
		delta0=0.0;
		ii=1;
		// This is a bit ugly, but we need to increment i0 by one
		// here or we duplicate the first point in the loop below.
		// Confusing but allows a common loop for all 3 cases 
		// with this oddity.
		i0=1;
	}
	else
	{
		//
		// Land here if this ray path needs to be projected
		// from below datum
		// Note i0 can be assumed 1 or larger here because of
		// conditional above sets i0
		//
		delta0 = path.delta[i0-1] 
			+ (r0-path.r[i0])
			*(path.delta[i0]-path.delta[i0-1])
			   /(path.r[i0-1]-path.r[i0]);  
		// Here we need to add another extra point to to 
		// connect first point to ray mesh.
		latlon(lat0,lon0,path.delta[i0]-delta0,theta,&lat,&lon);
		// This forces next point to r0 of ray mesh 
		this_point = grid.gtoc(lat,lon,path.r[i0]);
		pathout(0,1)=this_point.x1;
		pathout(1,1)=this_point.x2;
		pathout(2,1)=this_point.x3;
		ii=2;
	}
	// fill out the rest of the path
	for(i=i0;i<path.npts&&ii<pathout.columns();++i,++ii)
	{
		latlon(lat0,lon0,path.delta[i]-delta0,theta,&lat,&lon);
		radius = r0_ellipse(lat) - (path.r[0]-path.r[i]);
		this_point = grid.gtoc(lat,lon,radius);
		pathout(0,ii)=this_point.x1;
		pathout(1,ii)=this_point.x2;
		pathout(2,ii)=this_point.x3;
	}
	return(pathptr);
}
/* Similar to above but for a GCLgrid (2d) object.  Path then
starts at the radius of the surface this defines.  This is assumed
to be some surface that approximate earth's surface.  The ray is
projected from the point defined by ix1,ix2 downward at and angle
of theta.  The returned dmatrix is the same length as the input 
path, but the returned path has the coordinates of the projection
of this ray from the point defined by ix1,ix2 downward with 
the local vertical and at angle theta.  i.e. it is projected
correctly for a sphere, but into the GCLgrid cartesian reference
frame.  Note this algorithm is most useful in combination with a
3d grid that is an extension of the 2d (i.e. box-like with this as
it's top). 

This has not "up" or "down" qualifier as there is only one 
choice for a solid earth person.
*/

dmatrix *GCLgrid_Ray_project(GCLgrid& grid, RayPathSphere& path,
		double theta, int ix1, int ix2)
					throw(GCLgrid_error)
{
	Cartesian_point this_point;
	double radius;  // Radius corrected for ellipticity
	double lat,lon;
	double lat0,lon0;
	if(ix1>=grid.n1 || ix2>=grid.n2 || ix1<0 || ix2<0)
		throw GCLgrid_error("GCLgrid_Ray_project was passed an illegal index\n");

	dmatrix *pathptr;
	pathptr = new dmatrix(3,path.npts);
	dmatrix& pathout=*pathptr;
	lat0=grid.lat(ix1,ix2);
	lon0=grid.lon(ix1,ix2);
	for(int i=0;i<path.npts;++i)
	{
		latlon(lat0,lon0,path.delta[i],theta,&lat,&lon);
		radius = r0_ellipse(lat) - (path.r[0]-path.r[i]);
		this_point = grid.gtoc(lat,lon,radius);
		pathout(0,i)=this_point.x1;
		pathout(1,i)=this_point.x2;
		pathout(2,i)=this_point.x3;
	}
	return(pathptr);
}
/* like the "down" version above, except the path that is return is 
directed from the point at ix1,ix2,ix3 upward to the surface.  
*/
dmatrix *GCLgrid_Ray_project_up(GCLgrid3d& grid, RayPathSphere& path,
		double theta, int ix1, int ix2, int ix3)
					throw(GCLgrid_error)
{
	Cartesian_point this_point;
	double radius;  // Radius corrected for ellipticity
	double lat,lon;
	double lat0,lon0;
	// Note subtle difference from down version here.  test for ix3>=grid.n3-1 instead
	// of ix3>=grid.n3.  grid.n3-1 is assumed to be at or near earth's surface, so 
	// requesting up from there makes no sense.
	if(ix1>=grid.n1 || ix2>=grid.n2 || ix3>=(grid.n3-1)
			|| ix1<0 || ix2<0 || ix3<0)
		throw GCLgrid_error("GCLgrid_Ray_project_up was passed an illegal index\n");

	int np;
	int i0;
	double delta0;
	int i,ii;
	double depth = grid.depth(ix1,ix2,ix3);
	// could recover this kind of error by extrapolation, but it 
	// is better to consider it an error and throw an exception in this case
	if( (path.r[0]-path.r[path.npts-1])<depth )
		throw GCLgrid_error("GCLgrid_Ray_project_up:  Given ray does not reach to depth of requested grid point");
	//Search for point just above depth of ix3 point
	// Note:  down algorithm is point below not above
	for(i0=path.npts-1;i0>0;--i0)
		if((path.r[0]-path.r[i0])<depth) break;
	np = path.npts - i0;  // right because we add one point
	dmatrix *pathptr = new dmatrix(3,np);
	dmatrix& pathout=*pathptr;

	// first point is just the grid point
	pathout(0,0) = grid.x1[ix1][ix2][ix3];
	pathout(1,0) = grid.x2[ix1][ix2][ix3];
	pathout(2,0) = grid.x3[ix1][ix2][ix3];
	// Get the distance correction to subtract for this point
	// Conditional in down version not needed because other
	// tests above negate the purpose of the conditional there.
	delta0 = path.delta[i0-1] 
		+ (depth-path.r[i0])
		*(path.delta[i0]-path.delta[i0-1])
		   /(path.r[i0]-path.r[i0-1]);  
	lat0=grid.lat(ix1,ix2,ix3);
	lon0=grid.lon(ix1,ix2,ix3);
	for(i=i0,ii=1;i>=0;--i,++ii)
	{
		if(ii==1)
			latlon(lat0,lon0,path.delta[i]-delta0,theta,&lat,&lon);
		else
			 latlon(lat0,lon0,path.delta[i],theta,&lat,&lon);
		radius = r0_ellipse(lat) - (path.r[0]-path.r[i]);
		this_point = grid.gtoc(lat,lon,radius);
		pathout(0,ii)=this_point.x1;
		pathout(1,ii)=this_point.x2;
		pathout(2,ii)=this_point.x3;
	}
	return(pathptr);
}
} // Termination of namespace SEISPP definitions

