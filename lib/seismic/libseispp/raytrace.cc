#include <iostream>
#include <list>
#include "db.h"
#include "ray1d.h"
#include "dmatrix.h"
#include "gclgrid.h"
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

RayPathSphere::RayPathSphere(Velocity_Model_1d& vmod,
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
	double p; 
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
			dz=rnow*rnow*root_term*dt/(eta*eta);
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
/* basic assignment operator for this object*/
void RayPathSphere::operator=(const RayPathSphere& other)
{
	if(&other==this) return;
	npts=other.npts;
	if(r!=NULL) delete [] r;
	if(delta!=NULL) delete [] delta;
	if(t!=NULL) delete [] t;
	r=new double[npts];
	delta=new double[npts];
	t=new double[npts];
	dcopy(npts,other.r,1,r,1);
	dcopy(npts,other.delta,1,delta,1);
	dcopy(npts,other.t,1,t,1);
}
/*  This function takes an input of a ray path in spherical coordinates
 *  defined by a RayPathSphere object.  This is assumed to be spherical
 *  coordinate definitions of the path in a radius-distance (radians)
 *  reference frame.  This path is translated to the origin of the
 *  (input)GLCgrid object and projected along the positive x1 baseline
 *  of this GLCgrid.  Note this tacitly assumes the GCLgrid passed
 *  is a "standard" grid and not a more general GCLgrid object allowed
 *  by the definition of a GCLgrid.  
 *
 *  The output dmatrix contains cartesian vectors in the GCLgrid 
 *  reference frame for this path.  Note this means x2 coordinates
 *  are always machine zeros because the path is projected parallel
 *  to the x1 baseline.  
 *
 *  Note this function maybe should return a pointer to avoid
 *  copying this potentially fairly large object.
 *
 *  Author:  Gary L. Pavlis
*/
dmatrix *GCLgrid_Ray_project(GCLgrid3d& grid, RayPathSphere& path,
		double theta, int ix1, int ix2, int ix3)
					throw(GCLgrid_error)
{
	dmatrix *pathptr;
	dmatrix& pathout=*pathptr;
	Cartesian_point this_point;
	double radius;  // Radius corrected for ellipticity
	double lat,lon;
	double lat0,lon0;
	if(ix1>=grid.n1 || ix2>=grid.n2 || ix3>=grid.n3
			|| ix1<0 || ix2<0 || ix3<0)
		throw GCLgrid_error("GCLgrid_Ray_project was passed an illegal index\n");


	// Handle the Earth's surface case specially using the overloaded 
	// version.  It is slightly faster because it doesn't have to handle
	// the endpoint condition
	if(ix3>=(grid.n3 - 1)) // Could be == but a safer test
	{
		try{
		  pathptr = GCLgrid_Ray_project(grid, path,
			 theta,  ix1, ix2);
		} catch(GCLgrid_error err) { throw err;};

	}
	else
	{
		double depth;
		int np;
		int i0;
		double delta0;
		int i,ii;
		depth = grid.depth(ix1,ix2,ix3);
		//Search for point just below depth of ix3 point
		for(i0=0;i0<path.npts;++i0)
			if((path.r[0]-path.r[i0])>depth) break;
		np = path.npts - i0;  // right because we add one point
		pathptr = new dmatrix(3,np);
		// first point is just the grid poin
		pathout(0,0) = grid.x1[ix1][ix2][ix3];
		pathout(1,0) = grid.x2[ix1][ix2][ix3];
		pathout(2,0) = grid.x3[ix1][ix2][ix3];
		// Get the distance correction to subtract for this point
		if(i0==0)
			delta0=0.0;
		else
		{
			delta0 = path.delta[i0-1] 
				+ (depth-path.r[i0])
				*(path.delta[i0]-path.delta[i0-1])
				   /(path.r[i0]-path.r[i0-1]);  
		}
		lat0=grid.lat(ix1,ix2,ix3);
		lon0=grid.lon(ix1,ix2,ix3);
		for(i=i0,ii=1;i<path.npts;++i)
		{
			latlon(lat0,lon0,path.delta[i]-delta0,theta,&lat,&lon);
			radius = r0_ellipse(lat) - (path.r[0]-path.r[i]);
			this_point = grid.gtoc(lat,lon,radius);
			pathout(0,ii)=this_point.x1;
			pathout(1,ii)=this_point.x2;
			pathout(2,ii)=this_point.x3;
		}
	}
	return(pathptr);
}
/* overloaded version that assumes ix3=n3-1 */

dmatrix *GCLgrid_Ray_project(GCLgrid3d& grid, RayPathSphere& path,
		double theta, int ix1, int ix2)
					throw(GCLgrid_error)
{
	dmatrix *pathptr;
	dmatrix& pathout=*pathptr;
	Cartesian_point this_point;
	double radius;  // Radius corrected for ellipticity
	double lat,lon;
	double lat0,lon0;
	int ix3=grid.n3-1;
	if(ix1>=grid.n1 || ix2>=grid.n2 || ix1<0 || ix2<0)
		throw GCLgrid_error("GCLgrid_Ray_project was passed an illegal index\n");

	pathptr = new dmatrix(3,path.npts);
	lat0=grid.lat(ix1,ix2,ix3);
	lon0=grid.lon(ix1,ix2,ix3);
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
} // Termination of namespace SEISPP definitions

