#ifndef _RAY1D_H_
#define _RAY1D_H_
#include <vector>
#include "dmatrix.h"
#include "gclgrid.h"
#include "VelocityModel_1d.h"
namespace SEISPP 
{
using namespace std;
using namespace SEISPP;

/*! \brief Computes a simple ray path for a radially symmetric earth model.
*
* Rays are a common concept used in global scale seismology.  This
* object constructs a ray path in a radially symmetric earth for 
* a specified ray parameter to a specified depth or travel time 
* from the surface.  Construction of the object creates the 
* path desired so the model for use is to call the constructor 
* and then extract the path from the internal data attributes.
* The generated path is always oriented from the surface downward.
* i.e. the first point in the computed path is at Earth's surface.
* \par
* This algorithm runs in either equal time step or equal depth step 
* mode.  Depth steps are exact while time steps are a target time
* increment only.  
* \par
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

class RayPathSphere
{
public:
	/*! Number of points in the computed ray path.*/
	int npts;
	/*! Ray parameter from which this path was generated.*/
	double p;
	vector<double> r, /*!<  Vector or radii defining the path.*/
		delta, /*!<  Vector of distances (radians) of path.*/
		t; /*!<  Vector of travel times.*/
	/*! Allocate space for the path, but do no computations. */
	RayPathSphere(int n)
	{npts=n; r.resize(n); delta.resize(n); t.resize(n);};
	/*! Normal constructor using an input velocity model and ray parameter.
	*
	* This would be the normal method using this object.  Call this constructor
	* to get a path and then peel off the r,delta, and t vector values as needed.
	*
	* \param vm Velocity model to use for ray path computation.
	* \param p ray parameter (s/km units) to use to define ray path.  Do note
	*	this input is in s/km.  It is converted to s/radian internally to 
	* 	use the standard global earth model travel time and distance equations.
	* \param zmax depth floor.  Stop computing if this depth is reached.
	* \param tmax travel time floor. Stop computing if this travel time is exceeded.
	* \param dt sample interval.  In equal time step mode this is target time step.
	* 	In equal depth step this is an exact depth increment between points.  
	* \param mode sets step size mode of ray path.  When mode is "z" the ray path
	*	is computed with equal depth steps.  Otherwise it aims for approximate
	*	travel time steps.
	*/
	RayPathSphere(VelocityModel_1d& vm,
		double p, double zmax, double tmax, double dt, 
		const string mode);
	/*! Standard copy constructor.*/
	RayPathSphere(const RayPathSphere& raytocopy);
	/*! Standard assignment operator.*/
	RayPathSphere& operator = (const RayPathSphere&);
	/*! Return depth to a particular point on the ray path.

	Because the ray path is stored as triplets of radii, distance,
	and travel time it is convenient to have this method to 
	retrieve the depth. */
	double depth(int ip);
};
/*! Downward project of a ray path into a coordinate system at a specified
orientation.  

Takes a RayPathSphere path and projects it from a specified
point in a GCLgrid3d object at a given azimuth.  For more details see man GCLraytrace (3).
*/
dmatrix *GCLgrid_Ray_project_down(GCLgrid3d& grid, RayPathSphere& path,
     double theta, int ix1, int ix2, int ix3) throw(GCLgrid_error);
/*! Upward project of a ray path into a coordinate system at a specified
orientation.  

Takes a RayPathSphere path and projects it from a specified
point in a GCLgrid3d object at a given azimuth.  The "upward" in the name
for this procedure means the ray is reversed from the RayPathSphere convention.
That is, the output path is oriented from the deepest point to the surface.
For more details see man GCLraytrace (3).
*/
dmatrix *GCLgrid_Ray_project_up(GCLgrid3d& grid, RayPathSphere& path,
     double theta, int ix1, int ix2, int ix3) throw(GCLgrid_error);
/*! Downward project of a ray path into a coordinate system at a specified
orientation.  

Takes a RayPathSphere path and projects it from a specified
point in a GCLgrid3d object at a given azimuth.  This procedure is similar
to GCLgrid_Ray_project_down but projects the path from a 2d (assumed surface)
grid rather than a 3D grid.
For more details see man GCLraytrace (3).
*/
dmatrix *GCLgrid_Ray_project(GCLgrid& grid, RayPathSphere& path,
     double theta, int ix1, int ix2) throw(GCLgrid_error);

}
#endif
