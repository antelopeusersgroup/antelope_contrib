#ifdef __cplusplus
#ifndef _RAY1D_H
#define _RAY1D_H
#include "dmatrix.h"
#include "gclgrid.h"
#include "seispp.h"
namespace SEISPP 
{

class RayPathSphere
{
public:
	int npts;
	double p;
	vector<double> r,delta,t;
	RayPathSphere(int n)
	{npts=n; r.resize(n); delta.resize(n); t.resize(n);};
	// This fully parametrized version constructs a full path
	RayPathSphere(Velocity_Model_1d& vm,
		double p, double zmax, double tmax, double dt, 
		const string mode);
	RayPathSphere(const RayPathSphere& raytocopy);
	void operator = (const RayPathSphere&);
	double depth(int ip);
};

dmatrix *GCLgrid_Ray_project_down(GCLgrid3d& grid, RayPathSphere& path,
     double theta, int ix1, int ix2, int ix3) throw(GCLgrid_error);
dmatrix *GCLgrid_Ray_project_up(GCLgrid3d& grid, RayPathSphere& path,
     double theta, int ix1, int ix2, int ix3) throw(GCLgrid_error);
dmatrix *GCLgrid_Ray_project(GCLgrid& grid, RayPathSphere& path,
     double theta, int ix1, int ix2) throw(GCLgrid_error);

}
#endif
#endif
