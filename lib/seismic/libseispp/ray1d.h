#ifdef __cplusplus
#ifndef _RAY1D_H
#define _RAY1D_H
#include "dmatrix.h"
#include "gclgrid.h"
#include "seispp.h"
class RayPathSphere
{
public:
	int npts;
	double *r,*delta,*t;
	RayPathSphere(){r=NULL,delta=NULL,t=NULL;};
	RayPathSphere(int n)
	{npts=n; r=new double[n], delta=new double[n], t=new double[n];};
	// This fully parametrized version constructs a full path
	RayPathSphere(Velocity_Model_1d& vm,
		double p, double zmax, double tmax, double dt, 
		const string mode);
	~RayPathSphere(){if(r!=NULL)delete[]r;
		if(delta!=NULL)delete[]delta;
		if(t!=NULL)delete[]t;};
	void operator = (const RayPathSphere&);
};

dmatrix *GCLgrid_Ray_project(GCLgrid3d& grid, RayPathSphere& path,
     double theta, int ix1, int ix2, int ix3) throw(GCLgrid_error);
dmatrix *GCLgrid_Ray_project(GCLgrid3d& grid, RayPathSphere& path,
     double theta, int ix1, int ix2) throw(GCLgrid_error);

#endif
#endif
