#include "seispp.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP {
// In place conversion of a velocity model stored in 
// a GCL field from values stored with a flattening
// transformation to true spherical geometry.  
// Converts the model object vmodel in place.  i.e.
// on return the inverse flattening transformation is
// applied to each depth and velocity value
void ConvertFlatModelToSpherical(GCLscalarfield3d& vmodel)
{
	int i,j,k;
	Geographic_point p;
	Cartesian_point cp;
	double zflat,ztrue;
	double vflat,vtrue;
	for(i=0;i<vmodel.n1;++i)
	    for(j=0;j<vmodel.n2;++j)
		for(k=0;k<vmodel.n3;++k)
		{
			p=vmodel.geo_coordinates(i,j,k);
			zflat=vmodel.depth(i,j,k);
			ztrue=uflatz(zflat);
			p.r += zflat;
			p.r -= ztrue;
			vflat=vmodel.val[i][j][k];
			vtrue=uflatvel(vflat,zflat);
			cp=vmodel.gtoc(p);
			// Now change grid data
			vmodel.x1[i][j][k]=cp.x1;
			vmodel.x2[i][j][k]=cp.x2;
			vmodel.x3[i][j][k]=cp.x3;
			vmodel.val[i][j][k]=vtrue;
		}
}
			
} // End SEISPP namespace declaration
