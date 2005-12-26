#include "SeisppError.h"
#include "Metadata.h"
#include "slowness.h"
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
// Generic default slowness grid
RectangularSlownessGrid::RectangularSlownessGrid()
{
	name=string("Generic");
	uxlow = - 0.5;
	uylow = -0.5;
	nux = 101;
	nuy = 101;
	dux = 0.01;
	duy = 0.01;
}

// brute force constructor 
RectangularSlownessGrid::RectangularSlownessGrid(string nm, 
	double uxl, 
		double uyl,
			double du1,
				double du2,
					int n1, 
						int n2)
{
	name =nm;
	uxlow = uxl;
	uylow = uyl;
	nux = n1;
	nuy = n2;
	dux =du1;
	duy=du2;
}
RectangularSlownessGrid::RectangularSlownessGrid(Pf *pf,string tag)
{
	Metadata md(pf,tag);
	try {
		name=md.get_string("Slowness_Grid_Name");
		uxlow=md.get_double("uxlow");
		uylow=md.get_double("uylow");
		nux = md.get_int("nux");
		nuy = md.get_int("nuy");
		dux = md.get_double("dux");
		duy = md.get_double("duy");
	} catch (...) {throw;}
}
// Copy constructor needed due to string variable (always wise anyway they say)
RectangularSlownessGrid::RectangularSlownessGrid(const RectangularSlownessGrid& rsg)
{
	name=rsg.name;
	uxlow=rsg.uxlow;
	uylow=rsg.uylow;
	nux=rsg.nux;
	nuy=rsg.nuy;
	dux=rsg.dux;
	duy=rsg.duy;
}
// Returns a slowness vector for a grid position i,j
SlownessVector RectangularSlownessGrid::slow(int i, int j)
{
	if(i>=nux || j>=nuy || i<0 || j<0) 
		throw SeisppError(string("Illegal index request from RectangularSlownessGrid object"));
	SlownessVector u;
	u.ux=uxlow+i*dux;
	u.uy=uylow+j*duy;
	return(u);
}
// these are trivial constructors and could be done inline, but 
// decided to put them here to keep things together.  Learned this
// lesson the hard way
//
SlownessVector::SlownessVector()
{
	ux=0.0;
	uy=0.0;
}
SlownessVector::SlownessVector(const SlownessVector& old)
{
	ux=old.ux;
	uy=old.uy;
}

// These could (and once were) inline, but decided that was poor
// memory management
double SlownessVector::mag()
{
	return(hypot(ux,uy));
}
double SlownessVector::azimuth()
{
	if(this->mag() <= 0.0) return(0.0);
	double phi;
	phi=M_PI_2-atan2(uy,ux);
	if(phi>M_PI)
                return(phi-2.0*M_PI);
	else if(phi<-M_PI)
		return(phi+2.0*M_PI);
        else
                return(phi);
}
double SlownessVector::baz()
{
	if(this->mag() <= 0.0) return(0.0);
	double phi;
	phi=3.0*M_PI_2-atan2(uy,ux);
	if(phi>M_PI)
                return(phi-2.0*M_PI);
	else if(phi<-M_PI)
		return(phi+2.0*M_PI);
        else
                return(phi);
}


}  // End namespace SEISPP
