#include "seispp.h"
namespace SEISPP
{
// Generic default slowness grid
Rectangular_Slowness_Grid::Rectangular_Slowness_Grid()
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
Rectangular_Slowness_Grid::Rectangular_Slowness_Grid(string nm, 
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
Rectangular_Slowness_Grid::Rectangular_Slowness_Grid(Pf *pf,string tag)
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
Rectangular_Slowness_Grid::Rectangular_Slowness_Grid(const Rectangular_Slowness_Grid& rsg)
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
Slowness_vector Rectangular_Slowness_Grid::slow(int i, int j)
{
	if(i>=nux || j>=nuy || i<0 || j<0) 
		throw seispp_error(string("Illegal index request from Rectangular_Slowness_Grid object"));
	Slowness_vector u;
	u.ux=uxlow+i*dux;
	u.uy=uylow+j*duy;
	return(u);
}
// these are trivial constructors and could be done inline, but 
// decided to put them here to keep things together.  Learned this
// lesson the hard way
//
Slowness_vector::Slowness_vector()
{
	ux=0.0;
	uy=0.0;
}
Slowness_vector::Slowness_vector(const Slowness_vector& old)
{
	ux=old.ux;
	uy=old.uy;
}

// These could (and once were) inline, but decided that was poor
// memory management
double Slowness_vector::mag()
{
	return(hypot(ux,uy));
}
double Slowness_vector::azimuth()
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
double Slowness_vector::baz()
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
