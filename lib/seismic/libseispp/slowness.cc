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
		md.get_string("Slowness_Grid_Name");
		uxlow=md.get_double("uxlow");
		uylow=md.get_double("uylow");
		nux = md.get_int("nux");
		nuy = md.get_int("nuy");
		dux = md.get_double("dux");
		duy = md.get_double("duy");
	} catch (...) {throw;}
}

}  // End namespace SEISPP
