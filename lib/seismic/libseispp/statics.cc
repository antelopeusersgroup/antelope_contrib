#include "seispp.h"
// Rather trivial routines to apply a geometric static as a simple elevation
// divided by an input velocity.  
void apply_geometric_static(Time_Series *ts, double vel, double elev)
{
	ts->t0 -= elev/vel;
}
void apply_geometric_static(Three_Component_Seismogram *s, double vel, double elev)
{
	for(int i=0;i<3;++i) apply_geometric_static(&(s->x[i]),vel,elev);
}
// Here we use overloading to allow simplified and/or defaulted values
// This implementation is always verbose if errors are thrown in
// this process, but it never aborts.

void apply_geometric_static(Time_Series *ts)
{
	double elev,vel;
	const string help_message="EDIT metadata_defaults.pf\n";
	try {
		elev = ts->md.get_double("elevation");
	} catch (Metadata_error mde)
	{
		if(SEISPP_verbose)
		{
			mde.log_error();
			cerr << "elevation set to 0.0"<<endl<<help_message;
		}
	}
	try {
		vel = ts->md.get_double("surface_velocity");
	} catch (Metadata_error mdev)
	{
		if(SEISPP_verbose)
		{
			mdev.log_error();
			cerr << "surface_velocity set to 3.5"<<endl<<help_message;;
		}
	}
	apply_geometric_static(ts,vel,elev);
}
