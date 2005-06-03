#include "seispp.h"
namespace SEISPP
{

// Rather trivial routines to apply a geometric static as a simple elevation
// divided by an input velocity.  
void ApplyGeometricStatic(TimeSeries *ts, double vel, double elev)
{
	ts->t0 -= elev/vel;
}
// Here we use overloading to allow simplified and/or defaulted values
// This implementation is always verbose if errors are thrown in
// this process, but it never aborts.


void ApplyGeometricStatic(TimeSeries *ts)
{
	double elev,vel;
	const string help_message="EDIT metadata_defaults.pf\n";
	try {
		elev = ts->get_double("elevation");
	} catch (MetadataError& mde)
	{
		mde.log_error();
		cerr << "elevation set to 0.0"<<endl<<help_message;
	}
	try {
		vel = ts->get_double("surface_velocity");
	} catch (MetadataError& mdev)
	{
		mdev.log_error();
		cerr << "surface_velocity set to 3.5"<<endl<<help_message;;
	}
	ApplyGeometricStatic(ts,vel,elev);
}
} // Termination of namespace SEISPP definitions

