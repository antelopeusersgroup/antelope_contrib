#include "seispp.h"
namespace SEISPP
{

// This is a series of functions for applying a simple top mute
// one or more traces.  Overloading is used to sort out type of
// object this is to apply to.  
//
// First for a simple time series
void apply_top_mute(Time_Series &ts,Top_Mute& mute)
{
	int i,i2;
	double t;
	if(ts.t0>mute.t1) return;
	for(i=0;i<ts.ns;++i)
	{
		t = ts.t0 +(ts.dt)*((double)i);
		if(t>mute.t0e) break;
		ts.s[i]=0.0;
	}
	Time_Window tw(ts.t0,t);
	ts.add_gap(tw);  // zero portion needs to be flagged as a gap
	if(i>=ts.ns) return;
	for(i2=i;i2<ts.ns;++i2)
	{
		double weight;
		t = ts.t0 +(ts.dt)*((double)i2);
		if(t>mute.t1) return;
		weight = (t-ts.t0)/(mute.t1-mute.t0e);
		ts.s[i2]*=weight;
	}
}
// For a group of times eries (ensemble)
void apply_top_mute(Time_Series_Ensemble& t, Top_Mute& mute)
{
	vector<Time_Series>::iterator i;
	Time_Series& tseries = *i;

	// This might be doable with STL algorithms, but this is so
	// simple why bother  Note the reference makes tseries an anias for *i
	// for some strange reason the scope resolution operator 
	// is necessary to avoid an overload ambiguity
	for(i=t.tse.begin();i!=t.tse.end();++i) 
		apply_top_mute(tseries,mute);
}
// 
// For an ensemble of 3-component seismograms.
//
void apply_top_mute(Three_Component_Ensemble &t3ce, Top_Mute& mute)
{
	vector<Three_Component_Seismogram>::iterator i;
	Three_Component_Seismogram& t3c=*i;

	for(i=t3ce.tcse.begin();i!=t3ce.tcse.end();++i)
	{
		for(int j=0;j<2;++j) 
			apply_top_mute(t3c.x[j],mute);
	}
}
} // Termination of namespace SEISPP definitions
