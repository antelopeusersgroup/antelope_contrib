#include "seispp.h"
namespace SEISPP
{

// This is a series of functions for applying a simple top mute
// one or more traces.  Overloading is used to sort out type of
// object this is to apply to.  

//
//  for a simple time series
void apply_top_mute(Time_Series &ts,Top_Mute& mute)
{
	int i,i2;
	double t;
	if(ts.t0>mute.t1) return;
	for(i=0;i<ts.ns;++i)
	{
		t = ts.time(i);
		if(t>mute.t0e) break;
		ts.s[i]=0.0;
	}
	Time_Window tw(ts.t0,t);
	ts.add_gap(tw);  // zero portion needs to be flagged as a gap
	if(i>=ts.ns) return;
	for(i2=i;i2<ts.ns;++i2)
	{
		double weight;
		t = ts.time(i2);
		if(t>mute.t1) return;
		weight = (t-ts.t0)/(mute.t1-mute.t0e);
		ts.s[i2]*=weight;
	}
}
// Unfortunately need very repetitious code for 3-component case
void apply_top_mute(Three_Component_Seismogram &ts,Top_Mute& mute)
{
	int i,i2,j;
	double t;
	if(ts.t0>mute.t1) return;
	for(i=0;i<ts.ns;++i)
	{
		t = ts.time(i);
		if(t>mute.t0e) break;
		for(j=0;j<3;++j) ts.u(j,i)=0.0;
	}
	Time_Window tw(ts.t0,t);
	ts.add_gap(tw);  // zero portion needs to be flagged as a gap
	if(i>=ts.ns) return;
	for(i2=i;i2<ts.ns;++i2)
	{
		double weight;
		t = ts.time(i2);
		if(t>mute.t1) return;
		weight = (t-ts.t0)/(mute.t1-mute.t0e);
		for(j=0;j<3;++j) ts.u(j,i2)*=weight;
	}
}
// For a group of Time_Series objects (ensemble)
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
		apply_top_mute(t3c,mute);
	}
}
// Probably should have started with this, but we need constructors
// This uses a pf
Top_Mute::Top_Mute(Pf *pf,string tag)
{
	Metadata md(pf,tag);
	try {
		string reft = md.get_string("Time_Reference_Type");
		if(reft=="absolute")
			reftype = absolute;
		else
			reftype = relative;
		t0e = md.get_double("Zero_End_Time");
		t1 = md.get_double("End_Time");
	}
	catch (...) {throw;};
}
	
} // Termination of namespace SEISPP definitions
