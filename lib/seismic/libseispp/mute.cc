#include "seispp.h"
#include "mute.h"
#include "PfStyleMetadata.h"
namespace SEISPP
{
using namespace std;
using namespace SEISPP;

// This is a series of functions for applying a simple top mute
// one or more traces.  Overloading is used to sort out type of
// object this is to apply to.  

//
//  for a simple time series
void ApplyTopMute(TimeSeries &ts,TopMute& mute)
{
	int i,i2;
	double t;
	if(!mute.enabled) return;
	if(ts.t0>mute.t1) return;
	for(i=0;i<ts.ns;++i)
	{
		t = ts.time(i);
		if(t>mute.t0e) break;
		ts.s[i]=0.0;
	}
	TimeWindow tw(ts.t0,t);
	ts.add_gap(tw);  // zero portion needs to be flagged as a gap
	if(i>=ts.ns) return;
	for(i2=i;i2<ts.ns;++i2)
	{
		double weight;
		t = ts.time(i2);
		if(t>mute.t1) return;
		weight = (t-mute.t0e)/(mute.t1-mute.t0e);
		ts.s[i2]*=weight;
	}
}
// Unfortunately need very repetitious code for 3-component case
void ApplyTopMute(ThreeComponentSeismogram &ts,TopMute& mute)
{
	int i,i2,j;
	double t;
	if(!mute.enabled) return;
	if(ts.t0>mute.t1) return;
	for(i=0;i<ts.ns;++i)
	{
		t = ts.time(i);
		if(t>mute.t0e) break;
		for(j=0;j<3;++j) ts.u(j,i)=0.0;
	}
	TimeWindow tw(ts.t0,t);
	ts.add_gap(tw);  // zero portion needs to be flagged as a gap
	if(i>=ts.ns) return;
	for(i2=i;i2<ts.ns;++i2)
	{
		double weight;
		t = ts.time(i2);
		if(t>mute.t1) return;
		weight = (t-mute.t0e)/(mute.t1-mute.t0e);
		for(j=0;j<3;++j) ts.u(j,i2)*=weight;
	}
}


// For a group of TimeSeries objects (ensemble)
void ApplyTopMute(TimeSeriesEnsemble& t, TopMute& mute)
{
	if(!mute.enabled) return;
	vector<TimeSeries>::iterator i;

	// This might be doable with STL algorithms, but this is so
	// simple why bother  Note the reference makes tseries an anias for *i
	// for some strange reason the scope resolution operator 
	// is necessary to avoid an overload ambiguity
	for(i=t.member.begin();i!=t.member.end();++i) 
		ApplyTopMute(*i,mute);
}
// 
// For an ensemble of 3-component seismograms.
//
void ApplyTopMute(ThreeComponentEnsemble &t3ce, TopMute& mute)
{
	if(!mute.enabled) return;
	vector<ThreeComponentSeismogram>::iterator t3c;

	for(t3c=t3ce.member.begin();t3c!=t3ce.member.end();++t3c)
	{
		ApplyTopMute(*t3c,mute);
	}
}
#ifndef NO_ANTELOPE
TopMute::TopMute(Pf *pf,string tag)
{
	try {
		Metadata md(pf,tag);
		string reft = md.get_string("time_reference_type");
		if(reft=="absolute")
			reftype = absolute;
		else
			reftype = relative;
		t0e = md.get_double("zero_end_time");
		t1 = md.get_double("end_time");
		enabled=true;
	}
	catch (...) {throw;};
}
#endif
TopMute::TopMute(PfStyleMetadata& md,string tag)
{
    try{
        PfStyleMetadata branch=md.get_branch(tag);
	string reft = md.get_string("time_reference_type");
	if(reft=="absolute")
		reftype = absolute;
	else
		reftype = relative;
	t0e = md.get_double("zero_end_time");
	t1 = md.get_double("end_time");
	enabled=true;
    }
    catch (...) {throw;};
}

	
} // Termination of namespace SEISPP definitions
