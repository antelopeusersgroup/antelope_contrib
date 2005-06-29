#include "seispp.h"
namespace SEISPP
{

// This is a series of functions for applying a simple top mute
// one or more traces.  Overloading is used to sort out type of
// object this is to apply to.  

//
//  for a simple time series
void ApplyTopMute(TimeSeries &ts,TopMute& mute)
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
	TimeWindow tw(ts.t0,t);
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
void ApplyTopMute(ThreeComponentSeismogram &ts,TopMute& mute)
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
	TimeWindow tw(ts.t0,t);
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


/* THIS WAS PREVIOUS CODE.  REPLACED BELOW WITH TEMPLATE AFTER CHANGE TO MEMBER SYMBOL */
/***************************************
// For a group of TimeSeries objects (ensemble)
void ApplyTopMute(TimeSeriesEnsemble& t, TopMute& mute)
{
	vector<TimeSeries>::iterator i;

	// This might be doable with STL algorithms, but this is so
	// simple why bother  Note the reference makes tseries an anias for *i
	// for some strange reason the scope resolution operator 
	// is necessary to avoid an overload ambiguity
	for(i=t.tse.begin();i!=t.tse.end();++i) 
		ApplyTopMute(*i,mute);
}
// 
// For an ensemble of 3-component seismograms.
//
void ApplyTopMute(ThreeComponentEnsemble &t3ce, TopMute& mute)
{
	vector<ThreeComponentSeismogram>::iterator t3c;

	for(t3c=t3ce.tcse.begin();t3c!=t3ce.tcse.end();++t3c)
	{
		ApplyTopMute(*t3c,mute);
	}
}
***************************************/
template<class T>
void ApplyTopMute(T& t, TopMute& mute)
{
	foreach(t.member.begin(),t.member.end(),ApplyTopMute(t,mute));
}
// Probably should have started with this, but we need constructors
// This uses a pf
TopMute::TopMute(Pf *pf,string tag)
{
	try {
		Metadata md(pf,tag);
		string reft = md.get_string("TimeReferenceType");
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
