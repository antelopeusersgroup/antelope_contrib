#include "seispp.h"

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
void apply_top_mute_ensemble(Time_Series_Ensemble& t, Top_Mute& mute)
{
	vector<Time_Series>::iterator i;
	Time_Series& tseries = *i;

	// This might be doable with STL algorithms, but this is so
	// simple why bother  Note the reference makes tseries an anias for *i
	for(i=t.tse.begin();i!=t.tse.end();++i) apply_top_mute(tseries,mute);
}
void apply_top_mute_3c_ensemble(Three_Component_Ensemble &t3ce, Top_Mute& mute)
{
	vector<Three_Component_Seismogram>::iterator i;
	Three_Component_Seismogram& t3c=*i;

	for(i=t3ce.tcse.begin();i!=t3ce.tcse.end();++i)
	{
		for(int j=0;j<2;++j) apply_top_mute(t3c.x[j],mute);
	}
}


