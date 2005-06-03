/* This file contains member functions for a BasicTimeSeries object.*/
#include "seispp.h"
namespace SEISPP {
using namespace SEISPP;
void BasicTimeSeries::ator(double tshift)
{
	if(tref==relative) return;
	t0 -= tshift;
	// We have to shift all the gap windows definitions
	TimeWindow tw;
	set <TimeWindow,TimeWindowCmp> shifted_gaps;
	set <TimeWindow,TimeWindowCmp>::iterator this_gap;
	for(this_gap=gaps.begin();this_gap!=gaps.end();++this_gap)
	{
		tw.start = this_gap->start - tshift;
		tw.end = this_gap->end - tshift;
		shifted_gaps.insert(tw);
	}
	gaps = shifted_gaps;
}
// inverse of ator -- note minus becomes plus
// everything else is nearly identical
void BasicTimeSeries::rtoa(double tshift)
{
	if(tref==absolute) return;
	t0 += tshift;
	TimeWindow tw;
	set <TimeWindow,TimeWindowCmp> shifted_gaps;
	set <TimeWindow,TimeWindowCmp>::iterator this_gap;
	for(this_gap=gaps.begin();this_gap!=gaps.end();++this_gap)
	{
		tw.start = this_gap->start + tshift;
		tw.end = this_gap->end + tshift;
		shifted_gaps.insert(tw);
	}
	gaps = shifted_gaps;
}

BasicTimeSeries::BasicTimeSeries()
{
	t0=0.0;
	tref=absolute;
	live=false;
	dt=0.0;
	ns=0;
}
BasicTimeSeries::BasicTimeSeries(const BasicTimeSeries& tsin)
{
	t0=tsin.t0;
	tref=tsin.tref;
	live=tsin.live;
	dt=tsin.dt;
	ns=tsin.ns;
	gaps=tsin.gaps;
}

}  // end SEISPP namespace encapsulation
