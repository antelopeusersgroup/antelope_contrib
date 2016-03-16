/* This file contains member functions for a BasicTimeSeries object.*/
#include <ostream>
#include "stock.h"  // Antelope stock library for strtime in operator <<
#include "BasicTimeSeries.h"
namespace SEISPP {
using namespace SEISPP;
// Returns true if the requested sample number of a gap or outside the
// range of the data
bool BasicTimeSeries::is_gap(int n0)
{
	if(n0<0 || n0>ns) return true;
	if(gaps.empty()) return false;
	// We use a window of 1 sample centered on sample time
	// This is should always work as long for processing where
	// we don't need to worry about overlapping waveforms with slipperly 
	// clocks
	TimeWindow twin;
	double t=time(n0);
	twin.start = t - dt*0.5;
	twin.end = t +  dt*0.5;
	if(gaps.find(twin)==gaps.end()) 
		return false;
	else
		return true;
}
// query for gap by time window
bool BasicTimeSeries::is_gap(TimeWindow twin)
{
	if(gaps.find(twin)==gaps.end())
		return(false);
	else
		return(true);
}
bool BasicTimeSeries::is_gap(double t)
{
	if(t<t0 || t>(t0+((double)(ns-1))*dt)) return true;
	if(gaps.empty())return false;
	TimeWindow twin;
	twin.start = t - dt*0.5;
	twin.end = t + dt*0.5;
	if(gaps.find(twin)==gaps.end()) 
		return false;
	else
		return true;
}
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
	tref=relative;
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
	tref=absolute;
}

BasicTimeSeries::BasicTimeSeries()
{
	t0=0.0;
	tref=relative;
	live=false;
	dt=1.0;
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
ostream& operator<<(ostream& os,BasicTimeSeries& y)
{

	os << "ns=" <<y.ns<<endl
		<< "dt=" <<y.dt<<endl;
	if(y.live)
		os << "Data is marked live"<<endl;
	else
		os << "Data is marked dead"<<endl;
	if(y.tref==relative)
		os << "Data time standard is relative"<<endl
			<< "t0="<<y.t0<<endl;
	else
		os << "Data time standard is UTC (absolute)"<<endl
			<< "t0="<<strtime(y.t0)<<endl;
	set<TimeWindow,TimeWindowCmp>::iterator g;
	for(g=y.gaps.begin();g!=y.gaps.end();++g)
	{
            if(y.tref==relative)
            {
		os << "Data gap in TimeWindow=("
			<< g->start
			<<","
			<<g->end
			<<")"<<endl;
            }
            else
            {
		os << "Data gap in TimeWindow=("
			<< strtime(g->start)
			<<","
			<<strtime(g->end)
			<<")"<<endl;
            }
	}
	return(os);
}

}  // end SEISPP namespace encapsulation
