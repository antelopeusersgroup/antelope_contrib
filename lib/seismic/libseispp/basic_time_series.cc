/* This file contains member functions for a Basic_Time_Series object.*/
#include "seispp.h"
namespace SEISPP {
using namespace SEISPP;
void Basic_Time_Series::ator(double tshift)
{
	if(tref==relative) return;
	t0 -= tshift;
	// We have to shift all the gap windows definitions
	Time_Window tw;
	set <Time_Window,Time_Window_Cmp> shifted_gaps;
	set <Time_Window,Time_Window_Cmp>::iterator this_gap;
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
void Basic_Time_Series::rtoa(double tshift)
{
	if(tref==absolute) return;
	t0 += tshift;
	Time_Window tw;
	set <Time_Window,Time_Window_Cmp> shifted_gaps;
	set <Time_Window,Time_Window_Cmp>::iterator this_gap;
	for(this_gap=gaps.begin();this_gap!=gaps.end();++this_gap)
	{
		tw.start = this_gap->start + tshift;
		tw.end = this_gap->end + tshift;
		shifted_gaps.insert(tw);
	}
	gaps = shifted_gaps;
}

Basic_Time_Series::Basic_Time_Series()
{
	t0=0.0;
	tref=absolute;
	live=false;
	dt=0.0;
	ns=0;
}
Basic_Time_Series::Basic_Time_Series(const Basic_Time_Series& tsin)
{
	t0=tsin.t0;
	tref=tsin.tref;
	live=tsin.live;
	dt=tsin.dt;
	ns=tsin.ns;
	gaps=tsin.gaps;
}

}  // end SEISPP namespace encapsulation
