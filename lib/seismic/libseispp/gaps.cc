#include <math.h>
#include "seispp.h"
//Needed to compile on linux
int nint(double);
/* this group of functions implement gap processing for time series
 * objects and their descendents called Three_Component_Seismograms.
 * They use indexing through the STL standard container called a
 * set.  The comparison function is defined in the seispp.h file
 * that allows interval indexing.  That is, we can look up a time
 * interval through an indexing algorithm that is an intrinsic part
 * of the generic set object.  This provides a fast lookup mechanism
 * to make this processing reasonably efficient.
 *
 * Author:  Gary L. Pavlis
 * Written:  May 2003
 */

// Returns true if the requested sample number of a gap or outside the
// range of the data
bool Time_Series::is_gap(int n0)
{
	if(n0<0 || n0>ns) return true;
	if(gaps.empty()) return false;
	// We use a window of 1 sample centered on sample time
	// This is should always work as long for processing where
	// we don't need to worry about overlapping waveforms with slipperly 
	// clocks
	Time_Window twin;
	twin.start = t0 + dt*((double)ns) - dt*0.5;
	twin.end = t0 + dt*((double)ns) + dt*0.5;
	if(gaps.find(twin)==gaps.end()) 
		return false;
	else
		return true;
}
bool Time_Series::is_gap(double t)
{
	if(t<t0 || t>(t0+((double)(ns-1))*dt)) return true;
	if(gaps.empty())return false;
	Time_Window twin;
	twin.start = t - dt*0.5;
	twin.end = t + dt*0.5;
	if(gaps.find(twin)==gaps.end()) 
		return false;
	else
		return true;
}
// Forces samples in marked gaps to zero.  
void Time_Series::zero_gaps()
{
	double tsend;
	int i,istart,iend;
	set<Time_Window,Time_Window_Cmp>::iterator this_gap;
	tsend = t0+((double)(ns-1))*dt;

	for(this_gap=gaps.begin();this_gap!=gaps.end();++this_gap)
	{
		if(this_gap->end < t0) continue;
		if(this_gap->start > tsend) continue;
		if(this_gap->start<t0)
			istart = 0;
		else
			istart = nint((this_gap->start-t0)/dt);
		if(this_gap->end>tsend)
			iend = ns-1;
		else
			iend = nint((this_gap->end-t0)/dt);
		for(i=istart;i<=iend;++i) s[i]=0.0;
	}
}
// These are comparable functions for three-component data.  
// They are mostly &&ing of the individual components
// The approach of always usng time for the integer version
// is safer and more general.  
//

bool Three_Component_Seismogram::is_gap(double t)
{
	for(int i=0;i<3;++i)
	{
		if(x[i].is_gap(t)) return(true);
	}
	return(false);
}
bool Three_Component_Seismogram::is_gap(int is)
{
	double t=t0+((double)(is-1))*dt;
	if(is_gap(t)) 
		return(true);
	else
		return(false);
}
void Three_Component_Seismogram::zero_gaps()
{
	for(int i=0;i<3;++i) x[i].zero_gaps();
}



		
		

