/* This file contains member functions for a BasicTimeSeries object.*/
#include <ostream>
#include "stock.h"  // Antelope stock library for strtime in operator <<
#include "BasicTimeSeries.h"
#include "SeisppError.h"
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
        t0shift=tshift;
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
        t0shift_is_valid=true;
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
        t0shift_is_valid=true;
}
void BasicTimeSeries::rtoa()
{
        /* dead traces should to totally ignored */
        if(!(this->live)) return;
        const string errormess("BasicTimeSeries::rtoa() t0shift for conversion is not defined.");
	if(tref==absolute) return;
        /* A rather odd test for a nonzero.   We use 100 s assuming no active
         * source data would use a shift longer than that unless it really did
         * have an absolute time standard. Also assumes we'll never use data from 
         * the first 2 minutes of 1960.*/
        if(t0shift_is_valid || (t0shift>100.0) ) 
        {
    	    t0 += t0shift;
    	    TimeWindow tw;
    	    set <TimeWindow,TimeWindowCmp> shifted_gaps;
    	    set <TimeWindow,TimeWindowCmp>::iterator this_gap;
    	    for(this_gap=gaps.begin();this_gap!=gaps.end();++this_gap)
    	    {
		tw.start = this_gap->start + t0shift;
		tw.end = this_gap->end + t0shift;
		shifted_gaps.insert(tw);
	    }
	    gaps = shifted_gaps;
	    tref=absolute;
            t0shift_is_valid=false;
        }
        else
            throw SeisppError(errormess);
}

BasicTimeSeries::BasicTimeSeries()
{
	t0=0.0;
	tref=relative;
	live=false;
	dt=1.0;
	ns=0;
        t0shift=0.0;
        t0shift_is_valid=false;
}
BasicTimeSeries::BasicTimeSeries(const BasicTimeSeries& tsin)
{
	t0=tsin.t0;
	tref=tsin.tref;
	live=tsin.live;
	dt=tsin.dt;
	ns=tsin.ns;
	gaps=tsin.gaps;
        t0shift=tsin.t0shift;
        t0shift_is_valid=tsin.t0shift_is_valid;
}
BasicTimeSeries& BasicTimeSeries::operator=(const BasicTimeSeries& parent)
{
    if (this!=&parent)
    {
	t0=parent.t0;
	tref=parent.tref;
	live=parent.live;
	dt=parent.dt;
	ns=parent.ns;
	gaps=parent.gaps;
        t0shift=parent.t0shift;
        t0shift_is_valid=parent.t0shift_is_valid;
    }
    return *this;
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
        {
		os << "Data time standard is relative"<<endl
			<< "t0="<<y.t0<<endl;
        }
	else
		os << "Data time standard is UTC (absolute)"<<endl
			<< "t0="<<strtime(y.t0)<<endl;
        if(y.t0shift_is_valid)
            os << "t0shift_is_valid is true"<<endl
                << "t0shift value="<< setprecision(13)<<y.t0shift<<endl;
        else
            os << "t0shift_is_valid is set false time should be absolute"<<endl;
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
void BasicTimeSeries::shift(double dt)
{
    double oldt0shift=t0shift;
    this->rtoa();
    this->ator(oldt0shift+dt);
}
double BasicTimeSeries::time_reference()
{
    const string base_error("BasicTimeSeries::time_reference method: ");
    if(tref==absolute)
        throw SeisppError(base_error
                + "data have absolute time set so requesting the reference"
                + " time make no sense - likely a coding error");
    if(t0shift_is_valid)
        return(t0shift);
    else
        throw SeisppError(base_error
                + "cannot return time reference as it is marked invalid");
}
/* This used to be in the include file as a trivial insert.  Found it useful
 * to silently do nothing if the the window was less than one sample in length */
void BasicTimeSeries::add_gap(TimeWindow tw)
{
  const string base_error("BasicTimeSeries::add_gap:  ");
  /* This complicated construct is required as return code for set insert
   * method.   We need it to test for failed insertions that happen with 
   * TimeWindowCmp when a new time window overlaps with an existing one. */
  pair<set<TimeWindow,TimeWindowCmp>::iterator,bool> ret;
  double tlen;
  tlen=tw.end-tw.start;
  /* this also does nothing if the above resolves negatibe.  That case maybe
   * should throw an error.  The main idea is to do nothing if the window 
   * is less than one sample */
  if(tlen>(this->dt))
  {
      ret=gaps.insert(tw);
      if(ret.second) return;   //boolean is set true if insert succeeded
      set<TimeWindow,TimeWindowCmp>::iterator gptr;
      gptr=gaps.find(tw);
      // This error should not happen, but chaos would happen if it did
      if(gptr==gaps.end()) throw (base_error
               + "Coding problem with set find method.   Contact author");
      /* Create the new time window to replace this one as the range
       * define by both.   Fudge the ends by 1/2 sample to be sure the 
       * insert succeed with a unique interval */
      TimeWindow fullgap;
      fullgap.start=min(tw.start,(gptr->start));
      fullgap.end=max(tw.end,(gptr->end));
      fullgap.start -= ((this->dt)/2.0);
      fullgap.end += ((this->dt)/2.0);
      /* Testing shows we have to erase the old window we are replacing */
      gaps.erase(gptr);
      ret=gaps.insert(fullgap);
      /* This could probably be done through recursion, but will only 
       * allow two levels.  Found this was secondary scan was sometimes
       * necessary. */
      if(ret.second) return;
      gptr=gaps.find(fullgap);
      /* The 1/4 sample interval pad is artirary - want to avoid floating
       * point equal compares */
      if((gptr->start)<fullgap.start) 
          fullgap.start=(gptr->start)-((this->dt)/4.0);
      if((gptr->end)>fullgap.end) 
          fullgap.end=(gptr->end)+((this->dt)/4.0);
      gaps.erase(gptr);
      ret=gaps.insert(fullgap);
      if(!ret.second) throw SeisppError(base_error
              + "Coding problem with set insert method.  Contact author.");
  }
}
/* Useful procedure for any object inheriting BasicTimeSeries.  
 * It checks for a situation where all samples in a time series
 * are inside a gap.   It returns the number of samples samples
 * not flagged as a gap.  Be warned the process is not cheap 
 * as we have to test full ns range */
int number_valid_samples(BasicTimeSeries& d)
{
    /* First test to see if there are any gaps defined an return 
     * ns if there are none. */
    if(d.has_gap()<=0)
    {
        return d.ns;
    }
    else
    {
        int i,nvalid;
        for(i=0,nvalid=0;i<d.ns;++i)
        {
            if(!d.is_gap(i)) ++nvalid;
        }
        return nvalid;
    }
}

}  // end SEISPP namespace encapsulation
