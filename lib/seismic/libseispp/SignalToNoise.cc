#include <vector>
#include "perf.h"
#include "TimeWindow.h"
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
#include "ensemble.h"
#include "SignalToNoise.h"

using namespace std;
using namespace SEISPP;
namespace SEISPP
{
/* Compute rms over a range of samples testin each for gaps.
Return negative number if interval is invalid or all gaps. 
Otherwise return rms. */
double compute_rms_with_gaps(TimeSeries& d, SampleRange& srange)
{
	if(srange.nsamp<=0) return(-1.0);
	if(!d.live) return(-1.0);
	double ssq;
	int i,count;
	double val;
	for(i=srange.nstart,count=0,ssq=0.0;i<=srange.nend;++i)
	{
		if(d.is_gap(i)) continue;
		val=d.s[i];
		ssq+=val*val;
		++count;
	}
	if(count<=0) return(-1.0);
	ssq=ssq/static_cast<double>(count);
	return(sqrt(ssq));
}
double compute_rms(TimeSeries&d, TimeWindow win)
{
	SampleRange sr(get_sample_range<TimeSeries>(d,win));
	if(sr.nsamp<=0) return(-1.0);
	double rms;
	if(d.is_gap(win))
		rms=compute_rms_with_gaps(d,sr);
	else
	{
		/* use blas function for efficiency in this case */
		rms=dnrm2(sr.nsamp,&(d.s[sr.nstart]),1);
		rms=rms*rms/static_cast<double>(sr.nsamp);
		rms=sqrt(rms);
	}
	return(rms);
}
	
	
double SNR_rms(TimeSeries& d, TimeWindow signal, TimeWindow noise)
{
	double srms, nrms;
	srms=compute_rms(d,signal);
	if(srms<0.0) return(-1.0);
	nrms=compute_rms(d,noise);
	if(nrms<=0.0) return(-1.0);
	return(srms/nrms);
}
/* Overloaded version of above, but assumes all data before the start of the signal
window should be treated as noise */
double SNR_rms(TimeSeries& d, TimeWindow signal)
{
	TimeWindow noise(d.t0,signal.start);
	double snr=SNR_rms(d,signal,noise);
	return(snr);
}
/* These two are stubbed for now.  Can write very parallel code to TimeSeries version 
but best to debug that first to make sure I don't have some logic error. */
double SNR_rms(ThreeComponentSeismogram& d, TimeWindow signal, TimeWindow noise)
{
	return(-1.0);
}
double SNR_rms(ThreeComponentSeismogram& d, TimeWindow signal)
{
	return(-1.0);
}

} // End SEISPP namespace declaration
