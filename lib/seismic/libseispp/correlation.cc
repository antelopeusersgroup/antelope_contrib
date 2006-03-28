#include <string>
#include "perf.h"
#include "SeisppError.h"
#include "MultichannelCorrelator.h"
namespace SEISPP {
using namespace SEISPP;

TimeSeries correlation(TimeSeries& x, TimeSeries& y,bool normalize)
{
	int i;
	int lx,ly;
	const string base_message("Correlation :  ");

	lx=x.s.size();
	ly=y.s.size();
	if(ly<lx)
	{
//Peng Wang
//		build message string
//		throw a seispp exception
	    throw SeisppError(base_message+string("ly<lx\n"));
	}
	// The return series is cloned from y as the parent
	// This allows perservation of metadata to go with cross-correlation output.
	TimeSeries z(y);
	int lz=ly-lx;
	z.s.resize(lz);
	z.t0=y.t0-x.t0;
	z.dt=x.dt;  // probably not necessary, but forced initialization always good.
	z.ns=lz;
	// We could try to process around gaps, but this would
	// cost a lot in complication of uses of the results and
	// in efficiency.  We take the attitude here of dropping
	// data when there is any gap in the y vector
	if(y.has_gap() || !y.live)
	{
		z.live=false;
		for(i=0;i<lz;++i) z.s[i]=0.0;
	}
	else
	{
		// This should be done in frequency domain.  
		// It would be much faster, but done this way for now
		for(i=0;i<lz;++i)
		{
			z.s[i]=ddot(lx,&(x.s[0]),1,&(y.s[i]),1);
		}
		if(normalize)
		{
			double nrmx=dnrm2(lx,&(x.s[0]),1);
			double nrmy;
			for(i=0;i<lz;++i)
			{
				nrmy=dnrm2(lx,&(y.s[i]),1);
				z.s[i]/= (nrmx*nrmy);
			}
		}
	}
	return z;
}
		

TimeSeries correlation(TimeSeries& x, TimeSeries& y,TimeWindow lag_range, bool normalize)
{
	int i;
	int lx,ly;
	const string base_message("Correlation :  ");

	// This assumes default constructor marks output dead
	if( (!y.live) || (!x.live) ) return TimeSeries() ; 
	lx=x.s.size();
	ly=y.s.size();
	if(ly<lx)
	{
	    throw SeisppError(base_message+string("ly<lx\n"));
	}
	if(x.dt != y.dt)
		throw SeisppError(base_message+string(" sample rates do not match"));
	// The return series is cloned from y as the parent
	// This allows perservation of metadata to go with cross-correlation output.
	TimeSeries z(y);
	// Size of output is determined by lag_range combined with data range.
	// First get the range of lags to cover.
	double lagmin,lagmax;
	int iy0;
	lagmin=y.t0-x.t0;
	if(lagmin<lag_range.start)
	{
		iy0=nint((lag_range.start-lagmin)/x.dt);
		lagmin=lag_range.start;
	}
	else
	{
		iy0=0;
	}
	lagmax=y.t0-x.t0+(static_cast<double>(ly-lx-1))*x.dt;
	if(lagmax>lag_range.end)
		lagmax=lag_range.end;
	z.t0=lagmin;
	z.dt=x.dt;  // probably not necessary, but forced initialization always good.
	if(lagmin>lagmax)
	{
		z.live=false;
		z.ns=0;
		return(z); // return dead trace in this case.
	}
	int lz=static_cast<int>( (lagmax-lagmin)/x.dt ) + 1;
	z.s.resize(lz);
	z.ns=lz;
	// Test for gap in the lag window and discard if there
	// is a gap present
	TimeWindow gap_test(y.t0+y.dt*(static_cast<double>(iy0)),
			y.t0+y.dt*(static_cast<double>(iy0+lz+lx)));
	if(y.is_gap(gap_test) || !y.live)
	{
		z.live=false;
		for(i=0;i<lz;++i) z.s[i]=0.0;
	}
	else
	{
		for(i=0;i<lz;++i)
		{
			z.s[i]=ddot(lx,&(x.s[0]),1,&(y.s[i+iy0]),1);
		}
		if(normalize)
		{
			double nrmx=dnrm2(lx,&(x.s[0]),1);
			double nrmy;
			for(i=0;i<lz;++i)
			{
				nrmy=dnrm2(lx,&(y.s[i+iy0]),1);
				z.s[i]/= (nrmx*nrmy);
			}
		}
	}
	return z;
}
		
} // End SEISPP namespace declaration
