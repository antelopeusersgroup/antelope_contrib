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
	if(normalize)
	{
		for(i=0;i<lz;++i)
		{
			z.s[i]=ddot(lx,&(x.s[0]),1,&(y.s[i]),1);
			if(normalize) z.s[i]/=dnrm2(lx,&(y.s[i]),1);
		}
	}
	else
	{
		for(i=0;i<lz;++i)
		{
			z.s[i]=ddot(lx,&(x.s[0]),1,&(y.s[i]),1);
		}
	}

	return z;
}
		
} // End SEISPP namespace declaration
