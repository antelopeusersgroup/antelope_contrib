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
			double nrmy=dnrm2(ly,&(y.s[0]),1);
			// an approximate normalization for length
			nrmy*=(static_cast<double>(lx)/static_cast<double>(ly));
			dscal(lz,1.0/(nrmx*nrmy),&(z.s[0]),1);
		}
	}
	return z;
}
		
} // End SEISPP namespace declaration
