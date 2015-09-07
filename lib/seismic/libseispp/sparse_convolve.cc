#include "perf.h"
#include "ThreeComponentSeismogram.h"
#include "TimeSeries.h"
#include "seispp.h"
namespace SEISPP {
using namespace std;
using namespace SEISPP;
ThreeComponentSeismogram sparse_convolve(TimeSeries& wavelet, 
	ThreeComponentSeismogram& d)
{
	if( (wavelet.tref==absolute) || (d.tref==absolute) )
		throw SeisppError(string("Error (convolve procedure): ")
			+ "both functions to be convolved must have "
			+ "relative time base");
	ThreeComponentSeismogram out3c(d);
        int nw=wavelet.ns;
        double *wptr;
        wptr=&(wavelet.s[0]);
	/* Add a generous padding for out3c*/
	int nsout=d.ns+2*nw;

	out3c.u=dmatrix(3,nsout);
        out3c.u.zero();
        out3c.t0=d.t0-(out3c.dt)*static_cast<double>(wavelet.ns);
	out3c.ns=nsout;
        /* oi is the position of the moving index position in out3c */
        int oi=out3c.sample_number(d.t0);
        /* si is the index to the point where the wavelet is to be inserted. offset by 0 of wavelet*/
        int si=oi-wavelet.sample_number(0.0);
        if(si<0) throw SeisppError("Error computed out3c index is less than 0 ");
        /* Intentionally do not check for stray indices as padding above
           should guarantee no pointers fly outside the bounds of the data.*/
        int i,k;
        for(i=0;i<d.ns;++i,++si){
            double *sptr=out3c.u.get_address(0,si);
            double *dptr=d.u.get_address(0,i);
            for(k=0;k<3;++k){
                if((*dptr)!=0.0){
                    daxpy(nw,(*dptr),wptr,1,sptr,3);
                }
                ++sptr;
                ++dptr;
            }
        }
	return out3c;
}
}  // End SEISPP namespace encapsulation
