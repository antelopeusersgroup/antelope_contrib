#include "SimpleWavelets.h"
namespace SEISPP {

TimeSeries gaussian_wavelet(int n, double dt, double sigma,
	WaveletNormalizationMethod normalize)
{
	TimeSeries gwavelet(n);
	double tlength;
	tlength=dt*static_cast<double>(n-1);
        gwavelet.ns=n;
	gwavelet.dt=dt;
	gwavelet.t0=-tlength/2.0;
	gwavelet.live=true;
	gwavelet.tref=relative;
	gwavelet.put("samprate",1.0/dt);
	gwavelet.put("time",gwavelet.t0);
	gwavelet.put("nsamp",n);
	int i;
	for(i=0;i<n;++i)
	{
		gwavelet.s[i]=exp(-pow(gwavelet.time(i)/sigma,2.0));
	}
	double normfactor;
	int peaksample;
	switch (normalize)
	{
	case PEAK:
                peaksample=gwavelet.sample_number(0.0);
		normfactor=gwavelet.s[peaksample];
		break;
	case AREA:
		for(i=0,normfactor=0.0;i<n;++i)
			normfactor += gwavelet.s[i];
		break;
	case NONE:
		return gwavelet;
	}
	for(i=0;i<n;++i) gwavelet.s[i]/=normfactor;
	return gwavelet;
}
TimeSeries ricker_wavelet(int n, double dt, double nu,
	WaveletNormalizationMethod normalize)
{
	TimeSeries rwavelet(n);
	double tlength;
	tlength=dt*static_cast<double>(n-1);
        rwavelet.ns=n;
	rwavelet.dt=dt;
	rwavelet.t0=-tlength/2.0;
	rwavelet.live=true;
	rwavelet.tref=relative;
	rwavelet.put("samprate",1.0/dt);
	rwavelet.put("time",rwavelet.t0);
	rwavelet.put("nsamp",n);
	double pinu2=M_PI*M_PI*nu*nu;
	int i;
	for(i=0;i<n;++i)
	{
		double t2=rwavelet.time(i);
		t2=t2*t2;
		rwavelet.s[i]=(1.0 - 2.0*pinu2*t2)
				*exp(-pinu2*t2);
	}
	double normfactor;
	int peaksample;
	switch (normalize)
	{
	case PEAK:
                peaksample=rwavelet.sample_number(0.0);
		normfactor=rwavelet.s[peaksample];
		break;
	case NONE:
		return rwavelet;
	case AREA:
		throw SeisppError(string("ricker_wavlet procedure:  ")
                        + "request to normalize wavelet by area is illegal\n"
                        + "Coding error.  Fix are rebuild your application.");
	}
	for(i=0;i<n;++i) rwavelet.s[i]/=normfactor;
	return rwavelet;
}
} // End SEISPP namespace encapsulation
