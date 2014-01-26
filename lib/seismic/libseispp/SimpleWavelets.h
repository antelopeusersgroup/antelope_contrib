#ifndef _SIMPLEWAVELETS_H_
#define _SIMPLEWAVELETS_H_
#include "TimeSeries.h"
namespace SEISPP{
    /*! Used by simple wavlet procedures as a key for scaling.  
      Standard analytic wavelets can be scaled a variety of ways.
      This enum is used to define how that should be done.
      As implied by names PEAK means normalize by the largest 
      amplitude, AREA means normalize by integral of function */
enum WaveletNormalizationMethod {PEAK, AREA, NONE};
/*! Build a Gaussian wavelet.

  Gaussian wavelets are commonly an idealized deconvolution output.
  This procedure creates one with any set of parameters.  It will
  quietly produce garbage if the parameters are inconsistent. 

  \param n length of output time series object to hold wavelet
  \param dt sample interval of output time series.  Length with be
     (n-1)*dt with the 0 time at n/2.  
  \param sigma gaussian width parameter (units of seconds or at
     least the same units as dt)
  \param normalize is the normalization method to use.  Default is peak
     normalization, but AREA is and NONE are allowed.  */
TimeSeries gaussian_wavelet(int n, double dt, double sigma,
	WaveletNormalizationMethod normalize);
/*! Build a Ricker` wavelet.

  Ricker wavelets are commonly used in many signal processing applications.
  This procedure creates one with any set of parameters.  It will
  quietly produce garbage if the parameters are inconsistent. 

  \param n length of output time series object to hold wavelet
  \param dt sample interval of output time series.  Length with be
     (n-1)*dt with the 0 time at n/2.  
  \param nu ricker wavelet width parameter.  Note nu=1/f_m where f_m is
     the peak frequency.   Note time between negative sidebands is 
     sqrt(6)*nu/pi and zero crossing measure of peak width is 
     sqrt(2)*nu/pi.
  \param normalize is the normalization method to use.  Only PEAK
    or NONE are accepted.  If passed AREA this will throw an exception. 
 
 \exception throws a SeisppError object if told to normalize by AREA as this
 is nonsense for a Ricker wavelet.*/
TimeSeries ricker_wavelet(int n, double dt, double nu,
	WaveletNormalizationMethod normalize);
}
#endif
