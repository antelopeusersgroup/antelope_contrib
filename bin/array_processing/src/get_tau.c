#include <stdio.h>
#include <stdlib.h>
#include <math.h>
 
int correl(float x[],float y[],float *z,int npts,int lags,float *rmax,int *kmax);
int nint(double x);
int stats(float x[], int npts, int k, float *mean, float *stdev, float *skew, float *kurtosis);

int get_tau(float y1[],float y2[],double tstart1,double tstart2,int npts,
double laglen,double sr,double *tau,float *ccc,float *cccp)
/*
      This function cross-correlates time series.  
      It is assumed that the time series are float arrays in y1 and y2.
      The additional array z is created to hold the cross-correlation series.
 
      y1,y2:       data series to be cross-correlated
      tstart1,2    start times (epoch seconds) of data series
      npts:        length of the two series
      sr:          sampling rate of the two series (not a whole number)
      laglen:      maximum amount (seconds) of offset for cross-correlation
                       laglen << npts/sr (laglen ~ 0.2 npts/sr is typical)
      tau:         signal time diff. (epoch seconds) of second - first
                   (This closely reflects the origin time difference.)
      ccc:         cross-correlation coefficient (-1 <= ccc <= 1)
      cccp:        relative amplitude of second event over first event

      return value:0 -- no error
                   1 -- real sample rate too far from integer value
                   2 -- number of requested lags is larger than length of series
                   3 -- divisor = 0.0 in computing refined tau
*/ 
{
  int         iret,len,isr,lags,kmax,i;
  float       *z;
  float       dummy,amean1,amean2,stdev1,stdev2,del,tmax,rmax;
  float       ym,y0,yp,a,b,c,tpeak,tshift;
  float       tol=0.01;
/*tol is the tolerance to allow the real sampling rate to deviate from an
  integer value; for instance for tol = 0.01, 99.99 and 100.01 are
  acceptable and are taken to be nominally 100.  This value is set here
  to 0.01, and will produce only up to 1/10 of a millisecond error in 1 sec.*/

/*Check if sample rate is nearly integer.  Some tolerance must be allowed 
  because sample rates given by CSS3.0 data may not be exactly whole 
  numbers.*/
  isr = nint(sr);
  if (fabs(isr - sr) > tol) return 1;

/*Get mean and stdev estimate and remove mean.*/
  stats(y1,npts,2,&amean1,&stdev1,&dummy,&dummy);
  for (i=0;i<npts;i++) y1[i] = y1[i] - amean1;
  stats(y2,npts,2,&amean2,&stdev2,&dummy,&dummy);
  for (i=0;i<npts;i++) y2[i] = y2[i] - amean2;

  lags = nint(laglen*sr);
  if (lags > npts) return 2;
  len = 2*lags + 1;
  z = malloc(len*sizeof(float));
  del = 1.0/sr;

/*Get cross-correlation in time domain and normalize by standard deviations.
  Normalize by stn. dev. squared of the first event to get an estimate
  of the amplitude of the second event relative to that of the first.  The
  log10 of this estimate would estimate the magnitude difference.*/

  iret = correl(y1,y2,z,npts,lags,&rmax,&kmax);

  *ccc  = rmax/(stdev1*stdev2);
  *cccp = rmax/(stdev1*stdev1);

/*Occasional cases may arise where this leads to |CCF| > 1, so reset it 
  to +/-1 if greater.  Such cases arise when the interpolated peak of the 
  cross-correlation function goes above +1 or below -1.*/
  if (*ccc >  1.0) *ccc =  1.0;
  if (*ccc < -1.0) *ccc = -1.0;

/*Compute the delay.  A positive tmax means the 2nd signal is delayed
  wrt 1st by that amount.*/
  tmax=kmax*del;
/*Pick out 3 points around maximum and compute best-fit parabola.*/
  ym = z[kmax+lags-1];
  y0 = z[kmax+lags];
  yp = z[kmax+lags+1];
  a = ( ym/2 + yp/2 - y0)/pow((double) del,(double)2);
  b = (-ym/2 + yp/2)/del;
  c = y0;
/*Peak is at relative time where derivative = 0.*/
  if (a == 0.0) 
  {
    free(z);
    return 3;
  }
  tpeak = -b/(2*a);
/*Get total time shift.*/
  tshift = tmax + tpeak;
/*Compute absolute time shift between signals.  If the 2nd is later than the 
  1st, the shift is positive.*/
  *tau = tstart2 - tstart1 + tshift;

  free(z);
  return 0;
}


int correl(float x[],float y[],float *z,int npts,int lags,float *rmax,int *kmax)
/*
      This returns the crosscorrelation function, normalized by the number
      of points used but not by the stdev's of the two time series.

      x,y  = two time series
      z    = array to hold the cross-correlation function
      npts = length of the two time series
      lags = number of lags  (n = 2*lags + 1)
      rmax = maximum of the correlation function (positive or negative). 
             To handle cases where polarity reversal may occur, the maximum is 
             allowed to be positive or negative.
      kmax = point of this maximum (relative to center point)
             e.g., if the zero-lag coef. is maximum, kmax = 0

      Note: The zeroth lag will be the midpoint of the returned series z.
*/ 
{
  int n,j,i;

  *rmax = 0.0;
  n = 2*lags + 1;
  for (j=0;j<n;j++)
  {
    z[j] = 0.0;
    for (i=0;i<npts;i++)
      if(i+j-lags >= 0 && i+j-lags < npts) z[j] = z[j] + x[i]*y[i+j-lags];
/*  Normalize the CCF by the full length.  This is like doing a circular
    cross-correlation with zero padding.*/
    z[j] = z[j]/npts;
    if (fabs(z[j]) > fabs(*rmax))
    {
      *rmax = z[j];
      *kmax = j-lags;
    }
  }
  return 0;
}
