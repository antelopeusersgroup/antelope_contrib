#include "perf.h"
#include "seispp.h"
#include "WindowMetric.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP
{
/* Not sure it is necessary to explicitly implement this, but won't hurt 
   to do so. */
BasicWindowMetric& BasicWindowMetric::operator=(const BasicWindowMetric& parent)
{
  if(&parent!=this)
    window=parent.window;
  return *this;
}
double WindowRMS::metric(const TimeSeries& d)
{
  try{
    TimeSeries dwin(WindowData(d,this->window));
    double l2nrm;
      l2nrm=dnrm2(dwin.ns,&(dwin.s[0]),1);
      return(l2nrm/(double)(dwin.ns));
  }catch(...){throw;};
}
double WindowRMS::metric(const ThreeComponentSeismogram& d)
{
  try{
    ThreeComponentSeismogram dwin(WindowData(d,this->window));
    /* We define rms here not as sum of square of all data points
       but average 3c amplitude */
    double amp_i;
    double sumamp;
    int i,k;
    for(i=0,sumamp=0.0;i<dwin.ns;++i)
    {
      double *ptr;
      double compval;
      for(k=0,amp_i=0.0;k<3;++k) 
      {
        ptr=dwin.u.get_address(k,i);
        compval=*ptr;
        amp_i += compval*compval;
      }
      sumamp += sqrt(amp_i);
    }
    return(sumamp/((double)(dwin.ns)));
  }catch(...){throw;};
}
double WindowExtrema::metric(const TimeSeries& d)
{
  try{
    vector<double>::iterator minval,maxval;
    TimeSeries dwin(WindowData(d,this->window));
    minval=min_element(dwin.s.begin(),dwin.s.end());
    maxval=max_element(dwin.s.begin(),dwin.s.end());
    return((*maxval) - (*minval));
  }catch(...){throw;};
}
/* This algorithm defines the range as the maximum range on
   any component.  To simplify coding we use a fairly 
   expensive but simple algorithm that uses the TimeSeries 
   method. */
double WindowExtrema::metric(const ThreeComponentSeismogram& d)
{
  try{
    /* We don't window the data like this but depends upon the
     * metric methods to extract the window of interest assuming
     * this is a longer waveform segment. */
    //ThreeComponentSeismogram dwin(WindowData(d,this->window));
    int k;
    double amps[3];
    for(k=0;k<3;++k)
    {
      //Slight inefficency to not window before this step
      TimeSeries *dcomp;
      //dcomp=ExtractComponent(dwin,k);
      dcomp=ExtractComponent(d,k);
      amps[k]=this->metric(*dcomp);
      delete dcomp;
    }
    double retval(amps[0]);
    if(amps[1]>retval) retval=amps[1];
    if(amps[2]>retval) retval=amps[2];
    return retval;
  }catch(...){throw;};
}
/* We have to implement this as an exception or we get errors
   about unimplemented virtual abstract method */
double ComponentRange::metric(const TimeSeries& d)
{
  throw SeisppError(string("ComponentRange:  coding error\n")
      + "Attempted to call on scaled TimeSeries object - nonsense for this metric");
}
double ComponentRange::metric(const ThreeComponentSeismogram& d)
{
  try{
    ThreeComponentSeismogram dwin(WindowData(d,this->window));
    double cl2nrm[3];
    int k;
    for(k=0;k<3;++k)
    {
      double *dptr=dwin.u.get_address(k,0);
      cl2nrm[k] = dnrm2(dwin.ns,dptr,3);
    }
    /* Convert the amplitudes carefully to log values.  log10 will return nan if
     * the value passed is less than or equal to zero - hence the <= condition.
     * When we find that condition immediately return a ratio bigger than the 
     * largest dynamic range of any seismic instrument today - 24 bits */
    const double MaxdBRatio(180.0);
    for(k=0;k<3;++k)
    {
        if(cl2nrm[k]<=0.0) 
            return MaxdBRatio;
        else
            cl2nrm[k]=log10(cl2nrm[k]);
    }
    /* Now find the max and min amplitudes and return the difference 
     * between them (multiply by 20 to get db)*/
    double minamp(cl2nrm[0]),maxamp(cl2nrm[0]);
    for(k=1;k<3;++k)
    {
        if(cl2nrm[k]>maxamp)
            maxamp=cl2nrm[k];
        if(cl2nrm[k]<minamp)
            minamp=cl2nrm[k];
    }
    double ratio=maxamp-minamp;   //division is - for logs
    return 20.0*ratio;
  }catch(...){throw;};
}
/* SNR metrics.  These are near copies of each other and
probably should have been done with templates. For intended
use, however, that would have created some other issues so
will have to maintain these absurdly parallel code sections. */

/* RMS version */
double RMS_SNR::metric(const TimeSeries& d)
{
  try{
    WindowRMS sig(this->get_SignalTwin());
    WindowRMS n(this->get_NoiseTwin());
    double srms,nrms;
    srms=sig.metric(d);
    nrms=n.metric(d);
    double snr;
    if(nrms<=0.0)
        snr=-1.0;
    else
        snr=srms/nrms;
    return(snr);
  }catch(...){throw;};
}
double RMS_SNR::metric(const ThreeComponentSeismogram& d)
{
  try{
    WindowRMS sig(this->get_SignalTwin());
    WindowRMS n(this->get_NoiseTwin());
    double srms,nrms;
    srms=sig.metric(d);
    nrms=n.metric(d);
    double snr;
    if(nrms<=0.0)
        snr=-1.0;
    else
        snr=srms/nrms;
    return(snr);
  }catch(...){throw;};
}
double Range_SNR::metric(const TimeSeries& d)
{
  try{
    WindowExtrema sig(this->get_SignalTwin());
    WindowExtrema n(this->get_NoiseTwin());
    double srms,nrms;
    srms=sig.metric(d);
    nrms=n.metric(d);
    double snr;
    if(nrms<=0.0)
        snr=-1.0;
    else
        snr=srms/nrms;
    return(snr);
  }catch(...){throw;};
}
double Range_SNR::metric(const ThreeComponentSeismogram& d)
{
  try{
    WindowExtrema sig(this->get_SignalTwin());
    WindowExtrema n(this->get_NoiseTwin());
    double srms,nrms;
    srms=sig.metric(d);
    nrms=n.metric(d);
    double snr;
    if(nrms<=0.0)
        snr=-1.0;
    else
        snr=srms/nrms;
    return(snr);
  }catch(...){throw;};
}
/* These are monotonously similar.  I am not certain they 
   are necessary, but better to be explicit*/
WindowRMS& WindowRMS::operator=(const WindowRMS& parent)
{
  if(&parent!=this)
    this->BasicWindowMetric::operator=(parent);
  return *this;
}
WindowExtrema& WindowExtrema::operator=(const WindowExtrema& parent)
{
  if(&parent!=this)
    this->BasicWindowMetric::operator=(parent);
  return *this;
}
ComponentRange& ComponentRange::operator=(const ComponentRange& parent)
{
  if(&parent!=this)
    this->BasicWindowMetric::operator=(parent);
  return *this;
}
/* These snr versions differ slightly but are identical except 
   for type names */
RMS_SNR& RMS_SNR::operator=(const RMS_SNR& parent)
{
  if(&parent!=this)
  {
    this->BasicWindowMetric::operator=(parent);
    noisewin=parent.noisewin;
  }
  return *this;
}
Range_SNR& Range_SNR::operator=(const Range_SNR& parent)
{
  if(&parent!=this)
  {
    this->BasicWindowMetric::operator=(parent);
    noisewin=parent.noisewin;
  }
  return *this;
}
}// End SEISPP namespace 
