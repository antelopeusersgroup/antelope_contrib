#ifndef _WINDOWMETRIC_H_
#define _WINDOWMETRIC_H_
#include <vector>
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
#include "TimeWindow.h"
#include "ensemble.h"
namespace SEISPP
{
const string WindowRMS_key("WindowRMS");
const string WindowExtrema_key("WindowRange");
const string ComponentRange_key("CompRatio");
const string RMS_SNR_key("RMSSNR");
const string Range_SNR_key("RangeSNR");
/*! Object to compute some common time window based waveform metrics.

  In seismic processing a range of metrics are commonly computed to 
appraise data quality or test for failure in a processing algorithm.
The design here is to initialize the object with the time window for
which the metric is to be computed.   Methods define the metrics 
supported.  The overhead in creating one of these then is near 
zero.  All the effort happens on computation of the required metric. 
The BasicWindowMetric defines methods considered the lowest common
denominator.  More exotic methods should be added by adding children
through the standard inheritance methods.  The object uses templates
for maximum flexibility, but nothing is virtual because web research
suggest that virtual templates are not allowed in C++ (for good 
reasons it seems).   Hence, the base class here has no virtual 
methods. */
const double NullMetricValue(-99999.9);
class BasicWindowMetric
{
public:
  BasicWindowMetric(const TimeWindow tw) : window(tw){};
  BasicWindowMetric(const BasicWindowMetric& parent) : window(parent.window){};
  virtual double metric(const TimeSeries& d)=0;
  virtual double metric(const ThreeComponentSeismogram& d)=0;
  virtual string key()=0;
  void ChangeWindow(const TimeWindow tw){window=tw;};
  TimeWindow get_SignalTwin(){return window;};
  BasicWindowMetric& operator=(const BasicWindowMetric& parent);
protected:
  TimeWindow window;
};
class WindowRMS : public BasicWindowMetric
{
  public:
    WindowRMS(const TimeWindow tw) : BasicWindowMetric(tw)
    {
      name=WindowRMS_key;
    };
    WindowRMS(const WindowRMS& parent) : BasicWindowMetric(parent)
    {
      name=parent.name;
    };
    double metric(const TimeSeries& d);
    double metric(const ThreeComponentSeismogram& d);
    string key(){return name;};
    WindowRMS& operator=(const WindowRMS& parent);
  private:
    string name;
};
class WindowExtrema : public BasicWindowMetric
{
  public:
    WindowExtrema(const TimeWindow tw) : BasicWindowMetric(tw)
    {
      name=WindowExtrema_key;
    };
    WindowExtrema(const WindowExtrema& parent) : BasicWindowMetric(parent)
    {
      name=parent.name;
    };
    double metric(const TimeSeries& d);
    double metric(const ThreeComponentSeismogram& d);
    string key(){return name;};
    WindowExtrema& operator=(const WindowExtrema& parent);
  private:
    string name;
};
/*! Estimate maximum ratio of amplitudes for 3C data.

  This metric is useful for QC of three component data.  Sometimes
  sensors with independent components have one component that 
  quits functioning yielding either flat lines or weird bouncing
  between the rails.  Both produce large ratios between components
  that can be identified automatically with this metric.   The 
  warning is the very high snr polarized signals can also make
  this ratio large.  */
class ComponentRange : public BasicWindowMetric
{
  public:
    ComponentRange(const TimeWindow tw) : BasicWindowMetric(tw)
    {
      name=ComponentRange_key;
    };
    ComponentRange(const ComponentRange& parent) : BasicWindowMetric(parent)
    {
      name=parent.name;
    };
    double metric(const ThreeComponentSeismogram& d);
    /*! We have to implement this method to match interface, but if 
      used it will throw a SeisppError object immediately.  This metric
      makes no sense for scalar seismograms. */
    double metric(const TimeSeries& d);
    string key(){return name;};
    ComponentRange& operator=(const ComponentRange& parent);
  private:
    string name;
};
/*! \brief Compute rms based snr estimate. 

  Signal to noise ratio (snr) can be defined in a variety of ways.
This computes an rms based metric with the ratio of rms in a signal
and time window.   Note if the data have large dc offset this 
can be misleading so for best results assure the input is nearly zero mean.
*/
class RMS_SNR : public BasicWindowMetric
{
  public:
    RMS_SNR(const TimeWindow sigwin,const TimeWindow nwin) 
      : BasicWindowMetric(sigwin)
    {
      noisewin=nwin;
      name=RMS_SNR_key;;
    };
    RMS_SNR(const RMS_SNR& parent) : BasicWindowMetric(parent)
     {noisewin=parent.noisewin;name=parent.name;};
    double metric(const TimeSeries& d);
    double metric(const ThreeComponentSeismogram& d);
    string key(){return name;};
    TimeWindow get_NoiseTwin(){return noisewin;};
    RMS_SNR& operator=(const RMS_SNR& parent);
  private:
    TimeWindow noisewin;
    string name;
};
/*! \brief Compute absolute value range based snr.

An alternative way to compute snr is from a data range. That is, we 
compute the data range (high-low) in a signal and noise time window
and then compute snr as the ratio of the range metric in the signal
window divided by that in the noise window.  */
class Range_SNR : public BasicWindowMetric
{
  public:
    Range_SNR(const TimeWindow sigwin,const TimeWindow nwin) 
      : BasicWindowMetric(sigwin)
    {
      noisewin=nwin;
      name=Range_SNR_key;;
    };
    Range_SNR(const Range_SNR& parent) : BasicWindowMetric(parent)
     {noisewin=parent.noisewin; name=parent.name;};
    double metric(const TimeSeries& d);
    double metric(const ThreeComponentSeismogram& d);
    string key(){return name;};
    TimeWindow get_NoiseTwin(){return noisewin;};
    Range_SNR& operator=(const Range_SNR& parent);
  private:
    TimeWindow noisewin;
    string name;
};


template <typename Tens=ThreeComponentEnsemble,
   typename Tdata=ThreeComponentSeismogram,typename Metric=WindowRMS> 
   vector<double> ensemble_metrics(Tens& d,Metric metric_engine)
{
  try{
    vector<double> result;
    typename std::vector<Tdata>::iterator dptr;
    for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
    {
      if(dptr->live)
      {
        result.push_back(metric_engine(*dptr));
      }
      else
      {
        result.push_back(NullMetricValue);
      }
    }
    return result;
  }catch(...){throw;};
};
} // End SEISPP namespace declaration
#endif
