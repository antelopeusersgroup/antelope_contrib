#ifndef _BASICSEISPLOT_H
#define _BASICSEISPLOT_H
#include "ensemble.h"
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
/*! Abstract base class for family of seismic plotting objects.

  This provides a pure virtual base for seismic plotting centered
  around the SEISPP library of data objects.  
  
  \author Gary L Pavlis
  */

class BasicSeisPlot
{
    public:
        /* Support 3c seismograms only when this is set true */
        bool ThreeComponentMode;  
        virtual void plot(TimeSeriesEnsemble& d,bool block)=0;
        virtual void plot(ThreeComponentEnsemble& d,bool block)=0;
        virtual void plot(TimeSeries& d,bool block)=0;
        virtual void plot(ThreeComponentSeismogram& d,bool block)=0;
};
}
#endif
