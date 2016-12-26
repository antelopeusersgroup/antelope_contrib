#include "ThreeCPlot.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP
{
ThreeCPlot::ThreeCPlot() : comp0(),comp1(),comp2(){};
ThreeCPlot::ThreeCPlot(Metadata& md) : comp0(md),comp1(md),comp2(md)
{
  try{
    int k;
    char title[50];
    for(k=0;k<3;++k)
    {
      sprintf(title,"ThreeCPlot:  component %d",k);
      switch(k)
      {
          case 0:
              comp0.put("title",title);
              break;
          case 1:
              comp1.put("title",title);
              break;
          case 2:
              comp2.put("title",title);
      };
    }
  }catch(...){throw;};
}
void ThreeCPlot::plot(ThreeComponentEnsemble& d)
{
  try{
    int k;
    TimeSeriesEnsemble dtmp[3];
    for(k=0;k<3;++k)
    {
      auto_ptr<TimeSeriesEnsemble> ptr;
      ptr=ExtractComponent(d,k);
      /* Evil interface collision - this seems necessary to avoid mysterious
       * errors when the widget plots this */
      dtmp[k]=TimeSeriesEnsemble(*ptr);
      /*SeismicPlot copies the data so we don't bother to make a copy here
      and just recycle ptr for each component */
      switch(k)
      {
        case 0:
          comp0.plot(dtmp[0],true);
          break;
        case 1:
          comp1.plot(dtmp[1],true);
          break;
        case 2:
          comp2.plot(dtmp[2],true);
      };
    }
  }catch(...){throw;};
}
void ThreeCPlot::plot(ThreeComponentSeismogram& d1)
{
  try{
    ThreeComponentEnsemble d;
    d.member.push_back(d1);
    this->plot(d);
  }catch(...){throw;};
}
} // End SEISPP namespace encapsulation
