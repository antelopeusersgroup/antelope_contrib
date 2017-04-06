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
void ThreeCPlot::plot(ThreeComponentEnsemble& d,bool block)
{
  try{
    int k;
    TimeSeriesEnsemble dtmp[3];
    for(k=0;k<3;++k)
    {
      auto_ptr<TimeSeriesEnsemble> ptr;
      ptr=ExtractComponent(d,k);
      /*SeismicPlot copies the data so we don't bother to make a copy here
      and just recycle ptr for each component */
      switch(k)
      {
        case 0:
          comp0.plot(*ptr,false);
          break;
        case 1:
          comp1.plot(*ptr,false);
          break;
        case 2:
          comp2.plot(*ptr,block);
      };
    }
  }catch(...){throw;};
}
void ThreeCPlot::plot(ThreeComponentSeismogram& d1,bool block)
{
  try{
    ThreeComponentEnsemble d;
    d.member.push_back(d1);
    this->plot(d,block);
  }catch(...){throw;};
}
set<int> ThreeCPlot::report_kills()
{
    try{
        set<int> allkills,compkills;
        set<int>::iterator kptr;
        allkills=comp0.report_kills();
        compkills=comp1.report_kills();
        for(kptr=compkills.begin();kptr!=compkills.end();++kptr)
        {
            allkills.insert(*kptr);
        }
        compkills=comp2.report_kills();
        for(kptr=compkills.begin();kptr!=compkills.end();++kptr)
        {
            allkills.insert(*kptr);
        }
        return allkills;
    }catch(...){throw;};
}
} // End SEISPP namespace encapsulation
