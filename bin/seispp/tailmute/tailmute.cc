#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "seispp.h"
#include "ensemble.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;   
using namespace SEISPP;
void usage()
{
    cerr << "tailmute < in > out [-pf pffile -v --help -text]"
        <<endl
        << "Applies a tail mute (end of data segment) to ThreeComponentEnsembles"
        <<endl
        << "Behavior is controled by pffile"<<endl
        << " -pf - use pffile instead of default tailmute.pf"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
/* This object's apply method does the work of this program.  It
   currently supports only a linear ramp from 1 to 0 defined by
   two end points t1 and t0 respectively. */
class TailMute
{
  public:
    double t1; // Start of mute (where value is 1)
    double t0;  // time when mute is zero
    TailMute(PfStyleMetadata& pf);
    /* Apply the mute to a seismogram.   Returns number of samples changes*/
    int apply(ThreeComponentSeismogram& d);
  private:
    double dwdt;  //change in weight with time used to compute ramp mute
};
TailMute::TailMute(PfStyleMetadata& pf)
{
  try{
    t0=pf.get<double>("t0");
    t1=pf.get<double>("t1");
    if(t1>=t0)
    {
      stringstream serr;
      serr<<"TailMute constructor:  illegal parameters for mute definition"
        <<endl<<"Time of start (t1) ="<<t1<<endl
        <<"Time of mute end (t0) where data after that time are zeroed="<<t0
        <<endl
        << "t0 must be greater than t1"<<endl;
      throw SeisppError(serr.str());
    }
    dwdt = 1.0/(t0-t1);
  }catch(...){throw;};
}
int TailMute::apply(ThreeComponentSeismogram& d)
{
  int i,k;
  int n(0);
  double t;
  double te=d.endtime();
  for(t=t1+d.dt,i=d.sample_number(t+d.dt)+1;t<te&&i<d.ns;t+=d.dt,++i)
  {
    double w;
    if(t>=t0)
        w=0.0;
    else
        w=1.0-(t-t1)*dwdt;
    for(k=0;k<3;++k) d.u(k,i)*=w;
    ++n;
  }
  for(;i<d.ns;++i)
  {
    for(k=0;k<3;++k) d.u(k,i)=0.0;
    ++n;
  }
  return n;
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    bool binary_data(true);
    string pffile("tailmute.pf");
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-pf")
        {
          ++i;
          if(i>=argc) usage();
          pffile=string(argv[i]);
        }
        else if(sarg=="-text")
        {
            binary_data=false;
        }
        else if(sarg=="-v")
          SEISPP_verbose=true;
        else
            usage();
    }
    try{
        PfStyleMetadata control(pffile);
        TailMute mute(control);
        shared_ptr<StreamObjectReader<ThreeComponentEnsemble>> inp;
        if(binary_data)
        {
          inp=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>('b'));
        }
        else
        {
          inp=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>);
        }
        shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>> out;
        if(binary_data)
        {
          out=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>('b'));
        }
        else
        {
          out=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>);
        }
        ThreeComponentEnsemble d;
        int n(0);
        while(inp->good())
        {
            d=inp->read();
            int nchanged;
            for(i=0;i<d.member.size();++i)
            {
              nchanged=mute.apply(d.member[i]);
              if(SEISPP_verbose) cerr << "TailMute:  ensemble "<<n
                <<" member="<<i
               <<" mute altered "<<nchanged<<" vector samples"<<endl;
            }
            out->write(d);
            ++n;
        }
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}

