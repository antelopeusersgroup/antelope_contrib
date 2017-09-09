#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "seispp.h"
#include "ensemble.h"
#include "SimpleWavelets.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "template < in > out [-v --help -text -pf pfile]"
        <<endl
        << "Convolve a wavelet defined by pffile with single input ThreeComponentEnsemble"<<endl
        << "Output is altered ThreeComponentEnsemble."
        << "Implicit assumption is input is an impulse response, but not required."<<endl
        << "e.g. wavelet could be any fir filter"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl
        << " -pf use alternative parameter file pffile"<<endl;
    exit(-1);
}
TimeSeries load_wavelet(Pf *pf)
{
  try{
    Metadata control(pf);
    int ns=control.get<int>("number_samples");
    double dt=control.get<double>("dt");
    string wavelet_type=control.get<string>("wavelet_type");
    TimeSeries d;
    if(wavelet_type=="gaussian")
    {
      double sigma=control.get<double>("sigma");
      d=gaussian_wavelet(ns,dt,sigma,NONE);
    }
    else if(wavelet_type=="ricker")
    {
      double nu=control.get<double>("nu");
      d=ricker_wavelet(ns,dt,nu,NONE);
    }
    else if(wavelet_type=="general")
    {
      Tbl *t;
      /* This may create a small memory leak but in the expected usa of
      this program that is a trivial concern */
      char *tblkey=strdup("wavelet_samples");
      t=pfget_tbl(pf,tblkey);
      ns=maxtbl(t);
      if(ns<=0)
      {
        cerr << "wavelet_samples Tbl in parameter file is empty"<<endl;
        usage();
      }
      d.s.reserve(ns);
      int i;
      for(i=0;i<ns;++i)
      {
        char *s;
        s=(char *)gettbl(t,i);
        double y=atof(s);
        d.s.push_back(y);
      }
      d.live=true;
      d.dt=dt;
      d.ns=ns;
      d.tref=relative;
    }
    else
    {
      cerr << "unrecognized name for wavelet_type"<<endl
        << "pf has wavelet_type="<<wavelet_type<<endl
        << "Must be either gaussian, ricker, or general"
        <<endl;
      usage();
    }
    return d;
  }catch(...){throw;};
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    bool binary_data(true);
    string pffile("convolve_wavelet");
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
            if(i>=argc)usage();
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
    Pf *pf;
    char *stmp;
    stmp=strdup(pffile.c_str());
    if(pfread(stmp,&pf))
    {
      cerr << "pfread failed on parameter file="<<pffile<<endl;
      usage();
    }
    try{
        TimeSeries wavelet=load_wavelet(pf);
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
        ThreeComponentEnsemble d(inp->read());
        vector<ThreeComponentSeismogram>::iterator dptr;
        for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
        {
          (*dptr)=sparse_convolve(wavelet,(*dptr));
        }
        out->write(d);
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
