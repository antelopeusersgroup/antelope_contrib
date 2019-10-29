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
    cerr << "bundle n [-t objt -v --help -text]"
        <<endl
        << "assembles seismograms into fixed size blocks (bundlees) of size n"
        <<endl
        << " -t set seismogram object type: ThreeComponentSeismogram (default) or TimeSeries"
        <<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
enum AllowedObjects {TCS, TS};
AllowedObjects get_object_type(string otype)
{
    if(otype=="ThreeComponentSeismogram")
        return TCS;
    else if(otype=="TimeSeries")
        return TS;
    else
    {
        cerr << "Do not know how to handle object type="<<otype
            <<endl
            << "Must be either ThreeComponentSeismogram or TimeSeries"<<endl
            <<"Cannot continue"<<endl;
        exit(-1);
    }
}
template <typename Tdata, typename Tens> int bundle(int bundlesize,
                                               bool binary_data)
{
  try{
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<Tdata> inp(form);
        StreamObjectWriter<Tens>  outp(form);
        Tdata d;
        Tens e;
        int count(0),nb;
        while(inp.good())
        {
          e.member.clear();
          e.put("bundle_number",count);
          nb=0;
          for(nb=0;nb<bundlesize;++nb)
          {
            d=inp.read();
            e.member.push_back(d);
            if(inp.eof()) break;
          }
          outp.write(e);
          ++count;
        }
        return count;
  }catch(...){throw;};
}

bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    const int narg_required(1);
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    int bundlesize;
    string otype("ThreeComponentSeismogram");
    bundlesize=atoi(argv[1]);
    if(bundlesize<=0) usage();
    bool binary_data(true);

    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-text")
        {
            binary_data=false;
        }
        else if(sarg=="-t")
        {
          ++i;
          if(i>=argc) usage();
          otype=string(argv[i]);
        }
        else if(sarg=="-v")
          SEISPP_verbose=true;
        else
            usage();
    }
    try{
        AllowedObjects dtype=get_object_type(otype);
        int count;
        switch (dtype)
        {
            case TCS:
                count=bundle<ThreeComponentSeismogram,ThreeComponentEnsemble>
                  (bundlesize,binary_data);
                break;
            case TS:
                count=bundle<TimeSeries,TimeSeriesEnsemble>
                  (bundlesize,binary_data);
                break;
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };
        if(SEISPP_verbose) cerr<<"bundle:  wrote "<<count<<" ensembles"<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
