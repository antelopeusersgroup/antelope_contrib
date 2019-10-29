#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "ensemble.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "ator < in > out [-key key -t objt --help -text -v]"
        <<endl
        << "Switches input data from absolute to a relative time standard"<<endl
        << "Use -key to change attribute used to define time shift (default arrival.time)"
        <<endl
        << "Use -t to change expected object type"<<endl
        << "(default ThreeComponentSeismogram.  Alteratives are TimeSeries and PMTimeSeries)"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"
        << " -v - be more verbose"<<endl
        <<endl;
    exit(-1);
}
enum AllowedObjects {TCS, PMTS, TS};
AllowedObjects get_object_type(string otype)
{
    if(otype=="ThreeComponentSeismogram")
        return TCS;
    else if(otype=="TimeSeries")
        return TS;
    else if(otype=="PMTimeSeries")
        return PMTS;
    else
    {
        cerr << "ator: cannot handle object type="<<otype
            <<endl<< "Cannot continue"<<endl;
        exit(-1);
    }
}
/* Returns number of objects processed.   Read from stdin and write
 * to stdout defined object type.   Use key to define shift */
template <typename T> int ator(string key,bool binary_data)
{
  try{
    char form('t');
    if(binary_data)form='b';
    StreamObjectReader<T> inp(form);
    StreamObjectWriter<T> outp(form);
    int dcount(0);
    while(inp.good())
    {
      T d=inp.read();
      /* Skip and write an error message for any seismogram
      for which the key is not defined */
      double t;
      try{
        t=d.get_double(key);
        d.ator(t);
        outp.write(d);
      }catch(...)
      {
        cerr << "ator:  Problems processing seismogram number "<<dcount
          << " while fetching time with key="<<key<<endl
          << "Data for this seismogram will be skipped"<<endl;
      }
      ++dcount;
    }
    return dcount;
  }catch(...){throw;};
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    AllowedObjects objt(TCS);
    if(argc<=1) usage();
    if(string(argv[1])=="--help") usage();
    bool binary_data(true);
    string key("arrival.time");

    for(i=1;i<argc;++i)
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
        else if(sarg=="-v")
        {
            SEISPP_verbose=true;
        }
        else if(sarg=="-t")
        {
            ++i;
            if(i>=argc) usage();
            objt=get_object_type(string(argv[i]));
        }
        else if(sarg=="-key")
        {
            ++i;
            if(i>=argc) usage();
            key=string(argv[i]);
        }
        else
            usage();
    }
    try{
      int nprocessed;
      switch(objt)
      {
      case PMTS:
        nprocessed=ator<PMTimeSeries>(key,binary_data);
        break;
      case TCS:
        nprocessed=ator<ThreeComponentSeismogram>(key,binary_data);
        break;
      case TS:
      default:
        nprocessed=ator<TimeSeries>(key,binary_data);
      };
      if(SEISPP_verbose) cerr << "ator:  processed "<<nprocessed
          << " objects from stdin"<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
