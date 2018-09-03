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
    cerr << "rtoa < in > out [-t objt -t0shift xxx --help -text -v]"
        <<endl
        << "Switches input data from absolute to a relative time standard"<<endl
        <<endl
        << "Use -t to change expected object type"<<endl
        << "(default ThreeComponentSeismogram.  Alteratives are TimeSeries and PMTimeSeries)"<<endl
        << " -t0shift - force shift of xxx to each start time"<<endl
        << "  When applied sets attribute rtoa_t0shift to this value in output"
        <<endl
        << " (This option is useful to force shot data to look like absolute time)"
        <<endl
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
        cerr << "rtoa: cannot handle object type="<<otype
            <<endl<< "Cannot continue"<<endl;
        exit(-1);
    }
}
/* Returns number of objects processed.   Read from stdin and write
 * to stdout defined object type.   */
template <typename T> int rtoa(bool force_shift, double t0shift, bool binary_data)
{
  try{
    const string t0shiftkey("rtoa_t0shift");
    char form('t');
    if(binary_data)form='b';
    StreamObjectReader<T> inp(form);
    StreamObjectWriter<T> outp(form);
    int dcount(0);
    while(inp.good())
    {
      T d=inp.read();
      if(force_shift)
      {
          d.rtoa(t0shift);
          d.put(t0shiftkey,t0shift);
      }
      else
          d.rtoa();
      outp.write(d);
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
    bool force_shift(false);
    double t0shift(0.0);

    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-t0shift")
        {
            ++i;
            if(i>=argc) usage();
            force_shift=true;
            t0shift=atof(argv[i]);
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
        else
            usage();
    }
    cerr << "rtoa (WARNING):  t0 of output changed by "<<t0shift
        << " seconds"<<endl
        << "This mode is intended only for fake time shifts applied to active source data"
        <<endl;
    try{
      int nprocessed;
      switch(objt)
      {
      case PMTS:
        nprocessed=rtoa<PMTimeSeries>(force_shift,t0shift,binary_data);
        break;
      case TCS:
        nprocessed=rtoa<ThreeComponentSeismogram>(force_shift,t0shift,binary_data);
        break;
      case TS:
      default:
        nprocessed=rtoa<TimeSeries>(force_shift,t0shift,binary_data);
      };
      if(SEISPP_verbose) cerr << "rtoa:  processed "<<nprocessed
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
