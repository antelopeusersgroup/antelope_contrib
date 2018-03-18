#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "ThreeComponentSeismogram.h"
#include "PfStyleMetadata.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "window_streamfile tmin tmax [-t objt -cutgaps -v --help --text]"
        <<endl
        << "Cuts any of a set of time series object to specified time window"<<endl
        << "Window is defined by tmin and tmax range (saves tmin<=t<=tmax)"<<endl
        << "Note the times must be in a relative time standard.   If any data read"<<endl
        << "have an absolute time standard instead of relative time they will be left unaltered."<<endl
        << "A warning will be posted to stderr for all such occurences"<<endl
        <<endl
        << " Use -t to select object type expected for input. "<<endl
        << " (Allowed options=ThreeComponentEnsemble (default),ThreeComponentSeismogram, TimeSeries, and TimeSeriesEnsemble)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
/* This procedure parses an input string (normally from argv)
 * to set a list of allowed objects.   This could be a library
 * procedure, but with this one can customize the set of objects
 * supported. */
enum AllowedObjects {TCS, TCE,  TS, TSE};
AllowedObjects get_object_type(string otype)
{
    if(otype=="ThreeComponentSeismogram")
        return TCS;
    else if(otype=="ThreeComponentEnsemble")
        return TCE;
    else if(otype=="TimeSeries")
        return TS;
    else if(otype=="TimeSeriesEnsemble")
        return TSE;
    else
    {
        cerr << "Do not know how to handle object type="<<otype
            <<endl<< "Cannot continue"<<endl;
        exit(-1);
    }
}
template <typename DataType> int window_objects(TimeWindow cutwin, bool binary_data)
{
    try{
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<DataType> inp(form);
        StreamObjectWriter<DataType>  outp(form);
        int count;
        DataType d;
        while(inp.good())
        {
            d=inp.read();
            DataType dcut;
            if(d.live)
            {
              if(d.tref==relative)
              {
                d=WindowData(d,cutwin);
              }
              else
              {
                cerr << "Warning:  file object number "<<count
                   << " is using absolute time - copied without change"<<endl;
              }
            }
            outp.write(d);
            ++count;
        }
        return count;
    }catch(...){throw;};
}
template <typename DataType, typename MemberType>
   int window_ensembles(TimeWindow cutwin, bool binary_data)
{
    try{
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<DataType> inp(form);
        StreamObjectWriter<DataType>  outp(form);
        int count;
        DataType d;
        while(inp.good())
        {
            d=inp.read();
            int i;
            for(i=0;i<d.member.size();++i)
            {
              MemberType dcut;
              if(d.member[i].live)
              {
                if(d.member[i].tref==relative)
                {
                  dcut=WindowData(d.member[i],cutwin);
                  d.member[i]=dcut;
                }
                else
                {
                  cerr << "Warning:  member number "<<i<<" of file of ensembles with object number "<<count
                     << " is using absolute time - copied without change"<<endl;
                }
              }

            }
            outp.write(d);
            ++count;
        }
        return count;
    }catch(...){throw;};
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    const int narg_required(2);
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    string otype("ThreeComponentSeismogram");
    bool binary_data(true);
    double ts,te;
    ts=atof(argv[1]);
    te=atof(argv[2]);
    TimeWindow cutwin(ts,te);

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
        else if(sarg=="-v")
          SEISPP_verbose=true;
        else if(sarg=="-t")
        {
            ++i;
            if(i>=argc)usage();
            otype=string(argv[i]);
        }
        else
            usage();
    }
    try{
        /* This approach depends upon the use of a template as
         * a generic method to implement the algorithm being
         * implemented.   This example does nothing but copy
         * input to output but provides a starting point for
         * algorithms that can be done on multiple object types. */
        AllowedObjects dtype=get_object_type(otype);
        int count;
        switch (dtype)
        {
            case TCS:
                count=window_objects<ThreeComponentSeismogram>(cutwin,binary_data);
                break;
            case TCE:
                count=window_ensembles<ThreeComponentEnsemble,ThreeComponentSeismogram>
                    (cutwin,binary_data);
                break;
            case TS:
                count=window_objects<TimeSeries>(cutwin,binary_data);
                break;
            case TSE:
                count=window_ensembles<TimeSeriesEnsemble,TimeSeries>
                   (cutwin,binary_data);
                break;
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };
        cerr << "window_ensembles:  copied "<<count<<" objects from stdin to stdout"
            <<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
