#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "ThreeComponentSeismogram.h"
#include "PfStyleMetadata.h"
#include "WindowMetric.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "rms_scaling < in > out [-win xx yy -akey name -rmskey name -mdonly -t object_type -v --help -text]"
        <<endl
        << "Computes a scales data by rms amplitude (optionally only compute rms attribute"
        <<endl
        << " Use -win to change the time window for rms calculation to xx<=t<=yy"<<endl
        << " (default is the full seismogram)"<<endl
        << " Use -akey to set the key used to define the zero time of the rms window"
        <<endl
        << " (default is none, which assumes relative time.  Normal use of this"
        << endl<<" option is for absolute time data with arrival.time defining 0"
        <<endl
        << " Use -rmskey to set attribute key saved with the output data"<<endl
        << " (default is rms_amplitude"<<endl
        << " Use the -mdonly flag to only compute the rms amplitude"<<endl
        << " (default will change the amplitude of all output to have unit rms amplitude)"
        <<endl
        << " Use -t to select object type expected for input. "<<endl
        << " (Allowed options=ThreeComponentEnsemble (default),ThreeComponentSeismogram, TimeSeries, and TimeSeriesEnsemble)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
enum AllowedObjects {TCS, TCE,  TS, TSE, PMTS};
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
    else if(otype=="PMTimeSeries")
    {
        cerr << "PMTimeSeries not yet supported for this program."<<endl;
        usage();
    }
    else
    {
        cerr << "Do not know how to handle object type="<<otype
            <<endl<< "Cannot continue"<<endl;
        exit(-1);
    }
}
/* Helper to template procedures below.   Apply to member data of
 * an ensemble or directly to single seismogram objects.   We fetch 
 * the time range of the data and return a truncated time window if
 * the range of the data are smaller than the requested time window.
 * This is mostly a convenient way to allow a full seismogram rms 
 * measurement. */
TimeWindow DetermineTimeWindow(const BasicTimeSeries& d, const TimeWindow tw0, 
        const double toffset) noexcept
{
    TimeWindow tw(tw0);
    if(toffset!=0.0)
    {
        tw.start+=tw0.start;
        tw.end+=tw0.end;
    }
    double dts,dte;
    dts=d.t0;
    dte=d.endtime();
    if(tw.start<dts) tw.start=dts;
    if(tw.end>dte) tw.end=dte;
    return tw;
}
/* Processing procedure for ensembles.   EnsDtype is the type of the
 * ensemble object and MemDtype is the type of the objects in the member
 * container.   
 *
 * args:
 * tw - Relative time window where rms is to be computed.   Make tw larger
 *    than expected data size to use the full seismogram.
 * trefkey - is a metadata key used to establish an offset in time reference.  A typical
 *    example would be something like "arrival.time" to use a window 
 *    relative to an arrival time.  
 * rmskey - is the key used to store the computed rms amplitude.
 * binary_data - when true expect to read and write data in binary format.  If
 *    false assume the input and output are boost text serialization stream.
 * scale_data - if true the data will be scaled by 1/rms_amplitude so all 
 *    output will have unit rms amplitude in specified window. 
 *
 *  Returns number of ensembles processed.
 *    */
template <typename EnsDType,typename MemDtype> 
    int ensemble_rms_scaling(const TimeWindow tw,
           const string trefkey, const string rmskey, 
           const bool binary_data, const bool scale_data)
{
    try{
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<EnsDType> inp(form);
        StreamObjectWriter<EnsDType>  outp(form);
        EnsDType d;
        typename vector<MemDtype>::iterator dptr;
        int nensemble(0);
        while(inp.good())
        {
            int i;
            d=inp.read();
            ++nensemble;
            for(dptr=d.member.begin(),i=0;dptr!=d.member.end();++dptr,++i)
            {
              if(dptr->live)
              {
                double twref_to_use(0.0);
                double toffset;
                if(trefkey!="none")
                {
                    try{
                        toffset=dptr->get_double(trefkey);
                        twref_to_use+=toffset;
                    }catch(SeisppError& serr)
                    {
                        cerr << "Failure fething time offset with key="
                            <<trefkey<<endl
                            << "Cannot process member number "<<i
                            << " of ensemble number "<<nensemble<<endl;
                        continue;
                    }
                }
                /* This procedure guaranteeds tw2use is valid */
                TimeWindow tw2use=DetermineTimeWindow(*dptr,tw,twref_to_use);
                WindowRMS engine(tw2use);
                double amp=engine.metric(*dptr);
                dptr->put(rmskey,amp);
                /* Weird syntax requird by API for ScaleMember wanting
                 * a raw pointer */
                if(scale_data) ScaleMember(&(*dptr),1.0/amp);
              }
            }
            outp.write(d);
            ++nensemble;
        }
        return nensemble;
    }catch(...){throw;};
}
/* Comparable to ensemble processing version above but for single
 * objects - arguments are identical in concept. Return is 
 * number of seismograms processed not ensembles */
template <typename DataType> 
  int rms_scaling(const TimeWindow tw, 
           const string trefkey, const string rmskey,
           const bool binary_data, const bool scale_data)
{
    try{
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<DataType> inp(form);
        StreamObjectWriter<DataType>  outp(form);
        DataType d;
        int nobjects(0);
        while(inp.good())
        {
            d=inp.read();
              if(d.live)
              {
                double twref_to_use(0.0);
                double toffset;
                if(trefkey!="none")
                {
                    try{
                        toffset=d.get_double(trefkey);
                        twref_to_use+=toffset;
                    }catch(SeisppError& serr)
                    {
                        cerr << "Failure fething time offset with key="
                            <<trefkey<<endl
                            << "Cannot process object number "<<nobjects
                            <<endl;
                        continue;
                    }
                }
                /* This procedure guaranteeds tw2use is valid */
                TimeWindow tw2use=DetermineTimeWindow(d,tw,twref_to_use);
                WindowRMS engine(tw2use);
                double amp=engine.metric(d);
                d.put(rmskey,amp);
                if(scale_data) ScaleMember(&d,1/amp);
              }
            outp.write(d);
            ++nobjects;
        }
        return nobjects;
    }catch(...){throw;};
}

bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    const int narg_required(0);
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    bool binary_data(true);
    bool mdonly(false);
    string otype("ThreeComponentSeismogram");
    string alignkey("none");
    string rmskey("rms_amplitude");
    TimeWindow rmswin(-10000.0,1.0e14);

    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-win")
        {
            ++i;
            if(i>=argc)usage();
            rmswin.start=atof(argv[i]);
            ++i;
            if(i>=argc)usage();
            rmswin.end=atof(argv[i]);
        }
        else if(sarg=="-akey")
        {
            ++i;
            if(i>=argc)usage();
            alignkey=string(argv[i]);
        }
        else if(sarg=="-rmskey")
        {
            ++i;
            if(i>=argc)usage();
            rmskey=string(argv[i]);
        }
        else if(sarg=="-mdonly")
        {
            mdonly=true;
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
        AllowedObjects dtype=get_object_type(otype);
        int count;
        switch (dtype)
        {
            case TCS:
                count=rms_scaling<ThreeComponentSeismogram>(rmswin,
                        alignkey,rmskey,binary_data,mdonly);
                break;
            case TCE:
                count=ensemble_rms_scaling<ThreeComponentEnsemble,ThreeComponentSeismogram>
                        (rmswin,alignkey,rmskey,binary_data,mdonly);
                break;
            case TS:
                count=rms_scaling<TimeSeries>(rmswin,
                        alignkey,rmskey,binary_data,mdonly);
                break;
            case TSE:
                count=ensemble_rms_scaling<TimeSeriesEnsemble,TimeSeries>
                        (rmswin,alignkey,rmskey,binary_data,mdonly);
                break;
            case PMTS:
                    cerr << "PMTimeSeries not supported - exiting"<<endl;
                    usage();
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };
        if(SEISPP_verbose)
            cerr << "rms_scaling:  processed "<<count<<" data objects"<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
