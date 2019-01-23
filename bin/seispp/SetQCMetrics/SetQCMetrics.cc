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
#include "WindowMetric.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "SetQCMetrics [-pf pffile -t object_type -v --help -text] < in > out"
        <<endl
        << "Computes and sets one or more quality control (QC) metrics storing"
        <<endl
        << "results in metadata for seismogram.  Note the program currently "
        <<endl
        << "supports only metrics computed on a per station or component basis."
        <<endl
        << "What is computed an parameters for each metric are taken from a parameter file"
        <<endl
        << "Metrics computed:"<<endl
        << "Metadata_key   Description"<<endl
        << "RMSSNR     Signal to Noise Ratio computed from rms"<<endl
        << "RangeSNR   Signal to Noise Ratio computed by range (high-low) metric"
        << endl
        << "CompRatio  Ratio of largest component amplitude to minimum component"
        <<endl
        << " Use -pf to read from different pf file the default SetQCMetric.pf"
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
typedef list<BasicWindowMetric*> AllMetrics;
AllMetrics PfParseMetricDefinitions(PfStyleMetadata& control)
{
  try{
    AllMetrics result;
    PfStyleMetadata branch;
    branch=control.get_branch("ComponentRange");
    double ts,te;
    TimeWindow sigwin,noiswin;
    if(branch.get_bool("enable"))
    {
      ts=branch.get<double>("signal_window_start");
      te=branch.get<double>("signal_window_end");
      sigwin=TimeWindow(ts,te);
      ComponentRange *m=new ComponentRange(sigwin);
      result.push_back(dynamic_cast<BasicWindowMetric*>(m));
    }
    branch=control.get_branch("RMS_SNR");
    if(branch.get_bool("enable"))
    {
      ts=branch.get<double>("signal_window_start");
      te=branch.get<double>("signal_window_end");
      sigwin=TimeWindow(ts,te);
      ts=branch.get<double>("noise_window_start");
      te=branch.get<double>("noise_window_end");
      noiswin=TimeWindow(ts,te);
      RMS_SNR *m=new RMS_SNR(sigwin,noiswin);
      result.push_back(dynamic_cast<BasicWindowMetric*>(m));
    }
    branch=control.get_branch("Range_SNR");
    if(branch.get_bool("enable"))
    {
      ts=branch.get<double>("signal_window_start");
      te=branch.get<double>("signal_window_end");
      sigwin=TimeWindow(ts,te);
      ts=branch.get<double>("noise_window_start");
      te=branch.get<double>("noise_window_end");
      noiswin=TimeWindow(ts,te);
      Range_SNR *m=new Range_SNR(sigwin,noiswin);
      result.push_back(dynamic_cast<BasicWindowMetric*>(m));
    }
    return result;
  }catch(...){throw;}
}
template <typename Tdata> pair<int,int> process_single_object_file
     (PfStyleMetadata control, bool binary_data)
{
  /* First get the file handles for input and output*/
  try{
    AllMetrics m;
    m=PfParseMetricDefinitions(control);
    char form('t');
    if(binary_data) form='b';
    StreamObjectReader<Tdata> inp(form);
    StreamObjectWriter<Tdata> outp(form);
    int count(0),metrics_set(0);
    Tdata d;
    while(inp.good())
    {
      d=inp.read();
      AllMetrics::iterator mptr;
      BasicWindowMetric *mbase;
      if(d.live)
      {
        for(mptr=m.begin();mptr!=m.end();++mptr)
        {
          double val;
          string name;
          mbase=(*mptr);
          name=mbase->key();
          val=mbase->metric(d);
          d.put(name,val);
          ++metrics_set;
        }
      }
      /* Note we intentionally preserve data marked dead but do 
         not process them. This allows option to potentially revise
         an object later. */
      outp.write(d);
      ++count;
    }
    return std::pair<int,int>(count,metrics_set);
  }catch(...){throw;};
}
template <typename Tens,typename Tdata> pair<int,int> process_ensemble_file
     (PfStyleMetadata control, bool binary_data)
{
  /* First get the file handles for input and output*/
  try{
    AllMetrics m;
    m=PfParseMetricDefinitions(control);
    char form('t');
    if(binary_data) form='b';
    StreamObjectReader<Tens> inp(form);
    StreamObjectWriter<Tens> outp(form);
    int count(0),metrics_set(0);
    Tens d;
    while(inp.good())
    {
      d=inp.read();
      AllMetrics::iterator mptr;
      typename vector<Tdata>::iterator dptr;
      BasicWindowMetric *mbase;
      for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
      {
       if(dptr->live)
       {
        for(mptr=m.begin();mptr!=m.end();++mptr)
        {
          double val;
          string name;
          mbase=(*mptr);
          name=mbase->key();
          val=mbase->metric(*dptr);
          ++metrics_set;
        }
        ++count;
       }
      }
      outp.write(d);
    }
    return std::pair<int,int>(count,metrics_set);
  }catch(...){throw;};
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    string otype("ThreeComponentEnsemble");
    string pffile("SetQCMetrics.pf");
    bool binary_data(true);
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
        PfStyleMetadata control=pfread(pffile);
        pair<int,int> funcret;
        switch (dtype)
        {
            case TCS:
              funcret=process_single_object_file<ThreeComponentSeismogram>
                      (control,binary_data);
              break;
            case TCE:
              funcret=process_ensemble_file
                <ThreeComponentEnsemble,ThreeComponentSeismogram>
                (control,binary_data);
                break;
            case TS:
              funcret=process_single_object_file<TimeSeries>
                      (control,binary_data);
              break;
            case TSE:
              funcret=process_ensemble_file<TimeSeriesEnsemble,TimeSeries>
                      (control,binary_data);
              break;
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };

        cerr << "SetQCMetrics:  processed "<<funcret.first
          << " seismograms.  Number of metrics set="<<funcret.second
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
