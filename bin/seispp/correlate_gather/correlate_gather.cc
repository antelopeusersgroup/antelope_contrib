#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "seispp.h"
#include "ensemble.h"
#include "PfStyleMetadata.h"
#include "SignalToNoise.h"
#include "MultichannelCorrelator.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "correlate_gather < in > out [-c n -k -s -v --help -text -pf pffile]"
        <<endl
        << "Aligns a ThreeComponentEnsemble using the robust multichannel "<<endl
        << "cross correlation method used in dbxcor (SEISPP::MultichannelCorrelator object)"<<endl
        << "Major outputs are posted in metadata for each seismogram."<<endl
        << "Note:  (1) input ensemble must be in relative time"<<endl
        << "       (2) reference trace is set as signal with max rms snr on all 3 components"<<endl
        << " -c n uses component n for correlation (n=0,1, or 2).  Default = 2(vertical)"<<endl
        << " -k when defined kill all data exceeding correlation or time shift cutoff values"<<endl
        << "    (Default retains all data just posting xcor and shift data to headers)"<<endl
        << " -s when defined shift traces by lag to align (default simply posts computed lag)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl
        << " -pf - use pffile instead of default corrrelation_gather"<<endl;
    exit(-1);
}
const string snrkey("SNR_rms");
/* Small helper to extract a time window from control pf using common
tags.   May want to move to libseispp at some point */
TimeWindow pfget_tw(PfStyleMetadata& pf,string key)
{
  try{
    PfStyleMetadata pfb=pf.get_branch(key);
    double ts=pfb.get<double>("start");
    double te=pfb.get<double>("end");
    return TimeWindow(ts,te);
  }catch(MetadataError& mde){throw mde;};
}
/* Return index position of signal with the largest computed RMS */
int FindMaxTraceSNR(ThreeComponentEnsemble& d)
{
  try{
    int result(0);
    int i;
    double maxsnr,dsnr;
    vector<ThreeComponentSeismogram>::iterator dptr;
    for(dptr=d.member.begin(),i=0;dptr!=d.member.end();++dptr,++i)
    {
      if(i==0)
      {
        maxsnr=dptr->get<double>(snrkey);
        result=0;
      }
      else
      {
        dsnr=dptr->get<double>(snrkey);
        if(dsnr>maxsnr)
        {
          result=i;
          maxsnr=dsnr;
        }
      }
    }
    return result;
  }catch(...){throw;};
}
/* Warning thi sone should match the one used in seispp_keywords.h   Didn't want
the baggage of that include here so did not explitly include that file */
const string ampkey("amplitude_static");
const string lagkey("lag_from_xcor");
const string wtkey("xcor_weight");
const string pxcorkey("peak_xcor");
void TransferMCAttributes(MultichannelCorrelator& mc,ThreeComponentEnsemble& d,
   double tcutoff, double xcor_cutoff,bool kill_data_beyond_cutoff)
{
  int i,nd;
  try{
    /* Trust all size match with data and vectors in mc object */
    nd=d.member.size();
    for(i=0;i<nd;++i)
    {
      if(kill_data_beyond_cutoff)
      {
        if( (mc.peakxcor[i]<xcor_cutoff) || (fabs(mc.lag[i])>tcutoff))
        {
          d.member[i].live=false;
        }
      }
      /* Note we always post the numbers even if we kill the data member */
      d.member[i].put(ampkey,mc.amplitude_static[i]);
      d.member[i].put(lagkey,mc.lag[i]);
      d.member[i].put(wtkey,mc.weight[i]);
      d.member[i].put(pxcorkey,mc.peakxcor[i]);
    }
  }catch(...){throw;};
}
void ApplyLags(ThreeComponentEnsemble& d)
{
  try{
    vector<ThreeComponentSeismogram>::iterator dptr;
    for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
    {
      double lag;
      if(dptr->live)
      {
        lag=dptr->get<double>(lagkey);
        /* This is how this should work, but current library throws
         * an error in this condition with active source data 
        dptr->shift(lag);
        --this is a brutal solution that permanently alters the time base--
        */
        dptr->t0 -= lag;
      }
    }
  }catch(...){throw;};
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    int comp(2);
    string pffile("correlate_gather.pf");
    bool kill_data_beyond_cutoff(false);
    bool timeshift_data(false);
    bool binary_data(true);
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-c")
        {
            ++i;
            if(i>=argc)usage();
            comp=atoi(argv[i]);
            if( (comp<0) || (comp>2))
            {
              cerr << "Illegal data component = "<<comp<<" given"<<endl
                << "Must be 0, 1, or 2"<<endl;
              usage();
            }
        }
        else if(sarg=="-pf")
        {
            ++i;
            if(i>=argc)usage();
            pffile=string(argv[i]);
        }
        else if(sarg=="-k")
          kill_data_beyond_cutoff=true;
        else if(sarg=="-s")
          timeshift_data=true;
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
        PfStyleMetadata control=pfread(pffile);
        TimeWindow bw=pfget_tw(control,"beam_window");
        TimeWindow rw=pfget_tw(control,"robust_window");
        /* Reference trace in this program is set by signal with largest
        rms signal to noise ratio using beam_window / noise_window data*/
        TimeWindow nw=pfget_tw(control,"noise_window");
        double tcutoff=control.get<double>("lag_cutoff");
        double xcor_cutoff=control.get<double>("correlation_cutoff");
        /* Frozen as robust method of dbxcor for now */
        SEISPP::CorrelationMethod cmeth(RobustStack);
        SEISPP::StackType stktype(RobustSNR);

        ThreeComponentEnsemble d;
        shared_ptr<TimeSeriesEnsemble> dts;
        int n(0);
        while(inp->good())
        {
            d=inp->read();
            ensemble_SNR_rms<ThreeComponentEnsemble,ThreeComponentSeismogram>(d,
                   bw,nw,snrkey);
            int reftrace=FindMaxTraceSNR(d);
            dts=ExtractComponent(d,comp);
            MultichannelCorrelator mc(*dts,cmeth,bw,rw,tcutoff,stktype,NULL,reftrace);
            /* mc contains several vectors of attributes we need to transfer to
            the ThreeComponentEnsemble data.  This routine does that assuming
            parallel vectors */
            TransferMCAttributes(mc,d,tcutoff, xcor_cutoff,kill_data_beyond_cutoff);
            /* This applies time shifts */
            if(timeshift_data) 
            {
                cerr << "correlate_gahter:   using -s option.  Warning - output data have t0 altered. "<<endl;
                ApplyLags(d);
            }
            out->write(d);
            ++n;
        }
        if(SEISPP_verbose) cerr << "correlate_gather:  number of ensembles processed ="<<n<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
