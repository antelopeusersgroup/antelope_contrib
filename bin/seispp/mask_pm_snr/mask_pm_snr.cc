#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "mask_pm_snr [-v --help -text -pffile] < in > out"
        <<endl
        << "Sets low snr sections of a PMTimeSeries data as marked gap."<<endl
        << "This be useful to mask unreliable data sections"<<endl
        << "Noise amplitude computed from specified noise window is posted to header"<<endl
        << "Noise is computed from median of major axis amplitudes in PMTimeSeries"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl
        << "-pf - use pffile instead of default mask_pm_snr.pf"<<endl;
    exit(-1);
}
double median_noise(PMTimeSeries& d,TimeWindow nw)
{
  try{
    int is,ie;   // start and end sample numbers of this window
    is=d.sample_number(nw.start);
    ie=d.sample_number(nw.end);
    if(ie<0)
    {
        cerr << "Warning:  noise window range is before data start time"<<endl
          << "Requested noise time window end time="<<nw.end<<endl
          << "Processing PMTimeSeries object with endtime="<<d.endtime()<<endl
          << "Setting noise estimate to -1.0 and attempting to continue"<<endl;
        return(-1.0);
    }
    if(is<0)
    {
      cerr << "Warning:  reset noise window start time to "<<d.time(0)<<endl;
      is=0;
    }
    if(ie>=d.ns)
    {
      cerr << "Warning:  noise window end time exceeded data length"<<endl
        << "Requested noise time window end time="<<nw.end<<endl
        << "Processing PMTimeSeries object with endtime="<<d.endtime()<<endl
        << "Setting noise estimate to -1.0 and attempting to continue"<<endl;
      return(-1.0);
    }
    vector<double> amps;
    amps.reserve(ie-is+1);
    ParticleMotionEllipse pme;
    int i;
    for(i=is;i<=ie;++i)
    {
      pme=d.ellipse(i);
      amps.push_back(pme.majornrm);
    }
    double medamp;
    medamp=SEISPP::median<double>(amps);
    return(medamp);
  }catch(...){throw;};
}
void set_data_gaps(PMTimeSeries& d, double snr_floor, TimeWindow nw, int minsamp)
{
  try{
    int is=d.sample_number(nw.end);
    //silently do nothing if is is larger than number of samples
    if(is>=d.ns)return;
    TimeWindow gw;
    /* This may not be necessary  but better to always initialize */
    gw=TimeWindow(d.time(is),d.time(is+1));
    bool in_low_snr_section(false);
    int i;
    int nsamp_this_gap(0);
    for(i=is;i<d.ns;++i)
    {
      ParticleMotionEllipse pme=d.ellipse(i);
      if(pme.majornrm < snr_floor)
      {
        if(in_low_snr_section)
        {
          ++nsamp_this_gap;
          continue;
        }
        else
        {
          /* land here when we enter a low snr section */
          gw.start=d.time(i);
          /* We have to handle the (common) case of low snr at 
           * sample is.   Reason is that earlier code forces a 
           * gap in the noise window. To keep a low signal 
           * section immediately after the noise window (the 
           * usual situation) we fudge the start time by 1/2 
           * sample interval */
          if(i==is) gw.start+= (d.dt/2.0);
          in_low_snr_section=true;
        }
      }
      else
      {
        if(in_low_snr_section)
        {
          if(nsamp_this_gap<minsamp)
          {
            continue;
          }
          else
          {
            /* Land here when a low snr section ends and we exceed the
            gap threshold */
            gw.end=d.time(i);
            d.add_gap(gw);
            in_low_snr_section=false;
          }
        }
        /* The else condition for this situation would be here.
        Do nothing in that case as it means we are in a good data section
        and just skipping to look or the next low snr section */
      }
    }
    /* Handle low snr at the end of the data section */
    if(in_low_snr_section)
    {
      gw.end=d.endtime()+1.0;   // Intentionall pad if a gap is at end
      d.add_gap(gw);
    }
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
    string pffile("mask_pm_snr");
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
        else if(sarg=="-pf")
        {
            ++i;
            if(i>=argc)usage();
            pffile=string(argv[i]);
        }
        else
            usage();
    }
    Pf *pf;
    char *pff=strdup(pffile.c_str());
    if(pfread(pff,&pf))
    {
      cerr << "pfread failed on file "<<pffile<<endl;
      usage();
    }
    free(pff);
    try{
        Metadata control(pf);
        double nwstart,nwend;
        nwstart=control.get<double>("noise_window_start_time");
        nwend=control.get<double>("noise_window_end_time");
        TimeWindow nwin(nwstart,nwend);
        bool force_pmws=control.get_bool("force_mask_time_before_noise_window");
        double snr_floor=control.get<double>("snr_floor");
        string snrkey=control.get_string("snr_key");
        int minsamp=control.get<int>("minimum_samples_per_gap");
        shared_ptr<StreamObjectReader<PMTimeSeries>> inp;
        if(binary_data)
        {
          inp=shared_ptr<StreamObjectReader<PMTimeSeries>>
             (new StreamObjectReader<PMTimeSeries>('b'));
        }
        else
        {
          inp=shared_ptr<StreamObjectReader<PMTimeSeries>>
             (new StreamObjectReader<PMTimeSeries>);
        }
        shared_ptr<StreamObjectWriter<PMTimeSeries>> out;
        if(binary_data)
        {
          out=shared_ptr<StreamObjectWriter<PMTimeSeries>>
             (new StreamObjectWriter<PMTimeSeries>('b'));
        }
        else
        {
          out=shared_ptr<StreamObjectWriter<PMTimeSeries>>
             (new StreamObjectWriter<PMTimeSeries>);
        }
        PMTimeSeries d;
        int count(0);
        while(inp->good())
        {
            d=inp->read();
            ++count;
            if(d.live)
            {
              if((d.tref)!=relative)
              {
                cerr << argv[0]<<" (Fatal Error):  PMTimeSeries objects must be in relative time"<<endl
                  << "Implicit assumption is that 0 is some phase arrival time"<<endl
                  << "Problem found on object number "<<count<<" of input data set"<<endl;
                usage();
              }
              double noise_level;
              try{
                noise_level=median_noise(d,nwin);
                d.put(snrkey,noise_level);
              }catch(SeisppError& serr)
              {
                cerr << "Problem handling PMTimeSeries number "<<count<<endl;
                serr.log_error();
                cerr << "Passing this data along with no marked gap and noise level set 0"
                  <<endl;
                d.put(snrkey,0.0);
                continue; //this should transfer control outside the if live block
              }
              /* This is used by median_noise to signal a noise window larger
              than the data window.  In this case we want to do nothing. */
              if(noise_level<0.0) continue;
              /* the noise window is always added as a gap.   Expanded with
              force_pmws option */
              if(force_pmws)
              {
                TimeWindow startwin(d.t0,nwin.end);
                d.add_gap(startwin);
              }
              else
              {
                d.add_gap(nwin);
              }
              set_data_gaps(d,snr_floor,nwin,minsamp);
            }
            /* Perhaps should automatically drop data not marked live
            but for now copy such data */
            out->write(d);
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
