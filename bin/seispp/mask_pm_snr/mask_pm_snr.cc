//#include <stdlib.h>
//#include <stdio.h>
#include <string>
#include <iostream>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include <memory>
#include <float.h>
#include <algorithm>
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
        << "Options and processing parameters are all defined in pf file"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl
        << "-pf - use pffile instead of default mask_pm_snr.pf"<<endl;
    exit(-1);
}
double median_noise(PMTimeSeries& d,TimeWindow nw, int min_noise_samples)
{
  try{
    int is,ie;   // start and end sample numbers of this window
    is=d.sample_number(nw.start);
    ie=d.sample_number(nw.end);
    if(ie<0)
    {
      if(SEISPP_verbose)
        cerr << "Warning (mask_pm_snr):  noise window range is before data start time"<<endl
          << "Requested noise time window end time="<<nw.end<<endl
          << "Processing PMTimeSeries object with endtime="<<d.endtime()<<endl
          << "Setting noise estimate to -1.0 and attempting to continue"<<endl;
        return(-1.0);
    }
    if(is<0)
    {
      if(SEISPP_verbose)
        cerr << "Warning (mask_pm_snr):  reset noise window start time to "<<d.time(0)<<endl;
      is=0;
    }
    if(ie>=d.ns)
    {
      if(SEISPP_verbose)
       cerr << "Warning (mask_pm_snr):  noise window end time exceeded data length"<<endl
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
      if(d.is_gap(i))continue;
      pme=d.ellipse(i);
      amps.push_back(pme.majornrm);
    }
    if(amps.size()<min_noise_samples)
    {
      if(SEISPP_verbose)
        cerr << "Warning (mask_pm_snr):  marked gaps in noise window"<<endl
          << "Found only "<<amps.size()<<" valid samples"<<endl;
      return(-1.0);
    }
    double medamp;
    medamp=SEISPP::median<double>(amps);
    return(medamp);
  }catch(...){throw;};
}
/* This routine hunts for sections of data that are hard zeros not already
marked as gaps.   Returns number of gaps intervals added.   If negative
the algorithm thins the entire data vectors is zeros.   It is then marked dead
and the entire data interval marked as a gap.   Caller should handle
that condition and write a diagnostic message. */
const string maxampkey("maximum_major_axis_length");
int mask_zero_data(PMTimeSeries& d,double floor, int minsamples)
{
  try{
    if(!d.live) return(-1);
    int gapcount(0);
    vector<double> amps;
    amps.reserve(d.ns);
    ParticleMotionEllipse pme;
    int i;
    for(i=0;i<d.ns;++i)
    {
      if(!d.is_gap(i))
      {
        pme=d.ellipse(i);
        amps.push_back(pme.majornrm);
      }
    }
    TimeWindow gw;
    /* Watch for no valid data = all marked as gaps */
    if(amps.size()<=0)
    {
      d.live=false;
      gw.start=d.time(0);
      gw.end=d.endtime();
      return -1;
    }
    /* We define the floor relative to the largest amplitude in the PMTimeSeries data.
    We use the std algorithm to get that vaue */
    double maxamp;
    maxamp = *(std::max_element(amps.begin(),amps.end()));
    /* Always post this value for use later */
    d.put(maxampkey,maxamp);
    /* This is a cautious attempt to mark data that is all zeros */
    if(maxamp<(10.0*DBL_EPSILON))
    {
      d.live=false;
      gw.start=d.time(0);
      gw.end=d.endtime();
      return -1;
    }
    double dfloor=maxamp*floor;
    bool in_zero_section(false);
    bool gaps_is_small(true);
    int nsamp_this_gap(0);
    /* First we need to handle an existing gap defined at the start of the data */
    int is;
    for(i=0,is=0;i<d.ns;++i)
    {
      if(d.is_gap(i))
        ++is;
      else
        break;
    }
    if(is==d.ns)
    {
      d.live=false;
      return(-1);
    }
    for(i=is;i<d.ns;++i)
    {
      if(amps[i]<dfloor)
      {
        if(in_zero_section)
        {
          ++nsamp_this_gap;
          if(nsamp_this_gap>=minsamples) gaps_is_small=false;
          continue;
        }
        else
        {
          gw.start=d.time(i);
          /* avoid round off error issues for start */\
          if(i==0)gw.start -= (d.dt)/2.0;
          in_zero_section=true;
          gaps_is_small=true;
        }
      }
      else
      {
        if(nsamp_this_gap<minsamples)
        {
          continue;
        }
        else
        {
          /* Only add the gap if it is not too short */
          if(!gaps_is_small)
          {
            gw.end=d.time(i);
            d.add_gap(gw);
          }
          in_zero_section=false;
          ++gapcount;
        }
      }
    }
    /* Handle end - add a gap there if it is zeroed */
    if(in_zero_section)
    {
      gw.end=d.endtime()+1.0;   // Intentionall large pad if a gap is at end
      d.add_gap(gw);
      ++gapcount;
    }
    return gapcount;
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
    bool gaps_is_small(true);
    int i;
    int nsamp_this_gap(0);
    for(i=is;i<d.ns;++i)
    {
      ParticleMotionEllipse pme=d.ellipse(i);
      //DEBUG - verbose
      //cerr << "i="<<i<<" amp="<<pme.majornrm<<endl;
      if(pme.majornrm < snr_floor)
      {
        if(in_low_snr_section)
        {
          ++nsamp_this_gap;
          if(nsamp_this_gap>=minsamp) gaps_is_small=false;
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
            if(!gaps_is_small)
            {
              gw.end=d.time(i);
              d.add_gap(gw);
            }
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
        string noise_key=control.get_string("noise_level_key");
        /* This is an absolute floor for snr defined as level relative to the
        largest amplitude in a PMTimeSeries sequence.  This number comes into
        play only when the noise window is found insufficiently long to extablish
        a stable level - defined by the min_noise_samples parameter */
        double absolute_snr_floor=control.get<double>("absolute_snr_floor");
        int min_noise_samples=control.get<int>("minimum_noise_samples");
        int minsamp=control.get<int>("minimum_samples_per_gap");
        /* New option added Nov 2018 to maks hard zeros */
        bool scan_for_zeros=control.get_bool("scan_for_zeros");
        double floor_to_define_zero(FLT_EPSILON);
        if(scan_for_zeros)
        {
          try{
            floor_to_define_zero=control.get<double>("floor_to_define_zero");
          }catch(MetadataGetError& mde)
          {
            cerr << "mask_pm_snr:   parameter floor_to_define_zero not defined in pffile"<<endl
               << "Defaulting to "<<floor_to_define_zero<<endl;
          };
        }
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
              if(scan_for_zeros)
              {
                int ngaps_added;
                /* For now use the same miniumum for a gap definition */
                ngaps_added=mask_zero_data(d,floor_to_define_zero,minsamp);
                if(SEISPP_verbose && (ngaps_added>0))
                  cerr << "mask_pm_snr:  added "<<ngaps_added<<" gaps to object number "
                    <<count<<" from unmarked zero section"<<endl;
              }
              double noise_level;
              try{
                noise_level=median_noise(d,nwin,min_noise_samples);
                /* negative numbers flag an invalid estimate of noise level. when
                that happens use the absolute floor.*/
                if(noise_level<=0.0)
                {
                  double dmaxamp=d.get<double>(maxampkey);
                  noise_level=dmaxamp/absolute_snr_floor;
                }
                d.put(noise_key,noise_level);
                //DEBUG
                //cerr << "mask_pm_snr:  computed noise level power="<<noise_level<<endl;
              }catch(SeisppError& serr)
              {
                cerr << "Problem handling PMTimeSeries number "<<count<<endl;
                serr.log_error();
                cerr << "Passing this data along with no marked gap and noise level set 0"
                  <<endl;
                d.put(noise_key,0.0);
                continue; //this should transfer control outside the if live block
              }
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
              double snrlimit=noise_level*snr_floor;
              set_data_gaps(d,snrlimit,nwin,minsamp);
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
