#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include <sys/stat.h>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "ensemble.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "VectorStatistics.h"
#include "Vector3DBootstrapError.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "mwpmavg outfile [-v --help -text -pf pffile] < in "
        <<endl
        << "Averages multiwavelet particle motion estimates produced by mwpm"<<endl
        << "in a time window relative to time of a specified seismic phase"<<endl
        << "Results are a csv file defined by argument 1. "<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl
        << " -pf - read parameters from pffile instead of defaul mwpmavg.pf"<<endl;
    exit(-1);
}
class PMAverageData
{
public:
  double major[3],minor[3]; 
  double majornrm,majoraz,majorinc;
  double minornrm,minoraz,minorinc;
  double rectilinearity;
  double dmajornrm,dminornrm;
  double dmajoraz,dminoraz;
  double dmajorinc,dminorinc;
  double drect;
  double dtheta_major_axes; //scatter in major vectors dot product angle
  double dtheta_minor_axes;  // same for minor axis
  bool live;  // true of computed ok - false if the constructor found no data in twin
  int count;  // number of non-gap time steps averaged for this estimate
  PMAverageData(PMTimeSeries& d, string phase_time_key, TimeWindow twin);
  friend ostream& operator<<(ostream& os,PMAverageData& d);
};
/* PMAverageData is defined really by this constructor.   It computes
all the attributes by computing the median of all attributes and storing
them in the public attribures */
PMAverageData::PMAverageData(PMTimeSeries& d, string key, TimeWindow win)
{
  try{
    ParticleMotionEllipse pme;
    ParticleMotionError pmerr;
    int i;
    double time;
    /* This needs to be the zero reference time - typicall predicted arrival time
    or a measured arrival time */
    time=d.get<double>(key);
    /* First make sure we restore these data to absolute time standard */
    d.rtoa();
    double tas,tae;   //computed absolute start and end times
    tas=time+win.start;
    tae=time+win.end;
    double dt=d.dt;
    vector<double> vmajnrm,vmajinc,vmajaz;
    vector<double> vminnrm,vmininc,vminaz;
    vector<double> vdmajnrm,vdmajaz,vdmajinc;
    vector<double> vdminnrm,vdminaz,vdmininc;
    vector<double> vrect,vdrect;
    double t;
    for(this->count=0,t=tas;t<=tae;t+=dt)
    {
      int i=d.sample_number(t);
      if(i>=d.ns) break;
      if(i<0) continue;
      if(d.is_gap(t))continue;
      /* We land here if there is valid data.   Then we extract ellipse data
      and fill all these vectors that we reduce to medians below */
      pme=d.ellipse(i);
      vmajnrm.push_back(pme.majornrm);
      vminnrm.push_back(pme.minornrm);
      vmajaz.push_back(pme.major_azimuth());
      vmajinc.push_back(pme.major_inclination());
      vminaz.push_back(pme.minor_azimuth());
      vmininc.push_back(pme.minor_inclination());
      vrect.push_back(pme.rectilinearity());
      pmerr=d.errors(i);
      vdmajnrm.push_back(pmerr.dmajornrm);
      vdminnrm.push_back(pmerr.dminornrm);
      vdmajaz.push_back(pmerr.dphi_major);
      vdminaz.push_back(pmerr.dphi_minor);
      vdmajinc.push_back(pmerr.dtheta_major);
      vdmininc.push_back(pmerr.dtheta_minor);
      vdrect.push_back(pmerr.delta_rect);
      ++(this->count);
    }
    if((this->count)<=0)
    {
      /* We signal null result by setting this boolean false */
      live=false;
      return;
    }
    else
    {
      live=true;
      majornrm=median<double>(vmajnrm);
      majoraz=median<double>(vmajaz);
      majorinc=median<double>(vmajinc);
      minornrm=median<double>(vminnrm);
      minoraz=median<double>(vminaz);
      minorinc=median<double>(vmininc);
      rectilinearity=median<double>(vrect);
      dmajornrm=median<double>(vdmajnrm);
      dminornrm=median<double>(vdminnrm);
      dmajorinc=median<double>(vdmajinc);
      dminorinc=median<double>(vdmininc);
      dmajoraz=median<double>(vdmajaz);
      dminoraz=median<double>(vdminaz);
      drect=median<double>(vdrect);
    }
    /* We handle the major and minor axis vector data 
     * separately.   We compute a new bootstrap error
     * of average angle deviation of these vectors 
     * over the defined range.  Not sure how meaningful that
     * estimate is beause the numbers are so strongly 
     * correlated, but might be a useful qc measure even
     * if it may not mean anything in an absolute sense. */
    dmatrix majsamples(3,this->count);
    dmatrix minsamples(3,this->count);
    int k,ii;
    for(t=tas,ii=0;t<=tae;t+=dt)
    {
      if(ii==this->count)
      {
          cerr << "PMAverageData constructor:  gap count mismatch"
              <<" at sample count="<<ii<<endl
              << "This should not happen, but exiting major and minor"
              << " vector averaging loop on this nonfatal error"
              <<endl;
          break;
      }
      int i=d.sample_number(t);
      if(i>=d.ns) break;
      if(i<0) continue;
      if(d.is_gap(t))continue;
      pme=d.ellipse(i);
      for(k=0;k<3;++k)
      {
          majsamples(k,ii)=pme.major[k];
          minsamples(k,ii)=pme.minor[k];
      }
      ++ii;
    }
    /* Now we use the bootstrap error estimator in libmwpm.
     * The confidence value and multiplier on the number of trials
     * is fixed here.   May want to add that as a parameter to the
     * pf for htis program */
    const double conf(0.95),sampmultiplier(100);
    int numtrials=(this->count)*sampmultiplier;
    int count_floor(4);   // When less than this set the error to +-180
    vector<double> vtmp;
    if((this->count)==1)
    {
        for(k=0;k<3;++k)
        {
            this->major[k]=majsamples(k,1);
            this->minor[k]=minsamples(k,1);
            this->dtheta_major_axes=M_PI;
            this->dtheta_minor_axes=M_PI;
        }
    }
    else
    {
        Vector3DBootstrapError majerr(majsamples,conf,numtrials);
        Vector3DBootstrapError minerr(minsamples,conf,numtrials);
        vtmp=majerr.mean_vector();
        for(k=0;k<3;++k) this->major[k]=vtmp[k];
        vtmp=minerr.mean_vector();
        for(k=0;k<3;++k) this->minor[k]=vtmp[k];
        if((this->count)<count_floor)
        {
            this->dtheta_major_axes=M_PI;
            this->dtheta_minor_axes=M_PI;
        }
        else
        {
            this->dtheta_major_axes=majerr.angle_error();
            this->dtheta_minor_axes=minerr.angle_error();
        }
    }
  }catch(...){throw;};
}
ostream& operator<<(ostream& os,PMAverageData& d)
{
    /* Note we convert all angle terms to degrees as 
     * angles are always stored internally in radians, but 
     * humans mostly can't relate to radians */
  os << d.majornrm << ","<< d.dmajornrm<<","
    << deg(d.majoraz) << ","<< deg(d.dmajoraz)<<","
    << deg(d.majorinc) << ","<< deg(d.dmajorinc)<<","
    << d.minornrm << ","<< d.dminornrm<<","
    << deg(d.minoraz) << ","<< deg(d.dminoraz)<<","
    << deg(d.minorinc) << ","<< deg(d.dminorinc)<<","
    << d.rectilinearity<<","<< d.drect;
  int k;
  for(k=0;k<3;++k) os<<","<<d.major[k];
  os<<","<<deg(d.dtheta_major_axes);
  for(k=0;k<3;++k) os<<","<<d.minor[k];
  os<<","<<deg(d.dtheta_minor_axes);
  os<<","<<d.count;
  return os;
}
bool fileExists(const std::string& file) {
    struct stat buf;
    return (stat(file.c_str(), &buf) == 0);
}
enum allowed_key_types {MWINT, MWREAL, MWSTRING};
allowed_key_types parse_key_name(string s)
{
  allowed_key_types result;
  if( (s=="INT") || (s=="int") || (s=="integer"))
     result=MWINT;
  else if( (s=="real") || (s=="REAL") || (s=="double") || (s=="float"))
     result=MWREAL;
  else if( (s=="string") || (s=="STRING"))
      result=MWSTRING;
  else
  {
    cerr << "mwpmavg:   illegal specification for name key type="<<s<<endl;
    usage();
  }
  return result;
 }
string fetch_nametag(PMTimeSeries& d, string key, allowed_key_types kt)
{
  try{
    string result;
    int ival;
    double dval;
    stringstream ss;
    switch(kt)
    {
      case MWINT:
        ival=d.get<long>(key);
        ss << ival;
        result=ss.str();
        break;
      case MWREAL:
        dval=d.get<double>(key);
        ss << dval;
        result=ss.str();
        break;
      case MWSTRING:
      default:
         result=d.get_string(key);
    }
    return result;
  }catch(...){throw;};
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc>1)
    {
      if(string(argv[1])=="--help")
         usage();
    }
    string outfile(argv[1]);
    if(fileExists(outfile))
    {
      cerr << "mwpmavg:  files specified as argument 1="<<outfile<<" exists"<<endl
         << "Fatal error:   exiting to avoid overwriting possible previous results"<<endl;
      usage();
    }
    bool binary_data(true);
    string pffile("mwpmavg.pf");
    for(i=2;i<argc;++i)
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
        else if(sarg=="-pf")
        {
          ++i;
          if(i>=argc)usage();
          pffile=string(argv[i]);
        }
        else if(sarg=="-v")
          SEISPP_verbose=true;
        else
            usage();
    }
    Pf *pf;
    char pfchr[32];
    strcpy(pfchr,pffile.c_str());
    if(pfread(pfchr,&pf))
    {
      cerr << "pfread failed on file="<<pffile<<endl;
      usage();
    }
    try{
        Metadata control(pf);
        string phase=control.get_string("phase");
        string phase_time_key=control.get_string("phase_time_key");
        double ts,te;
        ts=control.get<double>("average_window_start");
        te=control.get<double>("average_window_end");
        TimeWindow avgwin(ts,te);
        string name_key=control.get_string("name_key");
        string key_type=control.get_string("name_key_type");
        allowed_key_types kt;
        kt=parse_key_name(key_type);
        ofstream ofs(outfile.c_str(),std::ios::out | std::ios::trunc);
        if(ofs.fail())
        {
          cerr << "Open failed for output file="<<outfile<<endl;
          usage();
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
        int nd(0);
        PMTimeSeries d;
        while(inp->good())
        {
            d=inp->read();
            /* First fetch the name key that will be used as a tag.  always
            returned as a string.  real numbers drop period */
            string nametag;
            try{
              nametag=fetch_nametag(d,name_key,kt);
            }catch(MetadataGetError& mde)
            {
              cerr << "mwpmavg:  Error reading name tag key = "<<name_key
                 << " with PMTimeSeries number "<< nd<<" of input"<<endl
                 << "Message posted:"<<endl;
              mde.log_error();
              cerr << "Attempting to continue"<<endl;
            }
            /* This routine does all the work.   Returns result in the class
            defined above */
            const string band_key("band");
            PMAverageData avg(d,phase_time_key,avgwin);
            int band;
            if(avg.live)
            {
              ofs << nametag<<","<<phase<<","<<band<<","
                  <<avg<<endl;
              ++nd;
            }
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
