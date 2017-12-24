#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include <sys/stat.h>
#include "perf.h"
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
    cerr << "mwpmavg outfile [-avglimit n -v --help -text -pf pffile] < in "
        <<endl
        << "Averages multiwavelet particle motion estimates produced by mwpm"<<endl
        << "in a time window relative to time of a specified seismic phase"<<endl
        << "Results are a csv file defined by argument 1. "<<endl
        << "To get related attributes run listhdr with -csv option to build a parallel matrix of metadata"<<endl
        << " -avglimit flag sets the maximum number of nongap samples to average to n"<<endl
        << "This is useful to limit averaging to only early part of the signal"
        <<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl
        << " -pf - read parameters from pffile instead of defaul mwpmavg.pf"<<endl;
    exit(-1);
}
/* Small helper procedure.  Truncates a 3xn matrix stored with extra
 * columns to n.  Returns the 3xn result. */
dmatrix truncate_cols(dmatrix& d, int n)
{
    /*This could be made faster with pointers or memcp but speed is not
     * an issue with the program at present */
    dmatrix result(3,n);
    int i,j;
    for(j=0;j<n;++j)
    {
        for(i=0;i<3;++i) result(i,j)=d(i,j);
    }
    return result;
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
  /*! \brief primary constructor

    \param d - input data 
    \param phase_time_key - used to fetch arrival time
    \param twin - time window to average (time relative to arrival time 
       defined by phase_time_key
    \param maxavg - maximum number of samples to average for estimate.
      Used to allow longer twin but limit estimate to earliest part of
      the signal.  If less than 0 set to number of samples in win */
  PMAverageData(PMTimeSeries d, string phase_time_key, 
          TimeWindow twin, int maxavg);
  friend ostream& operator<<(ostream& os,PMAverageData& d);
};

/* This is the older algorithm.  Retained for debugging to ease comparison.
delete when new algorithm is validated.
PMAverageData::PMAverageData(PMTimeSeries d, string key, TimeWindow win)
>>>>>>> adda8fc1e1ac0006f2d95b51bde533aee28a1218
{
  try{
    ParticleMotionEllipse pme;
    ParticleMotionError pmerr;
    int i;
    double time;

    time=d.get<double>(key);
    d.rtoa();
    double tas,tae;   //computed absolute start and end times
    tas=time+win.start;
    tae=time+win.end;
    double dt=d.dt;
    int windowsizelimit;
    if(maxavg<0)
    {
        windowsizelimit=floor((win.end-win.start)/dt);
        if(windowsizelimit<0) windowsizelimit=1;//perhaps should abort on this
    }
    else
        windowsizelimit=maxavg;
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
      if((this->count)>=windowsizelimit) break;
    }
    if((this->count)<=0)
    {
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

    dmatrix majsamples(3,this->count);
    dmatrix minsamples(3,this->count);
    int k,ii;
    for(t=tas,ii=0;(t<=tae)&&(ii<(this->count));t+=dt)
    {
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

    const double conf(0.95),sampmultiplier(100);
    int numtrials=(this->count)*sampmultiplier;
    int count_floor(4);   
    vector<double> vtmp;
    if((this->count)==1)
    {
        for(k=0;k<3;++k)
        {
            this->major[k]=majsamples(k,0);
            this->minor[k]=minsamples(k,0);
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
*/
PMAverageData::PMAverageData(PMTimeSeries d, string key, TimeWindow win,
   int maxsamples)
{
  try{
    int i,j,k,ii;
    ParticleMotionEllipse pme;
    ParticleMotionError pmerr;
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
    double t;
    /* inc and az vectors below are only used if count is one, but
    kept as a vector for parallel structure with a minor memory cost*/
    vector<double> vmajnrm,vmajinc,vmajaz;
    vector<double> vminnrm,vmininc,vminaz;
    vector<double> vdmajnrm,vdmajaz,vdmajinc;
    vector<double> vdminnrm,vdminaz,vdmininc;
    vector<double> vrect,vdrect;
    dmatrix majsamples(3,maxsamples);
    dmatrix minsamples(3,maxsamples);
    for(t=tas,ii=0;t<=tae;t+=dt)
    {
      i=d.sample_number(t);
      if(i>=d.ns) break;
      if(i<0) continue;
      if(!d.is_gap(t))
      {
        pme=d.ellipse(i);
        for(k=0;k<3;++k)
        {
          majsamples(k,ii)=pme.major[k];
          minsamples(k,ii)=pme.minor[k];
        }
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
        ++ii;
        if(ii>=maxsamples)break;
      }
    }
    this->count=ii;
    if((this->count)<=0)
    {
      live=false;
      return;
    }
    else
    {
      live=true;
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
        }
        this->dtheta_major_axes=M_PI;
        this->dtheta_minor_axes=M_PI;
        majornrm=vmajnrm[0];
        minornrm=vminnrm[0];
        majoraz=vmajaz[0];
        minoraz=vminaz[0];
        majorinc=vmajinc[0];
        minorinc=vmininc[0];
        rectilinearity=vrect[0];
        dmajornrm=vdmajnrm[0];
        dminornrm=vdminnrm[0];
        dmajorinc=vdmajinc[0];
        dminorinc=vdmininc[0];
        dmajoraz=vdmajaz[0];
        dminoraz=vdminaz[0];
        drect=vdrect[0];
    }
    else
    {
        /* We have truncate the input matrices due to the above
         * algorithm if the matrix is not filled */
        if((this->count)!=maxsamples)
        {
            majsamples=truncate_cols(majsamples,this->count);
            minsamples=truncate_cols(minsamples,this->count);
        }
        Vector3DBootstrapError majerr(majsamples,conf,numtrials);
        Vector3DBootstrapError minerr(minsamples,conf,numtrials);
        vtmp=majerr.mean_vector();
        for(k=0;k<3;++k) this->major[k]=vtmp[k];
        majoraz=M_PI_2 - atan2(vtmp[1],vtmp[0]);
        /* We need to force the minor axis to be perpendicular to
        major axis to match the particle ellipse model.   This algorithm
        is the same one used in PMTimeSeries */
        vtmp=minerr.mean_vector();
        double w[3],w2[3];
        dr3cros(this->major,&(vtmp[0]),w);
        dr3cros(w,this->major,w2);
        double wnrm;
        for(wnrm=0,j=0;j<3;++j)wnrm+=w2[j]*w2[j];
        wnrm=sqrt(wnrm);
        /* This sets minor axis and normalizes it to unit length in one pass*/
        for(j=0;j<3;++j) this->minor[j]=w2[j]/wnrm;
        minoraz = M_PI_2 - atan2(w2[1],w2[0]);
        /* the emergence angle for both major and minor is easily computed from
        dot product of units vectors.  azimuth requires a potentially dangerous
        - due to possible divide by 0 error - division by sin of the theta angle */
        double vert[3]={0.0,0.0,1.0};
        double vproj,theta;
        vproj=ddot(3,this->major,1,vert,1);
        theta=acos(vproj);
        /* This will be botched if theta is negative, which it will be if
        * the vector has a downward component.  Hence this correction.
        Not if we do this flip we also need to alter the aziiuth by 180
        degrees */
        if(theta<0.0)
        {
          theta=(-theta);
          majoraz+=M_PI;
          if(majoraz>(2.0*M_PI)) majoraz -= (2.0*M_PI);
        }
        majorinc=theta;
        /* repeat for minor axis */
        vproj=ddot(3,this->minor,1,vert,1);
        theta=acos(vproj);
        if(theta<0.0)
        {
          theta=(-theta);
          minoraz+=M_PI;
          if(minoraz>(2.0*M_PI)) minoraz -= (2.0*M_PI);
        }
        minorinc=theta;
        /* We compute these averages from medians values */
        minornrm=median<double>(vminnrm);
        majornrm=median<double>(vmajnrm);
        rectilinearity=median<double>(vrect);
        dmajornrm=median<double>(vdmajnrm);
        dminornrm=median<double>(vdminnrm);
        dmajorinc=median<double>(vdmajinc);
        dminorinc=median<double>(vdmininc);
        dmajoraz=median<double>(vdmajaz);
        dminoraz=median<double>(vdminaz);
        drect=median<double>(vdrect);
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
    if(argc<2) usage();
    if(argc>1)
    {
      if(string(argv[1])=="--help")
         usage();
    }
    string outfile(argv[1]);
    if(fileExists(outfile))
    {
      cerr << "mwpmavg(Warning):  files specified as argument 1="<<outfile<<" exists"<<endl
         << "Appending these results to this file - beware of duplicates"<<endl;
    }
    bool binary_data(true);
    string pffile("mwpmavg.pf");
    int windowlimit(-1); // negative signals to turn this feature off
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
        else if(sarg=="-avglimit")
        {
          ++i;
          if(i>=argc)usage();
          windowlimit=atoi(argv[i]);
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
        int maxsamples=control.get<int>("number_of_samples_cutoff");
        string name_key=control.get_string("name_key");
        string key_type=control.get_string("name_key_type");
        allowed_key_types kt;
        kt=parse_key_name(key_type);
        ofstream ofs(outfile.c_str(),std::ios::out | std::ios::app);
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
            int band;
            band=d.get<int>("band");
            try{
              nametag=fetch_nametag(d,name_key,kt);
            }catch(MetadataGetError& mde)
            {
              cerr << "mwpmavg:  Error reading name tag key = "<<name_key
                 << " with PMTimeSeries number "<< nd<<" of input"<<endl
                 << "Message posted:"<<endl;
              mde.log_error();
              cerr << "Attempting to continue"<<endl;
              nametag="BAD";
            }
            /* This routine does all the work.   Returns result in the class
            defined above */
            const string band_key("band");
            PMAverageData avg(d,phase_time_key,avgwin,maxsamples);
            if(avg.live)
            {
              ofs << nametag<<","<<band<<","
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
