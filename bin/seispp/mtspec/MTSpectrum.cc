#include <sstream>
#include <vector>
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
#include "MTSpectrum.h"
using namespace std;
using namespace SEISPP;
MTSpectrum::MTSpectrum()
{
  tbp=4.0;
  try{
    matlab = shared_ptr<MatlabProcessor>(new MatlabProcessor);
  }catch(...){throw;};
}
MTSpectrum::MTSpectrum(double time_bandwidth_product)
{
  tbp=time_bandwidth_product;
  try{
    matlab = shared_ptr<MatlabProcessor>(new MatlabProcessor);
  }catch(...){throw;};
}
MTSpectrum::MTSpectrum(const MTSpectrum& parent)
{
  tbp=parent.tbp;
  matlab=parent.matlab;
}
MTSpectrum& MTSpectrum::operator=(const MTSpectrum& parent)
{
  if(this!=(&parent))
  {
    tbp=parent.tbp;
    matlab=parent.matlab;
  }
}
const string vname("y");
const string mname("u");
const string specname("Y");
const string freqname("f");
/* This small helper creates a string of commands to send to
matlab to compute multitaper spectrum.  Args are

dname - matlab symbol name for data vector or matrix
tbp - time bandwidth product.
N - length of array sent to matlab
fs - sampling frequncy in Hz
*/
string build_process_commands(string dname, double tbp, int N, double fs)
{
  stringstream ss;
  ss << "["<<specname <<",f]"
    << "=pmtm("<<dname<<","<<tbp<<","<<N<<","<<fs<<")"<<endl;
  //DEBUG
  cerr << "Process script sent to matlab"<<endl
      << ss.str()<<endl;
  return(ss.str());
}
TimeSeries MTSpectrum::spectrum(Metadata& md, double *d, int nd)
{
  try{
    string commands;
    double fs;
    /* Attempt to extract sampling frequency from Metadata.  If
     * if tails default to 1.0 */
    try{
        fs=md.get<double>("samprate");
        fs=1.0/fs;
    }catch(MetadataGetError& mde)
    {
        cerr << "Warning(MTSpectrum::spectrum(Metadata& md, double *d, int nd) method"<<endl
            << "samprate attribute not defined - defaulting to 1.0. "<<endl
            << "Computed frequencies will likely be wrong"<<endl;
    }
    commands=build_process_commands(vname,this->tbp,nd,fs);
    matlab->load(d,nd,vname);
    matlab->process(commands);
    vector<double> s,f;
    s=matlab->retrieve_vector(specname);
    f=matlab->retrieve_vector(freqname);
    double df=f[1]-f[0];
    TimeSeries dts(md,false);
    dts.s=s;
    dts.ns=s.size();
    dts.dt=df;
    dts.t0=0.0;
    dts.tref=relative;
    dts.live=true;
    return dts;
  }catch(...){throw;};
}
TimeSeries MTSpectrum::spectrum(Metadata& md, vector<double>& d)
{
  try{
    return(this->spectrum(md,&(d[0]),d.size()));
  }catch(...){throw;};
}
TimeSeries MTSpectrum::spectrum(TimeSeries d)
{
  try{
    string commands;
    double fs;
    fs=1.0/(d.dt);  
    commands=build_process_commands(vname,this->tbp,d.s.size(),fs);
    /* There is an overloaded method to directoy load a TimeSeries data
    vector as vname */
    matlab->load(d,vname);
    matlab->process(commands);
    vector<double> s,f;
    s=matlab->retrieve_vector(specname);
    f=matlab->retrieve_vector(freqname);
    double df=f[1]-f[0];
    /* We modify the copy of d to hold the power spectrum estimate*/
    d.s=s;
    d.ns=s.size();
    d.dt=df;
    d.t0=0.0;
    d.tref=relative;
    d.live=true;
    return d;
  }catch(...){throw;};
}
ThreeComponentSeismogram MTSpectrum::spectrum(ThreeComponentSeismogram d)
{
  try{
    string commands;
    double fs;
    fs=1.0/(d.dt);  
    commands=build_process_commands(mname,this->tbp,d.ns,fs);
    /* Matlab requires the component data to be in the columns, but the
    3c object stores sample data in the columns. */
    dmatrix ut=tr(d.u);
    matlab->load(ut,mname);
    matlab->process(commands);
    shared_ptr<dmatrix> u;
    vector<double> f;
    u=matlab->retrieve_matrix(specname);
    f=matlab->retrieve_vector(freqname);
    double df=f[1]-f[0];
    /* We modify the copy of d to hold the power spectrum estimate*/
    ut=tr(*u);
    d.u=ut;
    d.ns=d.u.columns();
    d.dt=df;
    d.t0=0.0;
    d.tref=relative;
    d.live=true;
    return d;
  }catch(...){throw;};
}

TimeSeriesEnsemble MTSpectrum::spectrum(TimeSeriesEnsemble d)
{
  try{
    int i;
    for(i=0;i<d.member.size();++i)
    {
      TimeSeries spec;
      spec=this->spectrum(d.member[i]);
      d.member[i]=spec;
    }
    return d;
  }catch(...){throw;};
}
ThreeComponentEnsemble MTSpectrum::spectrum(ThreeComponentEnsemble d)
{
  try{
    int i;
    for(i=0;i<d.member.size();++i)
    {
      ThreeComponentSeismogram spec;
      spec=this->spectrum(d.member[i]);
      d.member[i]=spec;
    }
    return d;
  }catch(...){throw;};
}
