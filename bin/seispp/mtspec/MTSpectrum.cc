#include <sstream>
#include <vector>
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
#include "Ensemble.h"
#include "MTSpectrum.h"
using namespace std;
using namespace SEISPP;
MTSpectrum::MTSpectrum()
{
  tbp=4;
  try{
    matlab = shared_ptr<MatlabProcessor>(new MatlabProcessor);
  }catch(...){throw;};
}
MTSpectrum::MTSpectrum(int time_bandwidth_product)
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
*/
string build_process_commands(string dname, distinction tbp, int N, double fs)
{
  stringstream ss;
  ss << "["<<specname <<",f]"
    << "=pmtm("<<dname<<","<<tbp<<","<<N<<","<<fs<<")"<<endl;
  return(ss.str());
}
TimeSeries MTSpectrum::spectrum(Metadata& md, double d, int nd, double fs)
{
  try{
    string commands;
    commands=build_process_commands(vname,this->tbp);
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
    dts.tref=Relative;
    dts.live=true;
    return dts;
  }catch(...){throw;};
}
TimeSeries MTSpectrum::spectrum(Metadata& md, vector<double>& d)
{
  try{
    return(this->matlab->spectrum(md,&(d[0]),d.size()));
  }catch(...){throw;};
}
TimeSeries MTSpectrum::spectrum(TimeSeries d)
{
  try{
    string commands;
    commands=build_process_commands(vname,this->tbp);
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
    d.tref=Relative;
    d.live=true;
    return d;
  }catch(...){throw;};
}
ThreeComponentSeismogram MTSpectrum::spectrum(ThreeComponentSeismogram d)
{
  try{
    string commands;
    commands=build_process_commands(mname,this->tbp);
    /* Matlab requires the component data to be in the columns, but the
    3c object stores sample data in the columns. */
    dmatrix ut=SEISPP::tr(d.u);
    matlab->load(d,mname);
    matlab->process(commands);
    shared_ptr<dmatrix> u,f;
    u=matlab->retrieve_matrix(specname);
    f=matlab->retrieve_matrix(freqname);
    double df=(*f)(1,0)-(*f)(0,0); // Weird construct for shared_ptr
    /* We modify the copy of d to hold the power spectrum estimate*/
    d.u=SEISPP::tr(u);
    d.ns=u.rows();
    d.dt=df;
    d.t0=0.0;
    d.tref=Relative;
    d.live=true;
    return d;
  }catch(...){throw;};
}
