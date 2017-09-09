#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "seispp.h"
#include "ensemble.h"
#include "PfStyleMetadata.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "impulse_simulator > out [-i pattern -v --help -text -pf pffile]"
        <<endl
        << "Creates a simulated ThreeComponentEnsemble of 3C impulses according to"
        << " a recipe in a parameter file"<<endl
        << " -i is used to define the data as a clone of the contents of file=pattern. "<<endl
        << "pattern file must be a serialized (binary) file with one ThreeComponentEnsemble object"
        <<endl
        << "Default output is created according to parameter file definitions"
        <<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl
        << " -pf - use alternate parameter file pffile (default impulse_response.pf)"
        <<endl;
    exit(-1);
}
class Impulse3CDefinition
{
public:
  double time, A[3];
  int member_number;
  Impulse3CDefinition(string line);
  Impulse3CDefinition(const Impulse3CDefinition& parent);
  Impulse3CDefinition& operator=(const Impulse3CDefinition& parent);
};
Impulse3CDefinition::Impulse3CDefinition(string line)
{
  try{
    stringstream ss(line.c_str());
    ss >> member_number;
    ss >> time;
    int k;
    for(k=0;k<3;++k) ss >> A[k];
  }catch(...){throw;};
}
Impulse3CDefinition::Impulse3CDefinition(const Impulse3CDefinition& parent)
{
  member_number=parent.member_number;
  time=parent.time;
  int k;
  for(k=0;k<3;++k)A[k]=parent.A[k];
}
Impulse3CDefinition& Impulse3CDefinition::operator=(const Impulse3CDefinition& parent)
{
  if(this!=&parent)
  {
   member_number=parent.member_number;
   time=parent.time;
   int k;
   for(k=0;k<3;++k)A[k]=parent.A[k];
  }
  return *this;
}
vector<Impulse3CDefinition> LoadImpulses(PfStyleMetadata& control)
{
  try{
    const string key("impulses");
    vector<Impulse3CDefinition> result;
    list<string> lines(control.get_tbl(key));
    list<string>::iterator lptr;
    for(lptr=lines.begin();lptr!=lines.end();++lptr)
    {
      result.push_back(Impulse3CDefinition(*lptr));
    }
    return result;
  }catch(...){throw;};
}
ThreeComponentEnsemble load_pattern_file(string fname)
{
  try{
    ThreeComponentEnsemble d;
    /* Require this be binary */
    StreamObjectReader<ThreeComponentEnsemble> inp('b');
    d=inp.read();
    return d;
  }catch(...){throw;};
}
ThreeComponentEnsemble create_pattern_from_pf(PfStyleMetadata& control)
{
  try{
    int nmembers=control.get<int>("number_seismograms");
    int ns=control.get<int>("number_samples");
    double dt=control.get<double>("dt");
    double t0=control.get<double>("t0");
    /* this key defines the integer count key set for each seismogram*/
    string member_count_key=control.get<string>("member_count_key");
    ThreeComponentEnsemble result(nmembers,ns);
    int i;
    for(i=0;i<nmembers;++i)
    {
      ThreeComponentSeismogram d(ns);
      d.u.zero();
      d.t0=t0;
      d.ns=ns;
      d.dt=dt;
      d.components_are_cardinal=true;
      d.components_are_orthogonal=true;
      d.tref=relative;
      d.live=true;
      int k,j;
      for(k=0;k<3;++k)
      {
        for(j=0;j<3;++j)
        {
          if(i==k)
            d.tmatrix[k][k]=1.0;
          else
            d.tmatrix[k][j]=0.0;
        }
      }
      d.put("nsamp",ns);
      d.put("time",t0);
      d.put("dt",dt);
      d.put("samprate",1.0/dt);
      d.put(member_count_key.c_str(),i);
      /* Constructor used above creates the pattern so we 
       * copy this new d into slot i*/
      result.member[i]=d;
    }
    return result;
  }catch(...){throw;};
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    bool binary_data(true);
    bool use_pattern_file(false);
    string patternfile("");
    string pffile("impulse_simulator.pf");
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-i")
        {
            ++i;
            if(i>=argc)usage();
            patternfile=string(argv[i]);
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
        else
            usage();
    }
    try{
        PfStyleMetadata control=pfread(pffile);
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
        ThreeComponentEnsemble d;
        if(use_pattern_file)
        {
          d=load_pattern_file(patternfile);
        }
        else
        {
          d=create_pattern_from_pf(control);
        }
        vector<Impulse3CDefinition> impulses=LoadImpulses(control);
        int nmembers=d.member.size();
        vector<Impulse3CDefinition>::iterator impptr;
        for(impptr=impulses.begin();impptr!=impulses.end();++impptr)
        {
          int i,j,k;
          i=impptr->member_number;
          j=d.member[i].sample_number(impptr->time);
          if((j>=0) && (j<d.member[i].ns))
          {
            for(k=0;k<3;++k)d.member[i].u(k,j)=impptr->A[k];
          }
          else
          {
            cerr << "impulse_simulator:  Requested impulse at time="
              << impptr->time<<" is outside data range"<<endl
              << "Pattern seismogram has t0="<<d.member[i].t0
              << " with endtime="<<d.member[i].endtime()<<endl;
          }
        }
        out->write(d);
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
