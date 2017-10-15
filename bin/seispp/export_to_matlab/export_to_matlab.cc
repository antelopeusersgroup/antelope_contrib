#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <fstream>
#include <memory>
#include "seispp.h"
#include "ensemble.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "export_to_matlab [-v -text -o outfile --help] < in"
        <<endl
        << "Write data in a single TimeSeriesEnsemble object to a matrix format "<<endl
        << "that can be easily read into matlab (load procedure)"<<endl
        << "Use -o outfile to write the data to outfile.  By default the "<<endl
        << "matrix data are written to stdout"<<endl
        << "Use -h to write a data header with required time series attributes (default is none)"
        <<endl
        << "Format:  t0, dt, ns  (space delimited)"<<endl
        << "WARNING:  make sure the input has a relative time standard or span a single time window"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input (default is binary)"<<endl;
    exit(-1);
}
class AlignedGather
{
public:
  double t0;
  double dt;
  int ns;
  int nseis;
  TimeReferenceType tref;
  dmatrix d;
  AlignedGather(dmatrix& din, double t0in, double dtin, int nsin, int nseisin, TimeReferenceType trefin) : d(din)
  {
    this->t0=t0in;
    this->dt=dtin;
    this->ns=nsin;
    this->tref=trefin;
    this->nseis=nseisin;
  };
  AlignedGather(const AlignedGather& parent) : d(parent.d)
  {
    t0=parent.t0;
    dt=parent.dt;
    ns=parent.ns;
    nseis=parent.nseis;
    tref=parent.tref;
  }
};
/* Data with marked gaps are set to this value */
const double GapValue(-9.9999999999E99);
AlignedGather convert_to_matrix(TimeSeriesEnsemble& d)
{
  try{
    vector<TimeSeries>::iterator dptr;
    double dt,tmin,tmax;
    int i,j;
    for(i=0,dptr=d.member.begin();dptr!=d.member.end();++i,++dptr)
    {
      if(i==0)
      {
        tmin=dptr->time(0);
        tmax=dptr->endtime();
        dt=dptr->dt;
      }
      else
      {
        tmin=min(tmin,dptr->time(0));
        tmax=max(tmax,dptr->endtime());
        /* Bad form here using a fixed constant to define equivalent
        sample rates */
        if(fabs(dt-dptr->dt)>0.0001)
        {
          cerr << "export_to_matlab:  Mixmatched sample rates in input ensemble"<<endl
            << "This program requires fixed sample rate"<<endl
            << "Member "<<i<<" has dt="<<dptr->dt<<" but previous members had dt="
            << dt<<endl<<"No output will be generated"<<endl;
          exit(-1);
        }
      }
    }
    TimeReferenceType tref;
    tref=d.member[0].tref;  //  assume all have the same time base
    int n=d.member.size();
    int m= (int)((tmax-tmin)/dt)+1;
    if(SEISPP_verbose)
    {
      cerr << "export_to_matlab:  time range of output="<<tmin<<" to "<<tmax<<endl;
      cerr << " computed number of samples="<<m<<endl
        << " from "<<n<<" seismograms in the input gather"<<endl;;
    }
    /* fixed wall as sanity check */
    const int Mmax(100000000);
    if(m>Mmax)
    {
      cerr<< "export_to_matlab:  Computed number of samples,"<<m<<",  is very large."<<endl
          << "Aborting to avoid a likely malloc error."<<endl
          << "You are probably trying to to convert an  ensmble with absolute times set as t0"<<endl
          << "If so, run data through ator before running this program"<<endl;
      exit(-1);
    }
    dmatrix work(m,n);
    /* We initialize the matrix to GapValue.  That allows an easy
     * definition of gaps at start and end. */
    for(i=0;i<m;++i)
      for(j=0;j<n;++j)
          work(i,j)=GapValue;
    bool needs_gap_checking;
    for(j=0,dptr=d.member.begin();dptr!=d.member.end();++j,++dptr)
    {
      if(dptr->has_gap())
        needs_gap_checking=true;
      else
        needs_gap_checking=false;
      double t;
      for(t=tmin;t<tmax;t+=dt)
      {
        int kd,km;
        kd=dptr->sample_number(t);
        if( (kd<0) || (kd>=(dptr->ns)) ) continue;
        if(dptr->is_gap(t))
        {
          work(kd,j)=GapValue;
        }
        else
        {
          work(kd,j)=dptr->s[kd];
        }
        /*else is intensionally not present.  work was
         * initialized to GapValue - else condition is
         * index outside the range of this seismogram.*/
      }
    }
    AlignedGather result(work,tmin,dt,m,n,tref);
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
    bool write_to_stdout(true);
    bool write_header(false);
    string outfile;

    for(i=1;i<argc;++i)
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
        else if(sarg=="-o")
        {
          ++i;
          if(i>=argc) usage();
          outfile=string(argv[i]);
          write_to_stdout=false;
        }
        else if(sarg=="-h")
        {
          write_header=true;
        }
        else
            usage();
    }
    try{
        shared_ptr<StreamObjectReader<TimeSeriesEnsemble>> inp;
        if(binary_data)
        {
          inp=shared_ptr<StreamObjectReader<TimeSeriesEnsemble>>
             (new StreamObjectReader<TimeSeriesEnsemble>('b'));
        }
        else
        {
          inp=shared_ptr<StreamObjectReader<TimeSeriesEnsemble>>
             (new StreamObjectReader<TimeSeriesEnsemble>);
        }
        TimeSeriesEnsemble d;
        d=inp->read();
        AlignedGather dmat(convert_to_matrix(d));
        if(write_to_stdout)
        {
          if(write_header)
          {
            if(dmat.tref == absolute)
               cout << setprecision(13)<<dmat.t0<<" "<<setprecision(7);
            else
            {
              cout<<dmat.t0<<" ";
            }
            cout << dmat.dt<<" "<<dmat.ns<<" "<<dmat.nseis<<endl;
          }
          cout << dmat.d;
        }
        else
        {
          ofstream ofs;
          ofs.open(outfile.c_str(),ios::out);
          if(ofs.fail())
          {
            cerr << "export_to_matlab:  open failed for output file="<<outfile<<endl
              << "Data not saved"<<endl;
            usage();
          }
          if(write_header)
          {
            if(dmat.tref == absolute)
              ofs << setprecision(13)<<dmat.t0<<" "<<setprecision(7);
            else
            {
              ofs<<dmat.t0<<" ";
            }
            ofs << dmat.dt<<" "<<dmat.ns<<" "<<dmat.nseis<<endl;
          }
          ofs << dmat.d;
          ofs.close();
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
