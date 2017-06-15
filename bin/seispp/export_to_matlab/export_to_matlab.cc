#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include "seispp.h"
#include "StreamObjectReader.h"
#include "dmatrix.h"
#include "ensemble.h"
#include "Metadata.h"
using namespace std;
using namespace SEISPP;

void usage()
{
    cerr << "export_to_matlab [-x1 x1f -x2 x2f -x3 x3f -pf pffile] < in  "
        <<endl
        << "Exports serialized seispp 3C ensemble to 3 matlab matrices in 3 files"<<endl
        << "Use -x1, -x2, or -x3 to set file names for components 1, 2, 3"<<endl
        << "Default files are x1=T.dat, x2=R.dat, and x3=L.dat"<<endl
        << "Default names can also be changed by editing default pf file "
        << "export_to_matlab.pf"<<endl
        << "(Accepts only text format input)"<<endl;
    exit(-1);
}
vector<dmatrix> convert_to_matrices(ThreeComponentEnsemble& d)
{
  /* We need to make sure these data are set to relative time.  We
  also scan for the relative time range of the data. */
  vector<ThreeComponentSeismogram>::iterator dptr;
  double tmin,tmax;
  int i,j;
  double dt;
  for(i=0,dptr=d.member.begin();dptr!=d.member.end();++dptr)
  {
    if(dptr->tref == absolute) dptr->ator(dptr->t0);
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
        cerr << "export_to_matlab:  Mixmatched sample rates in Three C ensemble"<<endl
          << "This program requires fixed sample rate"<<endl
          << "Member "<<i<<" has dt="<<dptr->dt<<" put previous members had dt="
          << dt<<endl<<"Not output will be generated"<<endl;
        exit(-1);
      }
    }
  }
  cout << "Relative time range of output data is "<<tmin <<" to "<<tmax<<endl;
  int n=d.member.size();
  int m=(tmax-tmin)/dt;
  cout << "Output matrices will be of size "<<m<<"X"<<n<<endl;
  vector<dmatrix> work;
  for(i=0;i<3;++i) work.push_back(dmatrix(m,n));
  for(i=0;i<3;++i) work[i].zero();
  double t;
  for(dptr=d.member.begin(),j=0;dptr!=d.member.end();++dptr,++j)
  {
    for(t=tmin;t<tmax;t+=dt)
    {
      int kd,km;
      kd=dptr->sample_number(t);
      if( (kd>=0) && (kd<dptr->ns) )
      {
        if(kd<m)
        {
          for(i=0;i<3;++i)
          {
            work[i](kd,j)=dptr->u(i,kd);
          }
        }
      }
    }
  }
  return work;
}


bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{

    int i;
    char *pffile=strdup("export_to_matlab");

    /* We have to pass through the arg list twice - the first pass 
       is just to set alterntive pf if requested */
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-pf")
        {
            ++i;
            if(i>=argc)usage();
            pffile=argv[i];
        }
    }
    try{
        Pf *pf;
        if(pfread(pffile,&pf))
        {
          cerr << "Error reading parameter file ="<<pffile<<endl;
          exit(-1);
        }
        Metadata control(pf);
        string x1file=control.get_string("component1_filename");
        string x2file=control.get_string("component2_filename");
        string x3file=control.get_string("component3_filename");
        for(i=1;i<argc;++i)
        {
          string sarg(argv[i]);
          if(sarg=="-pf")
          {
            /* in this pass we just skip and can assume the arg following
               -pf was there */
            ++i;
          }
          else if(sarg=="-x1")
          {
            ++i;
            if(i>=argc)usage();
            x1file=string(argv[i]);
          }
          else if(sarg=="-x2")
          {
            ++i;
            if(i>=argc)usage();
            x2file=string(argv[i]);
          }
          else if(sarg=="-x3")
          {
            ++i;
            if(i>=argc)usage();
            x3file=string(argv[i]);
          }
          else
            usage();
        }
        StreamObjectReader<ThreeComponentEnsemble> ia;
        ThreeComponentEnsemble d;
        d=ia.read();
        vector<dmatrix> dmat=convert_to_matrices(d);
        ofstream ofs;
        ofs.open(x1file.c_str(),ios::out);
        ofs<<dmat[0];
        ofs.close();
        ofs.open(x2file.c_str(),ios::out);
        ofs<<dmat[1];
        ofs.close();
        ofs.open(x3file.c_str(),ios::out);
        ofs<<dmat[2];
        ofs.close();
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
