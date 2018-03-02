#include <stdlib.h>
#include <stdio.h>
#include <float.h>
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
    cerr << "pm2wulff dir < in [-v --help -text]"
        <<endl
        << "Reads a file of serialized PMTimeSeries objects and writes"
        << endl
        << "a set of text files that be used to plot the data as an equal angle"
        << "(Wulff net) streographic projection.   Text files can be plotted in"
        <<endl
        << "a range of ploting packages like matlab or gnuplot"<<endl
        << "required argument dir is directory where results will be written"
        << endl
        << "WARNING:  this program will generate one file per input PMTimeSeries"
        << " object"<<endl<< "File names are evid_sta_band.mat"<<endl
        << "Large data sets should be subsetted before running this program"
        <<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
class WulffData
{
public:
  WulffData(double *u);
  double phi;
  double r;
};
WulffData::WulffData(double *u)
{
  double theta;
  double rhorizontal=hypot(u[0],u[1]);
  /* A conservative test for vertical vector - assumes u is a unit vector */
  if(rhorizontal<FLT_EPSILON)
  {
    phi=0.0;
    r=0.0;
  }
  else
  {
    phi=atan2(u[0],u[1]);
    theta=atan2(rhorizontal,u[2]);
    if(theta>=0.0)
        r=cos(theta)/(1.0+sin(theta));
    else
    {
        phi+=M_PI;
        if(phi>M_PI) phi -= (2.0*M_PI);
        theta=-theta;
        r=cos(theta)/(1.0+sin(theta));
    }
  }
}
void write_Wulffnet_data(PMTimeSeries& d,string outdir)
{
  try{
    string base_error("pm2wulffne - write_Wulffnet_data procedure:  ");
    int evid=d.get<long>("evid");
    string sta=d.get_string("sta");
    int band=d.get<int>("band");
    string path;
    stringstream ss;
    ss<<outdir<<"/"<<evid<<"_"<<sta<<"_"<<band<<".mat";
    ofstream ofs(ss.str());
    if(!ofs.is_open())
    {
      throw SeisppError(base_error+"open failed for file="+ss.str());
    }
    /* We dump the entire data vector - tcecut should be used to
    produce reasonable output */
    int i;
    for(i=0;i<d.ns;++i)
    {
      ParticleMotionEllipse pme=d.ellipse(i);
      ParticleMotionError pmerr=d.errors(i);
      double t=d.time(i);
      /* For now we only save major axis data and theta uncertainty */
      WulffData wd(pme.major);
      /* Angles are computed in radians - converted to degrees for output*/
      ofs << t<<" "<<deg(wd.phi)<<" "<<wd.r<<" "<<deg(pmerr.dtheta_major)<<endl;
    }
  }catch(...){throw;};
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    bool binary_data(true);
    if(argc<2) usage();
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    string outdir(argv[1]);
    /*Stop immediately if outdir is not writable */
    int iac;
    iac=access(outdir.c_str(),W_OK);
    if(iac)
    {
      cerr << "Directory="<<outdir<<" is not writable (may not exist)"<<endl
        <<"Create directory and make sure it is writable and try again"<<endl
        <<endl;
      exit(-1);
    }
    for(i=2;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="--help")
        {
            usage();
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
        PMTimeSeries d;
        int n(0);
        while(inp->good())
        {
            d=inp->read();
            try{
              write_Wulffnet_data(d,outdir);
            } catch(SeisppError& serr)
            {
              cerr << "Error writing data for object number "<<n<<endl
                << "The message posted follows:"<<endl;
            }
            ++n;
        }
        cerr << "Total number of PMTimeSeries written to "
          << outdir<<" is "<<n<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
