/* General purpose routine to rotate an ensemble.   Works in one
   of two modes depending on switch constant_transformation.  When true
   rotates to a specified orientation defined by spherical angles phi and
   theta (see ThreeComponentSeismogram rotate method for details).  When
   false the program will attempt to extract source and receiver coordinates
   from each seismogram to compute backazimuth.  Then the horizontals are
   rotated to radial-transverse.

   Does not currently support something like a P wave emergence angle formula
   assuming that is better done with the free-surface transformation method.
   May want to actually implement that as an option. */
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include "seispp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "ensemble.h"
using namespace std;   // most compilers do not require this
using namespace SEISPP;  //This is essential to use SEISPP library
void usage()
{
    cerr << "rotate [--help --binary -pf pffile] < infile > outfile"
        <<endl
        << "infile is one the concatenation of one or more ThreeComponentEnsemble objects"
        <<endl
        << "Use -pf to specify alternate parameter file to default rotate.pf"
        <<endl;
    exit(-1);
}
class RotateControl
{
  public:
    bool constant_transformation;
    double phi,theta;
    string stalatkey,stalonkey;
    string evlatkey,evlonkey;
    RotateControl(string pffile);
};
RotateControl::RotateControl(string pffile)
{
  Pf *pf;
  if(pfread(const_cast<char *>(pffile.c_str()),&pf))
    throw SeisppError("RotateControl:  pfread failed for "+pffile);
  try{
    Metadata md(pf);
    constant_transformation=md.get_bool("constant_transformation");
    if(constant_transformation)
    {
      phi=md.get_double("phi");
      theta=md.get_double("theta");
      /* convert both to radians */
      phi=rad(phi);
      theta=rad(theta);
    }
    else
    {
      phi=0.0;
      theta=0.0;
      stalatkey=md.get_string("station_latitude_key");
      stalonkey=md.get_string("station_longitude_key");
      evlatkey=md.get_string("event_latitude_key");
      evlonkey=md.get_string("event_longitude_key");
    }
  }catch(...){throw;};
}

bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
  int i;
  string pffile("rotate");
  bool binary_data(false);
  for(i=1;i<argc;++i)
  {
    string sarg(argv[i]);
    if(sarg=="-pf")
    {
      ++i;
      if(i>=argc) usage();
      pffile=string(argv[i]);
    }
    else if(sarg=="-binary")
      binary_data=true;
    else if(sarg=="--help")
      usage();
    else
      usage();
  }
  try{
      RotateControl rc(pffile);
      SphericalCoordinate ctsc;
      ctsc.radius=1.0;
      ctsc.phi=rc.phi;
      ctsc.theta=rc.theta;
      shared_ptr<StreamObjectReader<ThreeComponentEnsemble>> ia;
      if(binary_data)
      {
          ia=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>('b'));
      }
      else
      {
          ia=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>);
      }
      shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>> oa;
      if(binary_data)
      {
          oa=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>('b'));
      }
      else
      {
          oa=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>);
      }
      ThreeComponentEnsemble d;
      while(!ia->eof())
      {
        d=ia->read();
        vector<ThreeComponentSeismogram>::iterator dptr;
        int k;
        for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
        {
          double slat,slon,evlat,evlon;
          double az,delta;
          if(rc.constant_transformation)
          {
            dptr->rotate(ctsc);
          }
          else
          {
            slat=dptr->get_double(rc.stalatkey);
            slon=dptr->get_double(rc.stalonkey);
            evlat=dptr->get_double(rc.evlatkey);
            evlon=dptr->get_double(rc.evlonkey);
            dist(slat,slon,evlat,evlon,&delta,&az);
            dptr->rotate(az+M_PI_2);
          }
        }
        oa->write(d);
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
