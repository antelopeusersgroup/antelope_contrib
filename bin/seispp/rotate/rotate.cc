/* General purpose routine to rotate an ensemble.   Works in one
   of two modes depending on switch constant_transformation.  When true
   rotates to a specified orientation defined by spherical angles phi and
   theta (see ThreeComponentSeismogram rotate method for details).  When
   false the program will attempt to extract source and receiver coordinates
   from each seismogram to compute backazimuth.  Then the horizontals are
   rotated to radial-transverse.

   Free-surface transformation option is available with parameters coming
   through the parameter file. */
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include "seispp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "ensemble.h"
#include "Hypocenter.h"
using namespace std;   // most compilers do not require this
using namespace SEISPP;  //This is essential to use SEISPP library
void usage()
{
    cerr << "rotate [-accumulate --help --text -pf pffile] < infile > outfile"
        <<endl
        << "General purpose three-component rotation processor"
        <<endl
        << "-accumulate transforms data without checking current orientation"
        << endl <<"(Default forces data to cardinal directions before applying transformation)"
        <<endl

        << "Use -pf to specify alternate parameter file to default rotate.pf"
        <<endl
        << "(Options are controlled by this file)"<<endl
        << "Default is simple rotation in horizontal plane to ZRT"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl
        << "--help will print this usage message"<<endl
        << "infile and outfile are a single ThreeComponentEnsemble boost serialization file"
        <<endl;
    exit(-1);
}
enum RotateMode {Fixed,ZRT,LQT,FST};
class RotateControl
{
  public:
    RotateControl(string pffile);
    /* When true use the same angle for all data */
    bool constant_transformation;
    /* When true get event metadata from ensemble metadata not 
     * trace data. This is a valid approximation only form small
     * arrays */
    bool saamode;
    double phi,theta;
    string stalatkey,stalonkey;
    string evlatkey,evlonkey,evdepthkey;
    string ttmethod,ttmodel;
    RotateMode mode;
    /* Surface P and S needed for or LQT */
    double vp0,vs0;
    /* When true LQT and FST modes will use S slowness.  Default is P*/
    bool use_S_slowness;
};
RotateControl::RotateControl(string pffile)
{
  const string base_error("RotateControl constructor:  ");
  Pf *pf;
  if(pfread(const_cast<char *>(pffile.c_str()),&pf))
    throw SeisppError(base_error+"pfread failed for "+pffile);
  try{
    Metadata md(pf);
    string modedef=md.get_string("rotation_type");
    constant_transformation=false;
    if(modedef=="constant")
    {
      constant_transformation=true;
      phi=md.get_double("phi");
      theta=md.get_double("theta");
      /* convert both to radians */
      phi=rad(phi);
      theta=rad(theta);
    }
    else if(modedef=="ZRT")
      mode=ZRT;
    else if(modedef=="LQT")
      mode=LQT;
    else if(modedef=="FST")
      mode=FST;
    else
      throw SeisppError(base_error 
          + "Illegal entry in parameter file for rotation_type="+modedef);
    saamode=md.get_bool("small_aperture_array_mode");
    if(saamode && constant_transformation)
    {
      cerr << "rotate (WARNING):  pf file specifies constant transformation "
        << "and small aperture array mode"<<endl
        << "Turning off small aperture array mode"<<endl;
      saamode=false;
    }
    string nullstring("");
    if(constant_transformation)
    {
      phi=md.get_double("phi");
      theta=md.get_double("theta");
      /* convert both to radians */
      phi=rad(phi);
      theta=rad(theta);
      stalatkey=nullstring;
      stalonkey=nullstring;
      evlatkey=nullstring;
      evlatkey=nullstring;
      evdepthkey=nullstring;
      ttmethod=nullstring;
      ttmodel=nullstring;
    }
    else
    {
      phi=0.0;
      theta=0.0;
      stalatkey=md.get_string("station_latitude_key");
      stalonkey=md.get_string("station_longitude_key");
      evlatkey=md.get_string("event_latitude_key");
      evlonkey=md.get_string("event_longitude_key");
      evdepthkey=md.get_string("event_depth_key");
      ttmethod=md.get_string("ttmethod");
      ttmodel=md.get_string("ttmodel");
      vp0=md.get_double("vp0");
      vs0=md.get_double("vs0");
    }
    use_S_slowness=md.get_bool("use_S_slowness");
  }catch(...){throw;};
}
SlownessVector ComputeSlownessVector(double stalat,double stalon,
    double evlat,double evlon,double evdepth, RotateControl& rc)
{
  /* Use this for origin time - times are not important to us 
     so it can be bogus */
  double otime(0.0);
  /* Assume in this procedure lat and lons are in degrees */
  Hypocenter h(rad(evlat),rad(evlon),evdepth,otime,rc.ttmethod,rc.ttmodel);
  SlownessVector u;
  if(rc.use_S_slowness)
    u=h.phaseslow(rad(stalat),rad(stalon),0.0,string("S"));
  else
    u=h.pslow(rad(stalat),rad(stalon),0.0);
  return u;
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
  int i;
  string pffile("rotate");
  bool binary_data(true);
  bool accum_mode(false);
  for(i=1;i<argc;++i)
  {
    string sarg(argv[i]);
    if(sarg=="-pf")
    {
      ++i;
      if(i>=argc) usage();
      pffile=string(argv[i]);
    }
    else if(sarg=="-text")
      binary_data=false;
    else if(sarg=="-accumulate")
      accum_mode=true;
    else if(sarg=="--help")
      usage();
    else if(sarg=="-v")
      SEISPP_verbose=true;
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
      int nensembles(0);
      while(!ia->eof())
      {
        d=ia->read();
        vector<ThreeComponentSeismogram>::iterator dptr;
        double slat,slon,evlat,evlon,evdep;
        int k;
        if(rc.saamode)
        {
           evlat=d.get_double(rc.evlatkey);
           evlon=d.get_double(rc.evlonkey);
           evdep=d.get_double(rc.evdepthkey);
        }
        for(dptr=d.member.begin(),k=0;dptr!=d.member.end();++dptr,++k)
        {
          if(!accum_mode)
          {
            if(!dptr->components_are_cardinal)
              dptr->rotate_to_standard();
          }
          double az,delta;
          if(rc.constant_transformation)
          {
            dptr->rotate(ctsc);
          }
          else
          {
            slat=dptr->get_double(rc.stalatkey);
            slon=dptr->get_double(rc.stalonkey);
            if(!rc.saamode)
            {
              evlat=dptr->get_double(rc.evlatkey);
              evlon=dptr->get_double(rc.evlonkey);
              evdep=dptr->get_double(rc.evdepthkey);
            }
            dist(rad(slat),rad(slon),rad(evlat),rad(evlon),&delta,&az);
            /* Azimuth, az, is backazimuth but all the rotation methods
               in libseispp use propagation azimuth so convert to that
               form */
            az += M_PI;
            if(az>(2.0*M_PI)) az -= (2.0*M_PI);
            SlownessVector u;
            double umag,ema,vtimesu;
            SphericalCoordinate sc;
            switch(rc.mode)
            {
              case ZRT:
                dptr->rotate(az);
                break;
              case LQT:
                u=ComputeSlownessVector(slat,slon,evlat,evlon,evdep,rc);
                umag=u.mag();
                if(rc.use_S_slowness)
                {
                  vtimesu=rc.vs0*umag;
                  if(vtimesu>1.0)
                  {
                    cerr << "Warning:  LQT vs0*slowness > 1 - assuming horizontal propagation"<<endl;
                    ema=M_PI_2;
                  }
                  else
                  {
                    ema=asin(vtimesu);
                  }
                }
                else
                {
                  vtimesu=rc.vp0*umag;
                  if(vtimesu>1.0)
                  {
                    cerr << "Warning:  LQT vp0*slowness > 1 - assuming vertical incidence"<<endl;
                    ema=0.0;
                  }
                  ema=asin(rc.vp0*umag);
                }
                sc.radius=1.0;
                /* Spherical coordinates are angles from x1 axis but 
                   the slowness vector returns azimuth from north -convert*/
                sc.phi=M_PI_2-u.azimuth();
                sc.theta=ema;
                dptr->rotate(sc);
                break;
              case FST:
                u=ComputeSlownessVector(slat,slon,evlat,evlon,evdep,rc);
                dptr->free_surface_transformation(u,rc.vp0,rc.vs0);
                break;
              default:
                cerr << "rotate:  coding error - this should never be executed"
                  <<endl;
                exit(-1);
            };
            /* az is backazimuth but rotate needs amount y should rotate
             * to be R = azimuth so add pi */
            dptr->rotate(az+M_PI);
            if(SEISPP_verbose)
            {
                cerr << "Ensemble index="<<nensembles<<" Member index="<<k<<endl
                    <<"Station coordinates(degrees):  "<<slat<<" "<<slon<<endl
                    <<"Event coordinates (degrees):  "<<evlat<<" "<<evlon<<endl
                    <<"Rotation angle (degrees)="<<deg(az+M_PI)<<endl;
            }
          }
        }
        oa->write(d);
        ++nensembles;
      }
      if(SEISPP_verbose) cerr << "rotate:  processed "<<nensembles+1
          << " ensembles"<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
       cerr << stexc.what()<<endl;
    }
}
