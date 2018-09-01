/* This is a special purpose program to rotate 3C data for the dugl
   experiment at homestake.   Uses a straight line ray approximation
   that would normally be a dumb idea.  Also has hard wired station
   list of surface stations that are handled differently.  That is underground
   sites rotate to P aligned to straight ray, radial perpendicular in the
   ray vertical plan, and transverse the horizontal perpendicular to that
   plane.  For surface sites only the horizontals are rotate. */
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
    cerr << "rotate [-phi x -theta y -binary --help] < infile > outfile"
        <<endl
        << "infile and outfile are a single ThreeComponentEnsemble boost serialization file"<<endl
        << "Default rotates coordinates to LRT defined by computed normal vector between source and receiver"<<endl
        << "(computed from metadaa rx,ry,relev, sx,sy,and selev - local coordinates)"<<endl
        << "To set the angles that define the LRT transformation use the -phi and -theta parameters"<<endl
        << "(phi and theta are spherical coordinate angles in degrees)"<<endl
        <<endl;
    exit(-1);
}
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    bool compute_from_coordinates(true);
    double phi(-99999.9),theta(-99999.9);
    bool binary_data(false);
    int i;
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-phi")
        {
            ++i;
            if(i>=argc)usage();
            phi=atof(argv[i]);
            compute_from_coordinates=true;
        }
        else if(sarg=="-theta")
        {
            ++i;
            if(i>=argc)usage();
            theta=atof(argv[i]);
            compute_from_coordinates=true;
        }
        else if(sarg=="-binary")
            binary_data=true;
        else
            usage();
    }
    if(!compute_from_coordinates)
    {
      if(theta<-180.0) usage();
      if(phi<-180.0) usage();
    }
    phi=rad(phi);
    theta=rad(theta);

    set<string>::iterator ssptr;
    try{
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
          if(compute_from_coordinates)
          {
            double r[3],s[3],nu[3];
            r[0]=dptr->get_double("rx");
            r[1]=dptr->get_double("ry");
            r[2]=dptr->get_double("relev");
            s[0]=dptr->get_double("sx");
            s[1]=dptr->get_double("sy");
            s[2]=dptr->get_double("selev");
            double offset(0.0);
            for(k=0;k<3;++k)
            {
                double dx=r[k]-s[k];
                nu[k]=dx;
                offset += dx*dx;
            }
            offset=sqrt(offset);
            for(k=0;k<3;++k) nu[k]=nu[k]/offset;
            dptr->rotate(nu);
          }
          else
          {
              SphericalCoordinate sc;
              sc.radius=1.0;
              sc.phi=phi;
              sc.theta=theta;
              dptr->rotate(sc);
          }
        }
        oa->write(d);
      }
    }catch(boost::archive::archive_exception const& e)
    {
        cerr << "Error in read or write of serialization object"<<endl
            << "This was the message posted:"<<endl
            << e.what()<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
       cerr << stexc.what()<<endl;
    }
}
