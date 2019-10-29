/* This is a program to rotate data to a simple form of LQT transformation
   based on a straight line approximation.   Optionally a constant 
   transformation can be applied with angles to define the L direction. */
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
    cerr << "rotate [-phi x -theta y -accumulate -text --help] < infile > outfile"
        <<endl
        << "Default rotates coordinates to LRT defined by computed normal vector between source and receiver"<<endl
        << "(computed from metadaa rx,ry,relev, sx,sy,and selev - local coordinates)"<<endl
        << "To set the angles that define the LRT transformation use the -phi and -theta parameters"<<endl
        << "(phi and theta are spherical coordinate angles in degrees)"<<endl
        << "-accumulate transforms data without checking current orientation"
        <<endl <<"(Default forces data to cardinal directions before applying transformation)"
        << " -text - switch to text input and output (default is binary)"<<endl
        << "--help will print this usage message"<<endl
        << "infile and outfile are a ThreeComponentEnsemble boost serialization files"
        <<endl;
    exit(-1);
}
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    bool compute_from_coordinates(true);
    double phi(-99999.9),theta(-99999.9);
    bool binary_data(true);
    bool accum_mode(false);
    int i;
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-phi")
        {
            ++i;
            if(i>=argc)usage();
            phi=atof(argv[i]);
            compute_from_coordinates=false;
        }
        else if(sarg=="-theta")
        {
            ++i;
            if(i>=argc)usage();
            theta=atof(argv[i]);
            compute_from_coordinates=false;
        }
        else if(sarg=="-accumulate")
            accum_mode=true;
        else if(sarg=="-text")
            binary_data=false;
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
          //Drop data marked dead 
          if(!dptr->live) continue;
          if(!accum_mode)
          {
            if(!dptr->components_are_cardinal) 
              dptr->rotate_to_standard();
          }
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
