#include <stdlib.h>
#include <stdio.h>
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
    cerr << "extract_pm_attribute < in > out [-a attribute_type -mec xxx -v --help -text]"
        <<endl
        << "extracts one of a set of supported attributes from PMTimeSeries objects"<<endl
        << "writes output as stream of TimeSeries objects or (optionally) a TimeSeriesEnsemble"<<endl
        << " -a set the attribute to be extracted to output (default is major axis azimuth)"<<endl
        << "(attribute_type must be one of:  major_axis_amplitude, minor_axis_amplitude, "<<endl
        << " major_azimuth, minor_azimuth, major_inclination, minor_inclination, or rectilinearity"<<endl
        << "-mec - optionally mask sections with error estimate for attribute "
        << "larger than xxx as data gap in output"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl
        << "Units note:  amplitude errors are db relative to avergae"<<endl
        << " angles are stored in PMTimeSeries in radians but argument is assumed in degrees"<<endl
        << " rectilinearity is intrinsicially nondimensional and so is error"
        <<endl;
    exit(-1);
}
enum PMAttributeType {MajAmp,MinAmp,MajAz,MinAz,MajInc,MinInc,Rect};
PMAttributeType GetPMAType(string name)
{
  PMAttributeType pmat;
  if(name=="major_axis_amplitude")
    pmat=MajAmp;
  else if(name=="minor_axis_amplitude")
    pmat=MinAmp;
  else if(name=="major_azimuth")
    pmat=MajAz;
  else if(name=="minor_aximuth")
    pmat=MinAz;
  else if(name=="major_inclination")
    pmat=MajInc;
  else if(name=="minor_inclination")
    pmat=MinInc;
  else if(name=="rectilinearity")
    pmat=Rect;
  else
  {
    cerr << "Unrecognized attribute_type="<<name<<endl;
    usage();
  }
  return pmat;
}
/* This little procedure adds gaps for sections with for which
 * the error estimate for the attribute exceeds threshold cutoff.  
 * Note we first test for existing gaps and only add a new one if
 * we are in section currently defined good. */
int mask_high_error_sections(PMTimeSeries& d, TimeSeries& dattr, 
        PMAttributeType pmat, double cutoff)
{
  try{
      int i;
      /* Windows are made 1/2 sample larger than marked edges 
       * to avoid roundoff errors */
      double dto2=(d.dt)/2.0;
      /* This loop assumes d and dattr both have d.ns samples AND
       * that dattr inherits the gaps definitions from d*/
      TimeWindow newgap;
      bool in_new_gap(false);
      for(i=0;i<d.ns;++i)
      {
        if(d.is_gap(i))
        {
            if(in_new_gap)
            {
                /* We terminate a new gap in this case when we 
                 * encounter a new gap.  Do this because the
                 * gaps intervals cannot overlap.*/
                newgap.end=d.time(i=1)+dto2;
                dattr.add_gap(newgap);
                in_new_gap=false;
                //DEBUG
                cerr << "Added gap for high error from "<<newgap.start
                    << " to "<<newgap.end<<endl;
            }
            else
            {
                continue;
            }
        }
        /* Get a copy of the error set to avoid ugly constructs */
        ParticleMotionError pme=d.errors(i);
        /* Use this switch to avoid repetitious code for each
         * case option.  Also note what defines good or bad 
         * can be either larger or small than the cutoff depending
         * on the attribute being handled */
        bool add_this_as_gap(false);
        switch(pmat)
        {
          case MajAmp:
            if(pme.dmajornrm<cutoff) add_this_as_gap=true;
            break;
          case MinAmp:
            if(pme.dminornrm<cutoff) add_this_as_gap=true;
            break;
          case MajAz:
            if(pme.dphi_major>cutoff) add_this_as_gap=true;
            break;
          case MinAz:
            if(pme.dphi_minor>cutoff) add_this_as_gap=true;
            break;
          case MajInc:
            if(pme.dtheta_major>cutoff) add_this_as_gap=true;
            break;
          case MinInc:
            if(pme.dtheta_minor>cutoff) add_this_as_gap=true;
            break;
          case Rect:
            if(pme.delta_rect>cutoff) add_this_as_gap=true;
            break;
          default:
            throw SeisppError("mask_high_error_section - passed illegal value for PMAttributeType argument");

        };
        if(in_new_gap)
        {
            /* This is executed when we hit a new good period or 
             * we hit the end of the data vector */
            if( (!add_this_as_gap) || (i>=d.ns))
            {
                newgap.end=d.time(i-1)+dto2;
                dattr.add_gap(newgap);
                in_new_gap=false;
                //DEBUG
                cerr << "Added gap for high error from "<<newgap.start
                    << " to "<<newgap.end<<endl;
            }
        }
        else
        {
            if(add_this_as_gap)
            {
                newgap.start=d.time(i)-dto2;
                in_new_gap=true;
            }
        }
      }
  }catch(...){throw;};
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    PMAttributeType pmat(MajAz);
    bool binary_data(true);
    bool use_error_cutoff(false);
    double error_cutoff(-99.0);
    if(argc>1)
      if(string(argv[1])=="--help") usage();

    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-a")
        {
            ++i;
            if(i>=argc)usage();
            pmat=GetPMAType(string(argv[i]));
        }
        else if(sarg=="-mec")
        {
            ++i;
            if(i>=argc)usage();
            use_error_cutoff=true;
            error_cutoff=atof(argv[i]);
            /* Argument is conveniently given in degrees, but all
             * angle terms are internally radians - hence conversion*/
            switch(pmat)
            {
              case MajAz:
              case MinAz:
              case MajInc:
              case MinInc:
                error_cutoff=rad(error_cutoff);
            };
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
      char form('t');
      if(binary_data) form='b';
      StreamObjectReader<PMTimeSeries> inp(form);
      StreamObjectWriter<TimeSeries> outp(form);
      PMTimeSeries d;
      TimeSeries dout;
      while(inp.good())
      {
        d=inp.read();
        switch(pmat)
        {
          case MajAmp:
            dout=d.major_axis_amplitude();
            break;
          case MinAmp:
            dout=d.minor_axis_amplitude();
            break;
          case MajAz:
            dout=d.major_azimuth();
            break;
          case MinAz:
            dout=d.minor_azimuth();
            break;
          case MajInc:
            dout=d.major_inclination();
            break;
          case MinInc:
            dout=d.minor_inclination();
            break;
          case Rect:
            dout=d.rectilinearity();
            break;

        };
        int ngaps;
        ngaps=mask_high_error_sections(d,dout,pmat,error_cutoff);
        if(SEISPP_verbose)
        {
            cerr<< "extract_pm_attributes:  added "<<ngaps
                << " gap sections for high uncertainty time periods"
                <<endl;
        }
        outp.write(dout);
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
