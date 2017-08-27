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
    cerr << "extract_pm_attribute < in > out [-a attribute_type -v --help -text]"
        <<endl
        << "extracts one of a set of supported attributes from PMTimeSeries objects"<<endl
        << "writes output as stream of TimeSeries objects or (optionally) a TimeSeriesEnsemble"<<endl
        << " -a set the attribute to be extracted to output (default is major axis azimuth)"<<endl
        << "(attribute_type must be one of:  major_axis_amplitude, minor_axis_amplitude, "<<endl
        << " major_azimuth, minor_azimuth, major_inclination, minor_inclination, or rectilinearity"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
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
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    PMAttributeType pmat(MajAz);
    bool binary_data(true);
    if(argc>1)
      if(string(argv[1])=="--help") usage();

    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-o")
        {
            ++i;
            if(i>=argc)usage();
            pmat=GetPMAType(string(argv[i]));
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

