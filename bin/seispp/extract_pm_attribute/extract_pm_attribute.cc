#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "AttributeTextWriter.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "extract_pm_attribute < in [(-dir outdir | -o outfile) -a attribute_type -mec "<<endl
        << "-save_error xxx -t0shift -file_string_key skey -file_integer_key ikey -v --help -text]"
        <<endl
        << "extracts one of a set of supported attributes from PMTimeSeries objects"<<endl
        << "Results are written as either a set of text files in directory defined by -dir "<<endl
        << "or a serialized set of TimeSeries objects written to outfile (-o option)"<<endl
        << "Type of output is controlled by whether or not the -dir or -o option appears"<<endl
        << "-dir implies text out put while -o implies serialized TimeSeries data stream"<<endl
        << "(text output order is:  time, value, lowerror, higherror) - natural input for errorlines of gnuplot"
        <<endl
        << " -a set the attribute to be extracted to output (default is major axis azimuth)"<<endl
        << "Attribute_type must be one of:  major_axis_amplitude, minor_axis_amplitude, "<<endl
        << " major_azimuth, minor_azimuth, major_inclination, minor_inclination, rectilinearity"<<endl
        << " -save_error - flag used to output error estimate of an attribute instead of the values"<<endl
        << "(e.g. -a major_azimuth -save_error would write estimated errors for major axis azimuth)"
        <<endl
        << "Note this option is ignored for text output as the error data is always written to output then"<<endl
        << "-mec - optionally mask sections with error estimate for attribute "
        << "larger than xxx as data gap in output"<<endl
        << "(Exit with an error if -mec is used with -save_error)"<<endl
        << " -save_error - use this option to write a TimeSeries of one of the error attributes"<<endl
        << "  (Note: this flag is ignored if the -dir option is used)"<<endl
        << " -t0shift - shift all output by wavelet duratio/2 to align attribute times to leading edge of wavelet"<<endl
        << "  (default puts time reference at center of each wavelet)"<<endl
        << " -file_string_key - set string key used to define unique file names for text files (default is sta)"<<endl
        << " -file_integer_key - set key for integer variable used to define unique text file name (default evid)"<<endl
        << " (Note -file_string_key and -file_integer_key are ignored when output is TimeSeries objects)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl
        << "Units note:  amplitude errors are db relative to average"<<endl
        << " angles are stored in PMTimeSeries in radians but argument is assumed in degrees"<<endl
        << " rectilinearity is intrinsicially nondimensional and so are error estimates"
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
  else if(name=="minor_azimuth")
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
/* builds a file name to save text format for use by matlab, gnuplot, or gmt. */
string build_filename(Metadata& d,PMAttributeType pmat,string skey, string ikey)
{
    string atype;
    switch(pmat)
    {
      case MajAmp:
          atype="MajAmp";
          break;
      case MinAmp:
          atype="MinAmp";
          break;
      case MajAz:
          atype="MajAz";
          break;
      case MinAz:
          atype="MinAz";
          break;
      case MajInc:
          atype="MajInc";
          break;
      case MinInc:
          atype="MinInc";
          break;
      case Rect:
          atype="Rect";
          break;
    };
    /* variable names reflect default - args can change actual values */
    string sta=d.get_string(skey);
    long evid=d.get<long>(ikey);
    /* This is frozen for mwpm data */
    int band=d.get<int>("band");
    stringstream ss;
    ss<<atype<<"_"<<evid<<"_"<<sta<<"_"<<band;
    string fname(ss.str());
    return fname;
}
/* This little procedure adds gaps for sections with for which
 * the error estimate for the attribute exceeds threshold cutoff.
 * Note we first test for existing gaps and only add a new one if
 * we are in section currently defined good. */
int mask_high_error_sections(PMTimeSeries& d, TimeSeries& dattr,
        PMAttributeType pmat, double cutoff)
{
  try{
      int number_added(0);
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
            if(i==0)
            {
                newgap.start=d.time(0)-dto2;
            }
            else if(in_new_gap)
            {
                /* We terminate a new gap in this case when we
                 * encounter a new gap.  Do this because the
                 * gaps intervals cannot overlap.*/
                newgap.end=d.time(i-1)+dto2;
                dattr.add_gap(newgap);
                ++number_added;
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
                ++number_added;
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
      return number_added;
  }catch(...){throw;};
}
TimeSeries extract_attribute_error_data(PMTimeSeries& d,PMAttributeType pmat)
{
  try{
    /* This clones header but does not create a data vector */
    TimeSeries derror(dynamic_cast<Metadata&>(d),false);
    derror.s.reserve(d.ns);
    derror.live=d.live;
    if(derror.live)
    {
      const double ERROR_UNDEFINED(-9.9999e99);
      derror.dt=d.dt;
      derror.tref=d.tref;
      derror.t0=d.t0;
      derror.ns=d.ns;
      int i;
      for(i=0;i<d.ns;++i)
      {
        ParticleMotionError perr;
        if(d.is_gap(i))
        {
          derror.s.push_back(ERROR_UNDEFINED);
        }
        else
        {
          perr=d.errors(i);
          switch(pmat)
          {
          case MajAmp:
            derror.s.push_back(perr.dmajornrm);
            break;
          case MinAmp:
            derror.s.push_back(perr.dminornrm);
            break;
          case MajAz:
            derror.s.push_back(perr.dphi_major);
            break;
          case MinAz:
            derror.s.push_back(perr.dphi_minor);
            break;
          case MajInc:
            derror.s.push_back(perr.dtheta_major);
            break;
          case MinInc:
            derror.s.push_back(perr.dtheta_minor);
            break;
          case Rect:
            derror.s.push_back(perr.delta_rect);
            break;
          default:
            throw SeisppError("extract_attribute_error_data - passed illegal value for PMAttributeType argument");
          };
        }
      }
    }
    return derror;
  }catch(...){throw;};
}
/* A odd mismatch in the PMTimeSeries object is that the errors
 * for amplitude are computed in dB relative to the measured
 * amplitude.   However, we store the amplitude in physical units.
 * Hence, we thus have to convert (cautiously due to gaps) amplitude
 * data to db.  This procedure does this. */
TimeSeries amptodb(TimeSeries& d)
{
    TimeSeries ddb(d);
    int i;
    for(i=0;i<d.ns;++i)
    {
        if(d.is_gap(i))
        {
            ddb.s[i]=0.0;
        }
        else if(d.s[i]==0.0)
        {
            ddb.s[i]=-999.9;
        }
        else
        {
            ddb.s[i]=20.0*log10(d.s[i]);
        }
    }
    return ddb;
}
/* PMTimeSeries store angles in radians.  We need to convert to
 * degrees to produce rational plots*/
TimeSeries radian_to_degree(TimeSeries& d)
{
    TimeSeries ddeg(d);
    int i;
    for(i=0;i<d.ns;++i)
    {
        /* We don't worry about gaps just blindly convert all samples */
        ddeg.s[i]=deg(d.s[i]);
    }
    return ddeg;
}
/* procedure to remove wraparound problems for azimuths*/
void fix_wraparound(TimeSeries& d,TimeSeries& e)
{
    if(d.ns != e.ns)
    {
        cerr << "extract_pm_attributes fix_wraparound procedure:  "
            << "data ns="<<d.ns <<" but error vector ns="<<e.ns<<endl
            << "This should not happen and indicates a coding error."
            << "Exiting"<<endl;
        exit(-1);
    }
    int i;
    for(i=0;i<d.ns;++i)
    {
        float az0,az;
        az0=d.s[i];
        /* This assumes data were converted to degrees */
        az=regularize_angle(az0);
        if(az!=az0)
        {
            d.s[i]=az;
        }
    }
}

bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    PMAttributeType pmat(MajAz);
    bool binary_data(true);
    bool use_error_cutoff(false);
    bool save_error(false);
    double error_cutoff(-99.0);
    /* This is used as a switch for output format.  When true use serialization */
    bool TSOutputMode(false);
    string outdir(".");
    string outfile("BAD");
    bool apply_t0shift(false);
    /* New options to change file naming convention from default */
    string file_string_key("sta"),file_integer_key("evid");
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
        else if(sarg=="-dir")
        {
            ++i;
            if(i>=argc)usage();
            outdir=string(argv[i]);
            TSOutputMode=false;
        }
        else if(sarg=="-o")
        {
            ++i;
            if(i>=argc)usage();
            outfile=string(argv[i]);
            TSOutputMode=true;
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
        else if(sarg=="-save_error")
        {
          save_error=true;
        }
        else if(sarg=="-t0shift")
            apply_t0shift=true;
        else if(sarg=="-file_string_key")
        {
          ++i;
          if(i>=argc)usage();
          file_string_key=string(argv[i]);
        }
        else if(sarg=="-file_integer_key")
        {
          ++i;
          if(i>=argc)usage();
          file_integer_key=string(argv[i]);
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

    if(save_error && use_error_cutoff)
    {
      cerr << "extract_pm_attribute:  illegal argument combination"<<endl
        << "Cannot specify both -mec and -save_error"<<endl;
      usage();
    }
    try{
      char form('t');
      if(binary_data) form='b';
      StreamObjectReader<PMTimeSeries> inp(form);
      StreamObjectWriter<TimeSeries> *outp;
      if(TSOutputMode)
      {
          outp=new StreamObjectWriter<TimeSeries>(outfile,form);
      }
      /* Always create this thing as the overhead is low*/
      AttributeTextWriter atw(outdir,true);
      PMTimeSeries d;
      TimeSeries dout;
      TimeSeries derr;
      while(inp.good())
      {
        int number_saved;
        d=inp.read();
        /* We time shift the PMTimeSeries data then the TimeSeries
         * used for output will also be shifted. */
        if(apply_t0shift)
        {
            double t0shift;
            t0shift=d.get_wavelet_duration();
            t0shift/=2.0;  // shift is by half length
            d.shift(-t0shift);
        }
        derr=extract_attribute_error_data(d,pmat);
        switch(pmat)
        {
          case MajAmp:
            dout=d.major_axis_amplitude();
            dout=amptodb(dout);
            break;
          case MinAmp:
            dout=d.minor_axis_amplitude();
            dout=amptodb(dout);
            break;
          case MajAz:
            dout=d.major_azimuth();
            dout=radian_to_degree(dout);
            derr=radian_to_degree(derr);
            fix_wraparound(dout,derr);
            break;
          case MinAz:
            dout=d.minor_azimuth();
            dout=radian_to_degree(dout);
            derr=radian_to_degree(derr);
            fix_wraparound(dout,derr);
            break;
          case MajInc:
            dout=d.major_inclination();
            dout=radian_to_degree(dout);
            derr=radian_to_degree(derr);
            break;
          case MinInc:
            dout=d.minor_inclination();
            dout=radian_to_degree(dout);
            derr=radian_to_degree(derr);
            break;
          case Rect:
            dout=d.rectilinearity();
            break;
         };
         int ngaps;
         if(use_error_cutoff)
         {
             ngaps=mask_high_error_sections(d,dout,pmat,error_cutoff);
             if(SEISPP_verbose)
             {
                cerr<< "extract_pm_attributes:  added "<<ngaps
                    << " gap sections for high uncertainty time periods"
                    <<endl;
             }
         }
         if(TSOutputMode)
         {
            if(save_error)
                outp->write(derr);
            else
                outp->write(dout);
         }
         else
         {
             /*This creates the file name */
             string fname=build_filename(d,pmat,file_string_key,file_integer_key);
             number_saved=atw.write(fname,dout,derr);
             if(number_saved<=0)
             {
                 cerr << "extract_pm_attributes:  no data saved for file="
                     <<fname<<endl
                     <<"All samples were marked as gaps"<<endl
                     << "If you ran mask_pm_snr likely means low snr"
                     <<endl;
             }
             else if(SEISPP_verbose)
             {
                 cerr << "extract_pm_attributes:  wrote "<<number_saved
                     <<" particle motion attribute estimates to file "
                     <<fname<<endl;
             }
         }
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
