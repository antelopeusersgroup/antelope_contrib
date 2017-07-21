#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "coords.h"
#include "PMTimeSeries.h"
#include "seispp.h"
#include "ThreeComponentSeismogram.h"
#include "ensemble.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "set_offset < in > out [-v --help -binary]"
        <<endl
        << "sets some standard measures of source-receiver offset"<<endl
        << "Also sets receiver to source back azimuth"<<endl
        << " -km - use Cartesian distance in km to set offset from rx,ry,sx, and sy"<<endl
        << " (Default is distance in degrees computed form site.lat, site.long, origin.lat, and origin.lon)"
        <<endl
        << " -t - object type"<<endl
        << " (allowed option=TimeSeries, ThreeComponentSeismogram (default), and ParticleMotionTimeSeries)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -binary - switch to binary input and output (default is text)"
        <<endl;
    exit(-1);
}
enum AllowedObjects {TCS, TCE, PMTS, TS, TSE};
AllowedObjects get_object_type(string otype)
{
    if(otype=="ThreeComponentSeismogram")
        return TCS;
    else if(otype=="TimeSeries")
        return TS;
    else if(otype=="ParticleMotionTimeSeries")
        return PMTS;
    else
    {
        cerr << "Do not know how to handle object type="<<otype
            <<endl<< "Cannot continue"<<endl;
        exit(-1);
    }
}
const string rlon("site.lon");
const string rlat("site.lat");
const string slat("origin.lat");
const string slon("origin.lon");
const string receiver_x("rx");
const string receiver_y("ry");
const string source_x("sx");
const string source_y("sy");
const string cartesian_offset("offset");
const string geo_offset("delta");
const string baz("baz");   // back azimuth - same for all
/* This will return count of total objects processes as first and total
that failed with errors as second.*/
template <class T> pair<int,int> set_offset(bool binary_data, bool use_cartesian)
{
  try{
    char form('t');
    if(binary_data)
      form='b';
    StreamObjectReader<T> inp(form);
    StreamObjectWriter<T> outp(form);
    int n_processed(0),n_errors(0);

    while(inp.good())
    {
      T d;
      double sx,sy,rx,ry,offset,az;
      d=inp.read();
      try{
        if(use_cartesian)
        {
          sx=d.get_double(source_x);
          sy=d.get_double(source_y);
          rx=d.get_double(receiver_x);
          ry=d.get_double(receiver_y);
          offset=sqrt((sx-rx)*(sx-rx) + (sy-ry)*(sy-ry));
          d.put(cartesian_offset,offset);
          az=atan2(sy-ry,sx-rx);
          d.put(baz,deg(az));
        }
        else
        {
          sx=d.get_double(slon);
          sy=d.get_double(slat);
          rx=d.get_double(rlon);
          ry=d.get_double(rlat);
          rx=rad(rx); ry=rad(ry);
          sx=rad(sx); sy=rad(sy);
          dist(ry,rx,sy,sx,&offset,&az);
          d.put(geo_offset,deg(offset));
          d.put(baz,deg(az));
        }
      }catch(...)
      {
        if(SEISPP_verbose) cerr << "Error processing seismogram number "<<n_processed<<endl;
        ++n_errors;
      }
      ++n_processed;
      outp.write(d);
    }
    pair<int,int> counts(n_processed,n_errors);
    return counts;
  }catch(...){throw;};
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    bool use_cartesian(false);
    bool binary_data(false);
    string otype("ThreeComponentSeismogram");
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-km")
        {
            use_cartesian=true;
        }
        else if(sarg=="-binary")
        {
            binary_data=true;
        }
        else if(sarg=="-t")
        {
          ++i;
          if(i>=argc)usage();
          otype=string(argv[i]);
        }
        else if(sarg=="-v")
          SEISPP_verbose=true;
        else
            usage();
    }
    try{
        AllowedObjects objt;
        objt=get_object_type(otype);
        pair<int,int> counts;
        switch(objt)
        {
          case TCS:
            counts=set_offset<ThreeComponentSeismogram>(binary_data,use_cartesian);
            break;
          case TS:
            counts=set_offset<TimeSeries>(binary_data,use_cartesian);
            break;
          case PMTS:
            counts=set_offset<PMTimeSeries>(binary_data,use_cartesian);
            break;
          default:
            cerr << "coding error:  cannot handle object type="<<otype<<endl;
            exit(-1);  // This should never happen
        };
        if(counts.second>0)
        {
          cerr <<"set_offset(WARNING):  processing failed on "<<counts.second
            << " seismograms"<<endl
            << "Total number of seismograms processed ="<<counts.first<<endl;
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
