#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <fstream>
#include <ostream>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include "seispp.h"
#include "ensemble.h"
#include "filter++.h"
#include "SourceData.h"
#include "GapDefinition.h"
#include "HFArray.h"
using namespace std;   // most compilers do not require this
using namespace SEISPP;  //This is essential to use SEISPP library
void usage()
{
    cerr << "dbactive_reader db [-g goodtimes -c coords -pf pffile] < shottimes"
        <<endl
        << "Constructs ensembles in a manner similar to db2segy for 3C data"<<endl
       << "stdout is boost serialized ThreeComponentSeismogram objects"<<endl
       << " (Always writes log file dbactive_reader.log)"<<endl
       << " db - is input antelope database descriptor" <<endl
       << " shottimes - is file identical to input for for db2segy with shot coordinates"<<endl
       << " -g - optional file of marked times when array stations have usable data"<<endl
       << " -c - construct HFArray object from file coords instead of db (default) " <<endl
       << " -pf - use alternate parameter file name pf"<<endl;
    exit(-1);
}
TimeWindow GetTimeRange(vector<SourceData>& sdv)
{
    double ts,te;
    vector<SourceData>::iterator dptr;
    dptr=sdv.begin();
    ts=dptr->time;
    te=ts;
    ++dptr;
    for(dptr=sdv.begin();dptr!=sdv.end();++dptr)
    {
        double t=dptr->time;
        if(t<ts) ts=t;
        if(t>te) te=t;
    }
    return TimeWindow(ts,te);
}
void load_precision_coordinates(ThreeComponentEnsemble& d,
        HFArray& array)
{
    try{
        vector<ThreeComponentSeismogram>::iterator dptr;
        /* We put this a global for all ensembles - origin of the
        local coordinate system */
        Geographic_point gp;
        gp=array.origin();
        /* Stored internally in radians, but since this is an export
        we need to put these in degrees */
        d.put("lat0",deg(gp.lat));
        d.put("lon0",deg(gp.lon));
        d.put("r0",gp.r);
        d.put("elev0",gp.r-r0_ellipse(gp.lat));
        for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
        {
            string sta=dptr->get_string("sta");
            vector<double> sloc=array.x(sta);
            dptr->put("rx",sloc[0]);
            dptr->put("ry",sloc[1]);
            dptr->put("rz",sloc[2]);
            gp=array.geographic_location(sta);
            /* geo coords are internally radians, but we convert
            them as metadata back to degrees */
            dptr->put("site.lat",deg(gp.lat));
            dptr->put("site.lon",deg(gp.lon));
            dptr->put("site.r",gp.r);
            double elev=gp.r-r0_ellipse(gp.lat);
            dptr->put("site.elev",elev);
        }
    }catch(...){throw;};
}
ThreeComponentEnsemble ExtractWindowedData(ThreeComponentEnsemble& full_line,
  SourceData sdata,RegionalCoordinates coords, TimeWindow exwin)
{
  try{
    TimeWindow winthis(exwin);
    winthis=winthis.shift(sdata.time);

    /* Note it is important that the origin data for the array is stored
    in the ensemble metadata - done in load_precision_coordinates*/
    ThreeComponentEnsemble dout(dynamic_cast<Metadata&> (full_line),
            full_line.member.size());
    /* The source coordinates for this ensemble are constant so we
      compute them now and just post them to each windowed seismogram.
      Note this computation assumes lat and lon were converted to radians 
      on input.  Note we als have to convert elevation to km*/
    double elevkm=r0_ellipse(sdata.lat)+0.001*sdata.elev;
    Cartesian_point cp=coords.cartesian(sdata.lat,
                sdata.lon,elevkm);
    dout.put("ffid",sdata.ffid);
    dout.put("sx",cp.x1);  dout.put("sy",cp.x2);  dout.put("sz",cp.x3);
    dout.put("origin.lat",deg(sdata.lat));
    dout.put("origin.lon",deg(sdata.lon));
    dout.put("origin.elev",sdata.elev);
    dout.put("origin.time",sdata.time);
    //DEBUG
    cerr << sdata.ffid<<" "<<cp.x1<<" "<<cp.x2<<" "<<cp.x3<<endl;
    vector<ThreeComponentSeismogram>::iterator dptr;
    for(dptr=full_line.member.begin();dptr!=full_line.member.end();++dptr)
    {
      ThreeComponentSeismogram dw;
      /* Silently skip data with marked taps */
      if(!dptr->is_gap(winthis))
      {
        dw=WindowData(*dptr,winthis);
        /* This makes the timing relative to origin.time.  New api for
         * basic time series posts that time so that it stays with the 
         * object */
        dw.ator(sdata.time);
        /* We intentionally duplicate source coordinates with each seismogram */
        dw.put("sx",cp.x1);  dw.put("sy",cp.x2);  dw.put("sz",cp.x3);
        dw.put("origin.lat",deg(sdata.lat));
        dw.put("origin.lon",deg(sdata.lon));
        dw.put("origin.elev",sdata.elev);
        dw.put("origin.time",sdata.time);
        /* This computes radial distance offset not horizontal offset as
        is standard in reflections seismology.  To maximize utility we
        compute both though. */
        double offset,dx;
        double rx,ry,rz;
        rx=dw.get_double("rx");
        ry=dw.get_double("ry");
        rz=dw.get_double("rz");
        dx=rx-cp.x1;
        offset=dx*dx;
        dx=ry-cp.x2;
        offset+=dx*dx;
        dw.put("offset",sqrt(offset));
        dx=rz-cp.x3;
        offset += dx*dx;
        dw.put("radial_offset",sqrt(offset));
        dout.member.push_back(dw);
      }
    }
    return dout;
  }catch(...){throw;};
}
/* This small procedure uses the gap definition methods of BasicTimeSeries
to set gaps in the master ensemble.   This means windows extrated from the
master for individual shots will be killed by the extract procedure. */
void mask_bad(ThreeComponentEnsemble& d, GapDefinition& badtimes)
{
    try{
    vector<ThreeComponentSeismogram>::iterator dptr;
    for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
    {
        string sta=dptr->get_string("sta");
        vector<TimeWindow> badtws=badtimes.get(sta);
        int i;
        for(i=0;i<badtws.size();++i)
        {
            dptr->add_gap(badtws[i]);
        }
    }
    }catch(...){throw;};
}
template <class OutputObject> void write_object(OutputObject& d,
        boost::archive::text_oarchive& oa)
{
    try {
        oa << d;
    }catch(...)
    {
        throw SeisppError(string("write_object failed\n")
                +"Is serialization defined for this object type?\n"
                +"Do you have write permission for output directory?");
    }
}

bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    const int narg_required(2);
    if(argc<narg_required) usage();
    string dbname(argv[1]);
    string pffile("dbactive_reader.pf");
    bool mask_bad_data(false);
    string goodtimesfile;
    string coordfile("NotUsed");
    string logfile("dbactive_reader.log");

    for(i=narg_required;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-g")
        {
            ++i;
            if(i>=argc)usage();
            goodtimesfile=string(argv[i]);
            mask_bad_data=true;
        }
        else if(sarg=="-c")
        {
          ++i;
          if(i>=argc)usage();
          coordfile=string(argv[i]);
        }
        else if(sarg=="-pf")
        {
          ++i;
          if(i>=argc)usage();
          pffile=string(argv[i]);
        }
        else
            usage();
    }
    Pf *pf;
    if(pfread(const_cast<char *>(pffile.c_str()),&pf))
    {
      cerr << "pfread failed on file="<<pffile<<endl;
      usage();
    }
    try{
      ofstream log;
      log.open(logfile.c_str(),ios::out);
      log << argv[0]<<" starting reading from db="<<dbname<<endl;
      Metadata control(pf);
      boost::archive::text_oarchive oa(cout);
      DatascopeHandle dbh(dbname,true);
      GapDefinition goodtimes;
      if(mask_bad_data)
      {
        log << "Reading good data periods from file "<<goodtimesfile<<endl;
        goodtimes=GapDefinition(goodtimesfile);
      }
      HFArray array;
      if(coordfile=="NotUsed")
      {
        /* I don't plan to use db mode so this is more of a place holder
        that I don't expect to full test so be warned.  Null string as
        arg2 means load the whole site table.  Constructor has a net
        option */
        log << "Loading coordinate data from site table"<<endl;
        array=HFArray(dbh,string(""));
      }
      else
      {
        log << "Reading coordinates from file="<<coordfile<<endl;
        /* this assumes input file has geographic coordinates */
        array=HFArray(coordfile,true);
      }
      /* This is needed by db constructor for 3c objects */
      StationChannelMap scm(pf);
      /* Now we read all the control parameters */
      double ts,te;
      /* Time window we cut around each input time */
      ts=control.get_double("data_window_start");
      te=control.get_double("data_window_end");
      TimeWindow data_window(ts,te);
      log << "Each seismogram will be "<<ts<<" to "<<te<<" s around input times"<<endl;
      /* We read a block of data around the full range of input times.
      This defines the number of seconds padded on each end to avoid
      filter transients */
      double padtime=control.get_double("read_window_pad_length");
      string filterstring=control.get_string("BRTT_filter_definition");
      TimeInvariantFilter foperator(filterstring);
      log<<"All data will be filtered using BRTT file with this definition: "
        << filterstring<<endl;
      double mrwl=control.get_double("maximum_read_window_length");
      /* We need to coordinate converter that is the same as the one
      used by the HFArray object.  We build it and the make sure it is
      consistent with the HFArraty object.  Note the 0 sent as arg 4
      assumes the current situation where the hfarray constructor always
      sets the azimuth attribute 0 */
      Geographic_point origin=array.origin();
      log << "Converting coordinates using local coordinates with origin:"<<endl
          << "Latitude:  "<<deg(origin.lat)<<" Longitude:  "<<deg(origin.lon)
          <<" Elevation (km): "<< origin.r - r0_ellipse(origin.lat)
          <<endl;
      RegionalCoordinates coords(origin.lat,origin.lon,origin.r,0.0);
      /* The first thing we do is eat up the whole input file of source
      data and store it in this vector.   For efficiency we then aim to
      read a whole block of data and exit if the request is absurd */
      vector<SourceData> source_coordinates;
      char inp[128];
      while(cin.getline(inp,128))
      {
        SourceData shotcorr(inp);
        source_coordinates.push_back(shotcorr);
      }
      TimeWindow full_read_window=GetTimeRange(source_coordinates);
      /* This guarantees a generous padding */
      full_read_window.start -= fabs(data_window.start);
      full_read_window.start -= 2.0*padtime;
      full_read_window.end += fabs(data_window.end);
      full_read_window.end += 2.0*padtime;
      if(fabs(full_read_window.end-full_read_window.start)>mrwl)
      {
        cerr << "Time window of shot time data read from stdin exceeds "
            << "maximum allowed time period="<<mrwl<<" sec"<< endl
            << "Break up your shot time data file or change the parameter "
            << "maximum_read_window_length if your system has enough memory"
            <<endl
            << "Range of input read is "<< strtime(full_read_window.start)
            <<" to "<<strtime(full_read_window.end)<<endl;
        exit(-1);
      }
      log << "Reading block of data for time spanned by these shots"<<endl
          << "Start time="<< strtime(full_read_window.start)
          << " end time="<< strtime(full_read_window.end)<<endl;
      /* This line reads the time period and bundles data to 3c objects.
      It does 90% of the work of this program. */
      ThreeComponentEnsemble full_line(dbh,full_read_window,scm);
      /* Since the input data is assumed from a fixed array, we set the
      station information header values in the full_line ensemble.  WE
      assume these are inherited when we exract windows of data from the
      larger block */
      load_precision_coordinates(full_line,array);
      FilterEnsemble(full_line,foperator);
      if(mask_bad_data) mask_bad(full_line,goodtimes);
      /* Now all we do is loop through the SourcData vector extracting
      time windows around each shot time.   We use the GapDefinition object
      to keep only data marked as usable. */
      vector<SourceData>::iterator sdptr;
      log << "Starting to write ensemble data to stdout"<<endl;
      for(sdptr=source_coordinates.begin();sdptr!=source_coordinates.end();++sdptr)
      {
        ThreeComponentEnsemble dout=ExtractWindowedData(full_line, *sdptr,
            coords, data_window);
        /* It is possible the windowed data ensemble has no data so we must
        test for this */
        if(dout.member.size()>0)
        {
          write_object<ThreeComponentEnsemble>(dout,oa);
          log << "Wrote "<<dout.member.size()<<" seismograms for ffid="
              << dout.get_int("ffid")<<endl;
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
