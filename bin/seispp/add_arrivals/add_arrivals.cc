#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;   
using namespace SEISPP;
void usage()
{
    cerr << "add_arrivals [--help -v -t otype -text -pf pffile]"<<endl
        <<endl
        << "Add predicted arrival time for a specified seismic phase"
        <<endl
        << "optionally save predicted slowness vector, delta, and/or back azimuth"
        <<endl
        << "Requires source and receiver coordinates be defined in each seismogram"
        <<endl
        << " Use -t to select object type expected for input. "<<endl
        << " (Allowed options=ThreeComponentSeismogram, "
        << "TimeSeries, and PMTimeSeries)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl
        << "-pf - use pffile instead of default add_arrivals.pf"<<endl;
    exit(-1);
}
/* This procedure parses an input string (normally from argv) 
 * to set a list of allowed objects.   This could be a library
 * procedure, but with this one can customize the set of objects
 * supported. */
enum AllowedObjects {TCS, TS, PMTS};
AllowedObjects get_object_type(string otype)
{
    if(otype=="ThreeComponentSeismogram")
        return TCS;
    else if(otype=="TimeSeries")
        return TS;
    else if(otype=="PMTimeSeries")
        return PMTS;
    else
    {
        cerr << "Do not know how to handle object type="<<otype
            <<endl<< "Cannot continue"<<endl;
        exit(-1);
    }
}
/* Generic function to read objects that are children of Metadata.   control 
 * has parameters that drive this procedure.   binary_data is switch for
 * reader and writer */
template <class DataType> int add_arrivals(Metadata control,bool binary_data)
{
    try{
        /* First fetch all the required parameters from control */
        string phase=control.get_string("phase");
        string phase_key=control.get_string("phase_time_key");
        string source_latitude_key=control.get_string("source_latitude_key");
        string source_longitude_key=control.get_string("source_longitude_key");
        string source_depth_key=control.get_string("source_depth_key");
        string source_origin_time_key=control.get_string("source_origin_time_key");
        string receiver_latitude_key=control.get_string("receiver_latitude_key");
        string receiver_longitude_key=control.get_string("receiver_longitude_key");
        string receiver_elevation_key=control.get_string("receiver_elevation_key");
        bool save_delta=control.get_bool("save_delta");
        bool save_baz=control.get_bool("save_baz");
        bool save_slowness=control.get_bool("save_slowness");
        string delta_key=control.get_string("delta_key");
        string baz_key=control.get_string("baz_key");
        string TTmethod=control.get_string("TTmethod");
        string TTmodel=control.get_string("TTmodel");
        string slowness_ux_key=control.get_string("slowness_ux_key");
        string slowness_uy_key=control.get_string("slowness_uy_key");
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<DataType> inp(form);
        StreamObjectWriter<DataType>  outp(form);
        int count;
        DataType dfull;
        double lon,lat,depth,elev,otime;
        double baz,delta;
        double atime;
        SlownessVector slow;
        while(inp.good())
        {
            dfull=inp.read();
            /* This dynamic_cast seems necessary because all the objects involved use
                 * multiple inheritance that makes this ambiguous without this explicit 
                 * cast*/
            Metadata *md=dynamic_cast<Metadata*>(&dfull);;
            /* will not make it a fatal error if a seismogram is missing
             * required metadata, but will print a warning and blunder on.*/
            try{
                lat=md->get<double>(source_latitude_key);
                lon=md->get<double>(source_longitude_key);
                depth=md->get<double>(source_depth_key);
                otime=md->get<double>(source_origin_time_key);
                Hypocenter h(rad(lat),rad(lon),depth,otime,TTmethod,TTmodel);
                lat=md->get<double>(receiver_latitude_key);
                lon=md->get<double>(receiver_longitude_key);
                lat=rad(lat);  lon=rad(lon);  // hypo wants these in radians
                elev=md->get<double>(receiver_elevation_key);
                atime=h.phasetime(lat,lon,elev,phase);
                atime += otime;  // phasetime returns travel time, not arrival time
                md->put(phase_key,atime);
                if(save_delta)
                {
                    delta=h.distance(lat,lon);
                    md->put(delta_key,deg(delta));
                }
                if(save_baz)
                {
                    baz=h.seaz(lat,lon);
                    md->put(baz_key,deg(baz));
                }
                if(save_slowness)
                {
                    /* Slowness vector calculation can fail when everything
                     * else works.  We make this a nonfatal error assuming
                     * slowness vector is of secondary interest. */
                    try{
                        slow=h.phaseslow(lon,lat,elev,phase);
                        md->put(slowness_ux_key,slow.ux);
                        md->put(slowness_uy_key,slow.uy);
                    }catch(SeisppError& serr)
                    {
                        cerr << "add_arrivals:  slowness vector calculation failed."
                            <<"  Error message posted:"<<endl;
                        serr.log_error();
                        cerr << "Slowness data not saved, but program continues"
                            <<endl;
                    }
                }
            }catch(MetadataGetError& mde)
            {
                cerr << "add_arrivals: Error fetching required attribute.  "
                  << "Message posted:"<<endl;
                mde.log_error();
                cerr<<"Actual content of this seismogram header:"<<endl;
                cerr << *md<<endl;
                cerr<<"Copying data without required arrival time attribute"
                  <<endl;  
            }
            outp.write(dfull);
            ++count;
        }
        return count;
    }catch(...){throw;};
}

bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    const int narg_required(0);
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    double example_real(0.0);
    bool example_boolean(false);
    bool binary_data(true);
    string otype("ThreeComponentSeismogram");
    string pffile("add_arrivals.pf");

    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-x")
        {
            ++i;
            if(i>=argc)usage();
            example_real=atof(argv[i]);
        }
        else if(sarg=="-flag")
        {
            example_boolean=true;
        }
        else if(sarg=="-text")
        {
            binary_data=false;
        }
        else if(sarg=="-v")
          SEISPP_verbose=true;
        else if(sarg=="-t")
        {
            ++i;
            if(i>=argc)usage();
            otype=string(argv[i]);
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
    char pffstr[32];
    strcpy(pffstr,pffile.c_str());
    if(pfread(pffstr,&pf))
    {
        cerr << argv[0] << " pffread failed on pffile="<<pffstr<<endl;
        usage();
    }
    try{
        Metadata control(pf);
        AllowedObjects dtype=get_object_type(otype);
        int count;
        switch (dtype)
        {
            case TCS:
                count=add_arrivals<ThreeComponentSeismogram>(control,binary_data);
                break;
            case TS:
                count=add_arrivals<TimeSeries>(control,binary_data);
                break;
            case PMTS:
                count=add_arrivals<PMTimeSeries>(control,binary_data);
                break;
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };
        if(SEISPP_verbose)
        {
            cerr << argv[0]<<" processed "<<count<<" seismograms"<<endl;
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

