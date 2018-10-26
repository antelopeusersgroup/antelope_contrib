#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "ThreeComponentSeismogram.h"
#include "PfStyleMetadata.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "sequence < in > out [-tag name -t object_type -v --help -text]"
        <<endl
        << "Adds a computed integer sequence to metadata (header) of each object"
        <<endl
        << " Use -tag to change the name attached to the counter (default nseq)"
        <<endl
        << " Use -t to select object type expected for input. "<<endl
        << " (Allowed options=ThreeComponentEnsemble,ThreeComponentSeismogram (default), PMTimeSeries, TimeSeries, and TimeSeriesEnsemble)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
enum AllowedObjects {TCS, TCE,  TS, TSE, PMTS};
AllowedObjects get_object_type(string otype)
{
    if(otype=="ThreeComponentSeismogram")
        return TCS;
    else if(otype=="ThreeComponentEnsemble")
        return TCE;
    else if(otype=="TimeSeries")
        return TS;
    else if(otype=="TimeSeriesEnsemble")
        return TSE;
    else if(otype=="PMTimeSeries")
        return PMTS;
    else
    {
        cerr << "Do not know how to handle object type="<<otype
            <<endl<< "Cannot continue"<<endl;
        exit(-1);
    }
}
template <typename DataType> int sequence(string tag,bool binary_data)
{
    try{
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<DataType> inp(form);
        StreamObjectWriter<DataType>  outp(form);
        int count(0);
        DataType d;
        while(inp.good())
        {
            d=inp.read();
            d.put(tag,count);
            outp.write(d);
            ++count;
        }
        return count;
    }catch(...){throw;};
}

bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    string otype("ThreeComponentSeismogram");
    string tag("nseq");
    bool binary_data(true);

    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
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
        else if(sarg=="-tag")
        {
            ++i;
            if(i>=argc)usage();
            tag=string(argv[i]);
        }
        else
            usage();
    }
    try{
        AllowedObjects dtype=get_object_type(otype);
        int count;
        switch (dtype)
        {
            case TCS:
                count=sequence<ThreeComponentSeismogram>(tag,binary_data);
                break;
            case TCE:
                count=sequence<ThreeComponentEnsemble>(tag,binary_data);
                break;
            case TS:
                count=sequence<TimeSeries>(tag,binary_data);
                break;
            case TSE:
                count=sequence<TimeSeriesEnsemble>(tag,binary_data);
                break;
            case PMTS:
                    count=sequence<PMTimeSeries>(tag,binary_data);
                    break;
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };
        cerr << "sequence:  added sequence numbers to "<<count
          << " objects posted with tag="<<tag
            <<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
