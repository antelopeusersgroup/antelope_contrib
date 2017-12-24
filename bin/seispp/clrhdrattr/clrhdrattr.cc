#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include <set>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "ensemble.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "clrhdrattr a1 a2 ... a3 [-t object_type -v --help -text] < in > out"
        <<endl
        << "Clear contents of header values define by arguments"<<endl
        << "Any contents of the header (metadata) for are removed"<<endl
        << "reads objects from sdtin and writes a copy to stdout with requested attributes removed"<<endl
        << " Use -t to select object type expected for input. "<<endl
        << " (Allowed options=ThreeComponentEnsemble (default),ThreeComponentSeismogram,"<<endl
        << " TimeSeries,TimeSeriesEnsemble, and PMTimeSeries)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
const std::set<string> not_allowed {"nsamp","samprate","time"};
/* This procedure parses an input string (normally from argv)
 * to set a list of allowed objects.   This could be a library
 * procedure, but with this one can customize the set of objects
 * supported. */
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
/* key thing for this program is the remove method of metadata.
 Also should use the dynamic cast of operator= for metadata only
 * Fairly simple changes to work for files - change the arguments
 * and calls to constructors.  Example returns a count of the
 * number of objects copied. */
template <typename DataType> int clrhdrattr(std::set<string> keys, bool binary_data)
{
    try{
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<DataType> inp(form);
        StreamObjectWriter<DataType>  outp(form);
        int count;
        DataType d;
        std::set<string>::iterator kptr;
        while(inp.good())
        {
            d=inp.read();
            for(kptr=keys.begin();kptr!=keys.end();++kptr)
            {
              d.remove(*kptr);
            }
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
    bool binary_data(true);
    std::set<string>keys;
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
        else
        {
            if( not_allowed.find(sarg) == not_allowed.end() )
            {
              cerr << "Attribute="<<sarg<<" cannot be cleared - it is used by some constructors"<<endl;
              usage();
            }
            keys.insert(sarg);
        }
    }
    try{
        AllowedObjects dtype=get_object_type(otype);
        int count;
        switch (dtype)
        {
            case TCS:
                count=clrhdrattr<ThreeComponentSeismogram>(keys, binary_data);
                break;
            case TCE:
                count=clrhdrattr<ThreeComponentEnsemble>(keys, binary_data);
                break;
            case TS:
                count=clrhdrattr<TimeSeries>(keys, binary_data);
                break;
            case TSE:
                count=clrhdrattr<TimeSeriesEnsemble>(keys, binary_data);
                break;
            case PMTS:
                count=clrhdrattr<PMTimeSeries>(keys, binary_data);
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };
        cerr << "clrhdratt:  cleared "<<count<<" attributes processing this data set"
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
