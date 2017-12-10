#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "TimeSeries.h"
#include "ensemble.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;   
using namespace SEISPP;
void usage()
{
    cerr << "BasicTimeSeriesAttribures < in > out [-objt object_type -v --help -text]"
        <<endl
        << "Most SEISPP data objects are children of BasicTimeSeries"<<endl
        << "The attibute defined in BasicTimeSeries are not Metadata and "
        << "need to be handled independently"<<endl
        << "This does that by calling operator << for all children of BasicTimeSeries"
        <<endl
        << "Input is assumed to be a stream file of the requested objects"<<endl
        << "The result is a verbose text file meant to be read by humans"<<endl
        << " Use -objt to select object type expected for input. "<<endl
        << " (Allowed options=ThreeComponentEnsemble (default),"
           << "ThreeComponentSeismogram, TimeSeries, PMTimeSeries,"
           << "and TimeSeriesEnsemble)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
/* This procedure parses an input string (normally from argv) 
 * to set a list of allowed objects.   This could be a library
 * procedure, but with this one can customize the set of objects
 * supported. */
enum AllowedObjects {TCE, TSE, TCS, TS, PMTS};
AllowedObjects get_object_type(string otype)
{
    if(otype=="ThreeComponentSeismogram")
        return TCS;
    else if(otype=="TimeSeries")
        return TS;
    else if(otype=="PMTimeSeries")
        return PMTS;
    else if(otype=="TimeSeriesEnsemble")
        return TSE;
    else if(otype=="ThreeComponentEnsemble")
        return TCE;
    else
    {
        cerr << "Unsupported object type specified on command line"<<endl
            << "Cannot handle objt="<<otype<<endl;
        usage();
    }
}
template <typename DataType,typename MemberType> 
   int ensemble_print(ostream& ofs, bool binary_data)
{
    try{
        const string seperator_line("=======================================");
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<DataType> inp(form);
        int count(0);
        DataType d;
        while(inp.good())
        {
            d=inp.read();
            ofs << "BasicTimeSeries attributes for data in ensemble number "
                << count<<endl<<seperator_line<<endl;
            typename vector<MemberType>::iterator dptr;
            int i(0);
            for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
            {
                MemberType *ptr;
                ptr=&(*dptr);
                BasicTimeSeries *btsptr=dynamic_cast<BasicTimeSeries*>(ptr);
                ofs<<"Attributes for ensemble member number "<<i<<endl
                    << seperator_line<<endl;
                ofs<< *btsptr<<endl<<seperator_line<<endl;
                ++i;
            }
            ++count;
        }
        return count;
    }catch(...){throw;};
}
template <typename DataType> int unbundled_print(ostream& ofs, bool binary_data)
{
    try{
        const string seperator_line("=======================================");
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<DataType> inp(form);
        int count(0);
        DataType d;
        while(inp.good())
        {
            d=inp.read();
            ofs<<"Attributes for object number "<<count<<endl
                << seperator_line<<endl;
            BasicTimeSeries *ptr=dynamic_cast<BasicTimeSeries*>(&d);
            ofs << *ptr<<seperator_line<<endl;
            ++count;
        }
        return(count);
    }catch(...){throw;}
}

bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    bool binary_data(true);
    string otype("ThreeComponentEnsemble");

    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-v")
          SEISPP_verbose=true;
        else if(sarg=="-text")
            binary_data=false;
        else if(sarg=="-objt")
        {
            ++i;
            if(i>=argc)usage();
            otype=string(argv[i]);
        }
        else
            usage();
    }
    try{
        AllowedObjects dtype=get_object_type(otype);
        int count;
        switch (dtype)
        {
            case TCE:
                count=ensemble_print<ThreeComponentEnsemble,
                    ThreeComponentSeismogram>(cout,binary_data);
                break;
            case TSE:
                count=ensemble_print<TimeSeriesEnsemble,
                    TimeSeries>(cout,binary_data);
                break;
            case TS:
                count=unbundled_print<TimeSeries>(cout,binary_data);
                break;
            case TCS:
                count=unbundled_print<ThreeComponentSeismogram>(cout,binary_data);
                break;
            case PMTS:
                count=unbundled_print<PMTimeSeries>(cout,binary_data);
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };
        cout << "BasicTimeSeriesAttributes:  Dumped attributes for "<<count<<" objects"<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}

