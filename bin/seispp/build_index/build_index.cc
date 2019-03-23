#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "ThreeComponentSeismogram.h"
#include "PfStyleMetadata.h"
#include "StreamObjectFileIndex.h"
using namespace std;
using namespace SEISPP;
void usage()
{
    cerr << "build_index dfile [-o indexfile -t object_type -v --help -pf pffile]"
        <<endl
        << "Builds an index from dfile and writes an indexfile"<<endl
        << "Index is defined by parameters in pffile"<<endl
        << " Use -o to change default index file name (basename.inx)"<<endl
        << " Use -t to select object type expected for input. "<<endl
        << " (Allowed options=ThreeComponentEnsemble (default),ThreeComponentSeismogram, TimeSeries, PMTimeSeries, and TimeSeriesEnsemble)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -pf use alternative pf file instead of default build_index.pf"
        <<endl;
    exit(-1);
}
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
/* This example only works with input from stdin and output to stdout.
 * Fairly simple changes to work for files - change the arguments
 * and calls to constructors.  Example returns a count of the
 * number of objects copied. */
template <typename DataType> int build_index(string dfile,
  string indexfile, MetadataList mdl)
{
    try{
      int count;
      StreamObjectFileIndex<DataType> indexer(dfile,mdl);
      if(indexfile=="DEFAULT")
        count=indexer.writeindex();
      else
        count=indexer.writeindex(indexfile);
      return count;
    }catch(...){throw;};
}

bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    if(argc<2) usage();
    string otype("ThreeComponentSeismogram");
    string dfile(argv[1]);
    string indexfile("DEFAULT");
    string pffile("build_index");
    for(i=2;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-v")
          SEISPP_verbose=true;
        else if(sarg=="-t")
        {
            ++i;
            if(i>=argc)usage();
            otype=string(argv[i]);
        }
        else if(sarg=="-o")
        {
            ++i;
            if(i>=argc)usage();
            indexfile=string(argv[i]);
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
    try{
      PfStyleMetadata control(pffile);
      const string mdlkey("IndexMetadata");
      MetadataList mdl=get_mdlist(control,mdlkey);
      AllowedObjects dtype=get_object_type(otype);
      int count;
        switch (dtype)
        {
            case TCS:
                count=build_index<ThreeComponentSeismogram>(dfile,indexfile,mdl);
                break;
            case TCE:
                count=build_index<ThreeComponentEnsemble>(dfile,indexfile,mdl);
                break;
            case TS:
                count=build_index<TimeSeries>(dfile,indexfile,mdl);
                break;
            case TSE:
                count=build_index<TimeSeriesEnsemble>(dfile,indexfile,mdl);
                break;
            case PMTS:
                count=build_index<PMTimeSeries>(dfile,indexfile,mdl);
                break;
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };
        if(SEISPP_verbose)
          cerr << "build_index:  constructed index for "<<count<<" objects for file "
          << dfile<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
