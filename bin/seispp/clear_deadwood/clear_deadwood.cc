#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include <vector>
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
    cerr << "clear_deadwood < in > out [-t object_type -v --help -text]"
        <<endl
        << "Removes any objects from a file that are marked dead."<<endl
        << "All supported data objects have a mechanism to mark an object dead"<<endl
        << "Most seispp filters will handle this but some do not requiring the data be passed"<<endl
        << "though this filter.  Further, if a lot of data are marked dead this filter will"<<endl
        << "reduce the file size by the number deleted times nominal size per object"<<endl
        << " Use -t to select object type expected for input. "<<endl
        << " (Allowed options=ThreeComponentEnsemble (default),ThreeComponentSeismogram, TimeSeries,TimeSeriesEnsemble, and PMTimeSeries)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
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
typedef pair<int,int> ProcessCount;
template <typename Tens, typename Tdata> ProcessCount clear_deadwood(bool binary_data)
{
  try{
      char form('t');
      if(binary_data) form='b';
      StreamObjectReader<Tens> inp(form);
      StreamObjectWriter<Tens>  outp(form);
      int count(0),total_count(0);
      Tens d;
      while(inp.good())
      {
          d=inp.read();
          /* For efficiency we scan the ensemble for anything marked dead.
          When true we enter the loop of check, copy below.  If none
          are marked dead just write the result to output. We do this because
          editing a vector container is very expensive and the untested theory
          is copying will, on average, be faster than using erase*/
          bool needs_editing(false);
          typename std::vector<Tdata>::iterator dptr;
          for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
          {
            if(!dptr->live)
            {
              needs_editing=true;
              break;
            }
          }
          if(needs_editing)
          {
            Tens dedit(dynamic_cast<Metadata&>(d),d.member.size());
            for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
            {
              if(dptr->live)
              {
                dedit.member.push_back(*dptr);
              }
            }
            /* If the result is totally empty delete it from output */
            if(dedit.member.size()>0)
            {
              outp.write(dedit);
              ++count;
            }
            else if(SEISPP_verbose)
            {
              cerr << "clear_deadwood (Warning):   all members in ensemble number "<<count
                << "were marked dead"<<endl
                << "Deleting this entire ensemble from output"<<endl;
            }
          }
          else
          {
            outp.write(d);
            ++count;
          }
          ++total_count;
      }
      ProcessCount pc;
      pc.first=count;
      pc.second=total_count;
      return pc;
  }catch(...){throw;};
}
template <typename DataType> ProcessCount clear_deadwood(bool binary_data)
{
    try{
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<DataType> inp(form);
        StreamObjectWriter<DataType>  outp(form);
        int count(0),total_count(0);
        DataType d;
        while(inp.good())
        {
            d=inp.read();
            if(d.live)
            {
              ++count;
              outp.write(d);
            }
            ++total_count;
        }
        ProcessCount pc;
        pc.first=count;
        pc.second=total_count;
        return pc;
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
        else
            usage();
    }
    try{
        /* This approach depends upon the use of a template as
         * a generic method to implement the algorithm being
         * implemented.   This example does nothing but copy
         * input to output but provides a starting point for
         * algorithms that can be done on multiple object types. */
        AllowedObjects dtype=get_object_type(otype);
        ProcessCount count;
        switch (dtype)
        {
            case TCS:
                count=clear_deadwood<ThreeComponentSeismogram>(binary_data);
                break;
            case TCE:
                count=clear_deadwood<ThreeComponentEnsemble,ThreeComponentSeismogram>(binary_data);
                break;
            case TS:
                count=clear_deadwood<TimeSeries>(binary_data);
                break;
            case TSE:
                count=clear_deadwood<TimeSeriesEnsemble,TimeSeries>(binary_data);
                break;
            case PMTS:
                    count=clear_deadwood<PMTimeSeries>(binary_data);
                    break;
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };
        if(SEISPP_verbose) cerr << "clear_deadwood:   processed "<<count.second
          <<" objects"<<endl
          << "Number copied to output="<<count.first<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
