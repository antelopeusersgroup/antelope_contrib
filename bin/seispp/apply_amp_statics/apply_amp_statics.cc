#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include <map>
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
    cerr << "apply_amp_statics [-key name:type -s statics_file -okey name -t object_type -v --help -text] < in > out" 
        <<endl
        << "apply amplitude statics to a data set based on a table of name-value pairs"
        << endl
        << "Many algorithms need amplitudes adjusted by some other algorithm"
        <<endl
        << "This program provides a fairly generic way to do that by using a "
        <<endl
        << "single metadata key to match data in a table entry a set of seismograms"
        << endl
        << "The matched values are treated as gain factors for each seismogram"
        <<endl
        << " Use -key to change the match key expected for the statics table"
        << endl 
        << " name is the key for the entry and type should be one of real, int, integer, or string"
        <<endl
        << " (default is fldr:int - appropriate for active source shot data"
        <<endl
        << " Use -s to change the file name for the table of name value pairs"
        <<endl
        << " (default file name is amplitude_statics.dat"<<endl
        << " Use -okey to change the key used to save this scaling applied by this program"
        <<endl
        << " (default is shot_amplitude_static)"<<endl

        << " Use -t to select object type expected for input. "<<endl
        << " (Allowed options=ThreeComponentEnsemble (default),ThreeComponentSeismogram, TimeSeries, and TimeSeriesEnsemble)"<<endl
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
    string prog("apply_amp_statics: ");
    if(otype=="ThreeComponentSeismogram")
        return TCS;
    else if(otype=="ThreeComponentEnsemble")
        return TCE;
    else if(otype=="TimeSeries")
        return TS;
    else if(otype=="TimeSeriesEnsemble")
        return TSE;
    else if(otype=="PMTimeSeries")
    {
        cerr << prog<<"No support at this time for PMTimeSeries"<<endl;
        usage();
    }
    else
    {
        cerr << prog << "Do not know how to handle object type="<<otype
            <<endl<< "Cannot continue"<<endl;
        usage();
    }
}
template <typename MatchType> map<MatchType,double> 
                    load_static_table(string fname)
{
    try{
      const string base_error("load_static_table:  ");
      ifstream ifs(fname.c_str());
      if(ifs.is_open())
      {
        map<MatchType,double> result;
        MatchType key;
        double val;
        char s[128];
        while(ifs.getline(s,128))
        {
            stringstream ss(s);
            ss>>key;
            ss>>val;
            result.insert(pair<MatchType,double>(key,val));
        }
        ifs.close();
        if(result.size()<=0) throw SeisppError(base_error
                + "static file ="+fname+" was empty - no data loaded");
        return result;
      }
      else
      {
          throw SeisppError(base_error + "Failure in trying to open file="
                  + fname);
      }
    }catch(...){throw;};
}
template <typename DataType,typename MatchType> int apply_amp_statics(
        string matchkey, string staticfile,string okey,bool binary_data)
{
    try{
        map<MatchType,double> statics(load_static_table<MatchType>(staticfile));
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<DataType> inp(form);
        StreamObjectWriter<DataType>  outp(form);
        int count(0);
        DataType d;
        double scale;
        MatchType mkval;
        typename map<MatchType,double>::iterator sptr;
        while(inp.good())
        {
            d=inp.read();
            /* This seems necessary for the compiler to unravel this
             * expression that has a multiple inheritance ambiguity */
            Metadata *mptr;
            mptr=dynamic_cast<Metadata*>(&d);
            mkval=mptr->get<MatchType>(matchkey);
            sptr=statics.find(mkval);
            if(sptr!=statics.end())
            {
                scale=sptr->second;
                ScaleMember(&d,scale);
                d.put(okey,scale);
            }
            else
            {
                cerr << "apply_amp_statics:   no entry in statics table for "
                    << "key="<<matchkey<<" with value="<<mkval<<endl
                    << "Scale NOT applied"<<endl;
            }
            outp.write(d);
            ++count;
        }
        return count;
    }catch(...){throw;};
}
template <typename DataType, typename MatchType> 
  int apply_amp_statics_ensembles(string matchkey, string staticfile,
        string okey,bool binary_data)
{
    try{
        map<MatchType,double> statics(load_static_table<MatchType>(staticfile));
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<DataType> inp(form);
        StreamObjectWriter<DataType>  outp(form);
        int count(0);
        DataType d;
        double scale;
        MatchType mkval;
        typename map<MatchType,double>::iterator sptr;
        while(inp.good())
        {
            int i;
            d=inp.read();
            for(i=0;i<d.member.size();++i)
            {
              /* This seems necessary for the compiler to unravel this
               * expression that has a multiple inheritance ambiguity */
              Metadata *mptr;
              mptr=dynamic_cast<Metadata*>(&(d.member[i]));
              mkval=mptr->get<MatchType>(matchkey);
              sptr=statics.find(mkval);
              if(sptr!=statics.end())
              {
                scale=sptr->second;
                ScaleMember(&(d.member[i]),scale);
                d.put(okey,scale);
              }
              else
              {
                cerr << "apply_amp_statics:   no entry in statics table for "
                    << "key="<<matchkey<<" with value="<<mkval<<endl
                    << "Scale NOT applied"<<endl;
              }
            }
            outp.write(d);
            ++count;
        }
        return count;
    }catch(...){throw;};
}

std::pair<string,MDtype> split_arg(const char *arg)
{
  std::string sep(":");   // this could be an argument
  std::string sarg(arg);
  std::size_t pos=sarg.find(sep);
  if(pos==string::npos)
  {
    cerr << "apply_amp_statics:  Incorrect format of argument passed to split_arg"<<endl
      << "Missing required : separator to define arg=key:type"<<endl
      << "Received this argument:  "<<arg<<endl;
    usage();
  }
  pair<string,MDtype> result;
  result.first=sarg.substr(0,pos);
  string strfield=sarg.substr(pos+1,sarg.length()-1);
  if(strfield=="int" || strfield=="INT" || strfield=="long")
    result.second=MDint;
  else if(strfield=="real" || strfield=="float" || strfield=="double")
    result.second=MDreal;
  else if(strfield=="string" || strfield=="STRING")
    result.second=MDstring;
  else
  {
    cerr << "subset_streamfile:  unrecognized type field for arg1="
      << strfield<<endl;
    usage();
  }
  return result;
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    bool binary_data(true);
    string otype("ThreeComponentSeismogram");
    string matchkey("fldr");
    MDtype matchkeytype;
    string statics_file("amplitude_statics.dat");
    string okey("shot_amplitude_static");
    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-s")
        {
            ++i;
            if(i>=argc)usage();
            statics_file=string(argv[i]);
        }
        else if(sarg=="-okey")
        {
            ++i;
            if(i>=argc)usage();
            okey=string(argv[i]);
        }
        else if(sarg=="-key")
        {
            ++i;
            if(i>=argc)usage();
            std::pair<string,MDtype> keypair=split_arg(argv[i]);
            matchkey=keypair.first;
            matchkeytype=keypair.second;
            if( !((matchkeytype==MDint) || (matchkeytype==MDstring) ) )
            {
                cerr << "apply_amp_statics:  illegal type specified for -key"
                    << " argument.  Must be either int or string"
                    <<endl;
                usage();
            }
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
        int count;
        const string bug_message("apply_amp_statics:  coding error\nIllegal match key type set at top of processing loop");
        switch (dtype)
        {
            case TCS:
                if(matchkeytype==MDint)
                    count=apply_amp_statics<ThreeComponentSeismogram,int>
                        (matchkey,statics_file,okey,binary_data);
                else if(matchkeytype==MDstring)
                    count=apply_amp_statics<ThreeComponentSeismogram,string>
                        (matchkey,statics_file,okey,binary_data);
                else
                {
                    cerr << bug_message<<endl;
                    usage();
                }
                break;
            case TCE:
                if(matchkeytype==MDint)
                    count=apply_amp_statics_ensembles<ThreeComponentEnsemble,int>
                        (matchkey,statics_file,okey,binary_data);
                else if(matchkeytype==MDstring)
                    count=apply_amp_statics_ensembles<ThreeComponentEnsemble,string>
                        (matchkey,statics_file,okey,binary_data);
                else
                {
                    cerr << bug_message<<endl;
                    usage();
                }
                break;
            case TS:
                if(matchkeytype==MDint)
                    count=apply_amp_statics<TimeSeries,int>
                        (matchkey,statics_file,okey,binary_data);
                else if(matchkeytype==MDstring)
                    count=apply_amp_statics<TimeSeries,string>
                        (matchkey,statics_file,okey,binary_data);
                else
                {
                    cerr << bug_message<<endl;
                    usage();
                }
                break;
            case TSE:
                if(matchkeytype==MDint)
                    count=apply_amp_statics_ensembles<TimeSeriesEnsemble,int>
                        (matchkey,statics_file,okey,binary_data);
                else if(matchkeytype==MDstring)
                    count=apply_amp_statics_ensembles<TimeSeriesEnsemble,string>
                        (matchkey,statics_file,okey,binary_data);
                else
                {
                    cerr << bug_message<<endl;
                    usage();
                }
                break;
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };
        cerr << "template:  copied "<<count<<" objects from stdin to stdout"
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
