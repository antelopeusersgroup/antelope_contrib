#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include <map>
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
    cerr << "apply_statics staticsfile < in > out [-t0shiftkey keyword -drop -t object_type -v --help -text]"
        <<endl
        << " staticsfile - table of key-value pairs with time shifts to apply as value"
        <<endl
        << "(First line of file must contain match key as first token on line 1)"<<endl
        << "Use -t0shiftkey to define the header key used to store the applied shifts"<<endl
        << "Default is source_static"<<endl
        << "Use -drop to discard unmatched data to statics table"
        << " (Default copies unmatched data with no change)"
        << " Use -t to select object type expected for input. "<<endl
        << " (Allowed options=ThreeComponentEnsemble,ThreeComponentSeismogram (default), TimeSeries, and TimeSeriesEnsemble)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
class StaticTable
{
public:
  StaticTable(string fname,string t0shiftkey);
  template<typename T> int apply(T& d);
  string key()
  {
    return matchkey;
  }
private:
  map<int,double> statics;
  string matchkey;
  string t0key;
};
StaticTable::StaticTable(string fname, string t0sk)
{
  const string base_error("apply_statics - constructor for StaticTable");
  t0key=t0sk;
  ifstream ifs(fname.c_str(),ios::in);
  if(ifs.good())
  {
    ifs>>matchkey;
    char inpline[128];
    int lnum=1;
    while(ifs.getline(inpline,128))
    {
      stringstream ss(inpline);
      int key;
      double val;
      ss>>key;  ss>>val;
      if(ifs.fail())
      {
        cerr <<base_error<< endl
           << "Read error at line number "<<lnum<<endl;
        exit(-1);
      }
      else
      {
        statics[key]=val;
      }
      ++lnum;
    }
    ifs.close();
  }
  else
  {
    /* If this is ever moved to a library this should be converted to
    a throw */
    cerr << base_error<< endl
      << "Open failed failed for input file="<<fname<<endl;
    exit(-1);
  }
}
/* this procedure returns 0 on success and -1 if it fails */
template<typename T> int StaticTable::apply(T& d)
{
  int keyval;
  int iret(0);
  try{
    keyval=d.get_int(matchkey);
  }catch(MetadataGetError& err)
  {
    cerr << "StaticTable::apply method: could not retrieve static match key="
      << matchkey<<endl
      << "Data object passed will not be altered"<<endl;
    return(-1);
  }
  map<int,double>::iterator sptr;
  sptr=statics.find(keyval);
  if(sptr!=statics.end())
  {
    d.t0+=sptr->second;
    d.put(t0key,sptr->second);
  }
  else
  {
    iret=-1;
  }
  return(iret);
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

template <typename DataType> int ApplyStaticSingle(StaticTable& statics,
  string matchkey, bool dropnull, bool binary_data)
{
    try{
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<DataType> inp(form);
        StreamObjectWriter<DataType>  outp(form);
        int count;
        DataType d;
        while(inp.good())
        {
            d=inp.read();
            int retcode=statics.apply<DataType>(d);
            if(dropnull && (retcode!=0))continue;
            outp.write(d);
            ++count;
        }
        return count;
    }catch(...){throw;};
}
template <typename Tens, typename Tmem>
  int ApplyStaticsEnsemble(StaticTable& statics,
    string matchkey, bool dropnull, bool binary_data)
{
    try{
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<Tens> inp(form);
        StreamObjectWriter<Tens>  outp(form);
        int count(0);
        Tens d;
        typename vector<Tmem>::iterator dptr;
        while(inp.good())
        {
            int retcode;
            d=inp.read();
            if(dropnull)
            {
              Tens dcpy(dynamic_cast<Metadata&>(d),d.member.size());
              for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
              {
                if(statics.apply<Tmem>(*dptr) == 0)
                {
                  dcpy.member.push_back(*dptr);
                  ++count;
                }
              }
              outp.write(dcpy);
            }
            else
            {
              for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
              {
                if(statics.apply<Tmem>(*dptr) == 0) ++count;
              }
              outp.write(d);
            }
        }
        return count;
    }catch(...){throw;};
}

bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    const int narg_required(1);
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    if(argc<2)usage();
    string statictablefile(argv[1]);
    bool binary_data(true);
    bool dropnull(false);
    string otype("ThreeComponentSeismgram");
    string t0shiftkey("source_static");
    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-t0shiftkey")
        {
            ++i;
            if(i>=argc)usage();
            t0shiftkey=string(argv[i]);
        }
        else if(sarg=="-dropnull")
        {
            dropnull=true;
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
        StaticTable statics(statictablefile,t0shiftkey);
        string matchkey=statics.key();
        AllowedObjects dtype=get_object_type(otype);
        int count;
        switch (dtype)
        {
            case TCS:
                count=ApplyStaticSingle<ThreeComponentSeismogram>(statics,
                  matchkey, dropnull, binary_data);
                break;
            case TCE:
                count=ApplyStaticsEnsemble<ThreeComponentEnsemble,ThreeComponentSeismogram>
                  (statics,matchkey, dropnull,binary_data);
                break;
            case TS:
                count=ApplyStaticSingle<TimeSeries>(statics,
                  matchkey, dropnull, binary_data);
                break;
            case TSE:
                count=ApplyStaticsEnsemble<TimeSeriesEnsemble,TimeSeries>
                  (statics,matchkey, dropnull,binary_data);
                break;
            case PMTS:
                    count=ApplyStaticSingle<PMTimeSeries>(statics,
                      matchkey, dropnull, binary_data);
                    break;
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };
        if(SEISPP_verbose)
          cerr << "apply_statics:  processed "<<count<<" objects from stdin to stdout"
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
