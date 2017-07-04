#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "seispp.h"
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
#include "ensemble.h"
/* Disabled for now */
//#include "PMTimeSeries.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;
using namespace SEISPP;
enum ObjectsSupported {TCS, TCE, TS, TSE, PMTS};
/* This is a generic range tester that will work on any type for
which <= and >= are defined.  */
template <typename T> class range_tester
{
public:
  /* Set minonly true to do only greater than test.  Set maxonly
  true for only less than test */
  range_tester(T minv, T maxv, bool minonly, bool maxonly);
  bool inside(T testval);
private:
  T minval;
  T maxval;
  bool use_min;
  bool use_max;
};
template <typename T>
   range_tester<T>::range_tester(T minv, T maxv, bool minonly, bool maxonly)
{
  minval=minv;
  maxval=maxv;
  use_min=true;
  use_max=true;
  if(minonly&&minonly)
  {
    throw SeisppError(string("range_tester constructor:  ")
         + "Coding error - cannot turn both start and end of range to test off");
  }
  else
  {
    if(minonly)
    {
      use_max=false;
    }
    if(maxonly)
    {
      use_max=true;
    }
  }
}
template <typename T> bool range_tester<T>::inside(T testval)
{
  if(use_min)
  {
    if(testval<=minval)return false;
  }
  if(use_max)
  {
    if(testval>=maxval) return false;
  }
  return true;
}
/* Generic test for equal condition to a common value set by the
constructor.   Simplifies tests for equality with variable types */
template <typename T>
class equal_tester
{
public:
  equal_tester(T val_to_set)
  {
    val_to_test=val_to_set;
  };
  bool is_equal(T testval)
  {
    if(testval==val_to_test)
      return true;
    else
      return false;
  };
private:
  T val_to_test;
};
void usage()
{
    cerr << "subset_streamfile key:type ( -eq val | -range minval maxval | -min minval -max maxval ) [-objt object_type --help -binary]"<<endl
        <<endl
        << " seispp unix filter to subset a data set read from stdin and write result to out"
        <<endl
        << "key is the Metadata key used to defined subset condition.  "
        << "optionally append :type of int or real (e.g. evid:int) "<<endl
        << "Default (no :type specified) is string (char * for C programmers)"
        <<endl
        <<  "  [-objt data_object_type]"<<endl
        << " Accepted object types for arg following -objt flag are:"<<endl
        << "  ThreeComponentSeismogram"<<endl
        << "  ThreeComponentEnsemble"<<endl
        << "  TimeSeries"<<endl
        << "  TimeSeriesEnsemble"<<endl
        << "  PMTimeSeries"<<endl
        << " -eq implement an exact match test"<<endl
        << " Use -range to select data with key value between (inclusive) minval and maxval"<<endl
        << " Use -min or -max to specify one sided range tests"
        <<endl
        << " --help - prints this message"<<endl
        << " -binary - switch to binary input and output (default is text)"
        <<endl;
    exit(-1);
}
std::pair<string,MDtype> split_arg1(const char *arg)
{
  std::string sep(":");   // this could be an argument
  std::string sarg(arg);
  std::size_t pos=sarg.find(sep);
  if(pos==string::npos)
  {
    cerr << "subset_streamfile:  Incorrect format of argument passed to split_arg1"<<endl
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

/* This does all the work of this procedure.  It is a double parameter
template to support multiple object types and generic types for key.
This procedure is used for a range test.   Nearly identical procedure below
for equal test.  There might be a fancieer way to do this with predicates, but
I still find that C++ construct produces very confusing code. Return
number written to output.*/
template <typename Tdata, typename Tkey>
   int filter_by_range(string key,range_tester<Tkey> rt,bool binary_data)
{
  StreamObjectReader<Tdata> *inp=NULL;
  StreamObjectWriter<Tdata> *outp=NULL;
  try{
    int nout(0);
    /* These are construtors for input from stdin and output to stdout */
    if(binary_data)
    {
      inp=new StreamObjectReader<Tdata>('b');
      outp=new StreamObjectWriter<Tdata>('b');
    }
    else
    {
      inp=new StreamObjectReader<Tdata>('t');
      outp=new StreamObjectWriter<Tdata>('t');
    }
    while(inp->good())
    {
      Tdata d=inp->read();
      Tkey mdtest;
      Metadata *md=dynamic_cast<Metadata *>(&d);
      mdtest=md->get<Tkey>(key);
      if(rt.inside(mdtest))
      {
        outp->write(d);
        ++nout;
      }
    }
    return nout;
  }catch(...)
  {
    if(inp!=NULL) delete inp;
    if(outp!=NULL) delete outp;
    throw;
  }
}
template <typename Tdata, typename Tkey>
   int filter_by_match(string key,equal_tester<Tkey> eqt,bool binary_data)
{
  StreamObjectReader<Tdata> *inp=NULL;
  StreamObjectWriter<Tdata> *outp=NULL;
  try{
    int nout(0);
    /* This procedure only supports input from stdin and output to stdout*/
    if(binary_data)
    {
      inp=new StreamObjectReader<Tdata>('b');
      outp=new StreamObjectWriter<Tdata>('b');
    }
    else
    {
      inp=new StreamObjectReader<Tdata>('t');
      outp=new StreamObjectWriter<Tdata>('t');
    }
    while(inp->good())
    {
      Tdata d=inp->read();
      Tkey mdtest;
      Metadata *md=dynamic_cast<Metadata*>(&d);
      mdtest=md->get<Tkey>(key);
      if(eqt.is_equal(mdtest))
      {
        outp->write(d);
        ++nout;
      }
    }
    return nout;
  }catch(...)
  {
    if(inp!=NULL) delete inp;
    if(outp!=NULL) delete outp;
    throw;
  }
}
/* A bit of an ugly generic function that is a top level template to do the full task of this
   program.   It was created because the original implementation had this block in a chain of
   switch-case blocks and was even uglier.   The procedure is a front end with variable
   object data types (Tdata parameter) for the templates filter_by_range and filter_by_match.

   The ugliest part of this procedure is the (evil by purest views) of opaque pointers to pass
   the range and qual test variables.  This was done to reduce the absurdly long arg list
   that would have been required otherwise.

Arguments:
   keytype - main switch for Metadata key type
   valeq - pointer to equal test value (ignore if equal_tester is false).
   valmin to valmax  - specify range values for ge and le tests (respectively) passed
     to range_tester generic function (ignored if equal_test is true).
     */
template <typename Tdata> int subset_processor(string key, MDtype keytype,
      void *valeq, void *valmin, void *valmax, bool equal_test,
      bool minonly, bool maxonly, bool binary_data)
{
  int nout;
  try{
    switch(keytype)
    {
      case MDint:
        if(equal_test)
        {
          int *ieq=static_cast<int*>(valeq);
          equal_tester<int> eqt(*ieq);
          nout=filter_by_match<Tdata,int>(key,eqt,binary_data);
        }
        else
        {
          int *imin,*imax;
          imin=static_cast<int*>(valmin);
          imax=static_cast<int*>(valmax);
          range_tester<int> rt(*imin,*imax,minonly,maxonly);
          nout=filter_by_range<Tdata,int>(key,rt,binary_data);
        }
        break;
      case MDreal:
        if(equal_test)
        {
          double *req=static_cast<double*>(valeq);
          equal_tester<double> eqt(*req);
          nout=filter_by_match<Tdata,double>(key,eqt,binary_data);
        }
        else
        {
          double *rmin,*rmax;
          rmin=static_cast<double*>(valmin);
          rmax=static_cast<double*>(valmax);
          range_tester<double> rt(*rmin,*rmax,minonly,maxonly);
          nout=filter_by_range<Tdata,double>(key,rt,binary_data);
        }
        break;
      case MDstring:
        if(equal_test)
        {
          string *seq=static_cast<string*>(valeq);
          equal_tester<string> eqt(*seq);
          nout=filter_by_match<Tdata,string>(key,eqt,binary_data);
        }
        else
        {
          string *smin,*smax;
          smin=static_cast<string*>(valmin);
          smax=static_cast<string*>(valmax);
          range_tester<string> rt(*smin,*smax,minonly,maxonly);
          nout=filter_by_range<Tdata,string>(key,rt,binary_data);
        }
        break;
      default:
        cerr<<"subset_streamfile:   coding problem.  Main processing routine passed invalid MDtype"
          <<endl;
        exit(-1);
    };
    return nout;
  }catch(...){throw;};
}

bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    int i;
    const int narg_required(1);
    if(argc<2) usage();
    if(string(argv[1])=="--help") usage();
    bool binary_data(false);
    bool equal_test(false);
    bool minonly(false);
    bool maxonly(false);
    bool test_range(false);
    std::pair<string,MDtype> arg1=split_arg1(argv[1]);
    string key=arg1.first;
    MDtype keytype=arg1.second;
    double req,rmin,rmax;
    int ieq,imin,imax;
    string seq,smin,smax;
    // Ugly opaque pointers used to reduce arg list complexity
    void *veq,*vmin,*vmax;
    ObjectsSupported object_type(TCS);

    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-eq")
        {
          ++i;
          if(i>=argc) usage();
          switch(keytype)
          {
            case MDint:
              ieq=atoi(argv[i]);
              veq=static_cast<void *>(&ieq);
              break;
            case MDreal:
              req=atof(argv[i]);
              veq=static_cast<void *>(&req);
              break;
            case MDstring:
            default:
              seq=string(argv[i]);
              veq=static_cast<void *>(&seq);
          }
          equal_test=true;
        }
        else if(sarg=="-min")
        {
          ++i;
          if(i>=argc) usage();
          switch(keytype)
          {
            case MDint:
              imin=atoi(argv[i]);
              vmin=static_cast<void *>(&imin);
              break;
            case MDreal:
              rmin=atof(argv[i]);
              vmin=static_cast<void *>(&rmin);
              break;
            case MDstring:
            default:
              smin=string(argv[i]);
              vmin=static_cast<void *>(&smin);
          }
          minonly=true;
          maxonly=false;
          test_range=false;
        }
        else if(sarg=="-max")
        {
          ++i;
          if(i>=argc) usage();
          switch(keytype)
          {
            case MDint:
              imax=atoi(argv[i]);
              vmax=static_cast<void *>(&imax);
              break;
            case MDreal:
              rmax=atof(argv[i]);
              vmax=static_cast<void *>(&rmax);
              break;
            case MDstring:
            default:
              smax=string(argv[i]);
              vmax=static_cast<void *>(&rmax);
          }
          maxonly=true;
          minonly=false;
          test_range=false;
        }
        else if(sarg=="-range")
        {
          i+=2;
          if(i>=argc) usage();
          switch(keytype)
          {
            case MDint:
              imin=atoi(argv[i-1]);
              imax=atoi(argv[i]);
              vmin=static_cast<void *>(&imin);
              vmax=static_cast<void *>(&imax);
              break;
            case MDreal:
              rmin=atof(argv[i-1]);
              rmax=atof(argv[i]);
              vmin=static_cast<void *>(&rmin);
              vmax=static_cast<void *>(&rmax);
              break;
            case MDstring:
            default:
              smin=string(argv[i-1]);
              smax=string(argv[i]);
              vmin=static_cast<void *>(&smin);
              vmax=static_cast<void *>(&smax);
          };
          minonly=false;
          maxonly=false;
          test_range=true;
        }
        else if(sarg=="-objt")
        {
          ++i;
          if(i>=argc)usage();
          sarg=string(argv[i]);
          if(sarg=="ThreeComponentSeismogram")
            object_type=TCS;
          else if(sarg=="ThreeComponentEnemble")
            object_type=TCE;
          else if(sarg=="TimeSeries")
            object_type=TS;
          else if(sarg=="TimeSeriesEnsemble")
            object_type=TSE;
          else if(sarg=="PMTimeSeries")
            object_type=PMTS;
          else
          {
            cerr<<"Unsupported object type="<<sarg<<endl;
            usage();
          }
        }
        else if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-binary")
        {
            binary_data=true;
        }
        else
            usage();
    }
    /* Some sanity checks */
    if(!(test_range || minonly || maxonly || equal_test))
    {
      cerr << "no subset test defined"<<endl;
      usage();
    }
    if((equal_test && minonly) || (equal_test && maxonly) || (equal_test && test_range) )
    {
      cerr << "Inconsistent arguments:   Cannot specify interval subset and match subset"<<endl;
      usage();
    }
    if(test_range)
    {
      switch (keytype) {
        case MDint:
          if(imin>=imax)
          {
            cerr << "Interval mismatch for integer:  min="<<imin
              << " max="<<imax<<endl;
            usage();
          }
          break;
        case MDreal:
            if(rmin>=rmax)
            {
              cerr << "Interval mismatch for real key:  min="<<rmin
                << " max="<<rmax<<endl;
              usage();
            }
            break;
        case MDstring:
            if(smin>=smax)
            {
              cerr << "Interval mismatch for string key:  min="<<smin
                  << " max="<<smax<<endl;
              usage();
            }
            break;
        default:
            /* This block should actually never be executed */
            cerr << "unknown key time"<<endl;
            usage();
      }
    }
    try{
      int nout;
      /* Wonder if there is a cleaner way to do this, but we need to tell
      the compiler to instantiate all the possible options here */
      switch(object_type)
      {
        case TCE:
          nout=subset_processor<ThreeComponentEnsemble>(key,keytype,
                    veq,vmin,vmax,equal_test,minonly,maxonly,binary_data);
          break;
        case TCS:
          nout=subset_processor<ThreeComponentSeismogram>(key,keytype,
                    veq,vmin,vmax,equal_test,minonly,maxonly,binary_data);
          break;
        case TS:
          nout=subset_processor<TimeSeries>(key,keytype,
                    veq,vmin,vmax,equal_test,minonly,maxonly,binary_data);
          break;
        case TSE:
          nout=subset_processor<TimeSeriesEnsemble>(key,keytype,
                    veq,vmin,vmax,equal_test,minonly,maxonly,binary_data);
          break;
        case PMTS:
          /*
          nout=subset_processor<PMTimeSeries>(key,keytype,
                    veq,vmin,vmax,equal_test,minonly,maxonly,binary_data);
                    */
          cerr << "ParticleMotionTimeSeries not yet supported"<<endl;
          exit(-1);
          break;
      }
      cerr << "subset_streamfile:  Total number of objects copied to output="<<nout<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
