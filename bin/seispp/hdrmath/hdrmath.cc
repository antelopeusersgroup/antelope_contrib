#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include <list>
#include "PMTimeSeries.h"
#include "seispp.h"
#include "ThreeComponentSeismogram.h"
#include "PfStyleMetadata.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "HdrmathArg.h"
typedef list<HdrmathArg> Args;
enum AllowedOperators {EQ,PEQ,MEQ,TEQ,DEQ,MODEQ,PLUS,MINUS,TIMES,DIVIDE,MOD};
typedef list<AllowedOperators> Ops;
using namespace std;
using namespace SEISPP;
void usage()
{
  cerr << "hdrmath X op1 Y [ op Z ... op XX] [-t object_type -v --help -text]"
    <<endl
    << "Fairly generic filter to do header (Metadata) math. "<<endl
    << "X, Y, Z, ..., and XX can be symbols or numbers depending on use"
    <<endl
    << "When these reference header (Metadata) attributes they must be of the form"
    << endl
    << " Name:Type where Name is the key and Type names a standard numeric type"
    << " Allowed are:  int, long, real, double, float"<<endl
    << " (Note all int's are promoted to long and float's are promoted to double." <<endl
    << "and if any decimal numbers are in a string the result is double."<<endl

    << "op1 and op here are short for (arithmetic) operators mostly following C convention"
    <<endl
    << "op1 must be one of:  = *= += or /=.  "
    << "(These behave as in C.  e.g. rx *= 1000.0 is the same as rx=rx*1000.0)"
    <<endl
    << "Similar op symbolizes binary operators between successive argv pairs"
    << "Allowed values are:  +, -, *, /, and %"<<endl
    << "Calculations always proceed left to right without normal associative rules."
    << "Examples:"
    << " (1)  hdrmath sta -= 1 * 3  is the same as sta = (sta-1)*3"
    << " (2)  hdrmath delta *= 0.0174532925 is the same as delta=delta*0.0174532925"
    << "      (Note that is a conversion from degrees to radians)"<<endl
    << " (3)  hdrmath dist=delta*0.0174532925 is similar but write as dist instead of overwriting delta"
    <<endl

    << " Use -t to select object type expected for input. "<<endl
    << " (Allowed options=ThreeComponentEnsemble,ThreeComponentSeismogram (default),"<<endl
    << " TimeSeries, TimeSeriesEnsemble, and PMTimeSeries)"<<endl
    << " Note ensembles reference ensemble metadata NOT member attributes"<<endl
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
AllowedOperators ParseOperatorString(const char *arg)
{
  AllowedOperators a;
  string sarg(arg);
  if(sarg=="=")
    a=EQ;
  else if(sarg=="+=")
    a=PEQ;
  else if(sarg=="-=")
    a=MEQ;
  else if(sarg=="*=")
    a=EQ;
  else if(sarg=="/=")
    a=DEQ;
  else if(sarg=="%=")
    a=MODEQ;
  else if(sarg=="+")
    a=PLUS;
  else if(sarg=="-")
    a=MINUS;
  else if(sarg=="*")
    a=TIMES;
  else if(sarg=="/")
    a=DIVIDE;
  else if(sarg=="%")
    a=MOD;
  else
    throw SeisppError("ParseOperatorString procedure:  syntax error for arg="
        +sarg+"Must be one of C operator definitions (e.g., =,+=, or +)");
  return a;
}
/* This algorith assums argc and argv have been cleaned with the 
   algorithm in main. It makes some sanity checks and exits for a
   range of likely blunders */
pair<Args,Ops> ParseArgList(int argc, char **argv)
{
  const string base_error("hdrmath: ParseArglist procedure:\n");
  try{
    Args a;
    Ops op;
    int i;
    /* A few basic sanity check */
    if(argc<3) usage();
    if((argc%2)!=1)
    {
      cerr << "Argument list is botched - argument count="<<argc<<endl
        << "The count must be an odd number"<<endl;
      usage();
    }
    /* Note the original argv[0] is assumed stripped in cleaning*/
    for(i=0;i<argc;++i)
    {
      AllowedOperators thisop;
      /* this somewhat odd construct assumes odd i contain data args
         and even i contain operator definitions.   We test each for
         syntax errors and exit if there are any issues. */
      if(i%2)
      {
        // Operators will have i even.  This procedure may throw an error
        thisop=ParseOperatorString(argv[i]);
        op.push_back(thisop);
      }
      else
      {
        //Args are odd - this also will throw errors for syntax. 
        HdrmathArg thisarg(argv[i]);
        a.push_back(thisarg);
      }
    }
    /* These conditions are not allowed so we have to abort */
    AllowedOperators fop(*(op.begin()));
    switch(fop)
    {
      case EQ:
      case PEQ:
      case TEQ:
      case DEQ:
      case MODEQ:
        break;
      default:
        cerr << "hdrmath(Fatal Error):  Illegal first operator in operation list"<<endl
          << "First operator must be one of:  =,+=,-=,*=,/=, or %="<<endl;
        usage();
    };
    /* Similarly anything after first op must be one of the binary operators */
    Ops::iterator optr;
    optr=op.begin();
    for(++optr;optr!=op.end();++optr)
    {
      switch((*optr))
      {
        case PLUS:
        case MINUS:
        case TIMES:
        case DIVIDE:
        case MOD:
          break;
        default:
          cerr << "hdrmath (Fatal Error):  Illegal operator in arithmetic chain"<<endl
            << "All operators after the first must be one of +, -, *, /, or %"<<endl;
          usage();
      };
    }
    return (pair<Args,Ops> (a,op));
  }catch(...){throw;};
}
HdrmathArg arg_compute(Metadata d, pair<Args,Ops>& mathlist)
{
  try{
    Args a(mathlist.first);
    Ops op(mathlist.second);
    Args::iterator aptr;
    Ops::iterator optr;
    /* The first arg is the left hand side.  Treat it specially */
    aptr=a.begin();
    HdrmathArg lhs(*aptr);
    optr=op.begin();
    /* When first op is = we will set it from the chain of arguments.  For
       operators like += we need an initia value.  This constructor is
       assumed to do that.*/
    if((*optr)!=EQ)
      lhs=HdrmathArg(d,lhs.key,lhs.mdt);
    else
    {
      Args::iterator ap2(aptr);
      ++ap2;
      switch((*optr))
      {
        case PEQ:
          lhs += (*ap2);
          break;
        case MEQ:
          lhs -= (*ap2);
          break;
        case TEQ:
          lhs *= (*ap2);
          break;
        case DEQ:
          lhs /= (*ap2);
          break;
        case MODEQ:
          lhs %= (*ap2);
          break;
        default:
          cerr<<"hdrmath coding error:   Illegal first operator"<<endl
            << "Earlier code should have fixed this - contact author if you get this message"
            <<endl;
          exit(-1);
      };
    }
    /* For simplicity and minor speed issue return immediately if there 
       are no more operators */
    if(op.size()==1) return(lhs);
    /* For robustness make sure the iterators are properly set before entering 
       the following loop.  Cost to not trust above is assumed negligible */
    aptr=a.begin();
    optr=op.begin();
    ++aptr; ++aptr;
    ++optr;
    for(;(aptr!=a.end())&&(optr!=op.end());++aptr,++optr)
    {
      /* We could do these with operators like += but I use this approach
         to make the logic clearer */
      HdrmathArg a1(lhs);
      switch((*optr))
      {
        case PLUS:
          lhs=a1+(*aptr);
          break;
        case MINUS:
          lhs=a1-(*aptr);
          break;
        case TIMES:
          lhs=a1*(*aptr);
          break;
        case DIVIDE:
          lhs=a1/(*aptr);
          break;
        case MOD:
          lhs=a1%(*aptr);
          break;
        default:
          cerr<<"hdrmath coding error:   Illegal first operator in chain"<<endl
            << "Earlier code should have prevented this problem - contact author if you get this message"
            <<endl;
          exit(-1);

      };
    }
    return lhs;
  }catch(...){throw;};
}
template <typename DataType> pair<int,int> hdrmath(pair<Args,Ops>& mathlist,bool binary_data)
{
  try{
    char form('t');
    if(binary_data) form='b';
    StreamObjectReader<DataType> inp(form);
    StreamObjectWriter<DataType>  outp(form);
    int count(0);
    int totalcount(-1); //-1 because of position of increment operator
    DataType d;
    while(inp.good())
    {
      ++totalcount;
      d=inp.read();
      HdrmathArg math_result;
      try{
        math_result=arg_compute(dynamic_cast<Metadata&>(d),mathlist);
        if(math_result.mdt == MDint)
        d.put(math_result.key,math_result.get_long());
        else if(math_result.mdt == MDreal)
        d.put(math_result.key,math_result.get_double());
        else
        {
        cerr << "hdrmath main procedure:  coding error"<<endl
          << "If this block is entered debug is required - contact author"
          <<endl;
        exit(-1);
        }
        ++count;
      }catch(SeisppError& serr)
      {
        cerr << "Error in processing object number "<<totalcount<<endl
        << "Passing this object downstream without change"<<endl
        << "Message posted was this:"<<endl;
        serr.log_error();
      }
      outp.write(d);
    }
    return pair<int,int>(totalcount,count);
  }catch(...){throw;};
}

bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    bool binary_data(true);
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    string otype("ThreeComponentSeismogram");
    /* This scan of the arg list does something nonstandard.  
       We generate a new series of args cleaning the args that
       contain things like -v.   The result can then be scanned
       more cleanly for errors. */
    int cargc(0);
    char **cargv;
    /* We need to allow for cargv.  Conservatively alloc for 
       a pointer to hold all argc initial values. */
    cargv=new char*[argc];
    for(i=0;i<argc;++i) cargv[i]=NULL;   // initialize for best practice
    /* Now scan for other args and generate clean argc and argv */
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
          cargv[cargc]=argv[i];
          ++cargc;
        }
    }
    try{
        AllowedObjects dtype=get_object_type(otype);
        pair<Args,Ops> mathlist=ParseArgList(cargc,cargv);
        pair<int,int> count;
        Args::iterator aptr;
        aptr=mathlist.first.begin();
        string resultkey=aptr->key;
        switch (dtype)
        {
            case TCS:
                count=hdrmath<ThreeComponentSeismogram>(mathlist, binary_data);
                break;
            case TCE:
                count=hdrmath<ThreeComponentEnsemble>(mathlist, binary_data);
                break;
            case TS:
                count=hdrmath<TimeSeries>(mathlist, binary_data);
                break;
            case TSE:
                count=hdrmath<TimeSeriesEnsemble>(mathlist, binary_data);
                break;
            case PMTS:
                    count=hdrmath<PMTimeSeries>(mathlist, binary_data);
                    break;
            default:
                cerr << "hdrmath coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };
        if(SEISPP_verbose)
          cerr << "hdrmath:  processed "<<count.first<<" objects"<<endl
            << "    Set header field "<<resultkey<<" in "<<count.second
            << " objects"<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
