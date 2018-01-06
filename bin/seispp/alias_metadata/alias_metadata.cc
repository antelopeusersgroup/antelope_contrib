#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <list>
#include <iostream>
#include <memory>
#include "stock.h"
#include "PMTimeSeries.h"
#include "seispp.h"
#include "ensemble.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;   
using namespace SEISPP;
void usage()
{
    cerr << "alias_metadata  < in > out [-pf pffile -t object_type -v --help -text]"
        <<endl
        << "Makes one or more metadata attribute names equivalent"<<endl
        << "In data processing name mismatches of metadata attributes are common"
        <<endl
        << "A type example for seismic data is the mismatch of active source "
        << endl<<"naming and passive array names for similar concepts"<<endl
        << "This program does with the names defined by a parameter file"
        <<endl
        << " use -pf option to change parameter file from default alias_metadata.pf"
        <<endl
        << " Use -t to select object type expected for input. "<<endl
        << " (Allowed options=ThreeComponentEnsemble (default),ThreeComponentSeismogram, TimeSeries, and TimeSeriesEnsemble)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
class AttributeAlias
{
    public:
        AttributeAlias();
        AttributeAlias(string onm, string nnm,MDtype mdtin);
        AttributeAlias(const AttributeAlias& parent);
        AttributeAlias& operator=(const AttributeAlias& parent);
        string old_name(){return oldnm;};
        string new_name(){return newnm;};
        MDtype AttributeType(){return mdt;};
    private:
        string oldnm;
        string newnm;
        MDtype mdt;
};
AttributeAlias::AttributeAlias()
{
    oldnm="UNDEFINED";
    newnm="BAD";
}
AttributeAlias::AttributeAlias(string o, string n, MDtype mdtin)
{
    oldnm=o;
    newnm=n;
    mdt=mdtin;
}
AttributeAlias::AttributeAlias(const AttributeAlias& parent)
{
    oldnm=parent.oldnm;
    newnm=parent.newnm;
    mdt=parent.mdt;
}
AttributeAlias& AttributeAlias::operator=(const AttributeAlias& parent)
{
    if(this != (&parent))
    {
        oldnm=parent.oldnm;
        newnm=parent.newnm;
        mdt=parent.mdt;
    }
    return *this;
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
list<AttributeAlias> parse_alias_list(Pf *pf)
{
    list<AttributeAlias> result;
    Tbl *t;
    string tag("aliases");
    t=pfget_tbl(pf,const_cast<char*>(tag.c_str()));
    if(t==NULL)
    {
        cerr << "alias_metadata: Required Tbl tag attribute aliases not found in parameter file"
            <<endl
            << "Fatal error - cannot continue"<<endl;
        exit(-1);
    }
    char *s;
    int i;
    int n=maxtbl(t);
    if(n<=0) 
    {
        cerr << "alias_metadata: aliases tbl is emnpty.  Fatal error"<<endl;
        exit(-1);
    }
    for(i=0;i<n;++i)
    {
        s=(char *)gettbl(t,i);
        stringstream ss(s);
        string o,n,tname;
        MDtype mdt;
        ss >> o;  ss>>n; ss>>tname;
        if(tname=="real" || tname=="REAL")
            mdt=MDreal;
        else if(tname=="int" || tname=="long" || tname=="INT" || tname=="LONG")
            mdt=MDint;
        else if(tname=="string" || tname=="STRING")
            mdt=MDstring;
        else if(tname=="bool" || tname=="boolean")
            mdt=MDboolean;
        else
        {
            cerr << "alias_metadata:  error in parameter file data"<<endl
                << "Illegal type name="<<tname<<" specified for attribute key="
                << o<<endl
                << "FATAL ERROR - cannot continue"<<endl;
            exit(-1);
        }
        AttributeAlias aa(o,n,mdt);
        result.push_back(aa);
    }
    return result;
}
template <typename DataType> int set_aliases(list<AttributeAlias> aliases, 
        bool binary_data)
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
            list<AttributeAlias>::iterator aptr;
            d=inp.read();
            for(aptr=aliases.begin();aptr!=aliases.end();++aptr)
            {
                double dval;
                long ival;
                string sval;
                bool bval;
                /* We make failure to find name yield only an error.
                 * That may not be the best but for now will do that. */
                try{
                    string key=aptr->old_name();
                    MDtype mdt=aptr->AttributeType();
                    switch (mdt)
                    {
                        case MDreal:
                            dval=d.get<double>(key);
                            d.put(key,dval);
                            break;
                        case MDint:
                            ival=d.get<long>(key);
                            d.put(key,ival);
                            break;
                        case MDstring:
                            sval=d.get_string(key);
                            d.put(key,sval);
                            break;
                        case MDboolean:
                            bval=d.get_bool(key);
                            d.put(key,bval);
                            break;
                        case MDinvalid:
                        default:
                            /* Without a memory fault this should not happen*/
                            cerr<<"alias_meatdata (Warning):  undefined type for metadata with key="
                                << key<<endl<<"Skipped"<<endl;
                    };
                }catch (MetadataGetError& mderr)
                {
                    cerr << "alias_metadata (Warning):   old name for alias="
                        << aptr->old_name()<< " not found in file member="
                        <<count<<endl
                        << "Blundering on - expect downstream errors "
                        << "if being run in pipeline"
                        <<endl;
                }
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
    bool binary_data(true);
    string pffile("alias_metadata.pf");
    string otype("ThreeComponentSeismogram");

    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-pf")
        {
            ++i;
            if(i>=argc)usage();
            pffile=string(argv[i]);
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
    Pf *pf;
    if(pfread(const_cast<char*>(pffile.c_str()),&pf))
    {
        cerr << "alias_metadata:: Fatal error"<<endl
            <<"pfread failed on file="<<pffile<<endl;
        exit(-1);
    }
    list<AttributeAlias> aalist=parse_alias_list(pf);
    try{
        /* This approach depends upon the use of a template as
         * a generic method to implement the algorithm being 
         * implemented.   This example does nothing but copy
         * input to output but provides a starting point for
         * algorithms that can be done on multiple object types. */
        AllowedObjects dtype=get_object_type(otype);
        int count;
        switch (dtype)
        {
            case TCS:
                count=set_aliases<ThreeComponentSeismogram>(aalist,binary_data);
                break;
            case TCE:
                count=set_aliases<ThreeComponentEnsemble>(aalist,binary_data);
                break;
            case TS:
                count=set_aliases<TimeSeries>(aalist,binary_data);
                break;
            case TSE:
                count=set_aliases<TimeSeriesEnsemble>(aalist,binary_data);
                break;
            case PMTS:
                count=set_aliases<TimeSeriesEnsemble>(aalist,binary_data);
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

