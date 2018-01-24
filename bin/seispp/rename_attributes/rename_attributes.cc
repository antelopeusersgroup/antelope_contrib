#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "stock.h"
#include "seispp.h"
#include "Metadata.h"
#include "ensemble.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;   
using namespace SEISPP;
void usage()
{
    cerr << "rename_attributes < in > out [-t object_type -v --help -text -pf pffile]"
        <<endl
        << "Changes the name (or copies) of one more attributes with generic headers"<<endl
        << "Editing is controlled by parameter file definition (see master)"<<endl
        << " Use -t to select object type expected for input. "<<endl
        << " (Allowed options=ThreeComponentEnsemble, ThreeComponentSeismogram(default), TimeSeries, and TimeSeriesEnsemble)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl
        << " -pf - use alternative paramter file pffile instead of default rename_attributes.pf"
        <<endl;
    exit(-1);
}
class EditDefinition
{
    public: 
        /* I think it is necessary to implement this to maek operator[] work right in
         * MetadataEditor below */
        EditDefinition();
        EditDefinition(const char *);
        bool delete_old;
        string oldname;
        string newname;
        MDtype mdt;
};
EditDefinition::EditDefinition()
{
    delete_old=false;
    oldname="UNDEFINED";
    newname="UNDEFINED";
    mdt=MDinvalid;
}
EditDefinition::EditDefinition(const char *line)
{
    try{
        const string base_error("EditDefinition constructor:  ");
        stringstream ss(line);
        string stoken;
        ss >> stoken;
        if(stoken=="copy")
        {
            delete_old=false;
        }
        else
        {
            delete_old=true;
        }
        ss >> oldname;
        ss >> newname;
        ss >> stoken;
        if( (stoken=="real") || (stoken=="REAL") || (stoken=="float") || (stoken=="double"))
        {
            mdt=MDreal;
        }
        else if( (stoken=="int") || (stoken=="INT") || (stoken=="integer") )
        {
            mdt=MDint;
        }
        else if( (stoken=="boolean") || (stoken=="BOOL") || (stoken=="bool") )
        {
            mdt=MDboolean;
        }
        else if ( (stoken=="string") || (stoken=="STRING") )
        {
            mdt=MDstring;
        }
        else
        {
            /* We use this as the main error trap.  If there is a format error
             * we assume this will generate name mismatch here. */
            stringstream errss;
            errss<<base_error<<"Error in edit definition"<<endl
                << "Offending line->"<<line<<endl;
            throw SeisppError(errss.str());
        }
    }catch(...){throw;};
}
class MetadataEditor
{
  public:
    MetadataEditor(Pf *pf);
    MetadataEditor(const MetadataEditor& parent);
    MetadataEditor& operator=(const MetadataEditor& parent);
    /*! Actually does the work on a Metadata object. 
     * 
     * Apply the edit rules defined in the constructor to a Metadata object.
     * As used here that will always come through inheritance.  
     *
     * \param d - data to edit
     * \return number of values changed
     * */
    int edit(Metadata& d);
  private:
    list<EditDefinition> editlist;
};
MetadataEditor::MetadataEditor(Pf *pf)
{
    const string base_error("MetadataEditor pf constructor:  ");
    string tblkey("edit_list");
    Tbl *t;
    t=pfget_tbl(pf,const_cast<char*>(tblkey.c_str()));
    if(t==NULL)
    {
        throw SeisppError(base_error+"pf file is missing the Tbl with key="
                + tblkey);
    }
    try{
        int i;
        char *s;
        for(i=0;i<maxtbl(t);++i)
        {
            s=(char *)gettbl(t,i);
            EditDefinition edef(s);
            editlist.push_back(edef);
        }
        freetbl(t,free);
    }catch(...){throw;};
}
MetadataEditor::MetadataEditor(const MetadataEditor& parent)
{
    editlist=parent.editlist;
}
MetadataEditor& MetadataEditor::operator=(const MetadataEditor& parent)
{
    if(&parent!=this)
    {
        editlist=parent.editlist;
    }
    return (*this);
}
int MetadataEditor::edit(Metadata& d)
{
    list<EditDefinition>::iterator eptr;
    int count(0);
    for(eptr=editlist.begin();eptr!=editlist.end();++eptr)
    {
        long ival;
        double dval;
        bool bval;
        string sval;
        try{
            switch(eptr->mdt)
            {
                case MDint:
                    ival=d.get<long>(eptr->oldname);
                    d.put(eptr->newname,ival);
                    if(eptr->delete_old) d.remove(eptr->oldname);
                    break;
                case MDreal:
                    dval=d.get<double>(eptr->oldname);
                    d.put(eptr->newname,dval);
                    if(eptr->delete_old) d.remove(eptr->oldname);
                    break;
                case MDboolean:
                    bval=d.get_bool(eptr->oldname);
                    d.put(eptr->newname,bval);
                    if(eptr->delete_old) d.remove(eptr->oldname);
                    break;
                case MDstring:
                default:
                    sval=d.get<string>(eptr->oldname);
                    d.put(eptr->newname,sval);
                    if(eptr->delete_old) d.remove(eptr->oldname);
            };
            ++count;
        }catch(SeisppError& serr)
        {
            cerr << "MetadataEditor::edit method:  edit failed.  Message posted follows:"
                <<endl;
            serr.log_error();
            cerr << "Output may cause downstream problems"<<endl;
        }
    }
    return count;
}


/* This procedure parses an input string (normally from argv) 
 * to set a list of allowed objects.   This could be a library
 * procedure, but with this one can customize the set of objects
 * supported. */
enum AllowedObjects {TCS, TCE,  TS, TSE};
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
    else
    {
        cerr << "Do not know how to handle object type="<<otype
            <<endl<< "Cannot continue"<<endl;
        exit(-1);
    }
}
template <typename DataType> pair<int,int> EditFile(MetadataEditor& mde,bool binary_data)
{
    try{
        char form('t');
        if(binary_data) form='b';
        StreamObjectReader<DataType> inp(form);
        StreamObjectWriter<DataType>  outp(form);
        int count;
        int n,nchanged;
        DataType d;
        while(inp.good())
        {
            d=inp.read();
            n=mde.edit(d);
            outp.write(d);
            ++count;
            nchanged+=n;
        }
        pair<int,int> countreturn(count,nchanged);
        return countreturn;
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
    string pffile("rename_attributes");

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
        Pf *pf;
        if(pfread(const_cast<char*>(pffile.c_str()),&pf))
        {
            cerr << "pfread failed on pffile="<<pffile<<endl;
            exit(-1);
        }
        MetadataEditor mde(pf);
        AllowedObjects dtype=get_object_type(otype);
        pair<int,int> count;
        switch (dtype)
        {
            case TCS:
                count=EditFile<ThreeComponentSeismogram>(mde,binary_data);
                break;
            case TCE:
                count=EditFile<ThreeComponentEnsemble>(mde,binary_data);
                break;
            case TS:
                count=EditFile<TimeSeries>(mde,binary_data);
                break;
            case TSE:
                count=EditFile<TimeSeriesEnsemble>(mde,binary_data);
                break;
            default:
                cerr << "Coding problem - dtype variable does not match enum"
                    <<endl
                    << "Fatal error - bug fix required. "<<endl;
                exit(-1);
        };
        if(SEISPP_verbose)
        {
            cerr << "rename_attributes:  processed "<<count.first<<" objects"<<endl
                << "Number of changes made="<<count.second<<endl;
        }
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}

