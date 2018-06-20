#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <vector>
#include <list>
#include "seispp.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "ensemble.h"
#include "PfStyleMetadata.h"
using namespace std;   // most compilers do not require this
using namespace SEISPP;  //This is essential to use SEISPP library
class MDTable
{
    public:
        MDTable(PfStyleMetadata pfmd);
        int number_attributes()
        {
            return(nm.size());
        };
        int number_tuples()
        {
            return(strval.size());
        };
        /*! Cautiously set a field from the table.

          This is the primary working method of this object.
          Intended to be applied using a loop over attributes
          without needing the baggage of type cracking. Will
          set attribute at position j associated with tuple i.

          \param d - Metadata object to set
          \param  i - tuple to access
          \param  j - set value associated with attribute at column j

          \return - name of attribute set
          \exception - throws a SeisppError if i and j are outside
          bounds of the table
          */
        string set(Metadata& d,int i, int j);
        /*! Set all attributes defined in tuple j.

        This method should be used if an entire list of attributes
        is always set.

        \param d - Metadata object to set
        \param i - tuple of attributes to be set.

        \return number of attributes set
        */
        int set(Metadata& d,int i);
        /*! Return name of attribute at position j */
        string name(int j){return nm[j];};
        /*! Return type of attribute at position j */
        MDtype type(int j){return t[j];};
    private:
        vector<string> nm;
        vector<MDtype> t;
        vector<string> strval;
};
MDTable::MDTable(PfStyleMetadata pfmd)
{
    MetadataList mdl=get_mdlist(pfmd,"types");
    MetadataList::iterator mptr;
    for(mptr=mdl.begin();mptr!=mdl.end();++mptr)
    {
        this->nm.push_back(mptr->tag);
        this->t.push_back(mptr->mdt);
    }
    list<string> vlist=pfmd.get_tbl("values");
    /* interface returns data we need as a list.  Need to
       convert to a vector container*/
    list<string>::iterator vptr;
    for(vptr=vlist.begin();vptr!=vlist.end();++vptr)
        strval.push_back(*vptr);
}
string MDTable::set(Metadata& d,int i0, int j0)
{
    const string range_error("MDTable::set method:  index out of range\n");
    if(i0<0) throw SeisppError(range_error + "tuple index requested was negative");
    if(j0<0) throw SeisppError(range_error
            + "attribute (column) index requested was negative");
    if(i0>=strval.size()) throw SeisppError(range_error
            + "tuple index requested is larger than table size");
    if(j0>=nm.size()) throw SeisppError(range_error
            + "attribute (column) index requested is larger than list of attributes");
    stringstream ss(strval[i0]);
    int j;
    for(j=0;j<=j0;++j)
    {
        double dval;
        long ival;
        string s_now;
        ss >> s_now;
        if(j<j0) continue;
        switch(this->t[j])
        {
            case MDreal:
                dval=atof(s_now.c_str());
                d.put(this->nm[j],dval);
                break;
            case MDint:
                ival=atol(s_now.c_str());
                d.put(this->nm[j],ival);
                break;
            case MDstring:
                d.put(this->nm[j],s_now);
                break;
            case MDinvalid:
            default:
                throw SeisppError(string("MDtable::set method")
                        + " Parsing problem.  Name="
                        + this->nm[j] + " has invalid type defined");
        }
    }
    return(nm[j]);
}


int MDTable::set(Metadata& d,int i0)
{
    /* Return immediately if the table is empty */
    if(strval.size()<=0) return 0;
    const string range_error("MDTable::set method:  index out of range\n");
    if(i0<0) throw SeisppError(range_error + "tuple index requested was negative");
    if(i0>=strval.size()) throw SeisppError(range_error
            + "tuple index requested is larger than table size");
    stringstream ss(strval[i0]);
    int j,count;
    for(j=0,count=0;j<this->number_attributes();++j)
    {
        double dval;
        long ival;
        string str;
        switch(this->t[j])
        {
            case MDreal:
                ss>>dval;
                d.put(this->nm[j],dval);
                ++count;
                break;
            case MDint:
                ss>>ival;
                d.put(this->nm[j],ival);
                ++count;
                break;
            case MDstring:
                ss>>str;
                d.put(this->nm[j],str);
                ++count;
                break;
            case MDinvalid:
            default:
                cerr << "MDtable::set(WARNING):  "
                    << "Attribute "<<this->nm[j]
                    << " has invalid type.  Attribure NOT SET"<<endl;
        }
    }
    return count;
}
void usage()
{
    cerr << "set_metadata [-text --help -pf pffile] < infile > outfile"
        <<endl
        << "Input and output are serialized ThreeComponentEnsemble objects"
        <<endl
        << "sets ensemble and/or member metadata using a pf format"<<endl
        << "Assumes members of ensemble are 3C seismogram objects"
        <<endl
        << " -text - switch to text input and output (default is binary)"<<endl
        << " -pf - use alternate pf file to default of set_metadata.pf"<<endl;
    exit(-1);
}
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    int i;
    const int narg_required(0);
    string pffile("set_metadata.pf");
    bool binary_data(true);
    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-pf")
        {
            ++i;
            if(i>=argc)usage();
            pffile=string(argv[i]);
        }
        else if(sarg=="-text")
            binary_data=false;
        else if(sarg=="--help")
            usage();
        else
            usage();
    }
    try{
        const string emdkey("EnsembleMetadata");
        const string memkey("MemberMetadata");
        PfStyleMetadata control=pfread(pffile);
        /* The concept here is that this object will contain
           metadata to be set as global for the ensemble */
        PfStyleMetadata ensmd=control.get_branch(emdkey);
        MDTable ensmdtbl(ensmd);
        /* These entries will have a vector of data for the ensemble
           with a tag */
        PfStyleMetadata memmd=control.get_branch(memkey);
        MDTable memmdtbl(memmd);
        shared_ptr<StreamObjectReader<ThreeComponentEnsemble>> ia;
        if(binary_data)
        {
          ia=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>('b'));
        }
        else
        {
          ia=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>);
        }
        shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>> oa;
        if(binary_data)
        {
          oa=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>('b'));
        }
        else
        {
          oa=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>);
        }
        ThreeComponentEnsemble d;
        int iens=0;
        while(!ia->eof())
        {
          d=ia->read();
        /* This is made a fatal error for now.  It perhaps should
           be handled more gracefully.  Certainly would need to be if
           this were in a larger system */
          if(memmdtbl.number_tuples() != d.member.size())
          {
            cerr << "set_metadata(FATAL ERROR):  "
                << "Size mistmatch in table in pffile="<<pffile
                << " and input data file"<<endl
                << "Ensemble number "<<iens<<" in file read from stdin"<<endl
                << "Table size="<<memmdtbl.number_tuples()
                << " while number of seismograms in ensemble = "
                << d.member.size()<<endl;
            exit(-1);
          }
          /* Sets this ensemble metadata  */
          ensmdtbl.set(dynamic_cast<Metadata&>(d),0);
          int count_returned,count_expected;
          count_expected=memmdtbl.number_attributes();
          for(i=0;i<d.member.size();++i)
          {
            count_returned=memmdtbl.set(dynamic_cast<Metadata&>(d.member[i]),i);
            if(count_returned!=count_expected)
                cerr << "Warning - some attributes not set for member "
                    <<i<<endl;
          }
          oa->write(d);
          ++iens;
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
