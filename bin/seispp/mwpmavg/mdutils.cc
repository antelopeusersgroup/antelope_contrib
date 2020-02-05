#include "iostream"
#include "Metadata.h"
using namespace std;
using namespace SEISPP;
/* This procedure will write one line to csv file extracting from Metadata d using
   the mdl vector to define the output format and null defaults.  It DOES NOT write a newline
   so caller can vary format of where these cells go in a line of output */
int WriteToCSVFile(Metadata& d,MetadataList& mdl, ostream& ofs)
{
    const string Undefined_Value("UNDEFINED");  //written for any cell when a get fails
    int nlast=mdl.size()-1;
    int i;
    int nsaved;
    double dval;
    long ival;
    bool bval;
    string sval;
    MetadataList::iterator mdptr;
    /* We do this to be sure epoch times get printed correctly in all cases.   Makes
       sense here as bloated files are unlikely to be an issue with metadata output*/
    ofs<<std::setprecision(13);
    for(i=0,mdptr=mdl.begin();mdptr!=mdl.end();++mdptr,++i)
    {
        try{
            switch (mdptr->mdt)
            {
                case MDreal:
                    dval=d.get<double>(mdptr->tag);
                    ofs << dval;
                    break;
                case MDint:
                    ival=d.get<long>(mdptr->tag);
                    ofs << ival;
                    break;
                case MDstring:
                    sval=d.get_string(mdptr->tag);
                    ofs << sval;
                    break;
                case MDboolean:
                    bval=d.get_bool(mdptr->tag);
                    ofs << bval;
                    break;
                default:
                    cerr << "Illegal mdtype specified"<<endl
                        << "This should not happen unless the program overwrites itself"<<endl
                        << "Fatal error:  exiting"<<endl;
                    exit(-1);
            }
            ++nsaved;
        }catch (SEISPP::MetadataGetError& err)
        {
            ofs << Undefined_Value;
        }
        if(i<nlast) ofs<<",";
    }
    return nsaved;
}
