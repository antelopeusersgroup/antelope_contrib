#include <sstream>
#include "stock.h"
#ifndef NO_ANTELOPE
#include "pf.h"
#endif
#include "Metadata.h"
#include "PfStyleMetadata.h"
namespace SEISPP
{
using namespace SEISPP;

#ifndef NO_ANTELOPE
MetadataList pfget_mdlist(Pf *pf,string tag)
{
        MetadataList mdl;
	Metadata_typedef mdt;	
	string mdname, mdtype;
	Tbl *t;

	t = pfget_tbl(pf,(char *)tag.c_str());
	if(t==NULL) return(mdl);  // an empty list means copy nothing

	for(int i=0;i<maxtbl(t);++i)
	{
		char *line;
		line=(char *)gettbl(t,i);
		istringstream instr(line);
		instr >> mdname;
		instr >> mdtype;
		mdt.tag = mdname;
		if(mdtype=="real" || mdtype=="REAL")
			mdt.mdt = MDreal;
		else if(mdtype=="int" || mdtype=="INT" || mdtype=="integer")
			mdt.mdt = MDint;
		else if(mdtype=="string" || mdtype=="STRING")
			mdt.mdt = MDstring;
		else if(mdtype=="boolean" || mdtype=="BOOLEAN")
			mdt.mdt = MDboolean;
                else
                        cerr << "pfget_mdlist:  Warning type keyword = "
                            << mdtype << " not recognized."<<endl
                            << mdname <<" attribute in input list ignored"
                            <<endl;

		mdl.push_back(mdt);
	}
	freetbl(t,0);
	return(mdl);
}
#endif
MetadataList get_mdlist(PfStyleMetadata& m,const string tag)
{
    try {
        MetadataList mdl;
	Metadata_typedef mdt;	
	string mdname, mdtype;
        list<string> t=m.get_tbl(tag);
        list<string>::iterator tptr;
        for(tptr=t.begin();tptr!=t.end();++tptr)
        {
            /* This loop is identical to above in pfget_mdlist
               except for the next line*/
		istringstream instr(*tptr);
		instr >> mdname;
		instr >> mdtype;
		mdt.tag = mdname;
		if(mdtype=="real" || mdtype=="REAL")
			mdt.mdt = MDreal;
		else if(mdtype=="int" || mdtype=="INT" || mdtype=="integer")
			mdt.mdt = MDint;
		else if(mdtype=="string" || mdtype=="STRING")
			mdt.mdt = MDstring;
		else if(mdtype=="boolean" || mdtype=="BOOLEAN")
			mdt.mdt = MDboolean;
                else
                        cerr << "get_mdlist:  Warning type keyword = "
                            << mdtype << " not recognized."<<endl
                            << mdname <<" attribute in input list ignored"
                            <<endl;

		mdl.push_back(mdt);
	}
        return(mdl);
    }catch(...){throw;};
}
} // Termination of namespace SEISPP definitions

