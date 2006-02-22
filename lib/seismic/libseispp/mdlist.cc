#include <sstream>
#include "stock.h"
#include "pf.h"
#include "Metadata.h"
namespace SEISPP
{

MetadataList pfget_mdlist(Pf *pf,string tag)
{
	list<Metadata_typedef> mdl;
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

		mdl.push_back(mdt);
	}
	freetbl(t,0);
	return(mdl);
}
} // Termination of namespace SEISPP definitions

