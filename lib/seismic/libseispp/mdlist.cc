#include <strstream.h>
#include "stock.h"
#include "pf.h"
#include "metadata.h"
namespace SEISPP
{

Metadata_list pfget_mdlist(Pf *pf,string tag)
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
		istrstream instr(line);
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
		else if(mdtype=="list" || mdtype=="LIST")
			mdt.mdt = MDlist;
		else if(mdtype=="map" || mdtype=="MAP")
			mdt.mdt = MDmap;

		mdl.push_back(mdt);
	}
	return(mdl);
}
} // Termination of namespace SEISPP definitions

