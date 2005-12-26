#include "dbpp.h"
#include "seispp.h"
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
//@{
//  Primary constructor for this object.  
//
// note matchkeys contains metadata namespace associations, not
// database associations.  

// this is an interface that does not encompass the full potential
// of dbmatches.  it is designed to find rows in a single table
// matching metadata driven keys.  

// note the "table" could be a view if it had a name and htis
// should work the same way.  
//@}
DatascopeMatchHandle::DatascopeMatchHandle(DatascopeHandle& parent,
	string table, list<string>keys_list,
	AttributeMap am) : DatascopeHandle(parent)
{
	// We save this copy in the private area of this object
	// so caller doesn't need to keep track.
	amap=am;
	// NULL hook initialized dbmatches.  Also serves as unambiguous
	// do not free implication in destructor 
	hook=NULL; 
	// dbt holds the reference to the requested table
	dbt = dblookup(parent.db,0,const_cast<char *>(table.c_str()),0,0);
	dbscratch_record=dbt;
	dbscratch_record.record=dbSCRATCH;
	// Need to build the translation from internal names in metadata
	// storage and attribute names.  Do this through the attribute map.
	// We push these into a new, private map 	
	map<string,AttributeProperties>::iterator keyiter;
	map<string,AttributeProperties>::iterator keyiterend=am.attributes.end();
	list<string>::iterator internal_name;
	for(internal_name=keys_list.begin();internal_name!=keys_list.end();
				++internal_name)
	{
		keyiter=am.attributes.find(*internal_name);
		if(keyiter==keyiterend)
		{
			throw SeisppError(string("DatascopeMatchHandle constructor:  internal name=")
				+(*internal_name)
				+ string(" is not in AttributeMap object.\n")
				+ string("Check initialization and/or fix coding error"));
		}
		matchkeys.push_back(keyiter->second);
	}
	// Intentionally did not put these in the above to make the
	// algorithm exception safe.  
	int nkeys=matchkeys.size();
	kpattern=newtbl(nkeys);
	tpattern=newtbl(nkeys);
	for(int i=0;i<nkeys;++i)
	{
		settbl(kpattern,i,
			static_cast<void *>(
			const_cast<char *>(matchkeys[i]
				.db_attribute_name.c_str())));
		settbl(tpattern,i,
			static_cast<void *>(
			const_cast<char *>(matchkeys[i]
				.db_attribute_name.c_str())));
	}
}
// This duplicates antelope's duptbl, but I can't make the 
// type casting in that functon work.  Simple solution is to just
// build this small more limited function.
Tbl *copy_string_tbl(Tbl *parent)
{
	Tbl *t;
	int n=maxtbl(parent);
	t=newtbl(n);
	for(int i=0;i<n;++i)
	{
		char *str;
		str=(char *)gettbl(parent,i);
		pushtbl(t,strdup(str));
	}
	return(t);
}

DatascopeMatchHandle::DatascopeMatchHandle(const DatascopeMatchHandle& parent)
	: DatascopeHandle(parent)
{
	dbt=parent.dbt;
	dbscratch_record=parent.dbscratch_record;
	matchkeys=parent.matchkeys;
	// tbl's have to be copied
	kpattern=copy_string_tbl(parent.kpattern);
	tpattern=copy_string_tbl(parent.tpattern);
	// Make the hook null in the copy to handle memory correctly
	hook=NULL;
}
DatascopeMatchHandle::~DatascopeMatchHandle()
{
	if(hook!=NULL) free_hook(&hook);
	freetbl(kpattern,0);
	freetbl(tpattern,0);
}
// should throw and exception when keys not found in metadata
// algorithm will assume keys do 
list<int> DatascopeMatchHandle::find(Metadata& md)
{
	int nscratch_records_set;
	int i;
	for(i=0,nscratch_records_set=0;i<matchkeys.size();++i)
	{
		double rval;
		int ival;
		string sval;
		try {
			switch (matchkeys[i].mdt)
			{
			case MDreal:
				rval=md.get_double(matchkeys[i].internal_name);
				// perhaps should check return of this, but
				//intentionally ignored for now
				dbputv(dbscratch_record,0,
					matchkeys[i].db_attribute_name.c_str(),
					rval,0);
				++nscratch_records_set;
				break;
			case MDint:
				ival=md.get_int(matchkeys[i].internal_name);
				dbputv(dbscratch_record,0,
					matchkeys[i].db_attribute_name.c_str(),
					ival,0);
				++nscratch_records_set;
				break;
			case MDstring:
				sval=md.get_string(matchkeys[i].internal_name);
				dbputv(dbscratch_record,0,
					matchkeys[i].db_attribute_name.c_str(),
					sval.c_str(),0);
				++nscratch_records_set;
				break;
			default:
				cerr << "DatascopeMatchHandle.find():  unrecognized type field for key " 
					<< matchkeys[i].internal_name 
					<< "with database attribute called "
					<< matchkeys[i].db_attribute_name
					<< endl
					<< "Attempting to match without this key"
					<< endl;
			}
		} catch (MetadataError mderr)
		{
			cerr << "DatascopeMatchHandle.find(): match key attribute not found in metadata of input data object" 
				<< endl
				<< "metadata routine sent this message:"
				<< endl;;
			mderr.log_error();
		}
	}
	if(nscratch_records_set<=0)
	{
		throw SeisppError("DatascopeMatchHandle.find(): error inputs, cannot proceed");
	}
	int nmatches;
	Tbl *records;
	list<int> result;
	nmatches=dbmatches(dbscratch_record,dbt,
		&kpattern,&tpattern,&hook,&records);
	if(nmatches==dbINVALID)
		throw SeisppDberror("DatascopeMatchHandle.find(0:  dbmatches failed and threw an error",dbt);
	// This will silently return an empty list if
	// nothing matches (nmatches=0)
	for(i=0;i<nmatches;++i)
	{
		int thisrecord;
		thisrecord=(int)gettbl(records,i);
		result.push_back(thisrecord);
	}
	freetbl(records,0);
	return(result);
}
} // End namespace SEISPP declaration
