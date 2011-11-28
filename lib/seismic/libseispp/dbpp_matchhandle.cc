#include <stdio.h>
#include "elog.h"
#include "dbpp.h"
#include "seispp.h"
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
// Default constructor produces invalid handle.
DatascopeMatchHandle::DatascopeMatchHandle() : DatascopeHandle(),amap("css3.0")
{
	dbscratch_record.database=dbINVALID;
	dbscratch_record.table=dbINVALID;
	dbscratch_record.field=dbINVALID;
	dbscratch_record.record=dbINVALID;
	dbt.database=dbINVALID;
	dbt.table=dbINVALID;
	dbt.field=dbINVALID;
	dbt.record=dbINVALID;
	kpattern=NULL;
	tpattern=NULL;
	hook=NULL;
}
//  Primary constructor for this object.  
//
DatascopeMatchHandle::DatascopeMatchHandle(DatascopeHandle& parent,
	string table, list<string>keys_list,
	AttributeMap& am) : DatascopeHandle(parent)
{
	const string base_error("DatascopeMatchHandle constructor:  ");
	// We save this copy in the private area of this object
	// so caller doesn't need to keep track.
	amap=am;
	// NULL hook initialized dbmatches.  Also serves as unambiguous
	// do not free implication in destructor 
	hook=NULL; 
	// dbt holds the reference to the requested table
	// If the table string is null just use the existing pointer.
	if(table.length()<=0)
	{
		if(db.table==dbINVALID)
			throw SeisppDberror(base_error
				+ string("Input handle Dbptr is invalid"),
				db,complain);
		dbt=parent.db;
	}
	else
	{
		dbt = dblookup(parent.db,0,
			const_cast<char *>(table.c_str()),0,0);
		if(dbt.table == dbINVALID)
		{
			throw SeisppDberror( base_error
				+ string("lookup failed for table=")
				+table,
				dbt,complain);
		}
	}
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
		string thisMDnamekey;
		string amkey;
		if(am.is_alias(*internal_name))
		{
			/* Intentionally do not check for alias errors here as those indicate
			an installation problem in the parameter file definition or in the 
			AttributeMap object code.  For efficiency with avoid trappin this here. 
			May prove a bad idea, but for now that's my story and I'm stickin to it.*/
                	list<string> tablenames=am.aliastables(*internal_name);
			Dbptr dbtmp=dbt;
			dbtmp.record=0;
			map<string,AttributeProperties> aliasmap=am.aliases(*internal_name);
			string viewtable;
			int ierr;
			if(parent.is_bundle)
			{
				// The algorithm in FindFirstValidTable will not work correctly
				// with a view.  We need to use this laternative instead.  Could
				// Make this a function, but for now will inline it 
				// Note intentionally use only the attribute name, as it would
				// be odd in a bundle (group) to use a fully qualified name
				list<string>::iterator tttptr;
				bool table_found=false;
				for(tttptr=tablenames.begin();tttptr!=tablenames.end();++tttptr)
				{
					AttributeProperties testalias=aliasmap[*tttptr];
					string dbattname=testalias.db_attribute_name;
					double rval;  long ival;  char sval[80]; bool bval;
					switch (testalias.mdt)
					{
					case MDreal:
						ierr=dbgetv(dbtmp,0,dbattname.c_str(),&rval,NULL);
						break;
					case MDint:
						ierr=dbgetv(dbtmp,0,dbattname.c_str(),&ival,NULL);
						break;
					case MDboolean:
						ierr=dbgetv(dbtmp,0,dbattname.c_str(),&ival,NULL);
						if(ival)
							bval=true;
						else
							bval=false;
						break;
					case MDstring:
					default:
						ierr=dbgetv(dbtmp,0,dbattname.c_str(),&sval,NULL);
						break;
					}
					if(ierr==0) 
					{
						viewtable=*tttptr;
						table_found=true;
						break;
					}
				}
				if(!table_found) throw SeisppError(base_error
					 + "Input view does not contain a table matching internal name="
					 + (*internal_name) );
			}
			else if(table.length()>0)
			{
				/* kind of an odd way to test for a simple
				table, but that is what this is */
				viewtable=table;
			}
			else
			{
				viewtable=FindFirstValidTable(dbtmp,tablenames);
				if(viewtable=="BAD")
					throw SeisppError(base_error
					 + "Input view does not contain a table containing the attribute "
					 + (*internal_name) );
			}
			if(aliasmap.find(viewtable)==aliasmap.end())
				throw SeisppError(base_error
				+ "Attribute map does not define an alias for key="
				+ (*internal_name)
				+ " for table="
				+ viewtable);
			AttributeProperties apalias=aliasmap[viewtable];
			matchkeys.push_back(apalias);
			MDnamekeys.push_back(*internal_name);
			if(SEISPP_verbose)
			{
				if(tablenames.size()>1)
				{
					cerr << "DatascopeMatchHandle (Caution):  "
						<< "Matching attribute with name="
						<< *internal_name
						<< " which is an alias."<<endl
						<< "Match will use value in table(relation)="
						<< viewtable <<endl;
				}
			}
		}
		else
		{
			amkey=*internal_name;
			keyiter=am.attributes.find(amkey);
			if(keyiter==keyiterend)
			{
			    throw SeisppError(string("DatascopeMatchHandle constructor:  internal name=")
				+(*internal_name)
				+ string(" is not in AttributeMap object.\n")
				+ string("Check initialization and/or fix coding error"));
			}
			matchkeys.push_back(keyiter->second);
			MDnamekeys.push_back(*internal_name);
		}
	}
	// Intentionally did not put these in the above to make the
	// algorithm exception safe.  
	int nkeys=matchkeys.size();
	kpattern=newtbl(static_cast<long>(nkeys));
	tpattern=newtbl(static_cast<long>(nkeys));
	for(int i=0;i<nkeys;++i)
	{
		settbl(kpattern,static_cast<long>(i),
			static_cast<void *>(
			const_cast<char *>(matchkeys[i]
				.db_attribute_name.c_str())));
		settbl(tpattern,static_cast<long>(i),
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
	long n=maxtbl(parent);
	t=newtbl(n);
	for(long i=0;i<n;++i)
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
	MDnamekeys=parent.MDnamekeys;
	// tbl's have to be copied cautiously
	if(parent.kpattern==NULL)
		kpattern=NULL;
	else
		kpattern=copy_string_tbl(parent.kpattern);
	if(parent.tpattern==NULL)
		tpattern=NULL;
	else
		tpattern=copy_string_tbl(parent.tpattern);
	// Make the hook null in the copy to handle memory correctly
	hook=NULL;
}
DatascopeMatchHandle& DatascopeMatchHandle::operator = (const DatascopeMatchHandle& parent)
{
	if(this!=&parent)
	{
		// From Datascope Handle
		db=parent.db;
		is_bundle=parent.is_bundle;
		// Object attributes of MatchHandle
		dbt=parent.dbt;
		dbscratch_record=parent.dbscratch_record;
		matchkeys=parent.matchkeys;
		MDnamekeys=parent.MDnamekeys;
		// tbl's have to be copied
		if(parent.kpattern==NULL)
			kpattern=NULL;
		else
			kpattern=copy_string_tbl(parent.kpattern);
		if(parent.tpattern==NULL)
			tpattern=NULL;
		else
			tpattern=copy_string_tbl(parent.tpattern);
		// Make the hook null in the copy to handle memory correctly
		hook=NULL;
		/* These are protected (formerly private) variables in DatascopeHandle */
		close_on_destruction=false;
		parent_table=parent.parent_table;
		views=parent.views;
		if(views!=NULL) 
		{
			/* In Datascope negative table numbers can 
			mean different things, but never things we want
			to memory manage */
			if(db.table>0) views->insert(db.table);
		}
		this->manage_parent();
		retain_parent=parent.retain_parent;
	}
	return(*this);		
}
DatascopeMatchHandle::~DatascopeMatchHandle()
{
	if(hook!=NULL) free_hook(&hook);
	if(kpattern!=NULL)freetbl(kpattern,0);
	if(tpattern!=NULL)freetbl(tpattern,0);
}
// should throw and exception when keys not found in metadata
// algorithm will assume keys do 
list<long> DatascopeMatchHandle::find(Metadata& md,bool use_fullnames)
{
	const string base_error("DatascopeMatchHandle.find(): ");
	int nscratch_records_set;
	int i;
	string fullname;
	int ierr;
	for(i=0,nscratch_records_set=0;i<matchkeys.size();++i)
	{
		double rval;
		int ival;
		string sval;
		if(use_fullnames)
			fullname=matchkeys[i].fully_qualified_name();
		else
			fullname=matchkeys[i].db_attribute_name;
		try {
			/* To handle aliases we use the MDnamekeys vector
			names as Metadata keys for fetching attributes. 
			The AttributeProperties vector matchkeys is
			used to make sure types agree. */
			switch (matchkeys[i].mdt)
			{
			case MDreal:
				rval=md.get_double(MDnamekeys[i]);
				ierr=dbputv(dbscratch_record,0,
					fullname.c_str(),
					rval,NULL);
				++nscratch_records_set;
				break;
			case MDint:
				ival=md.get_int(MDnamekeys[i]);
				ierr=dbputv(dbscratch_record,0,
					fullname.c_str(),
					(long)ival,NULL);
				++nscratch_records_set;
				break;
			case MDstring:
				sval=md.get_string(MDnamekeys[i]);
				ierr=dbputv(dbscratch_record,0,
					fullname.c_str(),
					sval.c_str(),NULL);
				++nscratch_records_set;
				break;
			default:
				cerr << "DatascopeMatchHandle.find():  unrecognized type field for key " 
					<< MDnamekeys[i] 
					<< "with database attribute called "
					<< fullname
					<< endl
					<< "Attempting to match without this key"
					<< endl;
			}
			if(ierr==dbINVALID) throw SeisppError(base_error
				+ "dbputv error writing attribute" 
				+ fullname
				+ " to scratch record for match view");
		} catch (MetadataError& mderr)
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
		throw SeisppError(base_error +"error inputs, cannot proceed");
	}
	long nmatches;
	Tbl *records;
	list<long> result;
	nmatches=dbmatches(dbscratch_record,dbt,
		&kpattern,&tpattern,&hook,&records);
	if(nmatches==dbINVALID)
		throw SeisppDberror(base_error +"dbmatches failed and threw an error",dbt);
	// This will silently return an empty list if
	// nothing matches (nmatches=0)
	for(i=0;i<nmatches;++i)
	{
		long thisrecord;
		thisrecord=(long)gettbl(records,i);
		result.push_back(thisrecord);
	}
	freetbl(records,0);
	return(result);
}
} // End namespace SEISPP declaration
