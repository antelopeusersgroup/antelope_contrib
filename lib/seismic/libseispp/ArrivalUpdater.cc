#include "ArrivalUpdater.h"
namespace SEISPP{

using namespace std;
using namespace SEISPP;
/*! Default constructor. 

Set things all invalid for stability.  Any attempt to use
this object in this condition will lead to an error.
*/
ArrivalUpdater::ArrivalUpdater() : aaview(), eogroup(), am("css3.0")
{
	dbassoc.database=dbINVALID;
	dbarrival.database=dbINVALID;
	dborigin.database=dbINVALID;
}
ArrivalUpdater::ArrivalUpdater(DatabaseHandle& dbhraw, MetadataList& mdl1,
	MetadataList& mdl2,string amname) : aaview(),am(amname)
{
	const string base_error("ArrivalUpdater constructor:  ");
	try {
		string empty("");
		// This implementation is datascope so we immediately downcast
		// dbhraw to a DatascopeHandle.
		DatascopeHandle dbh=dynamic_cast<DatascopeHandle&>(dbhraw);
		// First set these private variables that are cached in
		// the object for efficiency
		mdlassoc=mdl1;
		mdlarrival=mdl2;
		dbh.lookup("origin");
		dborigin=dbh.db;
		dbh.lookup("assoc");
		dbassoc=dbh.db;
		dbh.lookup("arrival");
		dbarrival=dbh.db;
		// Now build the core match handles into working views
		// These are core css tables so it is more than ok to hard wire this
		dbh.lookup("event");
		dbh.natural_join("origin");
		list<string> sortkeys;
		sortkeys.push_back("evid");
		sortkeys.push_back("orid");
		dbh.sort(sortkeys);
		list<string>groupkeys;
		groupkeys.push_back("evid");
		groupkeys.push_back("orid");
		DatascopeHandle dbgrp(dbh);
		dbgrp.group(groupkeys);
		list<string> grpmatchkeys;
		grpmatchkeys.push_back("evid");
		eogroup=DatascopeMatchHandle(dbgrp,empty,grpmatchkeys,am);
		dbh.natural_join("assoc");
		dbh.natural_join("arrival");
		list<string> view_match_keys;
		view_match_keys.push_back("evid");
		view_match_keys.push_back("orid");
		view_match_keys.push_back("sta");
		view_match_keys.push_back("phase");
		aaview=DatascopeMatchHandle(dbh,empty,view_match_keys,am);	

	}
	catch (...) {throw;};
}

ArrivalUpdater::ArrivalUpdater(const ArrivalUpdater& parent)
	: aaview(parent.aaview), eogroup(parent.eogroup)
{
	mdlassoc=parent.mdlassoc;
	mdlarrival=parent.mdlarrival;
	am=parent.am;
	dbassoc=parent.dbassoc;
	dbarrival=parent.dbarrival;
	dborigin=parent.dborigin;
}
ArrivalUpdater& ArrivalUpdater::operator=(const ArrivalUpdater& parent)
{
	if(this!=&parent)
	{
		aaview=parent.aaview;
		eogroup=parent.eogroup;
		mdlassoc=parent.mdlassoc;
		mdlarrival=parent.mdlarrival;
		am=parent.am;
		dbassoc=parent.dbassoc;
		dbarrival=parent.dbarrival;
		dborigin=parent.dborigin;
	}
	return(*this);
}
/*! Internal function to write a specified list of attributes
to the database.

\param md Metadata containing attributes to be written to db.
\param db Datascope Dbptr assumed to be pointing at record to be updated.
\param mdl Internal to external namespace mapper that defines what
	attributes will be updated in the db.  The function will
	attempt to update ALL attributes listed. 
\return number of failures.  Thus 0 means all done ok. 
*/
int  put_attributes_to_db(Metadata& md,Dbptr db, 
		MetadataList& mdl,AttributeMap& am)
{
	try {
		int err=0;
		double dval;
		int ival;
		string sval;
		string dbattributename;
		MetadataList::iterator mdptr;
		map<string,AttributeProperties>::iterator ap,ape;
		ape=am.attributes.end();
		for(mdptr=mdl.begin();mdptr!=mdl.end();mdptr++)
		{
			ap=am.attributes.find(mdptr->tag);
			if(ap==ape)
			{
				++err;
				continue;
			}
			if( (mdptr->mdt) != (ap->second.mdt) )
			{
				++err;
				continue;
			}
			dbattributename=ap->second.db_table_name
					+ string(".")
					+ ap->second.db_attribute_name;
			switch(mdptr->mdt)
			{
			case MDreal:
				dval=md.get_double(mdptr->tag);
				if(dbputv(db,0,dbattributename.c_str(),
					dval,0) == dbINVALID) ++err;
				break;
			case MDint:
				ival=md.get_int(mdptr->tag);
				if(dbputv(db,0,dbattributename.c_str(),
					ival,0) == dbINVALID) ++err;
				break;
			case MDstring:
				sval=md.get_string(mdptr->tag);
				if(dbputv(db,0,dbattributename.c_str(),
					sval.c_str(),0) == dbINVALID) ++err;
				break;
			default:
				++err;
			}
		}
		return(err);
	} catch(...) {throw;};
}
				
				
int ArrivalUpdater::update(Metadata& md)
{
	const string base_error("ArrivalUpdater::update method:");
	int err=0;
	Dbptr db;
	list<int>::iterator rowptr;
	/* First get matches for evid grouped view*/
	list<int> rowlist=aaview.find(md);
	int nrow=rowlist.size();
	if(nrow==0)
	{
		dbassoc.record=dbaddnull(dbassoc);
		dbarrival.record=dbaddnull(dbarrival);
		err+=put_attributes_to_db(md,dbassoc,mdlassoc,am);
		err+=put_attributes_to_db(md,dbarrival,mdlarrival,am);
		// When we append we have to get a new arid and set
		// it.  Assume we use old arid for record updates below
		int arid=dbnextid(dbassoc,"arid");
		dbputv(dbassoc,0,"arid",arid,0);
		dbputv(dbarrival,0,"arid",arid,0);
	}
	else
	{
		rowptr=rowlist.begin();
		aaview.db.record=*rowptr;
		// These need error trapping. Just getting it down for now
		dbgetv(aaview.db,0,"assoc",&db,0);
		if(db.table==dbINVALID)
			throw SeisppError(base_error
			 + string(" failure in fetching assoc pointer from view"));
		err+=put_attributes_to_db(md,db,mdlassoc,am);
		dbgetv(aaview.db,0,"arrival",&db,0);
		if(db.table==dbINVALID)
			throw SeisppError(base_error
			 + string(" failure in fetching arrival pointer from view"));
		err+=put_attributes_to_db(md,db,mdlarrival,am);
	}
	if(nrow>1)
	{
		cerr << "ArrivalUpdater::update:  Warning duplicate arids found"<<endl
			<< "Attempting to delete duplicates."<<endl
			<<  "Check results as this is a dangerous operation"
			<<endl;
		rowptr=rowlist.begin();
		++rowptr;
		for(;rowptr!=rowlist.end();rowptr++)
		{
			aaview.db.record=*rowptr;
			dbgetv(aaview.db,0,"assoc",&db,0);
			dbmark(db);
			dbgetv(aaview.db,0,"arrival",&db,0);
			dbmark(db);
		}
	}
	return(err);
}
int ArrivalUpdater::clear_when_not_prefor(int evid_to_clear)
{
	Metadata md;
	md.put("evid",evid_to_clear);
	list<int> reclist=eogroup.find(md);
	int nrec=reclist.size();
	int ndeleted=0;
	// This means there is nothing to do.  We'll do nothing silently
	// considering this not an unusual request
	if(nrec<=0)
		return(0);
	else
	{
		int rec;
		list<int>::iterator irptr;
		int prefor,orid,evid;
		for(irptr=reclist.begin();irptr!=reclist.end();irptr++)
		{
			Dbptr db,dbtokill;
			eogroup.db.record=*irptr;
			DBBundle bundle=eogroup.get_range();
			db=bundle.parent;
			db.record=bundle.start_record;
			dbgetv(db,0,"prefor",&prefor,"orid",&orid,0);
			if(orid!=prefor)
			{
				for(db.record=bundle.start_record;
					db.record<bundle.end_record;++db.record)
				{
					dbgetv(db,0,"assoc",&dbtokill,0);
					dbmark(dbtokill);
				}
				db.record=bundle.start_record;
				dbgetv(db,0,"origin",&dbtokill,0);
				dbmark(dbtokill);
				++ndeleted;
			}
		}
	}
	return(ndeleted);				
}	

/* The next two are simplified interface routines that could have been inlined*/
int ArrivalUpdater::update(TimeSeries& ts)
{
	try{
		return(this->update(dynamic_cast<Metadata&>(ts)));
	} catch (...) {throw;};
}
int ArrivalUpdater::update(ThreeComponentSeismogram& ts)
{
	try{
		return(this->update(dynamic_cast<Metadata&>(ts)));
	} catch (...) {throw;};
}


} // End SEISPP namespace declaration
	
