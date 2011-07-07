#include <sstream>
#include "ArrivalUpdater.h"
#include "seispp.h"
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
	timestamp=-1.0;
	current_evid=-1;
	view_has_data=false;
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
                dbh.subset("evid>0");
		dbh.natural_join("origin");
                dbh.subset("orid>0");
		list<string> sortkeys;
		sortkeys.push_back("evid");
		sortkeys.push_back("orid");
		dbh.sort(sortkeys);
                DatascopeHandle dbhrhj(dbh);
                dbhrhj.lookup("assoc");
                dbhrhj.subset("arid>0");
                list<string> join_keys;
                /* because we are joining a subset view we have
                   to specify the join keys explicitly */
                join_keys.push_back(string("orid"));
                dbh.join(dbhrhj,join_keys,join_keys);
                dbhrhj.lookup("arrival");
                dbhrhj.subset("arid>0");
                join_keys.clear();
                join_keys.push_back(string("arid"));
                dbh.join(dbhrhj,join_keys,join_keys);
		/* Set this private boolean if this view has no data */
		if(dbh.number_tuples()<=0) 
		{
			view_has_data=false;
			/* These must be initialized to avoid a seg fault in 
			copy operations */
			aaview=DatascopeMatchHandle();
			eogroup=DatascopeMatchHandle();
		}
		else
		{
			view_has_data=true;
			/* dbh is our working view.  Build and index to match on these keys 
			that defines aaview */
			list<string> view_match_keys;
			view_match_keys.push_back("evid");
			view_match_keys.push_back("orid");
			view_match_keys.push_back("sta");
			view_match_keys.push_back("phase");
			aaview=DatascopeMatchHandle(dbh,empty,view_match_keys,am);	
			list<string>groupkeys;
			groupkeys.push_back("evid");
			groupkeys.push_back("orid");
			DatascopeHandle dbgrp(dbh);
			dbgrp.group(groupkeys);
			list<string> grpmatchkeys;
			grpmatchkeys.push_back("evid");
			eogroup=DatascopeMatchHandle(dbgrp,empty,grpmatchkeys,am);
		}
		timestamp=now();
		current_evid=-1;
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
	current_evid=parent.current_evid;
	timestamp=parent.timestamp;
	view_has_data=parent.view_has_data;
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
		current_evid=parent.current_evid;
		timestamp=parent.timestamp;
		view_has_data=parent.view_has_data;
	}
	return(*this);
}
string mdt2str(MDtype mdt)
{
	string result;
	switch (mdt)
	{
	case MDreal:
		result=string("real");
		break;
	case MDint:
		result=string("integer");
		break;
	case MDstring:
		result=string("string");
		break;
	case MDboolean:
		result=string("boolean");
		break;
	case MDinvalid:
	default:
		result=string("invalid");
	}
	return result;
}
string post_mmerror(Metadata_typedef m, AttributeProperties ap)
{
	char buf[512];
	ostringstream message(buf);
	message << "put_attributes_to_db(WARNING):  type mismatch for data attribute = "
		<< m.tag << endl
		<< "While attempting to save as db attribute ="<<ap.db_attribute_name
		<< " to table "<<ap.db_table_name<<endl;
	string mtype,aptype;
	mtype=mdt2str(m.mdt);
	aptype=mdt2str(ap.mdt);
	message << "Internal name is tagged with type "<<mtype<<endl
		<< "Database attribute is tagged with type "<<aptype<<endl
		<< "Attribute not saved in output database." <<endl;
	return(message.str());
}
/*! Internal function to write a specified list of attributes
to the database.

\param md Metadata containing attributes to be written to db.
\param db Datascope Dbptr assumed to be pointing at record to be updated.
\param mdl Internal to external namespace mapper that defines what
	attributes will be updated in the db.  The function will
	attempt to update ALL attributes listed. 
\return number of nonfatal failures.  Thus 0 means all done ok. 
*/
int  put_attributes_to_db(Metadata& md,Dbptr db, 
		MetadataList& mdl,AttributeMap& am)
{
	int err=0;
	try {
		double dval;
		long ival;
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
				char buf[128];
				ostringstream message(buf);
				message << "put_attributes_to_db: "
					  << "AttributeMap does not contain requested attribute="
					  << mdptr->tag<<endl
					  <<"Attribute not written to output db"<<endl;
				throw SeisppError(message.str());
			}
			if( (mdptr->mdt) != (ap->second.mdt) )
			{
				throw SeisppError(post_mmerror(*mdptr,ap->second));
			}
			dbattributename=ap->second.fully_qualified_name();
			switch(mdptr->mdt)
			{
			case MDreal:
				dval=md.get_double(mdptr->tag);
				if(dbputv(db,0,dbattributename.c_str(),
					dval,NULL) == dbINVALID) ++err;
				break;
			case MDint:
				ival=md.get_long(mdptr->tag);
				if(dbputv(db,0,dbattributename.c_str(),
					ival,NULL) == dbINVALID) ++err;
				break;
			case MDstring:
				sval=md.get_string(mdptr->tag);
				if(dbputv(db,0,dbattributename.c_str(),
					sval.c_str(),NULL) == dbINVALID) ++err;
				break;
			default:
				char buf[128];
				ostringstream message(buf);
				message << "put_attributes_to_db: "
					<< "Metadata attribute "
					<< mdptr->tag
					<< " has an illegal data type. "
					<< endl;
				throw SeisppError(message.str());
			}
		}
		return(err);
	} catch(...) {throw;};
}
				
				
int ArrivalUpdater::update(Metadata& md)
{
	const string base_error("ArrivalUpdater::update method: ");
	int err=0;
	Dbptr db;
	list<long>::iterator rowptr;
	int nrow;
	list<long> rowlist;
	if(view_has_data)
	{
		/* Only do this search if the aaview is not empty.
		Seg fault results if we don't do this.
                Modified Oct 20009:  added false qualifier.
                This is essential since match keys are defined
                with shorthand names in constructor.  false 
                flag here tells the match handle to use it's 
                internal alias list consistent with the constructor */
		rowlist=aaview.find(md,false);
		nrow=rowlist.size();
	}
	else
	{
		// This is a simple way to assure we enter the append block
		// always if the original view was empty. 
		nrow=0;
	}
	if(nrow==0)
	{
		dbassoc.record=dbaddnull(dbassoc);
		dbarrival.record=dbaddnull(dbarrival);
		err+=put_attributes_to_db(md,dbassoc,mdlassoc,am);
		err+=put_attributes_to_db(md,dbarrival,mdlarrival,am);
		// When we append we have to get a new arid and set
		// it.  Assume we use old arid for record updates below
		long arid=dbnextid(dbassoc,"arid");
		dbputv(dbassoc,0,"arid",arid,NULL );
		dbputv(dbarrival,0,"arid",arid,NULL );
	}
	else
	{
		rowptr=rowlist.begin();
		aaview.db.record=*rowptr;
		dbgetv(aaview.db,0,"assoc",&db,NULL);
		if(db.table==dbINVALID)
			throw SeisppError(base_error
			 + string(" failure in fetching assoc pointer from view"));
		/* Must update lddate */
		dbputv(db,0,"lddate",now(),NULL );
		err+=put_attributes_to_db(md,db,mdlassoc,am);
		dbgetv(aaview.db,0,"arrival",&db,NULL);
		if(db.table==dbINVALID)
			throw SeisppError(base_error
			 + string(" failure in fetching arrival pointer from view"));
		err+=put_attributes_to_db(md,db,mdlarrival,am);
		dbputv(db,0,"lddate",now(),NULL );
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
			dbgetv(aaview.db,0,"assoc",&db,NULL);
			dbmark(db);
			dbgetv(aaview.db,0,"arrival",&db,NULL);
			dbmark(db);
		}
	}
	return(err);
}
int ArrivalUpdater::clear_when_not_prefor(int evid_to_clear)
{
	Metadata md;
	md.put("evid",evid_to_clear);
	list<long> reclist=eogroup.find(md);
	int nrec=reclist.size();
	int ndeleted=0;
	// This means there is nothing to do.  We'll do nothing silently
	// considering this not an unusual request
	if(nrec<=0)
		return(0);
	else
	{
		int rec;
		list<long>::iterator irptr;
		long prefor,orid,evid;
		/* This is actually very inefficient if there are lots of
		arrivals in the working view.  The reason is the call to dbmark
		is made or origin for every row in assoc that refers to an origin not
		set as prefor.  Only one is needed.   */
		for(irptr=reclist.begin();irptr!=reclist.end();irptr++)
		{
			Dbptr db,dbtokill;
			eogroup.db.record=*irptr;
			DBBundle bundle=eogroup.get_range();
			db=bundle.parent;
			db.record=bundle.start_record;
			dbgetv(db,0,"prefor",&prefor,"orid",&orid,NULL );
			if(orid!=prefor)
			{
				for(db.record=bundle.start_record;
					db.record<bundle.end_record;++db.record)
				{
					dbgetv(db,0,"assoc",&dbtokill,NULL );
					dbmark(dbtokill);
				}
				db.record=bundle.start_record;
				dbgetv(db,0,"origin",&dbtokill,NULL );
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
int ArrivalUpdater::clear_old(int evid_to_clear,string phase)
{
	//Silently return immediately if the original arrival view
	// was empty.  In that case be definition there is nothing old 
	// to clear 
	if(!view_has_data) return(0);
	const string base_error("ArrivalUpdater::clear_old:  ");
	int ndeleted;
	Metadata md;
	/* This is CSS3.0 schema specific, but since this entire 
	processing object is based on CSS3.0 database concepts this
	should not be an issue.  Still worth noting for maintenance.*/
	md.put("evid",evid_to_clear);
	list<long> reclist=eogroup.find(md,false);
	/* Silently do nothing if no such evid exists in the database */
	if(reclist.size()<=0) return(0);
	list<long>::iterator irptr;
	int prefor,orid,evid;
	ndeleted=0;
	for(irptr=reclist.begin();irptr!=reclist.end();irptr++)
	{
		Dbptr db,dbtokill;
		double lddate;
		eogroup.db.record=*irptr;
		DBBundle bundle=eogroup.get_range();
		db=bundle.parent;
		db.record=bundle.start_record;
		for(db.record=bundle.start_record;
				db.record<bundle.end_record;++db.record)
		{
			char iphase[10];
			dbgetv(db,0,"arrival",&dbtokill,NULL );
			dbgetv(dbtokill,0,"iphase",iphase,"lddate",&lddate,NULL);
			if( (lddate<timestamp) && (phase==iphase) )
			{
				dbmark(dbtokill);
				dbgetv(db,0,"assoc",&dbtokill,NULL );
				dbmark(dbtokill);
				++ndeleted;
			}
		}
	}
	return(ndeleted);
}

} // End SEISPP namespace declaration
	
