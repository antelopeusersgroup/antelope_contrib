#include <exception>
#include "coords.h"
#include "dbpp.h"
#include "EventCatalog.h"
#include "seispp.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP
{
EventCatalog::EventCatalog(MetadataList& mdl)
{
	mdloaded=mdl;
}

EventCatalog::EventCatalog(DatabaseHandle& dbh,MetadataList& mdl,AttributeMap& am,
	string ttmethod, string ttmodel,bool is_view)
{
	const string base_message("EventCatalog database constructor:  ");
	try {
		DatascopeHandle dsdbh=dynamic_cast<DatascopeHandle&>(dbh);
		if(!is_view)
		{
			dsdbh.lookup("event");
			dsdbh.natural_join("origin");
			dsdbh.subset("orid==prefor");
		}
		double lat,lon,depth,otime;
		Metadata md;
		int nrec=dsdbh.number_tuples();
		for(dsdbh.db.record=0;dsdbh.db.record<nrec;++dsdbh.db.record)
		{
			if(dbgetv(dsdbh.db,0,"lat",&lat,
				"lon",&lon,"depth",&depth,"time",&otime,NULL )
				== dbINVALID)
			{
				string message;
				message=base_message+"dbgetv error while loading";
				throw SeisppError(message);
			}
			Hypocenter h(rad(lat),rad(lon),depth,otime,ttmethod,ttmodel);
			try {
				md=Metadata(dynamic_cast<DatabaseHandle&>(dsdbh),
					mdl,am);
			} catch (SeisppError& serr)
			{
				if(SEISPP_verbose) 
				{
					cerr << "EventCatalog database constructor:  "
						<< "error extracting aux attribures."
						<< "MetadataError exception message:  ";
					serr.log_error();
				}
				/* Better an empty md than a partial one of unknown state */
				md=Metadata();
			}
			bool success=this->add(h,md);
			if(!success && SEISPP_verbose)
				cerr << base_message
					<< "Warning:  this event was not added because"
					<< " it was judged a duplicate:"
					<< endl
					<< lat << " "<< lon <<" "<<depth
					<< strtime(otime)<<endl;
		}
		current_hypo=catalog.begin();
		mdloaded=mdl;
	} catch (std::exception& e)
	{
		string message=base_message
			+ "dynamic_cast to DatascopeHandle failed.\nstdlib error message:"
			+ e.what();
		throw SeisppError(message);
	}
}
EventCatalog::EventCatalog(DatabaseHandle& dbh,string ttmethod, string ttmodel,
	bool is_view)
{
	const string base_message("EventCatalog database constructor:  ");
	Metadata emptymd;
	try {
		DatascopeHandle dsdbh=dynamic_cast<DatascopeHandle&>(dbh);
		if(!is_view)
		{
			dsdbh.lookup("event");
			dsdbh.natural_join("origin");
			dsdbh.subset("orid==prefor");
		}
		double lat,lon,depth,otime;
		int nrec=dsdbh.number_tuples();
		for(dsdbh.db.record=0;dsdbh.db.record<nrec;++dsdbh.db.record)
		{
			if(dbgetv(dsdbh.db,0,"lat",&lat,
				"lon",&lon,"depth",&depth,"time",&otime,NULL )
				== dbINVALID)
			{
				string message;
				message=base_message+"dbgetv error while loading";
				throw SeisppError(message);
			}
			Hypocenter h(rad(lat),rad(lon),depth,otime,ttmethod,ttmodel);
			bool success=this->add(h,emptymd);
			if(!success && SEISPP_verbose)
				cerr << base_message
					<< "Warning:  this event was not added because"
					<< " it was judged a duplicate:"
					<< endl
					<< deg(lat) << " "<< deg(lon) <<" "<<depth
					<< strtime(otime)<<endl;
		}
		current_hypo=catalog.begin();
	} catch (std::exception& e)
	{
		string message=base_message
			+ "dynamic_cast to DatascopeHandle failed.\nstdlib error message:"
			+ e.what();
		throw SeisppError(message);
	}
}
TimeWindow EventCatalog::range()
{
	map<Hypocenter,Metadata,SpaceTimeCompare>::iterator hs,he;
	hs=catalog.begin();
	he=catalog.end();
	return(TimeWindow(hs->first.time,he->first.time));
}
bool EventCatalog::add(Hypocenter& h,Metadata& md)
{
	if(catalog.find(h)!=catalog.end())
	{
		return false;
	}
	else
	{
		catalog.insert(pair<Hypocenter,Metadata>(h,md));
		return  true;
	}
}

bool EventCatalog::replace(Hypocenter&h, Metadata& md)
{
	bool result;
	result=false;
	if(catalog.find(h)==catalog.end()) result=true;
	catalog.insert(pair<Hypocenter,Metadata>(h,md));
	return result;
}
bool EventCatalog::find(Hypocenter& h)
{
	bool result;
	map<Hypocenter,Metadata,SpaceTimeCompare>::iterator hptr;
	hptr=catalog.find(h);
	if(hptr==catalog.end()) 
	{
		result=false;
	}
	else
	{
		current_hypo=hptr;
		result=true;
	}
	return result;
}
Hypocenter EventCatalog::current()
{
	return(current_hypo->first);
}
Metadata EventCatalog::current_aux()
{
	return(current_hypo->second);
}
void EventCatalog::delete_current()
{
	catalog.erase(current_hypo);
	if(catalog.size()>0) current_hypo=catalog.begin();
}
void EventCatalog::rewind()
{
	current_hypo=catalog.begin();
}
void EventCatalog::operator++()
{
	if(current_hypo==catalog.end())
		throw SeisppError(string("EventCatalog::opeator++:  ")
			+ "attempt to go outside valid range");
	++current_hypo;
}
int EventCatalog::size()
{
	return(catalog.size());
}
EventCatalog::EventCatalog(const EventCatalog& parent)
{
	catalog=parent.catalog;
	current_hypo=parent.current_hypo;
	mdloaded=parent.mdloaded;
}

EventCatalog& EventCatalog::operator=(const EventCatalog& parent)
{
    if(this!=&parent)
    {
	catalog=parent.catalog;
	current_hypo=parent.current_hypo;
	mdloaded=parent.mdloaded;
    }
    return(*this);
}

} /* End SEISPP namespace encapsulation*/
