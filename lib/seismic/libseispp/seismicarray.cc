#include <sstream>
#include <vector>
#include <map>
#include "coords.h"
#include "dbpp.h"
#include "seispp.h"
using namespace std;
using namespace SEISPP;
#include "resample.h"
#include "seismicarray.h"
namespace SEISPP
{

SeismicStationLocation::SeismicStationLocation()
{
	lat=0.0; 
	lon=0.0;
	elev=0.0;
	dnorth=0.0;
	deast=0.0;
	name="UNDEFINED";
	net="UNDEFINED";
	refsta="UNDEFINED";
}
SeismicStationLocation::SeismicStationLocation(double lat0, double lon0,
                double elev0, double dn0, double de0, string name0,
		string net0,string refsta0)
{
	lat=lat0;
	lon=lon0;
	elev=elev0;
	dnorth=dn0;
	deast=de0;
	name=name0;
	net=net0;
	refsta=refsta0;
}
SeismicStationLocation::SeismicStationLocation(const SeismicStationLocation& s0)
{
	lat=s0.lat;
	lon=s0.lon;
	elev=s0.elev;
	dnorth=s0.dnorth;
	deast=s0.deast;
	name=s0.name;
	net=s0.net;
	refsta=s0.refsta;
}
SeismicStationLocation& SeismicStationLocation::operator=(const SeismicStationLocation& s0)
{
	if(this!=&s0)
	{
		lat=s0.lat;
		lon=s0.lon;
		elev=s0.elev;
		dnorth=s0.dnorth;
		deast=s0.deast;
		name=s0.name;
		net=s0.net;
		net=s0.net;
	} 
	return(*this);
}

SeismicArray::SeismicArray()
{
	name="UNDEFINED";
}
SeismicArray::SeismicArray(DatabaseHandle& dbi,
	double time, string arrayname)
{
	name=arrayname;
	// Intentionally create a copy.  This algorithm assumes
	// this constructor is call rarely or this approach is
	// inefficient.  
	DatascopeHandle dbh(dynamic_cast<DatascopeHandle&>(dbi));
	dbh.natural_join("site","snetsta");
	int jdate=yearday(time);
	int ondate,offdate;
	dbh.rewind();
	vector<int> ondl,offdl;
	vector<int>::iterator ondlmin,offdlmin;
	for(int i=0;i<dbh.number_tuples();++i,++dbh)
	{
		double lat,lon,elev,dnorth,deast;
		string staname;
		string netname;
		string refsta;
		string stakey;
		try{
			ondate=dbh.get_int("ondate");
			offdate=dbh.get_int("offdate");
			// silently skip data for which jdate is not
			// between ondate and offdate
			// However, we load the ondate into the offdate
			// vector for such stations to build valid time
			// interval

			if( (jdate>=ondate)
				&& ((offdate<0)  || (jdate<=offdate) ))
			{
				lat=dbh.get_double("lat");
				lon=dbh.get_double("lon");
				elev=dbh.get_double("elev");
				// here we presume null is 0.0
				dnorth=dbh.get_double("dnorth");
				deast=dbh.get_double("deast");
				staname=dbh.get_string("sta");
				refsta=dbh.get_string("refsta");
				netname=dbh.get_string("snet");
				// convert from external degrees to radians
				lat=rad(lat);
				lon=rad(lon);
				// equivalent to
				//if((ondate>0)&&(ondate>jdate) )
				// since ondate is set -1 if not defined
				//
				if(ondate>jdate)
					ondl.push_back(ondate);
				if( (offdate>0) && (offdate>jdate) )
				// Same -1 issue about offdate
				if( offdate>jdate )
					offdl.push_back(offdate);
				stakey=netname+"_"+staname;
				array[staname]=SeismicStationLocation(lat,lon,
						elev,dnorth,deast,staname,netname,refsta);	
			}
			else if(ondate>jdate)
			{
				ondl.push_back(ondate); 
			}
		} catch (SeisppDberror dberr)
		{
			dberr.log_error();
		}
		catch (...)
		{
			cerr << "WARNING (SeismicArray database constructor):  "
			<< endl
			<<"Unhandled exception. SeismicArray object may be incomplete."<<endl;
		}
	}
	if(offdl.empty() || ondl.empty())
	{
		if(offdl.empty())offdate=2050001;
		if(!ondl.empty())
		{
			ondlmin=min_element(ondl.begin(),ondl.end());
			offdate=min(offdate,*ondlmin);
		}
	}
	else
	{
		// Get the end of the valid time as smaller of the
		// earliest offdate or the earliest ondate
		ondlmin=min_element(ondl.begin(),ondl.end());
		offdlmin=min_element(offdl.begin(),offdl.end());
		offdate=min(*ondlmin,*offdlmin);
	}
	// +1 on offdate because jdate is beginning of day
	valid_time_interval=TimeWindow(epoch(jdate),
				epoch(offdate+1));
}
/* This constructor uses the one above.  Not the most efficient way to do 
this, but I do not expect this constructor to called often.  Normally
once or at most a few times in a given program.  It works by reading
all stations and then selecting only those in the list using the map.  
The only trick is it has to deal with the possibility that 
a station in the list is not in the global array defined by the
database.  The approach here is to try to return the most complete
list possible.  If a station in the list is not in the database we just
post a message to cerr and that station is, of course, not defined in
the object that is created. 

Author:  G Pavlis
*/
SeismicArray::SeismicArray(DatabaseHandle& dbi,
	double time, string name_to_use,
	list<string> sta_to_use)
{
	list<string>::iterator sta_to_copy;
	map<string,SeismicStationLocation>::iterator statest,sta_not_present;

	name=name_to_use;
	// This creates the master array that is reduced by sta_to_use list
	SeismicArray master(dbi,time,string("allstations"));
	sta_not_present=master.array.end();
	for(sta_to_copy=sta_to_use.begin();sta_to_copy!=sta_to_use.end();++sta_to_copy)
	{
		statest=master.array.find(*sta_to_copy);
		if(statest==sta_not_present)
		{
			cerr << "SeismicArray constructor (WARNING):  requested station = "
				<< *sta_to_copy << " not found in database" <<endl
				<< "Verify ondate::offdate for julian date = "
				<< yearday(time) <<endl
				<< "Data from this station may be dropped"<<endl;
		}
		else
		{
			array[*sta_to_copy]=(*statest).second;
		}
	}
	// this is a conservative measure to handle ondate::offdate issues.
	// It may do bad things if the list is small compared to the database.
	valid_time_interval=master.valid_time_interval;
}

SeismicArray::SeismicArray(const SeismicArray& orig)
{
	name=orig.name;
	array=orig.array;
	valid_time_interval=orig.valid_time_interval;
}
SeismicArray& SeismicArray::operator=(const SeismicArray& orig)
{
	if(this!=&orig)
	{
		name=orig.name;
		array=orig.array;
		valid_time_interval=orig.valid_time_interval;
	}
	return(*this);
}
bool SeismicArray::GeometryIsValid(TimeWindow test)
{
	if( (test.start<valid_time_interval.start)
		|| (test.end>valid_time_interval.end) )
			return(false);
	else
		return(true);
}
} // End namespace SEISPP
			
