#include <sstream>
#include <vector>
#include <map>
#include "coords.h"
#include "stock.h"
#ifndef NO_ANTELOPE
#include "dbpp.h"
#endif
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
SeismicArray::SeismicArray(string fnamebase,string form)
{
    const string base_error("SeismicArray file-driven constructor:  ");
    string net,refsta;   // needed for SeismicStationLocation object
    if(form=="ascii_table_with_pf")
    {
        try {
            PfStyleMetadata params=pfread(fnamebase+".pf");
            name=params.get_string("array_name");
            net=params.get_string("net_name");
            refsta=params.get_string("refsta");
            double stime=params.get_double("start_time");
            double etime=params.get_double("end_time");
            valid_time_interval=TimeWindow(stime,etime);
            // A pf will allow definitions of subarrays as in dbxcor. 
            // this procedure parses pf for that and loads definitions to object
            load_subarrays(*this,params);
        }catch(...) {
            throw;
        };
    }
    else if (form=="simple_ascii_table")
    {
        name=fnamebase;
        net=name;
        refsta="UNDEFINED";
        // end time here is Jan 1, 2100
        valid_time_interval=TimeWindow(0.0,4102444800.0);
    }
    else
        throw SeisppError(base_error
                + "Unsupported format with name tag="
                + form);
    // For now both formats read a .dat file in common format
    // Small sanity checks done to avoid common problem of mixing
    // lat and lon 
    FILE *fp;
    string fname=fnamebase+".dat";
    if(fopen(fname.c_str(),"r")==NULL) throw SeisppError(base_error
                                + "cannot open file " + fname);
    double lat,lon,elev;
    char stain[20];
    double dnorth(0.0),deast(0.0);   // force these always 0
    while(fscanf(fp,"%s%lf%lf%lf",stain,&lat,&lon,&elev)!=EOF)
    {
        // stored internally in radians
        lat=rad(lat);
        lon=rad(lon);
        string sta(stain);
        // Set refsta to first station read for simple format
        if(refsta=="UNDEFINED")refsta=sta;
        SeismicStationLocation newstaloc(lat,lon,elev,dnorth,deast,
                sta,net,refsta);
        array.insert(pair<string,SeismicStationLocation>(sta,newstaloc));
    }
}
#ifndef NO_ANTELOPE
SeismicArray::SeismicArray(DatabaseHandle& dbi,
	double time, string arrayname)
{
        const string base_error("SeismicArray database constructor:  ");
	name=arrayname;
	// Intentionally create a copy.  This algorithm assumes
	// this constructor is call rarely or this approach is
	// inefficient.  
	DatascopeHandle dbh(dynamic_cast<DatascopeHandle&>(dbi));
	dbh.natural_join("site","snetsta");
	int ndbrows=dbh.number_tuples();
        if(ndbrows<=0) throw SeisppError(base_error
                +"natural join of site and snetsta channel is empty\n"
                +"You probably need an snetsta table");
	int jdate=yearday(time);
	int ondate=0,offdate=0;
	dbh.rewind();
	vector<int> ondl,offdl,future_ondl;
	vector<int>::iterator ondlmax,offdlmin;
	for(int i=0;i<ndbrows;++i,++dbh)
	{
		double lat,lon,elev,dnorth,deast;
		string staname;
		string netname;
		string refsta;
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
				lat=dbh.get_double("site.lat");
				lon=dbh.get_double("site.lon");
				elev=dbh.get_double("site.elev");
				// here we presume null is 0.0
				dnorth=dbh.get_double("dnorth");
				deast=dbh.get_double("deast");
				staname=dbh.get_string("sta");
				refsta=dbh.get_string("refsta");
				netname=dbh.get_string("snet");
				// convert from external degrees to radians
				lat=rad(lat);
				lon=rad(lon);
				if(ondate>0)
					ondl.push_back(ondate);
				if( (offdate>0) && (offdate>jdate) )
				// Same -1 issue about offdate
				if( offdate>jdate )
					offdl.push_back(offdate);
				// Use sta alone as a key.  In normal css database
				// this is required to be unique.  snet is needed
				// only for foreign name maps.
				SeismicStationLocation newstaloc(lat,lon,
					 elev,dnorth,deast,staname,netname,refsta);
				//array[staname]=newstaloc;
				array.insert(pair<string,SeismicStationLocation>(staname,newstaloc));
				
			}
			else
			{
				/* We post times of stations expected to 
				come on in the future.  The end of the current
				valid time period is either on offdate for
				the currently defined array or the time the
				next station gets turned on.  Complain
				if ondate is invalid */
				if(epoch(ondate)>0.0)
					future_ondl.push_back(ondate);
				else if (SEISPP_verbose)
					cerr << "SeismicArray(Warning):  "
					  << "Invalid ondate attribute in site table.   "
					  << "Check your database"<<endl;
			}
		} catch (SeisppDberror& dberr)
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
	// Old test
	//if(offdl.empty() && ondl.empty())
	if(array.size()<=0)
		throw SeisppError(base_error
			+ string("  no stations found in the db marked live")
			+ string(" at time ")+string(strtime(time)) );
	if(ondl.empty())
		//This really shouldn't happen, but a sane fallback
		ondate=jdate-1;
	else
	{
		ondlmax=max_element(ondl.begin(),ondl.end());
		ondate=*ondlmax;
	}
	if(offdl.empty())
	{
		if(future_ondl.empty())
			offdate=2050001;
		else
		{
			offdlmin=min_element(future_ondl.begin(),
				future_ondl.end());
			offdate= *offdlmin;
		}
	}
	else
	{
		offdlmin=min_element(offdl.begin(),offdl.end());
		offdate=*offdlmin;
		if(!future_ondl.empty())
		{
			offdlmin=min_element(future_ondl.begin(),
                                future_ondl.end());
			offdate=min(offdate,*offdlmin);
		}
	}
	valid_time_interval=TimeWindow(epoch(ondate),
				epoch(offdate));
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
#endif

SeismicArray::SeismicArray(const SeismicArray& orig)
{
	name=orig.name;
	array=orig.array;
	valid_time_interval=orig.valid_time_interval;
	subarrays=orig.subarrays;
}
SeismicArray& SeismicArray::operator=(const SeismicArray& orig)
{
	if(this!=&orig)
	{
		name=orig.name;
		array=orig.array;
		valid_time_interval=orig.valid_time_interval;
		subarrays=orig.subarrays;
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
SeismicArray SeismicArray::subset(string subsetname)
{
	list<string> stalist;
	SeismicArray result;
	result.name=subsetname;
	result.valid_time_interval=this->valid_time_interval;
	map<string,list<string> >::iterator it;
	it=subarrays.find(subsetname);
	if(it!=subarrays.end())
	{
		map<string,SeismicStationLocation>::iterator staiter,staiter_end;
		staiter_end=array.end();
		list<string>::iterator subiter;
		for(subiter=(*it).second.begin();subiter!=(*it).second.end();++subiter)
		{
			staiter=array.find(*subiter);
			// Silently ignores stations in the list that are not on.
			if(staiter!=staiter_end)
			{
				result.array[*subiter]=(*staiter).second;
			}
		}
	}
	// Note this implicitly returns default constructed 
	// SeismicArray object if subnetname is not found.
	return result;
}
SeismicArray SeismicArray::subset(int nsub)
{
	map<string,list<string> >::iterator it;
	// I am pretty sure we need to use a linear search because I don't think 
	// a map container can use a random access iterator
	int i;
	for(it=subarrays.begin(),i=0;it!=subarrays.end();++it,++i)
	{
		if(i==nsub)
		{
			string saname=(*it).first;
			SeismicArray ssresult;
			ssresult=this->subset(saname);
			return ssresult;
		}
	}
	char buf[256];
	ostringstream message(buf);
	message << "SeismicArray::subset(int) method: invalid integer index="
			<< nsub<<endl
		<< "Not that many subarrays are defined";
	throw SeisppError(message.str());
}

void SeismicArray::LoadSubarray(string saname, list<string> salist)
{
	pair<map<string,list<string> >::iterator,bool> insert_result;
	insert_result=subarrays.insert(map<string,list<string> >::value_type(saname,salist));
	if(!insert_result.second)
		throw SeisppError(string("SeismicArray::LoadSubarray:  load failed for subarray name=")
				+ saname);
}

// There is probably an algorithm that would do this more efficiently, but for now I'll
// use this crude version because I don't expect this to be called very often.
int SeismicArray::number_subarrays()
{
	map<string,list<string> >::iterator it;
/*
	int i;
	for(it=subarrays.begin(),i=0;it!=subarrays.end();++it,++i)
	return(i);
*/
	return(subarrays.size());
}

//  Helper functions that belong in this file
// This is a pretty general function to strip leading whitespace.
// Probably should be somewhere else and made externally visible
// but for now it will just be internal here.
//
string strip_white(const char *s)
{
	const string white(" \t\n");
	string work(s);
	int sstart,send;
	sstart=work.find_first_not_of(white,0);
	// clear leading white space
	if(sstart!=0) work.erase(0,sstart);
	return(work);
}

// A routine to make it easy to load a group of subarrays from pf
void load_subarrays_from_pf(SeismicArray& master,Pf *pf)
{
	Pf *pfarr,*pfva;  // return need for pfget
        list<string> stalist;  // result
        const string vakey("virtual_arrays");
        const string vatblkey("sta_list");
	const string base_message("SEISPP::load_subarrays_from_pf: ");

	if(pfget(pf,const_cast<char *>(vakey.c_str()),(void **)&pfarr)!=PFARR)
                throw SeisppError(base_message
                        +vakey
                        +string(" Arr&{} tag missing from parameter file"));
	Tbl *arraylist;
	arraylist=pfkeys(pfarr);
	if(maxtbl(arraylist)<=0)
		throw SeisppError(base_message+vakey
		  +string(" Arr&{} field for defining subarrays is empty"));
	int i,j;
	for(i=0;i<maxtbl(arraylist);++i)
	{
		char *arrayname;
		Tbl *t;
		arrayname=static_cast<char *>(gettbl(arraylist,i));
		if(pfget(pfarr,arrayname,(void **)&pfva)!=PFARR)
			throw SeisppError(base_message
			+ string("pfget returned error parsing subarray=")
			+ string(arrayname) );
		t=pfget_tbl(pfva,const_cast<char *>(vatblkey.c_str()));
		if(t==NULL)
			throw SeisppError(base_message
			+ string("pfget error.  Missing required Tbl tag=")
			+ vatblkey );

		for(j=0;j<maxtbl(t);++j)
		{
			char *line;
			line=static_cast<char *>(gettbl(t,j));
			string staname=strip_white(line);
			stalist.push_back(staname);
		}
		master.LoadSubarray(string(arrayname),stalist);
		// cleanup
		stalist.clear();
		freetbl(t,0);
	}
	freetbl(arraylist,0);
}
// A routine to make it easy to load a group of subarrays from pf
void load_subarrays(SeismicArray& master,PfStyleMetadata& md)
{
    list<string> stalist;  // result
    const string vakey("virtual_arrays");
    const string vatblkey("sta_list");
    const string base_message("SEISPP::load_subarrays: ");
    try{
        PfStyleMetadata vab=md.get_branch(vakey);
        list<string> arraylist=vab.arr_keys();

        if(arraylist.size()<=0)
		throw SeisppError(base_message+vakey
		  +string(" Arr&{} field for defining subarrays is empty"));
	int i,j;
        list<string>::iterator keyptr,tblptr;
        for(i=0,keyptr=arraylist.begin();keyptr!=arraylist.end();++i,++keyptr)
	{
            string arrayname(*keyptr);   // convience for readability
            PfStyleMetadata thisva=vab.get_branch(arrayname);
            list<string> tbllines=thisva.get_tbl(vatblkey);

            string staname;
            for(tblptr=tbllines.begin();tblptr!=tbllines.end();++tblptr)
            {
                staname=strip_white(tblptr->c_str());
                stalist.push_back(staname);
            }
	    master.LoadSubarray(string(arrayname),stalist);
	    stalist.clear();
	}
    }catch(...){throw;};
}

} // End namespace SEISPP
			
