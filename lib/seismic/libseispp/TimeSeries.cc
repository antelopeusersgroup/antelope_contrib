#include "tr.h" // Antelope trace library
#include "seispp.h"
#include "SeisppKeywords.h"
#include "SeisppError.h"
#include "TimeSeries.h"
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
//
// simple constructors for the TimeSeries object are defined inline
// in seispp.h.  
//
TimeSeries::TimeSeries() : BasicTimeSeries(), Metadata()
{
	s.reserve(0);
}
TimeSeries::TimeSeries(int nsin) : BasicTimeSeries(), Metadata()
{
	s.reserve(nsin);
	// This seems to be necessary at least for Sun's compiler
	for(int i=0;i<nsin;++i)s.push_back(0.0);
}
	
/*
TimeSeries::TimeSeries(const TimeSeries& tsi) : 
		BasicTimeSeries(dynamic_cast<const BasicTimeSeries&>(tsi)),
		Metadata(dynamic_cast<const Metadata&>(tsi))
*/
TimeSeries::TimeSeries(const TimeSeries& tsi) : 
		BasicTimeSeries(tsi),
		Metadata(tsi)
{
	if(live)
	{
		s=tsi.s;
	}
}
//
// This version uses the copy constructor from a Metadata object
// Wfdisc related fields are then extracted from the Metadata object
// and used to read actual data.  
// Note that this originally used a Pf as the input parameter but
// I changed it to a Metadata object instead.  To create a TimeSeries
// directly from a Pf you can just do this:
//  md = Metadata(pf);
//  ts = TimeSeries(md);  -- i.e. this constructor
//

TimeSeries::TimeSeries(const Metadata& md,bool load_data) : Metadata(md)
{
	string stref;
	string dfile, dir;
	long foff;
	FILE *fp;
	string dtype;
	float *inbuffer;

	live = false;
	try {
		// Names space is frozen.  Not as general as it
		// probably should be, but until proven necessary 
		// will do it the easy way
		dt = 1.0/this->get_double("samprate");
		t0 = this->get_double("time");
	        ns = this->get_int("nsamp");
		s.reserve(ns);
		if(load_data)
		{
			stref = this->get_string("timetype");
			if(stref == "relative")
				tref = relative;
			else
				tref = absolute;
			dtype = this->get_string("datatype");
#ifdef BIGENDIAN
			if(dtype!="t4") 
				throw(SeisppError("Unsupported datatype:  metadata-driven constructor only supports t4 data with external files"));
#else
			if(dtype!="u4") 
				throw(SeisppError("Unsupported datatype:  metadata-driven constructor only supports t4 data with external files"));
#endif
			dir = this->get_string("dir");
			dfile = this->get_string("dfile");
			foff = this->get_long("foff");
			string fname=dir+"/"+dfile;
			if((fp=fopen(fname.c_str(),"r")) == NULL) 
				throw("Open failure for file "+fname);
			if (foff>0)fseek(fp,foff,SEEK_SET);
			inbuffer = new float[ns];
			if(fread((void *)(inbuffer),sizeof(float),ns,fp)
					!= ns ) 
			{
				delete [] inbuffer;  // memory leak possible without this
				throw(SeisppError("TimeSeries constructor:  fread error on file "+fname));
			}
			for(int i=0;i<ns;++i) 
				s.push_back(static_cast<double>(inbuffer[i]));
			delete [] inbuffer;
			live = true;
			fclose(fp);
		}
	}
	catch (MetadataError& mderr)
	{
		throw mderr;

	}
}
/* Small error needed for recovery. Echos only in verbose mode. */
void TimeSeriesRequiredError(MetadataGetError mde,string name)
{
	cerr << "TimeSeries constructor:  Required Metadata not found"<<endl;
	mde.log_error();
	cerr << "Trying to recover by querying db again with default name="
		<< name<<endl;
	cerr << "Edit your parameter file to make this message go away"<<endl;
}

/* Constructor to read a single seismogram from an antelope
database.  Arguments:
	db - database handle
	md_to_extract - defines database attributes to be extracted and placed
		in the Metadata object (see Metadata(3)).
	am - defines namespace mapping from database naming to internal namespace
		(see Metadata(3)).
Uses Antelope's trgetwf function which should allow it to read almost any
common seismic trace format. 

Major modification Feb 2007:  previous version fetched ns,t0,dt,
and te from Metadata.  These are now dogmatically read from the
database using keywords defined in SeisppKeywords.h.  This insulates
this code from frozen names allowing a potential warping of the 
library to work on a different schema.  This function, however,
as currently implemented is totally locked into antelope anyway
so css3.0 attribute names can pretty easily be assumed to be 
workable.

Modification May 2008:  Changed this again to assume ns,t0,
and dt were stored in Metadata, but then to hit the db again
if they are not defined and post an error.  This was necessary
after a change to implement aliasing in a more consistent fashion.

*/

TimeSeries::TimeSeries(DatabaseHandle& rdb,
		MetadataList& md_to_extract, 
			AttributeMap& am) 
	: Metadata(rdb,md_to_extract,am)
{
	float *inbuffer=NULL;
	DatascopeHandle& dbh=dynamic_cast<DatascopeHandle&>(rdb); 
	double te,t0read,teread,srate;
#ifdef OLDANTELOPE
	int nread;
#else
	long int nread;
#endif
	/* Metadata constructor should load these parameters, but
	because they are required to build this object we attempt
	a recovery if they are not found by hitting the db 
	with a default name */
	try {
		ns=this->get_int(number_samples_keyword);
	}catch (MetadataGetError& mde)
	{
		if(SEISPP_verbose) 
		  TimeSeriesRequiredError(mde,number_samples_keyword);
		try{
			ns=dbh.get_int(number_samples_keyword);
		} catch(SeisppDberror& serr) {throw serr;}
		this->put(number_samples_keyword,ns);
	}
	try {
		t0=this->get_double(start_time_keyword);
	}catch (MetadataGetError& mde)
	{
		if(SEISPP_verbose) 
		  TimeSeriesRequiredError(mde,start_time_keyword);
		try{
			t0=dbh.get_double(start_time_keyword);
		} catch(SeisppDberror& serr) {throw serr;}
		this->put(start_time_keyword,t0);
	}
	try {
		srate=this->get_double(sample_rate_keyword);
	}catch (MetadataGetError& mde)
	{
		if(SEISPP_verbose) 
		  TimeSeriesRequiredError(mde,sample_rate_keyword);
		try{
			t0=dbh.get_double(start_time_keyword);
		} catch(SeisppDberror& serr) {throw serr;}
		this->put(sample_rate_keyword,srate);
	}
	dt=1.0/srate;
	// Safer to compute this quantity than require it to 
	// be read from the database
	te=this->endtime()+dt/2.0;
	this->put(end_time_keyword,te);
	/*
	absolute versus relative time is a Seispp library
	concept not universally used.  We need to set
	the tref variable appropriately and there is currently
	no unambiguous way to do this other than to try to
	read an attribute that tags this and assume something
	(absolute) if the attribute is not defined.  
	*/
	int ierr;
	Dbptr dbtest=dblookup(dbh.db,0,0,
		const_cast<char *>(timetype_keyword.c_str()),0);
	if(dbtest.field==dbINVALID)
		tref=absolute;
	else
	{
		char timetype[20];
		ierr=dbgetv(dbh.db,0,
			const_cast<char *>(timetype_keyword.c_str()),
			timetype,NULL);
		if(ierr==dbINVALID)
			tref=absolute;
		else if(strcmp(timetype,"r"))
			tref=absolute;
		else
			tref=relative;
	}

	s.reserve(ns);

	/* This will create a memory leak if trgetwf fails 
	// trgetwf returns an error for multiple conditions and
	// some are more fatal than others.  For most applications
	// I can imagine any error in trgetwf is serious and probably
	// would normally lead to an exit.
	//
	// Problem is that I can't just test for a NULL pointer 
	// and expect it is safe to free inbuffer before exit
	*/
	if(trgetwf(dbh.db,0,&inbuffer,NULL,t0,te,
				&t0read,&teread,&nread,
				0,0))
			throw SeisppDberror("TimeSeries database constructor:  trgetwf error",dbh.db);
	if(nread!=ns)
	{
	    if(abs(nread-ns)>1)
	    {
		// bitch if the mismatch is more than 1 
		cerr << "Data read mismatch on row "
			<< dbh.db.record 
			<< " of input database" << endl
			<< "Expected to read "
			<< ns 
			<< " data points but read "
			<< nread << endl;
	    	}
		ns = nread;
		t0 = t0read;
		this->put(end_time_keyword,teread);
		this->put(number_samples_keyword,ns);
	}
	if(ns<=0)
		live=false;
	else
	{
		s.reserve(ns);
		for(int i=0;i<this->ns;++i) 
			s.push_back(static_cast<double>(inbuffer[i]));
		live = true;
	}
	// trgetwf is a C function so we need to use free to 
	// release the space it allocated.
	free(inbuffer);
}
// standard assignment operator
TimeSeries& TimeSeries::operator=(const TimeSeries& tsi)
{
	if(this!=&tsi)
	{
		live=tsi.live;
		dt=tsi.dt;
		t0=tsi.t0;
		ns=tsi.ns;
		tref=tsi.tref;
		if(tsi.live)
		{
			s=tsi.s;
		}
		gaps=tsi.gaps;
		mreal=tsi.mreal;
		mint=tsi.mint;
		mbool=tsi.mbool;
		mstring=tsi.mstring;
	}
	return(*this);
}			
/*  Sum operator for TimeSeries object */

void TimeSeries::operator+=(const TimeSeries& data)
{
	int i,i0,iend,ntosum;
	int j,j0=0,jend=ns;
	// Sun's compiler complains about const objects without this.
	TimeSeries& d=const_cast<TimeSeries&>(data);
	// Silently do nothing if d is marked dead
	if(!d.live) return;
	// Silently do nothing if d does not overlap with data to contain sum
	if( (d.endtime()<t0) 
		|| (d.t0>(this->endtime())) ) return;
	if(d.tref!=(this->tref)) 
		throw SeisppError("TimeSeries += operator cannot handle data with inconsistent time base\n");
	//
	// First we have to determine range fo sum for d into this 
	//
	i0=d.sample_number(this->t0);
	if(i0<0)
	{
		j=-i0;
		i0=0;
	}
	iend=d.sample_number(this->endtime());
	if(iend>(d.ns-1))
	{
		iend=d.ns-1;
	}
	//
	// IMPORTANT:  This algorithm simply assumes zero_gaps has been called
	// and/or d was checked for gaps befor calling this operatr.  
	// It will produce garbage for most raw gap (sample level) marking schemes
	//
	for(i=i0,j=j0;i<iend;++i,++j)
		this->s[j]+=d.s[i];
}
double TimeSeries::operator[](int i)
{
	if(!live) 
	   throw SeisppError(string("TimeSeries operator[]: attempting to access data marked as dead"));
	if( (i<0) || (i>=ns) )
	{
		throw SeisppError(
			string("TimeSeries operator[]:  request for sample outside range of data"));
	}
	return(s[i]);
}
// Forces samples in marked gaps to zero.  
void TimeSeries::zero_gaps()
{
	double tsend;
	int i,istart,iend;
	set<TimeWindow,TimeWindowCmp>::iterator this_gap;
	tsend = t0+((double)(ns-1))*dt;

	for(this_gap=gaps.begin();this_gap!=gaps.end();++this_gap)
	{
		if(this_gap->end < t0) continue;
		if(this_gap->start > tsend) continue;
		if(this_gap->start<t0)
			istart = 0;
		else
			istart = nint((this_gap->start-t0)/dt);
		if(this_gap->end>tsend)
			iend = ns-1;
		else
			iend = nint((this_gap->end-t0)/dt);
		for(i=istart;i<=iend;++i) s[i]=0.0;
	}
}

ostream& operator << (ostream& os, TimeSeries& z)
{
	os << "==================TimeSeries Metadata====================="
		<< endl;
	os << dynamic_cast<Metadata &>(z);
	os << "=== BasicTimeSeries data members==="<<endl;
	os << dynamic_cast<BasicTimeSeries &>(z);
	os << "=========================================================="
		<<endl;
	vector<double>::iterator zptr;
	for(zptr=z.s.begin();zptr!=z.s.end();++zptr)
		os << *zptr <<endl;
	return(os);
}

} // End SEISPP namespace declaration
