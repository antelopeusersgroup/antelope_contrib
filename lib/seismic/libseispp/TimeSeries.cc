#include "tr.h" // Antelope trace library
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
	int foff;
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
			stref = this->get_string("TimeReferenceType");
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
			foff = this->get_int("foff");
			string fname=dir+"/"+dfile;
			if((fp=fopen(fname.c_str(),"r")) == NULL) 
				throw("Open failure for file "+fname);
			if (foff>0)fseek(fp,(long)foff,SEEK_SET);
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

/* Constructor to read a single seismogram from an antelope
database.  Arguments:
	db - database handle
	md_to_extract - defines database attributes to be extracted and placed
		in the Metadata object (see metadata(3)).
	am - defines namespace mapping from database naming to internal namespace
		(see metadata(3)).
Uses Antelope's trgetwf function which should allow it to read almost any
common seismic trace format. 
*/

TimeSeries::TimeSeries(DatabaseHandle& rdb,
		MetadataList& md_to_extract, 
			AttributeMap& am) 
	: Metadata(rdb,md_to_extract,am)
{
	float *inbuffer=NULL;
	DatascopeHandle& dbh=dynamic_cast<DatascopeHandle&>(rdb); 
	try{
		double te,t0read,teread;
		int nread;

		ns = this->get_int("nsamp");
		s.reserve(ns);
		dt = 1.0/(this->get_double("samprate"));
		t0 = this->get_double("time");
		tref = absolute;  // perhaps too dogmatic

		te = this->get_double("endtime");
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
			this->put("endtime",teread);
			this->put("nsamp",ns);
		}
		s.reserve(ns);
		for(int i=0;i<this->ns;++i) 
			s.push_back(static_cast<double>(inbuffer[i]));
		// trgetwf is a C function so we need to use free to 
		// release the space it allocated.
		live = true;
		free(inbuffer);
	}
	catch (MetadataError& mderr)
	{
		// Land here when any of the metadata routines fail
		mderr.log_error();
		throw SeisppDberror("Constructor for TimeSeries object failed from a Metadata error",
			dbh.db);

	}
}
// standard assignment operator
TimeSeries& TimeSeries::operator=(const TimeSeries& tsi)
{
	if(this!=&tsi)
	{
		//Can't figure out how to do this with the 
		// metadata abstraction.  Have to use the private pf 
		pf=pfdup(tsi.pf);
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

} // End SEISPP namespace declaration
