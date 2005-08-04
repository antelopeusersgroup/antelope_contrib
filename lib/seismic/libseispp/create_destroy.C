#include <memory>
#include <algorithm>
#include <math.h>
#include "coords.h"
#include "tr.h" // Antelope trace library
#include "dbpp.h"
#include "seispp.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP
{
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

TimeSeries::TimeSeries(Metadata& md,bool load_data) : Metadata(md)
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
// Default constructor for ThreeComponentSeismogram could be 
// done inline in seispp.h, but it is complication enough I put
// it here
//
ThreeComponentSeismogram::ThreeComponentSeismogram() : Metadata(),u(1,1)
{
	live = false;
	dt=0.0;
	t0=0.0;
	ns=0;
	tref=absolute;
	components_are_orthogonal=true;
	components_are_cardinal=true;
	for(int i=0;i<3;++i)
		for(int j=0;j<3;++j)
			if(i==j) 
				tmatrix[i][i]=1.0;
			else
				tmatrix[i][j]=0.0;
}
ThreeComponentSeismogram::ThreeComponentSeismogram(int nsamples) 
	: Metadata(),u(3,nsamples)
{
	live = false;
	dt=0.0;
	t0=0.0;
	ns=nsamples;
	tref=absolute;
	components_are_orthogonal=true;
	components_are_cardinal=true;
	for(int i=0;i<3;++i)
		for(int j=0;j<3;++j)
			if(i==j) 
				tmatrix[i][i]=1.0;
			else
				tmatrix[i][j]=0.0;
}
ThreeComponentSeismogram::ThreeComponentSeismogram(Metadata& md,
				bool load_data) : Metadata(md),u()
{
	string stref;
	string dfile, dir;
	int foff;
	FILE *fp;
	string dtype;
	string data_order;
	double *inbuffer;
	int i,j,ioff;

	components_are_orthogonal=true;
	live=false;
	try {
		// Names space is frozen.  Not as general as it
		// probably should be, but until proven necessary 
		// will do it the easy way
		dt = 1.0/this->get_double("samprate");
		t0 = this->get_double("time");
	        ns = this->get_int("nsamp");
		components_are_cardinal=this->get_bool("components_are_cardinal");
		if(components_are_cardinal)
		{
		    components_are_orthogonal=true;
		    for(i=0;i<3;++i)
		      for(j=0;j<3;++j)
			if(i==j) 
				tmatrix[i][i]=1.0;
			else
				tmatrix[i][j]=0.0;
		}
		else
		{
		// Assume transformation matrix is defined in the
		// metadata object using names defined here
			tmatrix[0][0]=this->get_double("U11");
			tmatrix[1][0]=this->get_double("U21");
			tmatrix[2][0]=this->get_double("U31");
			tmatrix[0][1]=this->get_double("U12");
			tmatrix[1][1]=this->get_double("U22");
			tmatrix[2][1]=this->get_double("U32");
			tmatrix[0][2]=this->get_double("U13");
			tmatrix[1][2]=this->get_double("U23");
			tmatrix[2][2]=this->get_double("U33");
		// can't accept default in this situation 
		// difference is small anyway as if !cardinal need
		// to construct an inverse to make them so
			components_are_orthogonal=false;
		}
		u=dmatrix(3,ns);
		if(load_data)
		{
			int i,j,ioff;
			stref = this->get_string("TimeReferenceType");
			if(stref == "relative")
				tref = relative;
			else
				tref = absolute;
			dtype = this->get_string("datatype");
			if(dtype!="t8") 
				throw(SeisppError("Unsupported datatype:  metadata-driven constructor only supports t8 data with external files"));
			
			dir = this->get_string("dir");
			dfile = this->get_string("dfile");
			foff = this->get_int("foff");
			string fname=dir+"/"+dfile;
			if((fp=fopen(fname.c_str(),"r")) == NULL) 
				throw("Open failure for file "+fname);
			if (foff>0)fseek(fp,(long)foff,SEEK_SET);
			inbuffer = new double[3*ns];
			if(fread((void *)(inbuffer),sizeof(double),ns,fp)
					!= ns ) 
			{
				delete [] inbuffer;  // memory leak possible without this
				throw(SeisppError("ThreeComponentSeismogram constructor:  fread error on file "+fname));
			}
			// another frozen namespace problem
			data_order = this->get_string("three_component_data_order");
			// Transpose is intentionally the exact match since I expect
			// most cases will be not time_order but channel order
			// The following could be done with the BLAS but this
			// makes the code more adaptable to changes in dmatrix implementation
			/* Handle different data orders.  Fortran order is
			intentionally an exact match since normal external 
			representation is expected to be in a time series 
			order to allow things like dbpick to access the
			data easily */
			if(data_order == "channel_order" 
				|| data_order == "multiplexed" )
			{
				for(j=0,ioff=0;j<ns;++j)
				{
					for(i=0;i<3;++i)
					{
						u(i,j)=static_cast<double>(inbuffer[ioff]);
						++ioff;
					}
				}
			}
			else
			{
				for(i=0,ioff=0;i<3;++i)
				{
					for(j=0;j<ns;++j)
					{
						u(i,j)=static_cast<double>(inbuffer[ioff]);
						++ioff;
					}
				}
			}
						
			delete [] inbuffer;
			fclose(fp);
		}
	}
	catch (MetadataError& mderr)
	{
		// Land here when any of the metadata routines fail
		mderr.log_error();
		throw SeisppError("Constructor for ThreeComponentSeismogram object failed");

	}
}

ThreeComponentSeismogram::ThreeComponentSeismogram
			(const ThreeComponentSeismogram& t3c):
	 BasicTimeSeries(dynamic_cast<const BasicTimeSeries&>(t3c)),
	 Metadata(dynamic_cast<const Metadata&>(t3c)),
					u(t3c.u)
{
	int i,j;
        components_are_orthogonal=t3c.components_are_orthogonal;
        components_are_cardinal=t3c.components_are_cardinal;
	for(i=0;i<3;++i)
		for(j=0;j<3;++j) tmatrix[i][j]=t3c.tmatrix[i][j];
}
// small helpers to avoid cluttering up code below.
bool tmatrix_is_cardinal(ThreeComponentSeismogram& seis)
{
	if( (seis.tmatrix[0][0]!=1.0) || (seis.tmatrix[0][1]!=0.0) 
		||  (seis.tmatrix[0][2]!=0.0) )return(false);
	if( (seis.tmatrix[1][0]!=0.0) || (seis.tmatrix[1][1]!=1.0) 
		||  (seis.tmatrix[1][2]!=0.0) )return(false);
	if( (seis.tmatrix[2][0]!=0.0) || (seis.tmatrix[2][1]!=0.0) 
		||  (seis.tmatrix[2][2]!=1.0) )return(false);
	return(true);
}
/* Constructor to read a three-component seismogram from an antelope
database.  Arguments:
	rdb - generic database handle (cast to Anteloep form)
	md_to_extract - defines database attributes to be extracted and placed
		in the Metadata object (see metadata(3)).
	am - defines namespace mapping from database naming to internal namespace
		(see metadata(3)).
Uses Antelope's trgetwf function which should allow it to read almost any
common seismic trace format. The routine MUST have the db pointer defined
as a bundle in groups of 3 channel.  (time:sta:chan group clause).  
It will throw an exception if the number per group is anything but 3.
It does contain a feature to handle mixed start and end times.  The
minimum start and maximum end times of the three components are used.
If there are any irregularies the front or end is marked as a gap.

Modified:  Jan 25, 2005
Added support for 3c data type = dmatrix allowed in wfprocess table.
*/

ThreeComponentSeismogram::ThreeComponentSeismogram(
	DatabaseHandle& rdb,
		MetadataList& md_to_extract, 
			AttributeMap& am) : Metadata(),u()
{
	TimeSeries component[3];
	const string this_function_base_message
		= "ThreeComponentSeismogram event database constructor:";
	int i,j,ierr;
	DatascopeHandle& dbh=dynamic_cast<DatascopeHandle&>(rdb);
	live = false;
	components_are_cardinal=false;
	components_are_orthogonal=false;
	try{
	    // a simple test for data stored in wfdisc is that
	    // in that scenario dbh must be a group pointer i
	    // datascope jargon.  In my object implementation this
	    // is easily detected with this conditional
	    if(dbh.is_bundle)
	    {
		double tsread[3],teread[3];
		double hang[3],vang[3];
		double samprate[3];
		int nread[3],nsamp[3];

		DBBundle bundle = dbh.get_range();
		if((bundle.end_record-bundle.start_record)!=3)
			throw(SeisppDberror(
			  string(this_function_base_message
			  +	"  database bundle pointer irregularity\n"
			  +     "All data must have exactly 3 channels per bundle"),
			  dbh.db,complain));
		// Use the simplified copy constructor.  This handle
		// to loop through data.  Sometimes is a bundle pointer
		// other times it could be one row at a time.  
		// The handle allows this to happen independent of that
		// nontrivial detail.
		DatascopeHandle dbhv(bundle.parent,bundle.parent);
		// This weird cast is necessary because the
		// the TimeSeries constructor uses a generic handle
		DatabaseHandle *rdbhv=dynamic_cast
				<DatabaseHandle*>(&dbhv);
		for(i=0,dbhv.db.record=bundle.start_record;
			dbhv.db.record<bundle.end_record;
				++i,++dbhv.db.record)
		{
		    try
		    {
			component[i]=TimeSeries(*rdbhv,
				md_to_extract,am);
			tsread[i]=component[i].get_double("time");
			teread[i]=component[i].get_double("endtime");
			nsamp[i]=component[i].get_int("nsamp");
			samprate[i]=component[i].get_double("samprate");
			hang[i]=component[i].get_double("hang");
			vang[i]=component[i].get_double("vang");
		    } catch (...) 
		    {
			throw;
		    }
		}
		// scan for irregular sample rates and abort if they don't match
		if(samprate[0]!=samprate[1] || samprate[1]!=samprate[2]
			|| samprate[0]!=samprate[2])
		{
			throw SeisppDberror(
				string("Irregular sample rates in ")
				+ string("three component data set"),
				dbh.db,complain);
		}
		// get the start and end time ranges and handle ragged
		// start and end time irregularities
		double tsmin,tsmax,temin,temax;
		for(tsmin=tsread[0],tsmax=tsread[0],temin=teread[0],temax=teread[0],
			i=1;i<3;++i)
		{
			tsmin=min(tsmin,tsread[i]);
			tsmax=max(tsmax,tsread[i]);
			temin=min(temin,teread[i]);
			temax=max(temax,teread[i]);
		}
		// arbitrarily clone the Metadata of component 0.  This 
		// assumes the list of attributes in md_to_extract
		// is generic and valid

		copy_selected_metadata(dynamic_cast<Metadata &>(component[0]),
			dynamic_cast<Metadata &>(*this),md_to_extract);

		ns = this->get_int("nsamp");
		dt = 1.0/(this->get_double("samprate"));
		t0 = this->get_double("time");
		tref = absolute;  // perhaps too dogmatic
		double te_md;
		te_md = this->get_double("endtime");
		if(t0!=tsmin)
		{
			t0=tsmin;
			this->put("starttime",tsmin);
		}
		if(te_md!=temax)
		{
			te_md=temax;
			this->put("endtime",temax);
		}
		// Don't mark a gap unless the irregularity is
		// greater than one sample
		if(tsmax-t0>dt)
			this->add_gap(TimeWindow(t0,tsmax));
		if(te_md-temin>dt)
			this->add_gap(TimeWindow(temin,te_md));
		ns = nint((te_md-t0)/dt) + 1;
		u=dmatrix(3,ns);
		for(i=0;i<3;++i)
			for(j=nint((tsread[i]-t0)/dt);j<nread[i],j<ns;++j)
				u(i,j)=component[i].s[j];
		/* Scan for gaps in any component.  This is a general 
		algorithm that is thorough but not fast.  We call the 
		is_gap function for each component and mark the whole
		object with a gap if there is a gap on any component */
		TimeWindow tw;
		bool in_a_gap;
		double t;
		tw.start=t0;  // probably an unnecessary initialization
		for(j=this->sample_number(tsmax),in_a_gap=false;j<ns;++j)
		{
			t=this->time(j);
			// Handling ragged endtimes is messy, but here it is
			if((t>temin) && (j<ns-1)) break;
			if(in_a_gap)
			{
				in_a_gap=false;
				for(i=0;i<3;++i)
				{
					if(component[i].is_gap(t))
					{
						in_a_gap=true;
						break;
					}
				}
				if(!in_a_gap)
				{
					tw.end=t-dt;
					this->add_gap(tw);
				}
			}
			else
			{
				for(i=0;i<3;++i)
				{
					if(component[i].is_gap(t))
					{
						in_a_gap=true;
						break;
					}
				}
				if(in_a_gap)
				{
					tw.start=t;
				}
			}
		}
	
		// Now we have to set the transformation matrix
		// The transformation matrix is defined as the matrix
		// that "was" applied to the data to put it in the form
		// it is now in.  This means the rows of tmatrix are
		// unit vectors in directions defined by vang and hang
		// An annoying inconvenience is that hang in CSS3.0 is
		// not the azimuth angle in spherical coordinates at 
		// the station but the azimuth from north.  Hence we have
		// convert it to a phi angle for spherical coordinates
		SphericalCoordinate scor;
		for(i=0;i<3;++i)
		{
			double *nu;
			scor.phi = M_PI_2 - rad(hang[i]);
			scor.theta = rad(vang[i]);
			nu=SphericalToUnitVector(scor);
			for(j=0;j<3;++j)tmatrix[i][j]=nu[j];
			delete [] nu;
		}
		components_are_cardinal = tmatrix_is_cardinal(*this);
		if(components_are_cardinal) components_are_orthogonal=true;
		// Could test for orthogonal, but simpler to just
		// let the transformation matrix get inverted.
		//
		// last thing we do is mark this live
		live = true;
	    }
	    else
	    {
		// Land here when data are not stored in wfdisc but
		// stored as dmatrix object binary form
		Metadata md(dbh,md_to_extract,am);
		// In this situation we have to load the definition of the
		// transformation matrix into the md object
		// We must assume the data are externally defined in cardinal coordinates
		md.put("U11",1.0);
		md.put("U22",1.0);
		md.put("U33",1.0);
		md.put("U12",0.0);
		md.put("U13",0.0);
		md.put("U21",0.0);
		md.put("U23",0.0);
		md.put("U31",0.0);
		md.put("U32",0.0);
		*this=ThreeComponentSeismogram(md,false);
		ns = this->get_int("nsamp");
		dt = 1.0/(this->get_double("samprate"));
		t0 = this->get_double("time");
		// default for processed data is relative time
		tref=relative;
		try {
			string timetype;
			timetype=md.get_string("timetype");
			if(timetype=="a") tref=absolute;
		} catch (...)
		{
			cerr << "timetype field not found.  Assuming relative time reference"<<endl
				<< "Make sure timetype is extracted from database to make this error go away"
				<< endl;
		}
		
		// these need to be forced
		components_are_cardinal=true;
		components_are_orthogonal=true;
		// Important consistency cross check.
		string datatype=md.get_string("datatype");
		if(datatype!="3c")
			throw SeisppError(
				string("ThreeComponentSeismogram constructor:")
				+string(" cannot handle datatype="+datatype));
		// frozen names but essential here.
		// assumes we alias these variables 
		string dfile=md.get_string("dfile");
		string dir=md.get_string("dir");
		string fname=dir+"/"+dfile;
		int foff=md.get_int("foff");

		FILE *fp=fopen(fname.c_str(),"r");
		if(fp==NULL)
			throw SeisppError("Open failed on file"+fname);
		if(foff>0) fseek(fp,static_cast<long int>(foff),SEEK_SET);
		int readsize=(this->ns)*3;
		if(fread(static_cast<void *>(this->u.get_address(0,0)),
			sizeof(double),readsize,fp) != readsize)
		{
			fclose(fp);
			throw SeisppError(string("fread error ")
			+ string("reading  three-component data object from ")
			+fname);
		}
		fclose(fp);
	    }
		
	}
	catch (MetadataError& mderr)
	{
		// Land here when any of the metadata routines fail
		mderr.log_error();
		throw SeisppDberror("Constructor for ThreeComponentSeismogram object failed from a metadata error",
			dbh.db,complain);

	}
}

//
// Ensemble constructors.  Both just create blank trace or 3c trace objects
// and push them into a vector container.
//
TimeSeriesEnsemble::TimeSeriesEnsemble()
{
	member.reserve(0);
}
TimeSeriesEnsemble::TimeSeriesEnsemble(int nensemble, int nsamples)
{
	for(int i=0; i<nensemble; ++i)
	{
		TimeSeries *ts = new TimeSeries(nsamples);
		member.push_back(*ts);
		delete ts;
	}
}
TimeSeriesEnsemble::TimeSeriesEnsemble(const TimeSeriesEnsemble& tceold)
	: Metadata(tceold)
{
	int nmembers=tceold.member.size();
	member.reserve(nmembers);
	for(int i=0; i<nmembers; ++i)
		member.push_back(tceold.member[i]);
}
// Partial copy constructor copies metadata only.  reserves nmembers slots
// in ensemble container
TimeSeriesEnsemble::TimeSeriesEnsemble(Metadata& md,int nmembers)
	: Metadata(md)
{
	member.reserve(nmembers);
}
	
ThreeComponentEnsemble::ThreeComponentEnsemble()
{
	member.reserve(0);
}
ThreeComponentEnsemble::ThreeComponentEnsemble(int nstations, int nsamples)
{
	member.reserve(nstations);
	for(int i=0;i<nstations;++i)
	{
		ThreeComponentSeismogram *tcs 
			= new ThreeComponentSeismogram(nsamples);
		member.push_back(*tcs);
		delete tcs;
	}
}
ThreeComponentEnsemble::ThreeComponentEnsemble(int nstations, 
		int nsamples,
			MetadataList& mdl)
{
	member.reserve(nstations);
	for(int i=0;i<nstations;++i)
	{
		ThreeComponentSeismogram *tcs 
			= new ThreeComponentSeismogram(nsamples);
		member.push_back(*tcs);
		delete tcs;
	}
}
/* Database-driven constructor for an ensemble.  This implementation uses
a Datascope database only through an immediate dynamic_cast to a 
DatascopeHandle, but the idea is that a more generic interface 
would change nothing in the calling sequence.

station_mdl and ensemble_mdl determine which metadata are extracted 
from the database for individual stations in the ensemble and 
as the global metadata for the ensemble respectively.   The 
station metadata is derived from the ThreeComponentSeismogram 
constructor, but the global metadata is arbitrarily extracted from
the first row of the view forming the requested ensemble.  This 
tacitly assumes then that all rows in view that forms this ensemble
have the same attributes (i.e. these are common attributes obtained
through some form of relational join.)  

The routine will pass along any exceptions thrown by functions 
called by the constructor.  It will also throw a SeisppDberror 
in cases best seen by inspecting the code below.

Author:  Gary L. Pavlis
Written:  July 2004
*/
ThreeComponentEnsemble::ThreeComponentEnsemble(DatabaseHandle& rdb,
	MetadataList& station_mdl,
        	MetadataList& ensemble_mdl,
        		AttributeMap& am)
{
	int i;
	int nsta;
	const string this_function_base_message("ThreeComponentEnsemble database constructor");
	ThreeComponentSeismogram *data3c;
	DatascopeHandle& dbh=dynamic_cast<DatascopeHandle&>(rdb);
	try {
		// Will throw an exception if this isn't a group pointer
		DBBundle ensemble_bundle=dbh.get_range();
		nsta = ensemble_bundle.end_record-ensemble_bundle.start_record;
		// We need a copy of this pointer 
		DatascopeHandle dbhv(ensemble_bundle.parent,
			ensemble_bundle.parent);
		// Necessary because the ThreeComponentSeismogram
		// constructor uses a generic handle
		DatabaseHandle *rdbhv=dynamic_cast
			<DatabaseHandle *>(&dbhv);
		// Loop over members
		for(i=0,dbhv.db.record=ensemble_bundle.start_record;
			dbhv.db.record<ensemble_bundle.end_record;
			++i,++dbhv.db.record)
		{
			try {
				data3c = new ThreeComponentSeismogram(*rdbhv,
					station_mdl,am);
			} catch (SeisppDberror dberr)
			{
				cerr << "Problem with member "
					<< i 
					<< " in ensemble construction" << endl;
				dberr.log_error();
				cerr << "Data for this member skipped" << endl;
				continue;
			}
			catch (MetadataError& mderr)
			{
				mderr.log_error();
				throw SeisppError(string("Metadata problem"));
			}
			member.push_back(*data3c);
			delete data3c;
			// copy global metadata only for the first 
			// row in this view
			if(i==0)
			{
				DatascopeHandle dbhvsta;
				if(dbhv.is_bundle)
				{
					DBBundle sta_bundle=dbhv.get_range();
					dbhvsta=DatascopeHandle(
						sta_bundle.parent,
						sta_bundle.parent);
					dbhvsta.db.record=sta_bundle.start_record;
				}
				else
				{
					dbhvsta=dbhv;
				}
				Metadata ens_md(dynamic_cast<DatabaseHandle&>(dbhvsta),
					ensemble_mdl,am);
				copy_selected_metadata(ens_md,
					dynamic_cast<Metadata&>(*this),
					ensemble_mdl);
			}
		}

	} catch (...) { throw;};
}
//copy constructor 
ThreeComponentEnsemble::ThreeComponentEnsemble(const ThreeComponentEnsemble& tceold)
	: Metadata(tceold)
{
	int nmembers=tceold.member.size();
	member.reserve(nmembers);
	for(int i=0; i<nmembers; ++i)
		member.push_back(tceold.member[i]);
}
// Partial copy constructor copies metadata only.  reserves nmembers slots
// in ensemble container
ThreeComponentEnsemble::ThreeComponentEnsemble(Metadata& md,int nmembers)
	: Metadata(md)
{
	member.reserve(nmembers);
}

} // Termination of namespace SEISPP definitions
