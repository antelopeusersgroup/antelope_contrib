#include <float.h>
#include <sstream>
#include "ThreeComponentSeismogram.h"
#include "perf.h"
#include "coords.h"
#include "seispp.h"
namespace SEISPP
{
using namespace SEISPP;
/*
 *  Start with all the constructors.
 *
*/
//
// Default constructor for ThreeComponentSeismogram could be 
// done inline in seispp.h, but it is complication enough I put
// it here
//
ThreeComponentSeismogram::ThreeComponentSeismogram() : Metadata(),u(0,0)
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
	long foff;
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
			stref = this->get_string("timetype");
			if(stref == "relative")
				tref = relative;
			else
				tref = absolute;
			dtype = this->get_string("datatype");
			if(dtype!="t8") 
				throw(SeisppError("Unsupported datatype:  metadata-driven constructor only supports t8 data with external files"));
			
			dir = this->get_string("dir");
			dfile = this->get_string("dfile");
			foff = this->get_long("foff");
			string fname=dir+"/"+dfile;
			if((fp=fopen(fname.c_str(),"r")) == NULL) 
				throw(
				SeisppError("Open failure for file "+fname));
			if (foff>0)fseek(fp,foff,SEEK_SET);
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
		in the Metadata object (see Metadata(3)).
	am - defines namespace mapping from database naming to internal namespace
		(see Metadata(3)).
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
		int nsamp[3];

		DBBundle bundle = dbh.get_range();
		if((bundle.end_record-bundle.start_record)!=3)
		{
			char buf[128];
			ostringstream message(buf);
			message<< this_function_base_message 
				<< "  database bundle pointer irregularity"<<endl
				<<"All data must have exactly 3 channels per bundle,"
				<< " but group bundle size="
				<< bundle.end_record-bundle.start_record<<endl;
			throw(SeisppDberror(message.str(),dbh.db,complain));
		}
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
			char buf[256];
			ostringstream message(buf);
			message << this_function_base_message
			  << "Irregular sample rates ="
			  << samprate[0] <<", "
			  << samprate[1] <<", "
			  << samprate[2] <<endl
			  << "Cannot handle mixed sample rate data in a 3c bundle"<<endl;
			throw SeisppDberror(message.str(),
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
			for(j=nint((tsread[i]-t0)/dt);j<nsamp[i],j<ns;++j)
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
		// One caution is to check value of ns
		if(ns>0) live = true;
	    }
	    else
	    {
		// Land here when data are not stored in wfdisc but
		// stored as dmatrix object binary form
		Metadata md(dbh,md_to_extract,am);
		copy_selected_metadata(md,
			dynamic_cast<Metadata &>(*this),md_to_extract);
		// In this situation we have to load the definition of the
		// transformation matrix into the md object
		// We must assume the data are externally defined in cardinal coordinates
		this->put("U11",1.0);
		this->put("U22",1.0);
		this->put("U33",1.0);
		this->put("U12",0.0);
		this->put("U13",0.0);
		this->put("U21",0.0);
		this->put("U23",0.0);
		this->put("U31",0.0);
		this->put("U32",0.0);
		for(i=0;i<3;++i)
			for(j=0;j<3;++j) tmatrix[i][j]=0.0;
		for(i=0;i<3;++i) tmatrix[i][i]=1.0;
		ns = md.get_int("nsamp");
		double samprate=md.get_double("samprate");
		dt = 1.0/samprate;
		t0 = md.get_double("time");
		this->put("nsamp",ns);
		this->put("samprate",samprate);
		this->put("time",t0);
		this->put("endtime",this->endtime());
		u=dmatrix(3,ns);
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
		if(datatype!="3c" && datatype!="c3")
			throw SeisppError(
				string("ThreeComponentSeismogram constructor:")
				+string(" cannot handle datatype="+datatype));
		// frozen names but essential here.
		// assumes we alias these variables 
		string dfile=md.get_string("dfile");
		string dir=md.get_string("dir");
		string fname=dir+"/"+dfile;
		long foff=md.get_long("foff");

		FILE *fp=fopen(fname.c_str(),"r");
		if(fp==NULL)
			throw SeisppError("Open failed on file"+fname);
		if(foff>0) fseek(fp,foff,SEEK_SET);
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
		// Decide if we need to do byte swapping and do it if
		// required
		if(IntelByteOrder())
		{
			if(datatype=="3c")
				swapdvec(this->u.get_address(0,0),readsize);
		}
		else
		{
			if(datatype=="c3")
				swapdvec(this->u.get_address(0,0),readsize);
		}
		if(ns>0) live=true;
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

ThreeComponentSeismogram::ThreeComponentSeismogram(vector<TimeSeries>& ts, 
	int component_to_clone) 
	 : Metadata(dynamic_cast<Metadata&>(ts[component_to_clone])),
	  BasicTimeSeries(dynamic_cast<BasicTimeSeries&>(ts[component_to_clone])),
	  u()
{
	int i,j;
	// exit immediately if we are given irregular sample rates.
	if( (ts[0].dt!=ts[1].dt) || (ts[1].dt!=ts[2].dt) )
		throw SeisppError(
		  "gather_components:  sample intervals of components are not consistent");
	// temporaries to hold component values
	double t0_component[3];
	double hang[3];
	double vang[3];
	// Load up these temporary arrays inside this try block and arrange to 
	// throw an exception if required metadata are missing
	try {
                // WARNING hang and vang attributes stored in metadata
                // always assumed to be radians
		hang[0]=ts[0].get_double("hang");
		hang[1]=ts[1].get_double("hang");
		hang[2]=ts[2].get_double("hang");
		vang[0]=ts[0].get_double("vang");
		vang[1]=ts[1].get_double("vang");
		vang[2]=ts[2].get_double("vang");
	} catch (MetadataError& mde)
	{
		mde.log_error();
		throw SeisppError(
		  "gather_components:  missing hang/vang variables in trace metadata");
	}
	// These are loaded just for convenience
	t0_component[0]=ts[0].t0;
	t0_component[1]=ts[1].t0;
	t0_component[2]=ts[2].t0;
	
	// Treat the normal case specially and avoid a bunch of work unless
	// it is required
	if( (ts[0].ns==ts[1].ns) && (ts[1].ns==ts[2].ns) 
		&& (fabs( (t0_component[0]-t0_component[1])/dt )<1.0) 
		&& (fabs( (t0_component[1]-t0_component[2])/dt )<1.0))
	{
		this->u=dmatrix(3,ns);
		// Load data by a simple copy operation
		/* This is a simple loop version
		for(j=0;j<ns;++ns)
		{
			this->u(0,j)=ts[0].s[j];
			this->u(1,j)=ts[1].s[j];
			this->u(2,j)=ts[2].s[j];
		}
		*/  
		// This is a vector version that I'll use because it will
		// be faster albeit infinitely more obscure and 
		// intrinsically more dangerous
		dcopy(ns,&(ts[0].s[0]),1,u.get_address(0,0),3);
		dcopy(ns,&(ts[1].s[0]),1,u.get_address(1,0),3);
		dcopy(ns,&(ts[2].s[0]),1,u.get_address(2,0),3);
	}
	else
	{
		// Land here if the start time or number of samples
		// is irregular.  
		double tsmin,tsmax,temin,temax;
		tsmin=min(t0_component[0],t0_component[1]);  
		tsmin=min(tsmin,t0_component[2]);
		tsmax=max(t0_component[0],t0_component[1]);  
		// It might be possible to use min in std algorithm for this
		tsmax=max(tsmax,t0_component[2]);
		temin=min(ts[0].endtime(),ts[1].endtime());
		temin=min(temin,ts[2].endtime());
		temax=max(ts[0].endtime(),ts[1].endtime());
		temax=max(temax,ts[2].endtime());
		ns=nint((temax-tsmin)/dt);
		this->u=dmatrix(3,ns);
		this->u.zero();
		if(fabs((tsmax-tsmin)/dt)>1.0) 
			this->add_gap(TimeWindow(tsmin,tsmax));
		if(fabs((temax-temin)/dt)>1.0)
			this->add_gap(TimeWindow(temin,temax));
		// Now load the data.  Use the time and sample number methods
		// to simplify process
		double t;
		for(int ic=0;ic<3;++ic)
		{
			for(j=0;j<ts[ic].ns;++j)
			{
				t=ts[ic].time(j);
				i=this->sample_number(t);
				// silently do nothing if outside bounds.  This
				// perhaps should be an error as it shouldn't really
				// happen with the above algorithm, but safety is good
				if( (i>0) && (i<ns) ) this->u(0,i)=ts[ic].s[j];
			}
		}
	}
	// gaps are the union of all gaps in each component.  There may be
	// a faster way to do this manipulating the STL set object, but this
	// is independent of those details and should always work even if it
	// is slower.  This is needed because the gaps container is keyed
	// by time intervals.  If two gaps overlap partially, we can get
	// incorrect results if we don't handle this through this more
	// complicated algorithm.  Note we skip all this if there
	// are no gaps defined in any of the components.
	//
	if(ts[0].has_gap() || ts[1].has_gap() || ts[2].has_gap() )
	{
	//Caution.  There is evidence this code section may not 
	// be working.  Needs a more rigorous test.  Feb. 11, 2008
	int istart=0,iend=0;
	bool in_a_gap=false;
	for(i=0;i<this->ns;++i)
	{
		// Skip over gaps already defined
		if( this->is_gap(i) )
		{
			do {
				++i;
			} while( (i<ns) && this->is_gap(i));
			if(i==(ns-1)) 
				break;
			// Need to force this
			in_a_gap=false;
		}
		if(in_a_gap)
		{
			// terminate scan if we hit a gap or the end of the data
			if(this->is_gap(i))
			{
				iend=i-1;
				this->add_gap(TimeWindow(this->time(istart),
					this->time(iend)));
				if(i>=(ns-1))
					break;
				else
					continue;
			}
			// skip forward if any of the ts components shows a gap
			if(ts[0].is_gap(this->time(i))) continue;
			if(ts[1].is_gap(this->time(i))) continue;
			if(ts[2].is_gap(this->time(i))) continue;
			in_a_gap=false;
			iend=i-1;
			this->add_gap(TimeWindow(this->time(istart),
					this->time(iend)));
		}
		else
		{
			// This will terminate scan if there is a gap
			// at the end of the trace already marked
			if(this->is_gap(i)) continue;
			// 
			// Mark the start of a new gap if any of the
			// input traces have a gap marked 
			//
			for(j=0;j<3;++j)
			{
				if(ts[j].is_gap(this->time(i)))
				{
					in_a_gap=true;
					istart=i;
					break;
				}
			}
		}
	}
	}  // End conditional for gap processing
	
	//
	// In any case before we quit we have to set the transformation 
	// matrix.  This is a direct application of conversion of routines
	// in spherical coordinate procedures.  They are procedural 
	// routines, not objects so the code is procedural.
	//
	SphericalCoordinate scor;
	double *nu;
	// convert all the hang values to spherical coordinate phi
	// (angle from postive east) from input assumed in degrees
	// azimuth from north.  At the same time convert vang to radians.
	for(i=0;i<3;++i)
	{
		hang[i]=rad(90.0-hang[i]);
		vang[i]=rad(vang[i]);
	}
	for(i=0;i<3;++i)
	{
		scor.phi=hang[i];
		scor.theta=vang[i];
		nu=SphericalToUnitVector(scor);
		for(j=0;j<3;++j)tmatrix[i][j]=nu[j];
		delete [] nu;
	}
        components_are_cardinal = tmatrix_is_cardinal(*this);
        if(components_are_cardinal) 
		components_are_orthogonal=true;
	else
		components_are_orthogonal=false;
}
// Note on usage in this group of functions.  The rotation algorithms used here
// all key on the BLAS for speed.  That is, a transformation matrix could be done
// by using the * operator between matrix objects.  

void ThreeComponentSeismogram::rotate_to_standard()
	throw(SeisppError)
{
	if( (ns<=0) || !live) return; // do nothing in these situations
	double *work[3];
	int i,j;
	if(components_are_cardinal) return;
	for(j=0;j<3;++j) work[j]=new double[ns];
	if(components_are_orthogonal)
	{
		//
		//Use a daxpy algorithm.  tmatrix stores the
		//forward transformation used to get current
		//Use the transpose to get back
		//
		for(i=0;i<3;++i)
		{
			// x has a stride of 3 because we store in fortran order in x
			dcopy(ns,u.get_address(0,0),3,work[i],1);
			dscal(ns,tmatrix[0][i],work[i],1);
			daxpy(ns,tmatrix[1][i],u.get_address(1,0),3,work[i],1);
			daxpy(ns,tmatrix[2][i],u.get_address(2,0),3,work[i],1);
		}
		for(i=0;i<3;++i) dcopy(ns,work[i],1,u.get_address(i,0),3);
	}
	else
	{
		//
		//Enter here only when the transformation matrix is
		//not orthogonal.  We have to construct a fortran 
		//order matrix a to use LINPACK routine in sunperf/perf
		//This could be done with the matrix template library 
		//but the overhead ain't worth it
		//
		double a[9];
		int ipivot[3];
		int info;
		double det;
		int asize=3;
		double awork[10];
		int ldwork=10;
		a[0] = tmatrix[0][0];
		a[1] = tmatrix[1][0];
		a[2] = tmatrix[2][0];
		a[3] = tmatrix[0][1];
		a[4] = tmatrix[1][1];
		a[5] = tmatrix[2][1];
		a[6] = tmatrix[0][2];
		a[7] = tmatrix[1][2];
		a[8] = tmatrix[2][2];
		//Perf lib matrix inversion routine using LU factorizatoin
		// Note this is changed from parent code.  Untested.
		dgetrf_(&asize,&asize,a,&asize,ipivot,&info);
		if(info!=0) throw(SeisppError(
			string("rotate_to_standard:  LU factorization of transformation matrix failed")));
		dgetri_(&asize,a,&asize,ipivot,awork,&ldwork,&info);
		if(info!=0) throw(SeisppError(
			string("rotate_to_standard:  LU factorization inversion of transformation matrix failed")));
		
		tmatrix[0][0] = a[0];
		tmatrix[1][0] = a[1];
		tmatrix[2][0] = a[2];
		tmatrix[0][1] = a[3];
		tmatrix[1][1] = a[4];
		tmatrix[2][1] = a[5];
		tmatrix[0][2] = a[6];
		tmatrix[1][2] = a[7];
		tmatrix[2][2] = a[8];
                /* The inverse is now in tmatrix so we reverse the 
                   rows and columms from above loop */
 
		for(i=0;i<3;++i)
		{
			dcopy(ns,u.get_address(0,0),3,work[i],1);
			dscal(ns,tmatrix[i][0],work[i],1);
			daxpy(ns,tmatrix[i][1],u.get_address(1,0),3,work[i],1);
			daxpy(ns,tmatrix[i][2],u.get_address(2,0),3,work[i],1);
		}
		for(i=0;i<3;++i) dcopy(ns,work[i],1,u.get_address(i,0),3);
		components_are_orthogonal = true;
	}
	//
	//Have to set the transformation matrix to an identity now
	//
	for(i=0;i<3;++i)
		for(j=0;j<3;++j)
			if(i==j)
				tmatrix[i][i]=1.0;
			else
				tmatrix[i][j]=0.0;

	components_are_cardinal=true;
	for(i=0;i<3;++i) delete [] work[i];
}


/* This routine takes a spherical coordinate vector that defines
a given direction in space and returns a transformation matrix that
should be viewed as a transformation to ray coordinates under an 
assumption that this vector points in the direction of P wave 
particle motion.  If the theta angle is greater than PI/2 it 
switches the azimuth by 180 degrees so that the direction of the
transformed x1 axis will be pointing upward in space.  This removes
ambiguities in the transformation that make it easier to sort out
handedness of the transformation.  

The transformation produced for a P wave will be true ray coordinates
with X1 = transverse, X2 = radial, and X3 = longitudinal.  
The best way to understand the transformation is as a pair of 
rotations:  (1) rotate North to radial about z, (2) rotate z to
transverse around X1 (transverse).  Note this leaves X1 (transverse)
always as a purely horizontal direction.  It should also work for a 
principal component direction determined for an S phase, but the 
appropriate the only component that will make any sense after the
transformation, in that case, is the X3 direction = direction of 
inferred peak particle motion.  

One special case has to be dealt with.  If the direction passed into
the program is purely vertical (up or down), the function can only 
return an identity matrix because there is no way to determine a 
horizontal rotation direction.  

Arguments:
	xsc - spherical coordinate structure defining unit vector used
		to define the transform (radius is ignored).  Angles
		are assumed in radians.

Author:  Gary L. Pavlis
Written:  Sept. 1999
Modified:  Feb 2003
Original was plain C.  Adapted to C++ for seismic processing
*/
void ThreeComponentSeismogram::rotate(SphericalCoordinate xsc)
{
	if( (ns<=0) || !live) return; // do nothing in these situations
	int i;
	double theta, phi;  /* corrected angles after dealing with signs */
	double a,b,c,d;

	//
	//Undo any previous transformations
	//
	this->rotate_to_standard();
       	if(xsc.theta == M_PI) 
	{
		//This will be left handed
		tmatrix[2][2] = -1.0;
		return;
	}

	if(xsc.theta < 0.0) 
	{
		theta = -(xsc.theta);
		phi = xsc.phi + M_PI;
		if(phi > M_PI) phi -= (2.0*M_PI);
	}
	else if(xsc.theta > M_PI_2)
	{
		theta = xsc.theta - M_PI_2;
		phi = xsc.phi + M_PI;
		if(phi > M_PI) phi -= (2.0*M_PI);
	}
	else
	{
		theta = xsc.theta;
		phi = xsc.phi;
	}
        /* Am using a formula here for azimuth with is pi/2 - phi*/
        double azimuth=M_PI_2-phi;  
        a = cos(azimuth);
        b = sin(azimuth);
        c = cos(theta);
        d = sin(theta);

	tmatrix[0][0] = a;
	tmatrix[1][0] = b*c;
	tmatrix[2][0] = b*d;
	tmatrix[0][1] = -b;
	tmatrix[1][1] = a*c;
	tmatrix[2][1] = a*d;
	tmatrix[0][2] = 0.0;
	tmatrix[1][2] = -d;
	tmatrix[2][2] = c;

        /* Now multiply the data by this transformation matrix.  */
	double *work[3];
	for(i=0;i<3;++i)work[i] = new double[ns];
	for(i=0;i<3;++i)
	{
		dcopy(ns,u.get_address(0,0),3,work[i],1);
		dscal(ns,tmatrix[i][0],work[i],1);
		daxpy(ns,tmatrix[i][1],u.get_address(1,0),3,work[i],1);
		daxpy(ns,tmatrix[i][2],u.get_address(2,0),3,work[i],1);
	}
	for(i=0;i<3;++i) dcopy(ns,work[i],1,u.get_address(i,0),3);
	components_are_cardinal=false;
	for(i=0;i<3;++i) delete [] work[i];
}
void ThreeComponentSeismogram::rotate(double nu[3])
{
	if( (ns<=0) || !live) return; // do nothing in these situations
	SphericalCoordinate xsc=UnitVectorToSpherical(nu);
	this->rotate(xsc);
}
void ThreeComponentSeismogram::apply_transformation_matrix(double a[3][3])
{
	if( (ns<=0) || !live) return; // do nothing in these situations
	int i,j,k;
	double *work[3];
	for(i=0;i<3;++i) work[i] = new double[ns];
	double twork[3];
	for(i=0;i<3;++i)
	{
		dcopy(ns,u.get_address(0,0),3,work[i],1);
		dscal(ns,a[i][0],work[i],1);
		daxpy(ns,a[i][1],u.get_address(1,0),3,work[i],1);
		daxpy(ns,a[i][2],u.get_address(2,0),3,work[i],1);
	}
	for(i=0;i<3;++i) dcopy(ns,work[i],1,u.get_address(i,0),3);
	 for(i=0;i<3;++i) delete [] work[i];
         /* Hand code this rather than use dmatrix or other library.
            Probably dumb, but this is just a 3x3 system.  This 
            is simply a multiply of a*tmatrix with result replacing
            the internal tmatrix */
         double tmnew[3][3];
         double prod;
         for(i=0;i<3;++i)
             for(j=0;j<3;++j)
             {
                 for(prod=0.0,k=0;k<3;++k)
                     prod+=a[i][k]*tmatrix[k][j];
                 tmnew[i][j]=prod;
             }
         for(i=0;i<3;++i)
             for(j=0;j<3;++j)tmatrix[i][j]=tmnew[i][j];
	components_are_cardinal = false;
        /* Rather than test to see if input matrix was orthogonal
           and carry along such baggage will just set this false 
           so calling rotate_to_standard involves a small matrix
           inversion.  Assumed less costly than the baggage required
           to sort this out here. */
	components_are_orthogonal = false;
}
/* This function computes and applies the free surface tranformaton
matrix described by Kennett 1991.  The result is a ray coordinate
transformation with x1=transverse, x2=radial, and x3=longitudinal.
Note this transformation is into a nonorthogonal system.  

Algorithm first applies a rotation of horizontal coordinates to 
horizonal radial and transverse, then applies free surface 
transformation to the radial-vertical plane.

The free surface transformation code segment is a direct 
translation of m file from Michael Bostock.  

Author:  Gary Pavlis
*/
void ThreeComponentSeismogram::free_surface_transformation(SlownessVector uvec,
		 double a0, double b0) 
{
	if( (ns<=0) || !live) return; // do nothing in these situations
	double a02,b02,pslow,p2;
	double qa,qb,vpz,vpr,vsr,vsz;
	double umag=uvec.mag();
	pslow=uvec.mag();
	// silently do nothing if magnitude of the slowness vector is 0
	// (vertical incidence)
	if(pslow<DBL_EPSILON) return;
	// Can't handle evanescent waves with this operator
	double vapparent=1.0/pslow;
	if(vapparent<a0 || vapparent<b0)
		throw SeisppError(string("free_surface_transformation:  ")
		+string("cannot handle evanescent waves.\n")
		+string("Surface velocities must be smaller than apparent velocity"));

	// First the horizonal rotation
	SphericalCoordinate scor;
	//rotation angle is - azimuth to put x2 (north in standard coord) 
	//in radial direction
	scor.phi=atan2(uvec.uy,uvec.ux);
	scor.theta=0.0;
	scor.radius=1.0;
	// after this transformation x1=transverse horizontal
	// x2=radial horizonal, and x3 is still vertical
	this->rotate(scor);

	a02=a0*a0;
	b02=b0*b0;
	p2=pslow*pslow;
	qa=sqrt((1.0/a02)-p2);
	qb=sqrt((1.0/b02)-p2);
	vpz=-(1.0-2.0*b02*p2)/(2.0*a0*qa);
	vpr=pslow*b02/a0;
	vsr=(1.0-2.0*b02*p2)/(2.0*b0*qb);
	vsz=pslow*b0;
	/* Now construct the transformation matrix
	 This is different from Bostock's original code
	 in sign and order.  Also note this transformation
         is not scaled to have a unit matrix norm so amplitudes
         after the transformation are distorted.  rotate_to_standard,
         however, should still restore original data within roundoff
         error if called on the result. */
	double fstran[3][3];
	fstran[0][0]=0.5;  fstran[0][1]=0.0;  fstran[0][2]=0.0;
	fstran[1][0]=0.0;  fstran[1][1]=vsr;  fstran[1][2]=vpr;
	fstran[2][0]=0.0;  fstran[2][1]=vsz;  fstran[2][2]=vpz;
	this->apply_transformation_matrix(fstran);

	components_are_cardinal=false;
	components_are_orthogonal=false;
}
//
//  Implementation of virtual function in BasicTimeSeries for a 3c
//  seismogram object.  Works cautiously and won't fail if a gap is
//  marked outside the actual range of data.  This can happen when
//  object is derived by chopping up a longer data segment and cloning
//  the gaps definitions.  
//
void ThreeComponentSeismogram::zero_gaps()
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
		for(i=0;i<3;++i)
			for(int j=istart;j<=iend;++j)
				u(i,j)=0.0;
	}
}
ThreeComponentSeismogram& ThreeComponentSeismogram::operator
		= (const ThreeComponentSeismogram& seisin)
{
	if(this!=&seisin)
	{
		mreal=seisin.mreal;
		mint=seisin.mint;
		mbool=seisin.mbool;
		mstring=seisin.mstring;
		live=seisin.live;
		dt=seisin.dt;
		t0=seisin.t0;
		ns=seisin.ns;
		tref=seisin.tref;
		components_are_orthogonal=seisin.components_are_orthogonal;
		components_are_cardinal=seisin.components_are_cardinal;
		for(int i=0;i<3;++i)
		{
			for(int j=0;j<3;++j)
			{
				tmatrix[i][j]=seisin.tmatrix[i][j];
			}
		}
		u=seisin.u;
		gaps=seisin.gaps;
	}
	return(*this);
}
double *ThreeComponentSeismogram::operator[](int i)
{
	if(!live) 
		throw SeisppError(string("ThreeComponentSeismogram operator[]: attempt to access data matrix marked as dead"));
	//
	// This one does not need to check bounds because in
	// this implemenation u is a dmatrix with range checking
	// Instead we have a try with a catch all block
	// 
	try {
		double *result;
		result=u.get_address(0,i);
		return(result);
	}
	catch (dmatrix_index_error& derr)
	{
		derr.log_error();
		throw SeisppError(string("ThreeComponentSeismogram::operator[]: index error"));
	}
}
/* this seems necessary for g++ even though it does nothing */
ThreeComponentSeismogram::~ThreeComponentSeismogram()
{
}
} // end namespace SEISPP 
