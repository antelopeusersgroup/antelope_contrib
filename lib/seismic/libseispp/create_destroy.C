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
// simple constructors for the Time_Series object are defined inline
// in seispp.h.  
//
Time_Series::Time_Series() : Basic_Time_Series(), Metadata()
{
	s.reserve(0);
}
Time_Series::Time_Series(int nsin) : Basic_Time_Series(), Metadata()
{
	s.reserve(nsin);
}
	
Time_Series::Time_Series(const Time_Series& tsi) : 
		Basic_Time_Series(dynamic_cast<const Basic_Time_Series&>(tsi)),
		Metadata(dynamic_cast<const Metadata&>(tsi))
{
	if(live)
	{
		s.reserve(ns);
		s=tsi.s;
	}
}
//
// This version uses the copy constructor from a Metadata object
// Wfdisc related fields are then extracted from the Metadata object
// and used to read actual data.  
// Note that this originally used a Pf as the input parameter but
// I changed it to a Metadata object instead.  To create a Time_Series
// directly from a Pf you can just do this:
//  md = Metadata(pf);
//  ts = Time_Series(md);  -- i.e. this constructor
//

Time_Series::Time_Series(Metadata& md,bool load_data) : Metadata(md)
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
			stref = this->get_string("Time_Reference_Type");
			if(stref == "relative")
				tref = relative;
			else
				tref = absolute;
			dtype = this->get_string("datatype");
#ifdef BIGENDIAN
			if(dtype!="t4") 
				throw(seispp_error("Unsupported datatype:  metadata-driven constructor only supports t4 data with external files"));
#else
			if(dtype!="u4") 
				throw(seispp_error("Unsupported datatype:  metadata-driven constructor only supports t4 data with external files"));
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
				throw(seispp_error("Time_Series constructor:  fread error on file "+fname));
			}
			for(int i=0;i<ns;++i) 
				s.push_back(static_cast<double>(inbuffer[i]));
			delete [] inbuffer;
			live = true;
			fclose(fp);
		}
	}
	catch (Metadata_error mderr)
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

Time_Series::Time_Series(Database_Handle& rdb,
		Metadata_list& md_to_extract, 
			Attribute_Map& am) 
	: Metadata(rdb,md_to_extract,am)
{
	float *inbuffer=NULL;
	Datascope_Handle& dbh=dynamic_cast<Datascope_Handle&>(rdb); 
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
				throw seispp_dberror("Time_Series database constructor:  trgetwf error",dbh.db);
		// allow one sample deviation
		if(abs(nread-ns)>1)
		{
			cerr << "Data read mismatch on row "
				<< dbh.db.record 
				<< " of input database" << endl
				<< "Expected to read "
				<< ns 
				<< " data points but read "
				<< nread << endl;
			ns = nread;
			t0 = t0read;
			this->put_metadata("endtime",teread);
		}
		s.reserve(ns);
		for(int i=0;i<this->ns;++i) 
			s.push_back(static_cast<double>(inbuffer[i]));
		// trgetwf is a C function so we need to use free to 
		// release the space it allocated.
		live = true;
		free(inbuffer);
	}
	catch (Metadata_error mderr)
	{
		// Land here when any of the metadata routines fail
		mderr.log_error();
		throw seispp_dberror("Constructor for Time_Series object failed from a Metadata error",
			dbh.db);

	}
}
// Default constructor for Three_Component_Seismogram could be 
// done inline in seispp.h, but it is complication enough I put
// it here
//
Three_Component_Seismogram::Three_Component_Seismogram() : Metadata(),u(1,1)
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
Three_Component_Seismogram::Three_Component_Seismogram(int nsamples) 
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
Three_Component_Seismogram::Three_Component_Seismogram(Metadata& md,
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
			stref = this->get_string("Time_Reference_Type");
			if(stref == "relative")
				tref = relative;
			else
				tref = absolute;
			dtype = this->get_string("datatype");
			if(dtype!="t8") 
				throw(seispp_error("Unsupported datatype:  metadata-driven constructor only supports t8 data with external files"));
			
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
				throw(seispp_error("Three_Component_Seismogram constructor:  fread error on file "+fname));
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
	catch (Metadata_error mderr)
	{
		// Land here when any of the metadata routines fail
		mderr.log_error();
		throw seispp_error("Constructor for Three_Component_Seismogram object failed");

	}
}

Three_Component_Seismogram::Three_Component_Seismogram
			(const Three_Component_Seismogram& t3c):
	 Basic_Time_Series(dynamic_cast<const Basic_Time_Series&>(t3c)),
	 Metadata(dynamic_cast<const Metadata&>(t3c)),
					u(t3c.u)
{
	int i,j;
        components_are_orthogonal=t3c.components_are_orthogonal;
        components_are_cardinal=t3c.components_are_cardinal;
	for(i=0;i<3;++i)
		for(j=0;j<3;++j) tmatrix[i][j]=t3c.tmatrix[i][j];
}
/* Constructor to read a three-componet seismogram from an antelope
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
*/

Three_Component_Seismogram::Three_Component_Seismogram(
	Database_Handle& rdb,
		Metadata_list& md_to_extract, 
			Attribute_Map& am) : Metadata(),u()
{
	Time_Series component[3];
	const string this_function_base_message
		= "Three_Component_Seismogram event database constructor:";
	int i,j,ierr;
	Datascope_Handle& dbh=dynamic_cast<Datascope_Handle&>(rdb);
	live = false;
	components_are_cardinal=false;
	components_are_orthogonal=false;
	try{
		double tsread[3],teread[3];
		double hang[3],vang[3];
		double samprate[3];
		int nread[3],nsamp[3];

		DBBundle bundle = dbh.get_range();
		if((bundle.end_record-bundle.start_record)!=3)
			throw(seispp_dberror(
			  string(this_function_base_message
			  +	"  database bundle pointer irregularity\n"
			  +     "All data must have exactly 3 channels per event"),
			  dbh.db,complain));
		// Use the simplified copy constructor 
		// Have to assume the parent is not a bundle pointer
		Datascope_Handle dbhv(bundle.parent,bundle.parent,false);
		// This weird cast is necessary because the
		// the Time_Series constructor uses a generic handle
		Database_Handle *rdbhv=dynamic_cast
				<Database_Handle*>(&dbhv);
		for(i=0,dbhv.db.record=bundle.start_record;
			dbhv.db.record<bundle.end_record;
				++i,++dbhv.db.record)
		{
		    try
		    {
			component[i]=Time_Series(*rdbhv,
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
			throw seispp_dberror(
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
			this->put_metadata("starttime",tsmin);
		}
		if(te_md!=temax)
		{
			te_md=temax;
			this->put_metadata("endtime",temax);
		}
		// Don't mark a gap unless the irregularity is
		// greater than one sample
		if(tsmax-t0>dt)
			this->add_gap(Time_Window(t0,tsmax));
		if(te_md-temin>dt)
			this->add_gap(Time_Window(temin,te_md));
		ns = nint((te_md-t0)/dt) + 1;
		u=dmatrix(3,ns);
		for(i=0;i<3;++i)
			for(j=nint((tsread[i]-t0)/dt);j<nread[i],j<ns;++j)
				u(i,j)=component[i].s[j];
		/* Scan for gaps in any component.  This is a general 
		algorithm that is thorough but not fast.  We call the 
		is_gap function for each component and mark the whole
		object with a gap if there is a gap on any component */
		Time_Window tw;
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
		Spherical_Coordinate scor;
		for(i=0;i<3;++i)
		{
			double *nu;
			scor.phi = M_PI_2 - rad(hang[i]);
			scor.theta = rad(vang[i]);
			nu=spherical_to_unit_vector(scor);
			for(j=0;j<3;++j)tmatrix[i][j]=nu[j];
			delete [] nu;
		}
		// last thing we do is mark this live
		live = true;
	}
	catch (Metadata_error mderr)
	{
		// Land here when any of the metadata routines fail
		mderr.log_error();
		throw seispp_dberror("Constructor for Three_Component_Seismogram object failed from a metadata error",
			dbh.db,complain);

	}
}

//
// Ensemble constructors.  Both just create blank trace or 3c trace objects
// and push them into a vector container.
//
Time_Series_Ensemble::Time_Series_Ensemble()
{
	tse.reserve(0);
}
Time_Series_Ensemble::Time_Series_Ensemble(int nensemble, int nsamples)
{
	for(int i=0; i<nensemble; ++i)
	{
		Time_Series *ts = new Time_Series(nsamples);
		tse.push_back(*ts);
		delete ts;
	}
}
Time_Series_Ensemble::Time_Series_Ensemble(int nensemble, 
	int nsamples,
		Metadata_list& mdl)
{
	for(int i=0; i<nensemble; ++i)
	{
		Time_Series *ts = new Time_Series(nsamples);
		tse.push_back(*ts);
		delete ts;
	}
	mdlist=mdl;
}
void set_global_metadata_list(Time_Series_Ensemble& tse, Metadata_list& mdl)
{
	tse.mdlist=mdl;
}
Three_Component_Ensemble::Three_Component_Ensemble()
{
	tcse.reserve(0);
}
Three_Component_Ensemble::Three_Component_Ensemble(int nstations, int nsamples)
{
	tcse.reserve(nstations);
	for(int i=0;i<nstations;++i)
	{
		Three_Component_Seismogram *tcs 
			= new Three_Component_Seismogram(nsamples);
		tcse.push_back(*tcs);
		delete tcs;
	}
}
Three_Component_Ensemble::Three_Component_Ensemble(int nstations, 
		int nsamples,
			Metadata_list& mdl)
{
	tcse.reserve(nstations);
	for(int i=0;i<nstations;++i)
	{
		Three_Component_Seismogram *tcs 
			= new Three_Component_Seismogram(nsamples);
		tcse.push_back(*tcs);
		delete tcs;
	}
	mdlist=mdl;
}
void set_global_metadata_list(Three_Component_Ensemble& tse, Metadata_list& mdl)
{
	tse.mdlist = mdl;
}
/* Database-driven constructor for an ensemble.  This implementation uses
a Datascope database only through an immediate dynamic_cast to a 
Datascope_Handle, but the idea is that a more generic interface 
would change nothing in the calling sequence.

station_mdl and ensemble_mdl determine which metadata are extracted 
from the database for individual stations in the ensemble and 
as the global metadata for the ensemble respectively.   The 
station metadata is derived from the Three_Component_Seismogram 
constructor, but the global metadata is arbitrarily extracted from
the first row of the view forming the requested ensemble.  This 
tacitly assumes then that all rows in view that forms this ensemble
have the same attributes (i.e. these are common attributes obtained
through some form of relational join.)  

The routine will pass along any exceptions thrown by functions 
called by the constructor.  It will also throw a seispp_dberror 
in cases best seen by inspecting the code below.

Author:  Gary L. Pavlis
Written:  July 2004
*/
Three_Component_Ensemble::Three_Component_Ensemble(Database_Handle& rdb,
	Metadata_list& station_mdl,
        	Metadata_list& ensemble_mdl,
        		Attribute_Map& am)
{
	int i;
	int nsta;
	const string this_function_base_message("Three_Component_Ensemble database constructor");
	Three_Component_Seismogram *data3c;
	Datascope_Handle& dbh=dynamic_cast<Datascope_Handle&>(rdb);
	try {
		// Will throw an exception if this isn't a group pointer
		DBBundle ensemble_bundle=dbh.get_range();
		nsta = ensemble_bundle.end_record-ensemble_bundle.start_record;
//DEBUG
cerr << "Ensemble has data for " << nsta << " stations"<<endl;
		// We need a copy of this bundle pointer 
		Datascope_Handle dbhv(ensemble_bundle.parent,
			ensemble_bundle.parent,true);
		// Necessary because the Three_Component_Seismogram
		// constructor uses a generic handle
		Database_Handle *rdbhv=dynamic_cast
			<Database_Handle *>(&dbhv);
		// Loop over stations
		for(i=0,dbhv.db.record=ensemble_bundle.start_record;
			dbhv.db.record<ensemble_bundle.end_record;
			++i,++dbhv.db.record)
		{
			try {
				data3c = new Three_Component_Seismogram(*rdbhv,
					station_mdl,am);
			} catch (seispp_dberror dberr)
			{
				cerr << "Problem with station "
					<< i 
					<< " in ensemble construction" << endl;
				dberr.log_error();
				cerr << "Data for this station skipped" << endl;
				continue;
			}
			tcse.push_back(*data3c);
			delete data3c;
			// copy global metadata only for the first 
			// row in this view
			if(i==0)
			{
				DBBundle sta_bundle=dbhv.get_range();
				Datascope_Handle dbhvsta(sta_bundle.parent,
						sta_bundle.parent,false);
				dbhvsta.db.record=sta_bundle.start_record;
				Metadata ens_md(dynamic_cast<Database_Handle&>(dbhvsta),
					ensemble_mdl,am);
				copy_selected_metadata(ens_md,
					dynamic_cast<Metadata&>(*this),
					ensemble_mdl);
			}
		}

	} catch (...) { throw;};
}

} // Termination of namespace SEISPP definitions
