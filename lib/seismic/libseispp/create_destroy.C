#include "tr.h" // Antelope trace library
#include "seispp.h"
namespace SEISPP
{
//
// simple constructors for the Time_Series object are defined inline
// in seispp.h.  First the copy constructors
//
Time_Series::Time_Series()
{
	live=false;
	s.reserve(0);
	dt=0.0;
	t0=0.0;
	ns=0;
	tref=absolute;
}
Time_Series::Time_Series(int nsin)
{
	live=false;
	s.reserve(nsin);
	dt=0.0;
	t0=0.0;
	ns=0;
	tref=absolute;
}
	
Time_Series::Time_Series(const Time_Series& tsi)
{
	int i;
	live=tsi.live;
        dt=tsi.dt;
        t0=tsi.t0;
        ns=tsi.ns;
        tref=tsi.tref;
        md= Metadata(tsi.md);
	if(live)
	{
		s.reserve(ns);
		s=tsi.s;
	}
}
Time_Series::Time_Series(const Time_Series *tsi)
{
	int i;

	live=tsi->live;
        t0=tsi->t0;
        ns=tsi->ns;
        dt=tsi->dt;
        tref=tsi->tref;
        md= Metadata(tsi->md);
	if(live)
	{
		s.reserve(ns);
		s=tsi->s;
	}
}
//
// This much more complex example uses a Pf to build a time series
// It does this by using external files parsed from dir and dfile
//

Time_Series::Time_Series(Pf *pf)
{
	char *pfstr;
	string stref;
	string dfile, dir;
	int foff;
	FILE *fp;
	string dtype;
	double *inbuffer;

	pfstr = pf2string(pf);
	try {
		md=Metadata(pfstr);
		free(pfstr);
		// Names space is frozen.  Not as general as it
		// probably should be, but until proven necessary 
		// will do it the easy way
		dt = 1.0/md.get_double("samprate");
		t0 = md.get_double("starttime");
	        ns = md.get_int("nsamp");
		stref = md.get_string("Time_Reference_Type");
		if(stref == "relative")
			tref = relative;
		else
			tref = absolute;
		dtype = md.get_string("datatype");
		if(dtype!="t8") 
			throw(seispp_error("Unsupported datatype:  pf constructor only supports t8 data with external files"));
		dir = md.get_string("dir");
		dfile = md.get_string("dfile");
		foff = md.get_int("foff");
		string fname=dir+"/"+dfile;
		if((fp=fopen(fname.c_str(),"r")) == NULL) 
			throw("Open failure for file "+fname);
		if (foff>0)fseek(fp,(long)foff,SEEK_SET);
		inbuffer = new double[ns];
		if(fread((void *)(inbuffer),sizeof(double),ns,fp)
				!= ns ) 
		{
			delete [] inbuffer;  // memory leak possible without this
			throw(seispp_error("fread error on file "+fname));
		}
		s.reserve(ns);
		for(int i=0;i<ns;++i) s[i]=inbuffer[i];
		delete [] inbuffer;
		fclose(fp);
	}
	catch (Metadata_error mderr)
	{
		// Land here when any of the metadata routines fail
		mderr.log_error();
		throw seispp_error("Constructor for Time_Series object failed");

	}
}
Time_Series::Time_Series(Dbptr db,
		Metadata_list& md_to_extract, 
			Attribute_map& am)
{
	float *inbuffer=NULL;
	try{
		double te,t0read,teread;
		int nread;

		md=Metadata(db,md_to_extract,am);
		dt = 1.0/md.get_double("samprate");
		t0 = md.get_double("starttime");
		te = md.get_double("endtime");
		ns = md.get_int("nsamp");
		tref = absolute;  // perhaps to dogmatic
		/* This will create a memory leak if trgetwf fails 
		// trgetwf returns an error for multiple conditions and
		// some are more fatal than others.  For most applications
		// I can imagine any error in trgetwf is serious and probably
		// would normally lead to an exit.
		//
		// Problem is that I can't just test for a NULL pointer 
		// and expect it is safe to free inbuffer before exit
		*/
		if(trgetwf(db,0,&inbuffer,NULL,t0,te,
					&t0read,&teread,&nread,
					0,0))
				throw seispp_error("Time_Series database constructor:  trgetwf error");
		if(nread!=ns)
		{
			cerr << "Data read mismatch on row "
				<< db.record 
				<< " of input database" << endl
				<< "Expected to read "
				<< ns 
				<< " data points but read "
				<< nread << endl;
			ns = nread;
			t0 = t0read;
			md.put_metadata("endtime",teread);
		}
		s.reserve(ns);
		for(int i=0;i<ns;++i) s[i]=inbuffer[i];
		// trgetwf is a C function so we need to use free to 
		// release the space it allocated.
		free(inbuffer);
	}
	catch (Metadata_error mderr)
	{
		// Land here when any of the metadata routines fail
		mderr.log_error();
		throw seispp_error("Constructor for Time_Series object failed");

	}
}

	

// Default constructor for Three_Component_Seismogram could be 
// done inline in seispp.h, but it is complication enough I put
// it here
//
Three_Component_Seismogram::Three_Component_Seismogram()
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

//
// here is the parameterized constructor for a 3c seismogram
//
Three_Component_Seismogram::Three_Component_Seismogram(int nsin) 
{
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
	for(int i=0; i<3;++i) 
		x[i].s.reserve(ns);
	
}
Three_Component_Seismogram::Three_Component_Seismogram
			(const Three_Component_Seismogram& t3c)
{
	int i,j;
	md=Metadata(t3c.md);
        dt=t3c.dt;
        t0=t3c.t0;
        ns=t3c.ns;
        tref=t3c.tref;
        components_are_orthogonal=t3c.components_are_orthogonal;
        components_are_cardinal=t3c.components_are_cardinal;
	for(i=0;i<3;++i)
		for(j=0;j<3;++j) tmatrix[i][j]=t3c.tmatrix[i][j];
	for(i=0;i<3;++i) x[i]=Time_Series(t3c.x[i]);
}
//No destructor is needed for current Three_Component_Seismogram object
// because the Time_Series destructor and default deletions should
// handle it fine.  

//
// Ensemble constructors.  Both just create blank trace or 3c trace objects
// and push them into a vector container.
//
Time_Series_Ensemble::Time_Series_Ensemble()
{
	mdlist=newtbl(0);
}
Time_Series_Ensemble::Time_Series_Ensemble(int nensemble, int nsamples)
{
	for(int i=0; i<nensemble; ++i)
	{
		Time_Series *ts = new Time_Series(nsamples);
		tse.push_back(*ts);
	}
	mdlist=newtbl(0);
}
Three_Component_Ensemble::Three_Component_Ensemble()
{
	mdlist=newtbl(0);
}
Three_Component_Ensemble::Three_Component_Ensemble(int nstations, int nsamples)
{
	for(int i=0;i<nstations;++i)
	{
		Three_Component_Seismogram *tcs 
			= new Three_Component_Seismogram(nsamples);
		tcse.push_back(*tcs);
	}
	mdlist=newtbl(0);
}
} // Termination of namespace SEISPP definitions
