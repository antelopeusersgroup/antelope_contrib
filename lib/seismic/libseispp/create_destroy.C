#include "seispp.h"
//
// simple constructors for the Time_Series object are defined inline
// in seispp.h.  First the copy constructors
//
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
	        s=new double[ns];
	        for(i=0;i<tsi.ns;++i) s[i]=tsi.s[i];
	}
	else
		s=NULL;
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
        	s=new double[ns];
        	for(i=0;i<tsi->ns;++i) s[i]=tsi->s[i];
	}
	else
		s=NULL;
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

	s=NULL;  // explictly necessary to avoid problems when an error is thrown
	pfstr = pf2string(pf);
	try {
		md.load_metadata((string)pfstr);
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
		if(dtype!="t4") 
			throw(seispp_error("Unsupported datatype:  pf constructor only supports t4 data with external files"));
		dir = md.get_string("dir");
		dfile = md.get_string("dfile");
		foff = md.get_int("foff");
		string fname=dir+"/"+dfile;
		if((fp=fopen(fname.c_str(),"r")) == NULL) 
			throw("Open failure for file "+fname);
		if (foff>0)fseek(fp,(long)foff,SEEK_SET);
		s = new double[ns];
		if(fread((void *)(s),sizeof(double),ns,fp)
				!= ns ) 
		{
			delete [] s;  // memory leak possible without this
			throw(seispp_error("fread error on file "+fname));
		}
		fclose(fp);
	}
	catch (Metadata_error mderr)
	{
		// We can land here if the object is only partially constructed
		// A NULL value of s is handled correctly in the destructor
		// so we clear s before returning with anexception
		//
		if(s!=NULL) delete [] s;
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
		x[i].s = new double(nsin);
	
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
	for(i=0;i<3;++i) x[i]=Time_Series((t3c.x)+i);
}
//No destructor is needed for current Three_Component_Seismogram object
// because the Time_Series destructor and default deletions should
// handle it fine.  

//
// Ensemble constructors.  Both just create blank trace or 3c trace objects
// and push them into a vector container.
//
Time_Series_Ensemble::Time_Series_Ensemble(int nensemble, int nsamples)
{
	for(int i=0; i<nensemble; ++i)
	{
		Time_Series *ts = new Time_Series(nsamples);
		tse.push_back(*ts);
	}
}
Three_Component_Ensemble::Three_Component_Ensemble(int nstations, int nsamples)
{
	for(int i=0;i<nstations;++i)
	{
		Three_Component_Seismogram *tcs 
			= new Three_Component_Seismogram(nsamples);
		tcse.push_back(*tcs);
	}
}
