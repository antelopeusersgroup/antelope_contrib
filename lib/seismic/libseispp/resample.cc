/*
 This is a collection of functions to implement resampling and decimations.
The routines here take a bias toward segmented data.  The methods would
not work as efficiently on continuous data as one would have to deal with 
overlapping buffers and edge effects.  The intention is it will be used as a
basis for a dbresample/decimator program and in applications to reample
segmented data on the fly.

Author:  Gary L. pavlis
Written:  Winter spring 2004
*/
#include <list>
#include <vector>
using namespace std;
#include "seispp.h"
using namespace SEISPP;
#include "interpolator1d.h"
using namespace INTERPOLATOR1D;
#include "resample.h"
#include "response.h"
#include "perf.h"
// Constructs this object from exact sample rate key (e) 
// and using a pf 
Resample_Operator::Resample_Operator(double e,Pf *pf)
{
	Tbl *t;
	exact = e;
	char *line;
	char decfilename[128];
	Decimator *dec;
	string fname;
	double decfac_test;
	const double DECFAC_TOLERANCE=0.00001;
	int i;
	
	low = pfget_double(pf,const_cast<char *>("low_limit"));
	high = pfget_double(pf,const_cast<char *>("high_limit"));
	t = pfget_tbl(pf,const_cast<char *>("Decimator_response_files"));

	for(i=0;i<maxtbl(t);++i)
	{
		line = static_cast<char *>(gettbl(t,i));
		sscanf(line,"%lf%s",&decfac_test,decfilename);
		fname=string(decfilename);
		dec = new Decimator(fname,decfac_test);
		if(fabs((decfac_test-(dec->decfac))/decfac_test)>DECFAC_TOLERANCE)
			throw  seispp_error("Decimator constructor failure:  mismatch of sample rate between response file "
				+fname+" and input line\n"+string(line));
		declist.push_back(*dec);
	}
	freetbl(t,0);
}
/* builds an empty Resample_Operator with an empty declist, but 
with the key fields used by the Resampling_Definitions operator set.
*/
Resample_Operator::Resample_Operator(double e,double l, double h)
{
	low = l;
	high = h;
	exact = e;
	declist.push_back(Decimator());
}

// copy constructor for this class
Resample_Operator::Resample_Operator(const Resample_Operator& roi)
{
	low = roi.low;
	high = roi.high;
	exact = roi.exact;
	declist = roi.declist;
}
// assignment operator
Resample_Operator& Resample_Operator::operator=(const Resample_Operator& roi)
{
	if(this != &roi)
	{
		low = roi.low;
		high = roi.high;
		exact = roi.exact;
		declist = roi.declist;
	}
	return(*this);
}
/*  Main, top level function for this group of objects.  This method applies
a series of decimation and resample stages to the input vector, s, returning
the result as a Decimated_vector object.  dtin is the input sample rate and
dtout_target is the desired output sample rate.  A feature of the algorithm is
that if the computed sample rate after applying the series of decimators is
outside a (frozen) tolerance the data are resampled again onto a grid of dtout_target
interval values.  This should work well in dealing with data from digitizers with
slippery clocks.  

trim determines if the time series is trimmed on the left and right to 
remove fir filter transient sections.
*/
Decimated_vector& Resample_Operator::apply(int ns, double *s,double dtin, 
	double dtout_target, bool trim)
{
	list<Decimator>::iterator this_decimator;
	Decimated_vector *result=new Decimated_vector(ns);
	int i;
	const double DT_FRACTIONAL_ERROR=0.001;
	double dtout=dtin;
	double decout=1.0;
	int total_lag=0;

	for(i=0;i<ns;++i) result->d.push_back(s[i]);
	result->lag=0;  // not really necessary, but clearly shows initialization

	for(this_decimator=declist.begin();
		this_decimator!=declist.end();++this_decimator)
	{
		*result = this_decimator->apply(result->d,trim);
		total_lag += rint(static_cast<double>(result->lag)*decout);
		dtout *= this_decimator->decfac;
		decout *= this_decimator->decfac;
	}
	if(fabs(dtout-dtout_target)/dtout_target > DT_FRACTIONAL_ERROR)
	{
		double final_decfac= dtout_target/dtout;
		Decimator dfinal("resample",final_decfac);
		*result = dfinal.apply(result->d,trim);
	}
	result->lag = total_lag;
	return(*result);
}
	

// beware the undefined default constructor -- so define it
Decimator::Decimator()
{
	coefs.reserve(0);
	lag=0;
	decfac=1.0;
}
		
	
Decimator::Decimator(string fname,double decfac_target)
{
	FILE *file;
	Response *rsp;
	int i,n;
	char stage_type[128];
	double srate;
	int nnum,nden;
	const string err_mess_head="Decimator object constructor: ";
	const double MAX_UPSAMPLE_DECFAC=1.1;
	int ncoefs;

	lag = 0;
	if(fname=="none" || fname=="resample") 
	{
		decfac = decfac_target;
		coefs.reserve(0);
		lag=0;
		if(decfac_target>MAX_UPSAMPLE_DECFAC)
			throw seispp_error(err_mess_head+"illegal decfac for upsample defininition.  Must specify decfac less than 1");
		return;
	}

	file = fopen(fname.c_str(),"r");
	if(file==NULL) 
		throw seispp_error(err_mess_head
		+"cannot open response file "+fname);

	rsp=(Response *)new_response();

	if(read_response(file,&rsp))
	{
		fclose(file);
		throw seispp_error(err_mess_head+"read_response() failed on file "+fname);
	}
	fclose(file);
	get_response_nstages(rsp,&n);
	// this probably should be a verbose option
	if(n!=1) cerr<<"FIR_decimator constructor:  warning found multistage definition in file="
			<< fname << endl << "Using only the first stage"<<endl;	
	get_response_stage_type(rsp,0,stage_type);
	if(strcmp(stage_type,"fir"))
		throw seispp_error(err_mess_head+"Response file "+fname
			+"is not a FIR filter");
	int idecfac;
	get_response_stage_fir_ncoefs(rsp,0,&srate,&idecfac,&nnum,&nden);
	if(nden>1) throw seispp_error(err_mess_head+
		"file "+fname+"has IIR component\nIIR Filter component not allowed\n");
	if(nnum<1) throw seispp_error(err_mess_head
		+ string("No FIR filter coefficients defined in file ")+fname);
	decfac=static_cast<double>(idecfac);
	ncoefs=nnum;
	lag = ncoefs/2;
	double *coefsi,*coefs_err,*coefdi,*coefd_err;  // required by function below
	get_response_stage_fir_coefs(rsp,0,&nnum,&coefsi,&coefs_err,&nden,&coefdi,&coefd_err);
	// copy coefsi to coefs and free the response and work vectors
	coefs.reserve(ncoefs);
	for(i=0;i<ncoefs;++i) coefs.push_back(coefsi[i]);
	free(coefsi);
	free(coefs_err);
	free(coefd_err);
	// This created duplicate free errors.  I don't understand why, but
	// I'll let it leak in this program because I expect these to only
	// be called at startup.
	// free_response(rsp);
}
// copy constructor
Decimator::Decimator(const Decimator& di)
{
	decfac = di.decfac;
	coefs = di.coefs;
	lag = di.lag;
}
//assignment
Decimator& Decimator::operator=(const Decimator& di)
{
    if(this != &di)
    {
	decfac = di.decfac;
	coefs=di.coefs;
	lag = di.lag;
    }
    return(*this);
}
/*  This is the key method for a decimator.  It takes the contents of
an input vector of doubles and returns a decimated trace with the 
Decimator filtering applied and with the reduced sample rate defined by
decfac.  The boolean variable trim defines how the edges are handled.
When trim is true the vector is shorted on both ends to allow the 
decimator filter to not have an edge transient.  lag is set appropriately.
*/
Decimated_vector& Decimator::apply(int nsamp_in, double *s,bool trim)
{
	int nsamp_out;
	int i,ii;
	int ncoefs=coefs.size();
	Decimated_vector *dout;

	// quietly refuse to attempt anything in either case when the
	// input is less than the length of the filter
	// Warning:  this assumes that ncoefs is initialized to 0 for upsample case
	if(nsamp_in<ncoefs)
	{
		// copy data 
		dout = new Decimated_vector(nsamp_in);
		for(i=0;i<nsamp_in;++i)dout->d.push_back(s[i]);
		return(*dout);
	}
	// upsampling is triggered by a decfac less than 1.0
	if(decfac<1.0)
	{
		double dt;
		nsamp_out = static_cast<int>( (((double)nsamp_in)/decfac));
		dout = new Decimated_vector(nsamp_out);
		dout->d.resize(nsamp_out);  // need this to actually alloc vector 
		dout->lag=0;
		// calculate a sample rate from decfac based a nondimensional
		// input sample rate of 1.  Interpolation doesn't care about
		// units of abscissa because decfac is also nondimensional
		dt = decfac;
		// call interpolator with 0 start for similar reasons to dt.  
		// that is this interpolator is general and here we want output
		// time aligned to first sample
		linear_scalar_regular_to_regular(nsamp_in,0.0,1.0,s,
			nsamp_out,0.0,dt,&(dout->d[0]));
	}
	else
	{
		// This assumes decfac is close to an integer
		if(trim)
			nsamp_out = (nsamp_in - ncoefs)/rint(decfac);
		else
			nsamp_out = nsamp_in/rint(decfac);
		dout = new Decimated_vector(nsamp_out);
		if(trim)
		{
			dout->lag = lag;
			for(i=0,ii=lag;i<nsamp_out;++i,ii+=decfac)
			{
				double dotprd;
				dotprd = ddot(ncoefs,&coefs[0],1,s+ii-lag,1);
				dout->d.push_back(dotprd);
			}
		}
		else
		{
			dout->lag = 0;
			for(i=0,ii=1-lag;i<nsamp_out;++i,ii+=decfac)
			{
				int ndot,id0;
				double dotprd;
				if(ii<0)
				{
					ndot=ncoefs+ii; // right because ii is negative
					// pointer lag in coefs sets pointer to leading edge of coefs
					dotprd=ddot(ndot,&coefs[-ii],1,s,1);
					dout->d.push_back(dotprd);
				}
				else if(ii+ncoefs>nsamp_in)
				{
					ndot = nsamp_in - ii;
					dotprd=ddot(ndot,&coefs[i],1,s+ii,1);
					dout->d.push_back(dotprd);
				}
				else
				{
					dotprd=ddot(ncoefs,&coefs[0],1,s+ii,1);
					dout->d.push_back(dotprd);
				}
			}
		}
	}
	return(*dout);
}
Decimated_vector& Decimator::apply(int nsamp_in, double *s)
{
	return(apply(nsamp_in,s,false));
}
Decimated_vector& Decimator::apply(vector<double>s,bool trim)
{
	int ns=s.size();
	return(apply(ns,&s[0],trim));
}

/*  For Decimated_vector object */
Decimated_vector::Decimated_vector(int ns)
{
	lag=0;
	d.reserve(ns);
}
Decimated_vector::Decimated_vector(const Decimated_vector& dvi)
{
	d = dvi.d;
	lag = dvi.lag;
}
Decimated_vector& Decimated_vector::operator=(const Decimated_vector& dvi)
{
	if(this != &dvi)
	{
		d = dvi.d;
		lag = dvi.lag;
	}
	return(*this);
}

Resampling_Definitions::Resampling_Definitions(Pf *pf)
{
	Pf *pfrda,*pfsr;
	Tbl *t;
	char *key;
	double exact;
	Resample_Operator *rop;
	Interval si_range;
	typedef map<Interval,Resample_Operator> ROmap;

	
	if(pfget(pf,"resample_definitions",(void **)&pfrda) != PFARR)
		throw seispp_error("Reample_Definition constructor:  pfget failure looking for Reample_Definition keyword");
	
	t = pfkeys(pfrda);
	for(int i=0;i<maxtbl(t);++i)
	{
		key = static_cast<char *>(gettbl(t,i));
		// thrown error here can create a memory leak if this exception is caught and one tries to continue
		if(pfget(pfrda,key,(void **)&pfsr)!=PFARR)
			throw seispp_error("Resampling_Definitions constructor:  Syntax error in parameter file Arr block tagged Resampling_Definitions");
		exact=atof(key);
		rop = new Resample_Operator(exact,pfsr);
		si_range.low=rop->low;
		si_range.high=rop->high;
		decset.insert(ROmap::value_type(si_range,*rop));
		delete rop;
	}
}
/* Higher level function that applies a Resampling_Definitions operator to an
input time series.  Output is a new Time_Series object decimated/resampled to target
sample rate dtout.  As above if trim is true the output will be truncated at the edges to
avoid transients from decimators.  When trim is false the edges may contain artifacts, but
the full time window will be preserved (well, within a few samples anyway depending on 
roundoffs). 

Written:  winter and spring 2004
Author:  Gary Pavlis
*/
Time_Series Resample_Time_Series(Time_Series& ts, Resampling_Definitions& rd,double dtout,bool trim)
{
	Decimated_vector dv;
	Interval si_range;
	map<Interval,Resample_Operator,Interval_Cmp>::iterator this_ro;
	// First we need to find the right resampling operator for this sample rate
	double sr_in=1.0/(ts.dt);
	si_range.low=sr_in;
	si_range.high=sr_in;
	this_ro = rd.decset.find(si_range);
	if(this_ro == rd.decset.end())
	{
		char  dt_str[20];
		sprintf(dt_str,"%lf",ts.dt);
		throw seispp_error(string("Resample_Time_Series:  ")
			+string("don't know how to resample data with sample interval ")
			+string(dt_str));
	}
	dv = this_ro->second.apply(ts.ns,&(ts.s[0]),ts.dt,dtout,trim);
	Time_Series tsout=ts;
	tsout.dt=dtout;
	// necessary because tsout.s is a container
	tsout.ns = dv.d.size();
	tsout.s.resize(dv.d.size());
	for(int i=0;i<tsout.ns;++i) tsout.s[i]=dv.d[i];
	tsout.t0 = ts.t0 + (dv.lag)*ts.dt; // assumes lag is in units of original sample rate
	tsout.put_metadata("nsamp",tsout.ns);
	tsout.put_metadata("samprate",1.0/tsout.dt);
	tsout.put_metadata("starttime",tsout.t0);
	tsout.put_metadata("endtime",tsout.t0+tsout.time(tsout.ns-1));
	return(tsout);
}
