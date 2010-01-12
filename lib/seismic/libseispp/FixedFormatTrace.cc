#include <sstream>
#include "FixedFormatTrace.h"
using namespace std;
using namespace SEISPP;

namespace SEISPP
{
/* Simple internal function to return byte count for different sample
data types */
int stype_bytes_per_sample(AttributeType stype)
{
	int bytes_per_sample;
	switch(stype)
	{
	case INT16:
		bytes_per_sample=2;
		break;
	case REAL64:
        case INT64:
		bytes_per_sample=8;
		break;
	case INT32:
	case REAL32:
	default:
		bytes_per_sample=4;
	}
	return(bytes_per_sample);
}

/* This constructor only builds a skeleton with no data.
It is normally used to build a template that is copied, 
data are loaded, and the pointers it contains are resolved to 
something not NULL.   When nsamp is 0 (default) only a header
is written and the d pointer is set NULL.*/
FixedFormatTrace::FixedFormatTrace(string type, int nsamp) 
	: BasicTimeSeries()
{
	const string base_error("FixedFormatTrace initialization constructor  ");
	const string header_map_pf("HeaderMap");
	Pf *pf;
	if(pfread(const_cast<char *>(header_map_pf.c_str()),&pf))
		throw SeisppError(base_error
			+ string("pfread failed on parameter file ")
			+ header_map_pf
			+ string(".pf used to store header definitions"));
	try {
		header=HeaderMap(pf,type);
	} catch (...) {throw;}
	data_loaded=false;
	size_of_this=0;
	/* ns less than 0 makes no sense and would cause problem so handle
	this in case used incorrectly.*/
	if(nsamp<0)
		ns=0;
	else
		ns=nsamp;
	h=NULL;
	d=NULL;
	/* We intentionally don't check return of this pfget
	call here because we can't get here if it failed earlier
	because the HeaderMap constructor does this same thing. */
	Pf *pfa;
	pfget(pf,const_cast<char *>(type.c_str()),(void **)&pfa);
	char *stypename=pfget_string(pfa,"sample_data_type");
	if(stypename==NULL)
		throw SeisppError(base_error
		 + string("missing required parameter sample_data_type for trace type=")
		+ type );
	string sample_type(stypename);
	if(sample_type=="int16" || sample_type=="INT16" 
		|| sample_type=="short")
		stype=INT16;
	else if(sample_type=="int32" || sample_type=="INT32" 
		|| sample_type=="int")
		stype=INT32;
	else if(sample_type=="int64" || sample_type=="INT64" 
		|| sample_type=="long")
		stype=INT64;
	else if(sample_type=="real32" || sample_type=="REAL32" 
		|| sample_type=="float")
		stype=REAL32;
	else if(sample_type=="real64" || sample_type=="REAL64" 
		|| sample_type=="double")
		stype=REAL64;
	else
		throw SeisppError(base_error
		 + string("Invalid entry for sample_data_type for trace type=")
		 + type);
	/* First we have to create the block of memory used to hold
	data for ns samples and the header.  We use malloc here 
	with opaque pointers to access pieces of the result.  */
	size_t headeroffset=header.size();
	int bytes_per_sample=stype_bytes_per_sample(stype);
	size_of_this=headeroffset+(size_t)(bytes_per_sample*ns);
	h=static_cast<unsigned char *>(malloc(size_of_this));
	if(h==NULL)
		throw SeisppError(base_error
			+ string("malloc failure"));
	if(ns==0)
		d=NULL;
	else
		d=h+headeroffset;
	/* We already have pfa which is a handle to the parameter file
	block bounded by type &Arr{ }.  Here we pull default parameters from
	the same block.  parsed by the HeaderMap constructor.  That constructor,
	however, ignored the defaults we can now load in the loop below.
	*/
	Tbl *t;
	t=pfget_tbl(pfa,(char *)"attribute_defaults");
	if(t==NULL) throw SeisppError(base_error
		+ string("Missing required parameter attribute_defaults for trace type=")
		+ type ) ;
	string attribute_name,attribute_type;
	double dvalue;
	int ivalue;
	bool bvalue;
	char svalue[128];
	for(int i=0;i<maxtbl(t);++i)
	{
		char *line;
		line=(char *)gettbl(t,i);
		istringstream ss(line);
		ss >> attribute_name;
		ss >> attribute_type;
		try {
			if(attribute_type=="real" || attribute_type=="REAL")
			{
				ss >> dvalue;
				this->put<double>(attribute_name,dvalue);
			}
			else if(attribute_type=="bool" || attribute_type=="BOOL")
			{
				ss >>  bvalue;
				this->put<bool>(attribute_name,bvalue);
			}
			else if(attribute_type=="int" || attribute_type=="INT")
			{
				ss >>  ivalue;
				this->put<int>(attribute_name,ivalue);
			}
			else if(attribute_type=="string" || attribute_type=="STRING")
			{
				ss >>  svalue;
				this->put<char *>(attribute_name,svalue);
			}
		}
		catch (SeisppError& serr)
		{
			cerr << "Warning:  problem parsing the following line"
				<< " from parameter file for trace type="
				<< type<<endl
				<< line <<endl;
			cerr << "Error message from handled exception:" <<endl;
			serr.log_error();
		}
	}
}
FixedFormatTrace::FixedFormatTrace(const FixedFormatTrace& parent)
	: BasicTimeSeries(parent)
{
	header=parent.header;
	stype=parent.stype;
	ns=parent.ns;
	data_loaded=parent.data_loaded;
	/* This requires h always be initialized NULL by the constructor that
	produced parent in the event there is not data loaded. */
	if( parent.h==NULL)
	{
		h=NULL;
		d=NULL;
		size_of_this=0;
	}
	else
	{
		size_of_this=parent.size_of_this;
		h=static_cast<unsigned char *>(malloc(size_of_this));
		if(h==NULL) 
		  throw SeisppError(string("FixedFormatTrace constructor:  malloc failure"));
		/* Handle case of no data correctly */
		if(ns==0)
			d=NULL;
		else
			d=h+header.size();
		memcpy(h,parent.h,size_of_this);
	}
}
FixedFormatTrace::FixedFormatTrace(const FixedFormatTrace& parent,
	FILE *fp,int nsamp,string key, bool key_is_dt)
{
	const string base_error("FixedFormatTrace constructor:  ");
	header=parent.header;
	stype=parent.stype;
	ns=nsamp;
	size_t datasize=(size_t)(stype_bytes_per_sample(stype)*ns);
	size_of_this=header.size()+datasize;
	h=static_cast<unsigned char *>(malloc(size_of_this));
	if(h==NULL) throw SeisppError(string("FixedFormatTrace constructor:  malloc failure"));
	d=h+header.size();
	size_t test;
	test=fread((void *)h,datasize,1,fp);
	if(test!=1)
		throw SeisppError(base_error
		 + string("fread error"));
	try {
		dt=this->get<double>(key);
		if(!key_is_dt) dt=1.0/dt;
	} catch (SeisppError& serr)
	{
		throw SeisppError(base_error
		 + string("failed to fetch sample intervale using keyword=")
		 + key);
	}
	data_loaded=true;
	live=true;
	t0=0.0;
	tref=relative;
}
FixedFormatTrace::FixedFormatTrace(const FixedFormatTrace& parent,
                FILE *fp,string nsamp_keyword,string dt_keyword,
		bool key_is_dt)
{
	const string base_error("FixedFormatTrace constructor:  ");
	header=parent.header;
	stype=parent.stype;
	h=static_cast<unsigned char *>(malloc(header.size()));
	if(h==NULL) throw SeisppError(base_error
		+ string("malloc failure trying to alloc space for header."));
	size_t test;
	test=fread((void *)h,header.size(),1,fp);
	if(test!=1) 
		throw SeisppError(base_error
			+ string("fread error reading header block") );
	try {
		ns=this->get<int>(nsamp_keyword);
		dt=this->get<double>(dt_keyword);
		if(!key_is_dt) dt=1.0/dt;
	} catch (SeisppError& serr)
	{
		throw SeisppError(base_error
		 + string("failure fetching BasicTimeSeries attributes from header"));
	}
	size_t datasize=(size_t)(stype_bytes_per_sample(stype)*ns);
	realloc(h,datasize+header.size());
	d=h+header.size();
	test=fread((void *)d,datasize,1,fp);
	if(test!=1)
		throw SeisppError(base_error
		 + string("fread error reading daa block"));
	data_loaded=true;
	data_loaded=true;
	live=true;
	t0=0.0;
	tref=relative;
}
FixedFormatTrace::~FixedFormatTrace()
{
	/* Note we must absolutely not free d as in all uses here d is a
	secondary pointer in block with an offset. */
	if(h!=NULL) free(h);
}
/* Returns sample value at i independent of sample data
type.  Limited data types are accepted at present AND
I intentionally do not throw an exception in any case
here as this routine has to be fast to be useful.  
It has some very dangerous constructs so user beware.
A classic sharp knife. Note to support byte swapped
data this would be the place to deal with the issue.
For now not supported and assume one deals with this 
externally (e.g. suswapbytes).   */
double FixedFormatTrace::operator()(int i)
{
	size_t soffset=static_cast<size_t>(i);
	unsigned char *sptr=d;
	double result;
        long *liraw;
	int *iraw;
	short int *sraw;
	float *fraw;
	switch (stype)
	{
	case INT64:
		sptr=sptr+8*soffset;
		/* this assumes long means int64 */
		liraw=reinterpret_cast<long *>(sptr);
		result=static_cast<double>(*iraw);
		break;
	case INT32:
                /* assumes int means int32 */
		sptr=sptr+4*soffset;
		iraw=reinterpret_cast<int *>(sptr);
		result=static_cast<double>(*iraw);
		break;
	case INT16:
		sptr=sptr+2*soffset;
		/* this assumes short int means int16.  */
		sraw=reinterpret_cast<short int *>(sptr);
		result=static_cast<double>(*sraw);
		break;
	case REAL64:
		sptr=sptr+8*soffset;
		result=static_cast<double>(*sptr);
		break;
	case REAL32:
	default:
		sptr=sptr+4*soffset;
		fraw=reinterpret_cast<float *>(sptr);
		result=static_cast<double>(*fraw);
		break;
	}
	double& ref = result;
	return ref;
}
/* This is stubbed in as it is required because we inherit
BasicTimeSeries.  Ultimately something like the method used
in BRTT's trace library should be used to mark gaps. */
void FixedFormatTrace::zero_gaps()
{
}
vector<double> FixedFormatTrace::data()
{
	vector<double> result;
	/* ns comes from BasicTimeSeries */
	result.reserve(ns);
	/* I can't figure out the obscure syntax it would take
	to do this with the raw this pointer so we do this 
	conversion to a reference.  Clearer in the long run anyway.*/
	FixedFormatTrace& thistrace=(*this);
	for(int i=0;i<ns;++i) result.push_back(thistrace(i));
	return(result);
}

}  // End SEISPP Namespace encapsulation
