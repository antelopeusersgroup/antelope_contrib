#include "tr.h" // Antelope trace library
#include "SeisppError.h"
#include "ComplexTimeSeries.h"
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
//
// simple constructors for the TimeSeries object are defined inline
// in seispp.h.  
//
ComplexTimeSeries::ComplexTimeSeries() : BasicTimeSeries(), Metadata()
{
	s.reserve(0);
}
ComplexTimeSeries::ComplexTimeSeries(int nsin) : BasicTimeSeries(), Metadata()
{
	Complex defvalue(0.0,0.0);
	s.reserve(nsin);
	ns=nsin;
	for(int i=0;i<nsin;++i)s.push_back(defvalue);
}
	
ComplexTimeSeries::ComplexTimeSeries(const ComplexTimeSeries& tsi) : 
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
//  ts = ComplexTimeSeries(md);  -- i.e. this constructor
//

ComplexTimeSeries::ComplexTimeSeries(const Metadata& md,bool load_data) : Metadata(md)
{
	const string required_datatype("cx");
	string stref;
	string dfile, dir;
	long foff;
	FILE *fp;
	string dtype;
	Complex *inbuffer;

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
			if(dtype!=required_datatype)
				throw(SeisppError("ComplexTimeSeries Metadata constructor:  Unsupported datatype\nMetadata-driven constructor only supports cx (single precision complex pairs)  data with external files"));
			dir = this->get_string("dir");
			dfile = this->get_string("dfile");
			foff = this->get_long("foff");
			string fname=dir+"/"+dfile;
			if((fp=fopen(fname.c_str(),"r")) == NULL) 
				throw("Open failure for file "+fname);
			if (foff>0)fseek(fp,foff,SEEK_SET);
			inbuffer = new Complex[ns];
			if(fread((void *)(inbuffer),sizeof(float),ns,fp)
					!= ns ) 
			{
				delete [] inbuffer;  // memory leak possible without this
				throw(SeisppError("ComplexTimeSeries constructor:  fread error on file "+fname));
			}
			for(int i=0;i<ns;++i) 
				s.push_back(inbuffer[i]);
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
		in the Metadata object (see Metadata(3)).
	am - defines namespace mapping from database naming to internal namespace
		(see Metadata(3)).
Uses Antelope's trgetwf function which should allow it to read almost any
common seismic trace format. 
*/

ComplexTimeSeries::ComplexTimeSeries(DatabaseHandle& rdb,
		MetadataList& md_to_extract, 
			AttributeMap& am) 
	: Metadata(rdb,md_to_extract,am)
{
	const string required_datatype("cx");
	FILE *fp;
	double *inbuffer=NULL;
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
		string stref = this->get_string("timetype");
		if(stref == "relative")
			tref = relative;
		else
			tref = absolute;
		string dtype = this->get_string("datatype");
		if(dtype!=required_datatype)
			throw(SeisppError("ComplexTimeSeries Metadata constructor:  Unsupported datatype\nMetadata-driven constructor only supports cx (single precision complex pairs)  data with external files"));
		string dir = this->get_string("dir");
		string dfile = this->get_string("dfile");
		long foff = this->get_long("foff");
		string fname=dir+"/"+dfile;
		if((fp=fopen(fname.c_str(),"r")) == NULL) 
			throw("Open failure for file "+fname);
		if (foff>0)fseek(fp,foff,SEEK_SET);
		// Portability consideration.  This freezes 
		// this as complex<double>.  
		int ns_to_read=2*ns;
		inbuffer = new double[ns_to_read];
		if(fread((void *)(inbuffer),sizeof(double),ns_to_read,fp)
				!= ns_to_read ) 
		{
			delete [] inbuffer;  // memory leak possible without this
			throw(SeisppError("ComplexTimeSeries constructor:  fread error on file "+fname));
		}
		s.reserve(ns);
		for(int i=0,ii=0;i<ns;++i,ii+=2) 
			s.push_back(Complex(inbuffer[ii],inbuffer[ii+1]));
		delete [] inbuffer;
		live = true;
		fclose(fp);
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
ComplexTimeSeries& ComplexTimeSeries::operator=(const ComplexTimeSeries& tsi)
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

void ComplexTimeSeries::operator+=(const ComplexTimeSeries& data)
{
	int i,i0,iend,ntosum;
	int j,j0=0,jend=ns;
	// Sun's compiler complains about const objects without this.
	ComplexTimeSeries& d=const_cast<ComplexTimeSeries&>(data);
	// Silently do nothing if d is marked dead
	if(!d.live) return;
	// Silently do nothing if d does not overlap with data to contain sum
	if( (d.endtime()<t0) 
		|| (d.t0>(this->endtime())) ) return;
	if(d.tref!=(this->tref)) 
		throw SeisppError("ComplexTimeSeries += operator cannot handle data with inconsistent time base\n");
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
	for(i=i0,j=j0;i<=iend;++i,++j)
	{
		this->s[j]+=d.s[i];
	}
}
// Near duplicate for -=
void ComplexTimeSeries::operator-=(const ComplexTimeSeries& data)
{
	int i,i0,iend,ntosum;
	int j,j0=0,jend=ns;
	// Sun's compiler complains about const objects without this.
	ComplexTimeSeries& d=const_cast<ComplexTimeSeries&>(data);
	// Silently do nothing if d is marked dead
	if(!d.live) return;
	// Silently do nothing if d does not overlap with data to contain sum
	if( (d.endtime()<t0) 
		|| (d.t0>(this->endtime())) ) return;
	if(d.tref!=(this->tref)) 
		throw SeisppError("ComplexTimeSeries += operator cannot handle data with inconsistent time base\n");
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
	{
		this->s[j]-=d.s[i];
	}
}
ComplexTimeSeries ComplexTimeSeries::operator * (const double zr)
{
	int i;
	ComplexTimeSeries result(*this);
	vector<Complex>::iterator zptr;
	for(zptr=this->s.begin(),i=0;zptr!=this->s.end();++zptr,++i)
	{
		result.s[i]=zr*(*zptr);
	}
	return(result);
}
ComplexTimeSeries ComplexTimeSeries::operator * (const Complex z)
{
	int i;
	ComplexTimeSeries result(*this);
	vector<Complex>::iterator zptr;
	for(zptr=this->s.begin(),i=0;zptr!=this->s.end();++zptr,++i)
	{
		result.s[i]=z*(*zptr);
		/*
		result.s[i].re=((z.re)*(zptr->re)-(z.im)*(zptr->im));
		result.s[i].im=((z.im)*(zptr->re)+(z.re)*(zptr->im));
		*/
	}
	return(result);
}
Complex ComplexTimeSeries::operator[](int i)
{
	if(!live)
		throw SeisppError(string("ComplexTimeSeries operator[]:  attempt to access data vector marked as dead"));
	if( (i<0) || (i>=ns) )
	{
		throw SeisppError(
			string("TimeSeries operator[]:  request for sample outside range of data"));
	}
	return(s[i]);
}
void ComplexTimeSeries::initialize(const Complex z)
{
	vector<Complex>::iterator zptr;
	for(zptr=this->s.begin();zptr!=this->s.end();++zptr)
		*zptr=z;
}

ComplexTimeSeries ComplexTimeSeries::conj()
{
	int j;
	ComplexTimeSeries result(*this);
	vector<Complex>::iterator zptr;
	for(j=0,zptr=this->s.begin();zptr!=this->s.end();++zptr,++j)
	{
		result.s[j]=std::conj(*zptr);
	}
	return(result);
}

TimeSeries ComplexTimeSeries::real()
{
	TimeSeries result(dynamic_cast <Metadata &>(*this),false);
	// These are from BasicTimeSeries.  Don't how to do thi
	// other than an explicitly like this.  This is a nontrivial
	// maintenance issue.
	result.live=true;
	result.dt=dt;
	result.t0=t0;
	result.ns=ns;
	result.tref=tref;
	set<TimeWindow,TimeWindowCmp>::iterator this_gap;
	for(this_gap=gaps.begin();this_gap!=gaps.end();++this_gap)
		result.add_gap(*this_gap);
	result.s.reserve(s.size());
	vector<Complex>::iterator zptr;
	for(zptr=this->s.begin();zptr!=this->s.end();++zptr)
		result.s.push_back(zptr->real());
	return(result);
}
TimeSeries ComplexTimeSeries::imag()
{
	TimeSeries result(dynamic_cast <Metadata &>(*this),false);
	// These are from BasicTimeSeries.  Don't how to do thi
	// other than an explicitly like this.  This is a nontrivial
	// maintenance issue.
	result.live=true;
	result.dt=dt;
	result.t0=t0;
	result.ns=ns;
	result.tref=tref;
	set<TimeWindow,TimeWindowCmp>::iterator this_gap;
	for(this_gap=gaps.begin();this_gap!=gaps.end();++this_gap)
		result.add_gap(*this_gap);
	result.s.reserve(s.size());
	vector<Complex>::iterator zptr;
	for(zptr=this->s.begin();zptr!=this->s.end();++zptr)
		result.s.push_back(zptr->imag());
	return(result);
}
TimeSeries ComplexTimeSeries::mag()
{
	TimeSeries result(dynamic_cast <Metadata &>(*this),false);
	// These are from BasicTimeSeries.  Don't how to do thi
	// other than an explicitly like this.  This is a nontrivial
	// maintenance issue.
	result.live=true;
	result.dt=dt;
	result.t0=t0;
	result.ns=ns;
	result.tref=tref;
	set<TimeWindow,TimeWindowCmp>::iterator this_gap;
	for(this_gap=gaps.begin();this_gap!=gaps.end();++this_gap)
		result.add_gap(*this_gap);
	result.s.reserve(s.size());
	vector<Complex>::iterator zptr;
	double mag;
	for(zptr=this->s.begin();zptr!=this->s.end();++zptr)
	{
		mag=abs(*zptr);
		result.s.push_back(mag);
	}
	return(result);
}
TimeSeries ComplexTimeSeries::phase()
{
	TimeSeries result(dynamic_cast <Metadata &>(*this),false);
	// These are from BasicTimeSeries.  Don't how to do thi
	// other than an explicitly like this.  This is a nontrivial
	// maintenance issue.
	result.live=true;
	result.dt=dt;
	result.t0=t0;
	result.ns=ns;
	result.tref=tref;
	set<TimeWindow,TimeWindowCmp>::iterator this_gap;
	for(this_gap=gaps.begin();this_gap!=gaps.end();++this_gap)
		result.add_gap(*this_gap);
	result.s.reserve(s.size());
	vector<Complex>::iterator zptr;
	double phase;
	for(zptr=this->s.begin();zptr!=this->s.end();++zptr)
	{
		phase=arg(*zptr);
		result.s.push_back(phase);
	}
	return(result);
}
// Forces samples in marked gaps to zero.  
void ComplexTimeSeries::zero_gaps()
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
		for(i=istart;i<=iend;++i) 
		{
			s[i]=Complex(0.0,0.0);
		}
	}
}
// Declared as friend so this form is correct.  
ostream& operator << (ostream& os,ComplexTimeSeries& z)
{
	os << "==================ComplexTimeSeries Metadata=============="
		<< endl;
	os << dynamic_cast<Metadata &>(z);
	os << "=== BasicTimeSeries data members==="<<endl;
	os << dynamic_cast<BasicTimeSeries &>(z);
	os << "=========================================================="
		<<endl;
	vector<Complex>::iterator zptr;
	for(zptr=z.s.begin();zptr!=z.s.end();++zptr)
		os << *zptr <<endl;
	return(os);
}

} // End SEISPP namespace declaration
