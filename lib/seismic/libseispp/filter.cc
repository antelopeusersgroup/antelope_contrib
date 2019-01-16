#include "seispp.h"
#include "filter++.h"
using namespace SEISPP;
namespace SEISPP
{

TimeInvariantFilter::TimeInvariantFilter(string fspec)
{
	filter_spec=fspec;
	if(!fspec.compare(0,2,"BW"))
	{
		char ftkey[10];
		sscanf(fspec.c_str(),"%s%lf%d%lf%d",ftkey,
			&f1,&npole1,&f2,&npole2);
		if( npole1==0 && f1<=0.0 && npole2==0 && f2<=0.0)
			throw SeisppError(string("Illegal filter definition = ")
				+ fspec);
		else if(npole1==0 || f1<=0.0)
			type = lowpass;
		else if(npole2==0 || f2<=0.0)
			type = highpass;
		else
			type = bandpass;
	}
	else
	{
		npole1 = 0;  npole2=0;
		f1=0.0;  f2=0.0;
		if(fspec=="WAA")
			type=WAA;
		else if(fspec=="WAV")
                        type=WAV;
		else if(fspec=="WAD")
                        type=WAD;
		else if(fspec=="INT")
			type=INT;
		else if(fspec=="INT2")
			type=INT2;
		else if(fspec=="DIF")
			type=DIF;
		else if(fspec=="DIF2")
			type=DIF2;
		else if(fspec=="DEMEAN")
			type=DEMEAN;
		else
			type=none;
	}
}
TimeInvariantFilter::TimeInvariantFilter(double flow, int npl, 
	double fhigh, int nph)
{
	type = bandpass;
	npole1=npl;  npole2=nph;
	f1=flow;  f2=fhigh;
	char s[50];
	sprintf(s,"BW %lf %d %lf %d",f1,npole1,f2,npole2);
	filter_spec=string(s);
}
// copy constructor -- perhaps unnecessary, but books all say never
// depend on the default
TimeInvariantFilter::TimeInvariantFilter(const TimeInvariantFilter& tin)
{
	type=tin.type;
	npole1=tin.npole1;
	npole2=tin.npole2;
	f1=tin.f1;
	f2=tin.f2;
	filter_spec=tin.filter_spec;
}
// same for assignment operator here
TimeInvariantFilter& TimeInvariantFilter::operator=(const TimeInvariantFilter& tin)
{
	if(&tin!=this)
	{
		type=tin.type;
		npole1=tin.npole1;
		npole2=tin.npole2;
		f1=tin.f1;
		f2=tin.f2;
		filter_spec=tin.filter_spec;
	}
	return(*this);
}

	
double TimeInvariantFilter::fmin()
{
	switch (type)
	{
	case highpass:
	case bandpass:
		return(f1);
	case lowpass:
	default:
		throw SeisppError(
			string("fmin not defined for filter type")
				+ this->filter_spec);
	}
}
double TimeInvariantFilter::fmax()
{
	switch (type)
	{
	case lowpass:
	case bandpass:
		return(f2);
	case highpass:
	default:
		throw SeisppError(
			string("fmax not defined for filter type")
				+ this->filter_spec);
	}
}
int TimeInvariantFilter::fmin_poles()
{
	switch (type)
	{
	case highpass:
	case lowpass:
	case bandpass:
		return(npole1);
	default:
		throw SeisppError(
			string("number poles for fmin corner not defined for filter type")
				+ this->filter_spec);
	}
}
int TimeInvariantFilter::fmax_poles()
{
	switch (type)
	{
	case highpass:
	case lowpass:
	case bandpass:
		return(npole2);
	default:
		throw SeisppError(
			string("number poles for fmax corner not defined for filter type")
				+ this->filter_spec);
	}
}
string TimeInvariantFilter::type_description(bool verbose)
{
	if(!verbose) return(filter_spec);

	string retstr;
	switch (type)
	{
	case highpass:
		retstr=string("highpass: ") + filter_spec;
		break;
	case lowpass:
		retstr=string("lowpass: ")+filter_spec;
		break;
	case bandpass:
		retstr=string("bandpass: ")+filter_spec;
		break;
	case WAA:
		retstr=string("Wood-Anderson");
		break;
	case WAV:
		retstr=string("Wood-Anderson Velocity");
		break;
	case WAD:
		retstr=string("Wood-Anderson Displacement");
		break;
	case DIF:
		retstr=string("First Derivative");
		break;
	case DIF2:
		retstr=string("Second Derivative");
		break;
	case INT:
		retstr=string("Integrator");
		break;
	case INT2:
		retstr=string("Double Integrator");
		break;
	case DEMEAN:
		retstr=string("Mean Removal filter");
		break;
	case none:
	default:
		retstr=string("no filter");
	}
	return(retstr);
}
// apply to simple float vector of length ns and sample rate dt
void TimeInvariantFilter::apply(int ns, float *s,double dt)
{
	if(type==none) return;
	if(trfilter_segs(1,&ns,&dt,&s,const_cast<char*>(filter_spec.c_str()))<0)
			throw SeisppError(string("Error in trfilter_segs"));
}
// same as above for array of doubles
void TimeInvariantFilter::apply(int ns, double *s,double dt)
{
	if(type==none) return;
	int i;
	float *d=new float[ns];
	for(i=0;i<ns;++i) d[i]=static_cast<float>(s[i]);
	if(trfilter_segs(1,&ns,&dt,&d,const_cast<char*>(filter_spec.c_str()))<0)
			throw SeisppError(string("Error in trfilter_segs"));
	for(i=0;i<ns;++i) s[i]=static_cast<double>(d[i]);
	delete [] d;
}
void TimeInvariantFilter::apply(TimeSeries& ts)
{
	if(type==none) return;
	if(!ts.live) return;
	int i;
	float *d=new float[ts.ns];
	for(i=0;i<ts.ns;++i) d[i]=static_cast<float>(ts.s[i]);
	if(trfilter_segs(1,&(ts.ns),&(ts.dt),&d,const_cast<char*>(filter_spec.c_str()))<0)
			throw SeisppError(string("Error in trfilter_segs"));
	for(i=0;i<ts.ns;++i) ts.s[i]=static_cast<double>(d[i]);
	// Append this filter name to Metadata part of TimeSeries.
	ts.append_string(string("filter_spec"),string("; "),filter_spec);

	delete [] d;
}
void TimeInvariantFilter::apply(ThreeComponentSeismogram& ts)
{
	if(type==none) return;
	if(!ts.live) return;
	int i,j;
	float *d=new float[ts.ns];
	for(j=0;j<3;++j)
	{
		for(i=0;i<ts.ns;++i) d[i]=static_cast<float>(ts.u(j,i));
		if(trfilter_segs(1,&(ts.ns),&(ts.dt),&d,const_cast<char*>(filter_spec.c_str()))<0)
			throw SeisppError(string("Error in trfilter_segs"));
		for(i=0;i<ts.ns;++i) ts.u(j,i)=static_cast<double>(d[i]);
	}
	delete [] d;
	// Append this filter name to Metadata part of object
	ts.append_string(string("filter_spec"),string("; "),filter_spec);
}
/* Small helper for routine below.   We use pass by reference for 
   efficiency.   dr is the time reversed version of d */
void time_reverse_vector(vector<double>& d, vector<double>& dr)
{
  vector<double>::iterator dptr,drptr;
  /* We initialize the reverse iterator outside the for loop 
     because we want it to start at end()-1. */
  drptr=dr.end();
  --dptr;
  for(dptr=d.begin();dptr!=d.end();++dptr,--drptr) *drptr = *dptr;
}
void TimeInvariantFilter::zerophase(TimeSeries& ts)
{
  const string base_error("TimeInvariantFilter::zerophase method:  ");
  int i;
  vector<double> sr;
  /* sr has to be initialized */
  for(i=0;i<ts.ns;++i) sr.push_back(0.0);
  switch (this->type)
  {
    case highpass:
    case lowpass:
    case bandpass:
     time_reverse_vector(ts.s,sr); 
     try{
       this->apply(ts.ns,&(sr[0]),ts.dt);
     }catch(SeisppError& serr){throw serr;};
     time_reverse_vector(sr,ts.s);
     break;
    default:
      throw SeisppError(base_error + "Cannot run zerophase version of filter "
          + filter_spec);
  };
}
void TimeInvariantFilter::zerophase(ThreeComponentSeismogram& tcs)
{
  const string base_error("TimeInvariantFilter::zerophase method:  ");
  int i,ii,k;
  vector<double> sr;
  sr.reserve(tcs.ns);
  switch (this->type)
  {
    case highpass:
    case lowpass:
    case bandpass:
     for(k=0;k<3;++k)
     {
       sr.clear();
       for(i=0,ii=tcs.ns-1;i<tcs.ns;++i) sr.push_back(tcs.u(k,ii));
       try{
         this->apply(tcs.ns,&(sr[0]),tcs.dt);
       }catch(SeisppError& serr){throw serr;};
       for(i=0,ii=tcs.ns-1;i<tcs.ns;++i) tcs.u(k,ii)=sr[i];
     }
     break;
    default:
      throw SeisppError(base_error + "Cannot run zerophase version of filter "
          + filter_spec);
  };
}
#ifndef NO_ANTELOPE
void TimeInvariantFilter::apply(Dbptr tr)
{
	if(type==none) return;
	if(trfilter(tr,const_cast<char*>(filter_spec.c_str()))<0)
		throw SeisppError(string("Error in trfilter"));
}
#endif

// helpers for ensembles.  There is probably a way to do this with templates,
// but it isn't that much code
void FilterEnsemble(TimeSeriesEnsemble& ensemble,TimeInvariantFilter& filter)
{
	if(filter.type == none) return;
	try 
	{
	    for(int i=0;i<ensemble.member.size();++i)
		if(ensemble.member[i].live)filter.apply(ensemble.member[i]);
	} catch (...) {throw;};
}
void FilterEnsemble(ThreeComponentEnsemble& ensemble,TimeInvariantFilter& filter)
{
	if(filter.type == none) return;
	try 
	{
	    for(int i=0;i<ensemble.member.size();++i)
		if(ensemble.member[i].live)filter.apply(ensemble.member[i]);
	} catch (...) {throw;};
}

}  // end namespace encapsulation for SEISPP
