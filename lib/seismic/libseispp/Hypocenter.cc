// These are methods for the Hypocenter object to compute common standard things.`
// These are mostly interface routines to Antelopes coords library.
// Note all returns are distances and angles are returned in units of radians.  

#include "coords.h"
#include "stock.h"
#include "tt.h"
#include "Hypocenter.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP
{

// copy constructor and = operators commonly are implicitly needed
Hypocenter::Hypocenter(const Hypocenter& h0)
{
	lat = h0.lat;
	lon = h0.lon;
	z = h0.z;
	time = h0.time;
	method = h0.method;
	model = h0.model;
}
Hypocenter& Hypocenter::operator=(const Hypocenter& h0)
{
    if(this!=&h0)
    {
	lat = h0.lat;
	lon = h0.lon;
	z = h0.z;
	time = h0.time;
	method = h0.method;
	model = h0.model;
    }
    return(*this);
}
// This constructor creates a Hypocenter object using parameters
// read from a Metadata object.  It would be useful to have a 
// mechanism to not have a frozen set of names used to fetch
// the required parameters but I'm not sure how to do that
// without making the interface clumsy. 
//
// Note that things like TimeSeries objects can be used to create
// a Hypocenter through this mechanism through the use of a dynamic_cast
//
Hypocenter::Hypocenter(Metadata& md)
{
	try {
		lat=md.get_double("source_lat");
		lon=md.get_double("source_lon");
		z=md.get_double("source_depth");
		time=md.get_double("source_time");
	} catch (MetadataError& mderr) {throw mderr;};
	// We run a separate try block here and recover from
	// model and method not being defined -- a common thing
	// we will probably need
	//
	method=string("tttaup");
	model = string("iasp91");
	try {
		method=md.get_string("TTmethod");
		model=md.get_string("TTmodel");
	} catch (MetadataError& mderr){}
}
// the obvious fully parameterized constructor
Hypocenter::Hypocenter(double lat0, double lon0, double z0, double t0,
		string meth0, string mod0)
{
	lat=lat0;
	lon=lon0;
	z=z0;
	time=t0;
	method=meth0;
	model=mod0;
}
		


double Hypocenter::distance(double lat0, double lon0)
{
	double epidist, az;

	dist(lat,lon,lat0,lon0,&epidist, &az);
	return(epidist);
}

double Hypocenter::esaz(double lat0, double lon0)
{
	double epidist, az;

	dist(lat,lon,lat0,lon0,&epidist, &az);
	return(az);
}
double Hypocenter::seaz(double lat0, double lon0)
{
	double epidist, az;

	dist(lat0,lon0,lat,lon,&epidist, &az);
	return(az);
}

void Hypocenter::tt_setup(string meth, string mod)
{
	method=meth;
	model=mod;
}
// companion to below made a function to allow use in both tt and slowness calculations
string tterror_code_translation(int ierr)
{
	string s;

	switch (ierr)
	{
	case -1:
		s="Phase name not specified";
		break;
	case -2:
		s="Error in parameters passed to travel time function";
		break;
	case -3:
		s="Requested travel time method function not found";
		break;
	case -4:
		s="Requested slowness vector compute  method function not found";
		break;
	case -5:
		s="Requested model unknown";
		break;
	case 1:
		s="requested derivatives but calculation failed";
		break;
	case 2:
		s="requested station corrections be applied but corrections aren't available";
		break;
	default:
		s="travel time interface function return unknown error code";
	}
	return s;
}

			

double Hypocenter::phasetime(double lat0, double lon0, double elev, string phase)
		throw(SeisppError)
{
	TTGeometry p;
	int ierr;
	Tbl *tt=NULL;
	Hook *h=NULL;  // forced to be released on each call.  `
	TTTime *t;

	// note tt interface uses degrees as units for lat a lon.  I always use radians 
	// internally
	p.source.lat=deg(lat);
	p.source.lon=deg(lon);
	p.source.z=z;
	p.source.time=time;
	p.receiver.lat=deg(lat0);
	p.receiver.lon=deg(lon0);
	p.receiver.z=-elev;
	p.receiver.time=0.0;
	p.source.name[0]='\0';
	p.receiver.name[0]='\0';
	// the 0 in the mode arg means compute only the time
	ierr = ttcalc(const_cast<char *>(method.c_str()),
			const_cast<char *>(model.c_str()),
			const_cast<char *>(phase.c_str()),0,&p,&tt,&h);
	if(ierr) 
	{
		string mess;
		mess = tterror_code_translation(ierr);
		freetbl(tt,0);
		free_hook(&h);
		throw SeisppError(mess);
	}
	else
	{
		t=(TTTime *)gettbl(tt,0);
		if(t==NULL) 
			throw SeisppError("ttcalc function returned an empty list of times");
	}	
	// this temporary is needed to prevent as small leak
	double tret = t->value;
	freetbl(tt,0);
	free_hook(&h);
	return(tret);
}

double Hypocenter::ptime(double lat0, double lon0, double elev)
		throw(SeisppError)
{
	string phs="P";
	double ttime;

	try{
		ttime = this->phasetime(lat0,lon0,elev,phs);
	} catch (SeisppError& tte)
	{
		throw tte;
	}
	return(ttime);
}
SlownessVector  Hypocenter::phaseslow(double lat0, double lon0, double elev, string phase)
		throw(SeisppError)
{
	TTGeometry p;
	int ierr;
	Tbl *tt=NULL;
	Hook *h=NULL;  // forced to be released on each call.  `
	TTSlow *u;
	SlownessVector uout;

	// note tt interface uses degrees as units for lat a lon.  I always use radians 
	// internally
	p.source.lat=deg(lat);
	p.source.lon=deg(lon);
	p.source.z=z;
	p.source.time=time;
	p.receiver.lat=deg(lat0);
	p.receiver.lon=deg(lon0);
	p.receiver.z=-elev;
	p.receiver.time=0.0;
	p.source.name[0]='\0';
	p.receiver.name[0]='\0';
	// the 0 in the mode arg means compute only the time
	ierr = ucalc(const_cast<char *>(method.c_str()),
		const_cast<char *>(model.c_str()),
		const_cast<char *>(phase.c_str()),0,&p,&tt,&h);
	if(ierr) 
	{
		string mess;
		mess = tterror_code_translation(ierr);
		freetbl(tt,0);
		free_hook(&h);
		throw SeisppError(mess);
	}
	else
	{
		u=(TTSlow *)gettbl(tt,0);
		if(u==NULL) 
                {
                    freetbl(tt,0);
                    free_hook(&h);
		    throw SeisppError("Returned list of computed slowness vectors was empty");
                }
		uout.ux=u->ux;
		uout.uy=u->uy;
		freetbl(tt,0);
		free_hook(&h);
	}	
	return(uout);
}
SlownessVector Hypocenter::pslow(double lat0, double lon0, double elev)
		throw(SeisppError)
{
	string phs="P";
	SlownessVector u;

	try{
		u = this->phaseslow(lat0,lon0,elev,phs);
	} catch (SeisppError& tte)
	{
		throw tte;
	}
	return(u);
}
} // Termination of namespace SEISPP definitions

