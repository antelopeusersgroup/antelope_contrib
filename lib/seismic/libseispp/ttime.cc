
// These are methods for the Hypocenter object to compute common standard things.`
// These are mostly interface routines to Antelopes coords library.
// Note all returns are distances and angles are returned in units of radians.  

#include "coords.h"
#include "tt.h"
#include "seispp.h"

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
	free(method);
	free(model);
	method=strdup(meth.c_str());
	mod=strdup(meth.c_str());
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
		throw(seispp_error)
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
	ierr = ttcalc(method,model,
			(char *)phase.c_str(),0,&p,&tt,&h);
	if(ierr) 
	{
		string mess;
		mess = tterror_code_translation(ierr);
		freetbl(tt,0);
		free_hook(&h);
		throw seispp_error(mess);
	}
	else
	{
		t=(TTTime *)gettbl(tt,0);
		if(t==NULL) 
			throw seispp_error("ttcalc function returned an empty list of times");
	}	
	// this temporary is needed to prevent as small leak
	double tret = t->value;
	freetbl(tt,0);
	free_hook(&h);
	return(tret);
}

double Hypocenter::ptime(double lat0, double lon0, double elev)
		throw(seispp_error)
{
	string phs="P";
	double time;

	try{
		time = this->phasetime(lat0,lon0,elev,phs);
	} catch (seispp_error tte)
	{
		throw tte;
	}
	return(time);
}
Slowness_vector  Hypocenter::phaseslow(double lat0, double lon0, double elev, string phase)
		throw(seispp_error)
{
	TTGeometry p;
	int ierr;
	Tbl *tt=NULL;
	Hook *h=NULL;  // forced to be released on each call.  `
	TTSlow *u;
	Slowness_vector uout;

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
	ierr = ucalc(method,model,(char *)phase.c_str(),0,&p,&tt,&h);
	if(ierr) 
	{
		string mess;
		mess = tterror_code_translation(ierr);
		freetbl(tt,0);
		free_hook(&h);
		throw seispp_error(mess);
	}
	else
	{
		u=(TTSlow *)gettbl(tt,0);
		freetbl(tt,0);
		free_hook(&h);
		if(u==NULL) 
			throw seispp_error("Returned list of computed slowness vectors was empty");
	}	
	uout.ux=u->ux;
	uout.uy=u->uy;
	freetbl(tt,0);
	free_hook(&h);
	return(uout);
}
Slowness_vector Hypocenter::pslow(double lat0, double lon0, double elev)
		throw(seispp_error)
{
	string phs="P";
	Slowness_vector u;

	try{
		u = this->phaseslow(lat0,lon0,elev,phs);
	} catch (seispp_error tte)
	{
		throw tte;
	}
	return(u);
}
