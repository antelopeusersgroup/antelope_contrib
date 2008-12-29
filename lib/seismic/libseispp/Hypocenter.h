#ifndef _HYPOCENTER_H_
#define _HYPOCENTER_H_
#include <string>
#include "tt.h"
#include "Metadata.h"
#include "SeisppError.h"
#include "slowness.h"
namespace SEISPP 
{
/*! \brief Defines a source position and a set of useful methods with which a source
position can be associated.

Seismic processing is commonly revolved around geometry defined by source and
receiver positions.  This object encapsulates information about a source and 
provides a set of methods useful for waveform processing that depends on
source information.  
\author Gary L. Pavlis
**/
class Hypocenter
{
public:
	/*!
	// Latitude of the source in radians.
	**/
	double lat;
	/*!
	// Longitude of the source in radians.
	**/
	double lon;
	/*!
	// Depth below sea level of the source in kilometers.
	**/
	double z;
	/*!
	// Origin time of the source in seconds.  If absolute time is being used
	// this is an epoch time.
	**/
	double time;
	//* Default constructor.  Defines travel time interface, but not source coordinates.*/
	Hypocenter(){method=string("tttaup"); model=string("iasp91");};  // default
	/*!
	// Metadata object driven constructor.  Looks for these keywords:
	//  origin.lat, origin.lon, origin.depth, and origin.time.  
	// Note it is assumed the latitude and longitude values contained in 
	// the parent metadata are given already in radians.  
	**/
	Hypocenter(Metadata& );
	/*!
	// Fully parameterized constructor fill in all data members of
	// Hypocenter object.  
	//
	//\param lat0 latitude of the source (in radians)
	//\param lon0 longitude of the source (in radians)
	//\param z0 depth of the source in km
	//\param t0 orign time of the source (epoch time).
	//\param meth0 method to be assigned as travel to define travel time calculations
	//\param mod0 earth model name to use for travel time calculations.

	**/
	Hypocenter(double lat0, double lon0, double z0, double t0,
                string meth0, string mod0);
	//* Standard copy constructor */
	Hypocenter(const Hypocenter&);
	//* Standard assignment operator.*/
	Hypocenter& operator=(const Hypocenter&);
	//* Compute distance from source to surface position (lat0,lon0).*/
	double distance(double lat0, double lon0);
	/*!
	// Compute event to station azimuth.  
	// Event to source azimuth is defined as the azimuth of the great 
	// circle path joining event and station at the position of the 
	// source and looking toward the station.
	//\param lat0 latitude of the station
	//\param lon0 longitude of the station
	**/
	double esaz(double lat0, double lon0);
	/*!
	// Compute station to event azimuth.
	// Station to event azimuth is defined as the azimuth of the
	// great circle path joining event and station but evaluated
	// at the position of the station and looking back toward 
	// the source.  
	//\param lat0 latitude of the station
	//\param lon0 longitude of the station
	**/
	double seaz(double lat0, double lon0);
	/*!
	// Compute the P wave arrival time using the currently defined travel time
	// calculator.  
	//\exception SeisppError object when travel time calculator fails for any reason
	//
	//\param lat0 latitude of the station
	//\param lon0 longitude of the station
	//\param elev elevation of the station relative to sea level
	**/
	double ptime(double lat0, double lon0, double elev)
		throw(SeisppError);
	/*!
	// Compute the P wave slowness vector using the currently defined travel time
	// calculator.  
	//\exception SeisppError object when travel time calculator fails for any reason
	//
	//\param lat0 latitude of the station
	//\param lon0 longitude of the station
	//\param elev elevation of the station relative to sea level
	**/
	SlownessVector pslow(double lat0, double lon0, double elev)
		throw(SeisppError);
	/*!
	// Compute the predicted arrival time of a requested phase
	// using the currently defined travel time
	// calculator.  
	//\exception SeisppError object when travel time calculator fails for any reason
	//
	//\param lat0 latitude of the station
	//\param lon0 longitude of the station
	//\param elev elevation of the station relative to sea level
	//\param phase name of phase for which arrival time is requested.
	**/
	double phasetime(double lat0, double lon0, double elev, string phase)
		throw(SeisppError);
	/*!
	// Compute the predicted slowness vector for a 
	// specified seismic phase using the currently defined travel time
	// calculator.  
	//\exception SeisppError object when travel time calculator fails for any reason
	//
	//\param lat0 latitude of the station
	//\param lon0 longitude of the station
	//\param elev elevation of the station relative to sea level
	//\param phase name of phase for which slowness estimate is requested.
	**/
	SlownessVector phaseslow(double lat0, double lon0, double elev, 
			string phase) throw(SeisppError);
	/*!
	// Initialize the travel time calculator or change travel time model or method.
	// This function can be used to alter the model or method used to 
	// compute travel times and/or slowness vectors with this
	// object.  The object uses the Antelope ttime library that allows
	// runtime changes in the method and Earth model.  This 
	// function is used to invoke that type of change in the object's \
	// behaviour.
	//
	//\param meth set the travel time method to meth
	//\param mod set the earth model to that with the name mod.
	**/
	void tt_setup(string meth, string mod); // change default method:model
	/*! Return a string that describes the method used to compute travel times.
	*
	* The hypocenter object used here contains an embedded definition of
	* a travel time calculator.  The object interface requires a way to 
	* return a string that uniquely defines the method and earth model
	* used to compute these arrival times. */
	string tt_definition(){return(method+":"+model);};
private:
	string method;
	string model;
};

} // End SEISPP namespace declaration
#endif
