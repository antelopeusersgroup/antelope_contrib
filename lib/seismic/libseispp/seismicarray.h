#ifndef _SEISMICARRAY_H_
#define _SEISMICARRAY_H_
#include <string>
#include <map>
#include <list>
#include "TimeWindow.h"
#include "Metadata.h"
#include "databasehandle.h"
#include "Hypocenter.h"
#include "ensemble.h"
#include "resample.h"

namespace SEISPP{
using namespace std;
using namespace SEISPP;

//@{
// Useful typedef to produce a simple map of times associated with
// station names used as a key.
//@}
typedef map<string,double> StationTime;

//@{
// Object to encapsulate data commonly found in css3.0 site table.
//@}
class SeismicStationLocation
{
public:
//@{
// Latitude (in radians) of station.
//@}
	double lat;
//@{
// Longitude (in radians) of station.
//@}
	double lon;
//@{
// Elevation (in kilometers) of station.
//@}
	double elev;
//@{
// Station code used to uniquely define this station.
//@}
	string name;
//@{
// SEED network code for this station.
//@}
	string net;
//@{
// Distance north from lat,lon of reference position.  Used mainly for 
// phased arrays.  
//@}
	double dnorth;
//@{
// Distance east from lat,lon of reference position.  
//@}
	double deast;
//@{
//  Station name of reference stations (used only when dnorth and deast are used).
//@}
	string refsta; 
//@{
// Default constructor.  Initializes all to zero or null strings.
//@}
	SeismicStationLocation();
//@{
// Completely parameterized constructor.  Parameters associations should
// be obvious.
//@}
	SeismicStationLocation(double lat0, double lon0,
		double elev0, double dn0, double de0, string name0,
		string net0, string refsta0);

//@{
// Standard copy constructor.
//@}
	SeismicStationLocation(const SeismicStationLocation& sta);
//@{
// Standard assignment operator.
//@}
	SeismicStationLocation& operator=(const SeismicStationLocation&);
};
//@{
// Object to encapsulate concept of a group of stations (array) that have
// some generic relationship such that they can be viewed as a useful
// entity collectively.  This means what we call seismic networks, seismic
// (phased) array, and virtual seismic networks.  
//@}
class SeismicArray
{
public:
//@{
// Name associated with the array entity.  
//@}
	string name;
//@{
// Coordinates of receivers in this array are indexed by a key.
// In the current implementation the key is like the Antelope srcid
// used in the orb.  That is, the key is net_sta where net is the net
// code for this station and sta is the station code.  We do not implement
// loc codes as that is channel issue not directly related to station
// geometry, which is what this object is all about.  
// This variable thus holds receiver geometry in an the associative
// array defined by the STL map container.  
//@}
	map<string,SeismicStationLocation> array;
//@{
//  Default constructor.  Sets name UNDEFINED and defines an empty receiver geometry.
//@}
	SeismicArray();
//@{
//  Constructs the object from a database.  Loads all stations marked as
//  on at the time defined by the time argument.  Note this can be ambiguous
//  if one is interested in long time spans.  We use only a single time 
//  time instead of a time interval to avoid the serious complications this
//  causes.  For analyzing event data this should not be an issue.  This 
//  object is not for examining geometry changes in time.  
//
//@param dbh is a handle to a database (currently only DatascopeHandle is supported)
//   from which the table is to be loaded.
//@param time is the epoch time for which geometry is requested.  All stations
//   open at the time defined will be loaded.  
//@param netname is network name assigned to the result.  It can be a single SEED
//   net code if that is appropriate, but it is probably 
//  best viewed as a virtual network name.
//@}
	SeismicArray(DatabaseHandle& dbh,double time, string netname);
//@{
//  Constructs the object from a database, but load only stations from a specified
//  list.  This is essential a way to construct a virtual network with the name
//  of the result being set to the netname parameter.
//
//@param dbh is a handle to a database (currently only DatascopeHandle is supported)
//   from which the table is to be loaded.
//@param time is the epoch time for which geometry is requested.  All stations
//   open at the time defined will be loaded. 
//@param netname is the name that will be assigned to the result.
//@param sta_to_use is a list of station names to be loaded.  Only stations in that
//   list will be loaded into the object definition.
//   It is VERY IMPORTANT to note this list must define the keys used to index
//   the station geometry container (see above).  In this implementation that
//   means the key must be of the form net_sta. 
//@}
	SeismicArray(DatabaseHandle& dbh, double time, string netname, 
		list<string> sta_to_use);
//@{
//  Standard copy constructor.
//@}
	SeismicArray(const SeismicArray& sa);
//@{
//  Standard assignment operator
//@}
	SeismicArray& operator=(const SeismicArray&);
//@{
// Seismic station geometry is commonly dynamic.  Stations do not exist for
// all times but only exist for finite time intervals.  This object keeps track
// of the range of times stations it contains are valid.  If any of the stations
// are marked off during any portion of the time interval defined by test this
// method returns false.  In that situation the caller should refresh the object
// with a call to the constructor for a time within the test interval.
//@}
	bool GeometryIsValid(TimeWindow test);
private:
	TimeWindow valid_time_interval;
};
/* ***************End Class Declarations -- Begin helper procedures ***********/

//@{
// Computes predicted arrival times for a given phase at all stations
// defined for an input array.  
//
//@param stations object defining station geometry.
//@param hypo object defining source coordinates.
//@param phase seismic phase to compute predicted arrival times.
//
//@returns map keyed by station named containing predicted arrival times
//   for requested phase.
//@author Gary L. Pavlis
//@}
StationTime ArrayPredictedArrivals(SeismicArray& stations,
                Hypocenter& hypo, string phase);

//@{
// Reads a block of data defined by a time window derived from
// input specifications and a time range computed from a set of theoretical
// arrival times.  
//
// The algorithm used to define the time window is this:
// start = atmin + data_window.start - tpad while
// end = atmax + data_window.end + tpad.  In this relation atmin and
// atmax are computed minimum and maximum arrival times of all stations
// defined by the input receiver geometry.  data_window defines
// the base time window needed around each arrival time and tpad is a
// necessary fudge factor to allow for errors in the predicted arrival time
// and extra time needed to remove filter endpoint transients.  
// The algorithm keys on a chan string to select data from the database.
// Only 6 values are allowed for chan: Z,N,E,L,R, and T.  If anything
// else is passed through chan the functio will throw a SeisppError exception.
// Z,N, or E are assumed cardinal directions.  They generate subsets of the 
// input defined by SEED channel codes.  The result will contain ALL Z,N, or
// E channels found in the requested time windows.  For example, if a station
// has HHZ, BHZ, and LHZ channels the output will contain three members from
// that station.  It is the callers job to sort out any redundancy this 
// will produce.  When chan is L,R, or Z the data is assumed to be only 
// from three component stations.  All single channel stations will be 
// dropped automatically.  Like channels are assembled into 3c seismogram
// objects.  We then apply the free surface transformation to longitudinal
// (L), radial (R), and transverse (T) coordinates.  The requested channel
// is extracted and pushed into the output ensemble.  Like the cardinal
// direction version (Z,N,E) the output may contain multiple channels for
// the same station with different sample rates.  As before this means
// this has to be sorted out for the output.  In both cases the only
// safe test is the dt variable of each member.  
//
//@returns a pointer to a newly allocated TimeSeriesEnsemble containing all 
//   data requested.  Since this is effectively a constructor this means
//   the caller is responsible for handling this memory block.  Save the
//   result in an auto_ptr or make sure you manage this memory carefully
//   as this object can easily get huge.  This is why we return a pointer
//   instead of the object itself.
//@throws SeisppError object for one of several possible unrecoverable
//   errors.  If this routine ever throws an exception the output is,
//   of course, invalid by definition.   
//
//@param stations Receiver geometry definition through SeismicArray object.
//@param hypo Source geometry definition through Hypocenter object.
//@param phase Theoretical arrival times are computed for this seismic phase name.
//@param chan channel code definition (see above for important constraints on this)
//@param data_window base time window for required output.  View this as the 
//    minimum time window desired around each arrival.
//@param tpad extra time pad (in second) on each end of the window to be
//    read.  This needs to be large enough to mute out filter startup 
//    transients for simple filters and decimation filters when resampling is
//    required.
//@param generalized database handle to read data.  At present this is immediately
//    converted to a DatascopeHandle as the methods used use the Antelope API.
//@param ensemble_mdl list of data to be read from the database and placed in
//    the metadata area for the full ensemble.
//@param member_mdl list of attributes to be taken from the database for each
//    data member (seismogram).
//@param am AttributeMap object defining mapping from database external namespace
//    to internal namespace.  
//@}
TimeSeriesEnsemble *array_get_data(SeismicArray& stations, Hypocenter& hypo,
	string phase, string chan, TimeWindow data_window, double tpad,
	DatabaseHandle& dbh, MetadataList& ensemble_mdl, MetadataList& member_mdl,
	AttributeMap& am);
//@{
// Takes an ensemble of data with potentially irregular sample
// rates and irregular start times and returns a gather with 
// uniform sample rates and in relative time reference frame
// defined by an input map.
//
// This is a relatively high level function to produce a data gather
// with three useful properties:  (1) all the data can be assumed to
// be at a common sample rate, although not on sample by sample
// common time base like multichannel data (i.e. t0 can vary at least
// within + or - one sample); (2) the input data are assumed on an
// absolute time base, but the output is in a relative time base with
// zero times defined by the StationTimes map object;  and (3) the 
// result is "clean" in the sense that one can assume all data are
// associated with a known station and were cleanly handled in the 
// processing to build this gather.  Clean, however, does NOT 
// mean that the data are gap free.  If there are marked data gaps
// in the raw data they will be quietly copied to the result and
// the user is expected to deal with them as appropriate for a given
// algorithm.  
//
//  This function tries to catch any errors found in processing with
// the when in doubt throw it out attitude.  i.e. the result can be
// expected to frequently be smaller than then input.
//
//@throws MetadataError if any member of the input data does not 
//   have the sta or chan attribute defined.
//@param raw contains the base data.  It is assumed to be in an
//  absolute time reference frame.  The metadata attributes  sta 
//  and chan must be defined or this function will throw a MetadataError
//  exception.
//@param times is a map keyed by station.  It's contents are used to
//  convert from absolute to relative time.  The most common example
//  of this would be measured or predicted arrival times.  For this
//  reason the results are written to the input and output gather
//  as the field "predarr.time".   
//@param phase is the phase name to be assigned to the predarr.time
//  field.  See LoadPredictedArrivalTimes for how this is used.
//@param result_twin is the time window definition for times
//  relative to the times contained in the "times" StationTime object.
//@param target_dt is the target sample rate for the output data.  All
//  data returned will be sampled at this rate.
//@param rsdef is the ResamplingDefinitions object passed to the
//  resample function used to convert data to a common sample rate.
//  see ResampleTimeSeries page or resample(3) for details.
//@param trim is a boolean that controls edge transients in resampling.
//  if true resample results will be truncated on the left and right
//  to eliminate edge transients in using the FIR filters used for 
//  decimation.
//@}
TimeSeriesEnsemble *AssembleRegularGather(TimeSeriesEnsemble& raw,
	StationTime& times,
		string phase,
			TimeWindow result_twin,
				double target_dt,
					ResamplingDefinitions& rsdef,
						bool trim);

//@{ Scans an array of times to return the range spanned by the group.
//   This is a simple algorithm complicated by the fact that the 
// input is an STL map object with station names keying a set of times.
// This function simply scans the contents of the map to find the maximum
// and mininum times found.  It returns this range as a TimeWindow
// object.
//
//@param times is an STL map containing times keyed by station name.
//@returns TimeInterval of the range of times found.
//@}
TimeWindow StationTimeRange(StationTime& times);
//@{ Template for use by either a TimeSeries or ThreeComponent Ensemble.
//It loads arrival times defined in the times map into the individual
//station metadata area using the keyword defined by key.
//The phase name is loaded into each trace's metadata area using the
//key "assoc.phase" to be consistent with the common css3.0 database
//definitions.
//
//Function prints a message if any station in the gather has no defined
//time in the input.  Caller must be aware of this as the result is
//not set for stations that contain that error.  The result should always
//be viewed as potentially incomplete.
//
//@param ensemble is the input data to be processed
//@param times is a map of station names containing times to be inserted
//   into the metadata area for each member trace of the input ensemble.
//@param key is the keyword used to store the contents of times for each station.
//@param phase is the phase name to be stored under the "assoc.time" metadata key.
//
//@}

template <class T>
void LoadPredictedTimes(T& ensemble, StationTime& times,string key,string phase);

} // End Namespace SEISPP declaration
#endif
