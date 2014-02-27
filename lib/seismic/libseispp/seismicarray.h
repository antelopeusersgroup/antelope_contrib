#ifndef _SEISMICARRAY_H_
#define _SEISMICARRAY_H_
#include <string>
#include <map>
#include <list>
#include "TimeWindow.h"
#include "StationChannelMap.h"
#include "Metadata.h"
#ifndef NO_ANTELOPE
#include "databasehandle.h"
#endif
#include "Hypocenter.h"
#include "ensemble.h"
#include "resample.h"

namespace SEISPP{
using namespace std;
using namespace SEISPP;

/*! Useful typedef to produce a simple map of times associated with
station names used as a key.
**/
typedef map<string,double> StationTime;

/*! \brief Object to encapsulate receiver location data commonly found in css3.0 site table.
**/
class SeismicStationLocation
{
public:
/*!
// Latitude (in radians) of station.
**/
	double lat;
/*!
// Longitude (in radians) of station.
**/
	double lon;
/*!
// Elevation (in kilometers) of station.
**/
	double elev;
/*!
// Station code used to uniquely define this station.
**/
	string name;
/*!
// SEED network code for this station.
**/
	string net;
/*!
// Distance north from lat,lon of reference position.  Used mainly for 
// phased arrays.  
**/
	double dnorth;
/*!
// Distance east from lat,lon of reference position.  
**/
	double deast;
/*!
//  Station name of reference stations (used only when dnorth and deast are used).
**/
	string refsta; 
/*!
// Default constructor.  Initializes all to zero or null strings.
**/
	SeismicStationLocation();
/*! Completely parameterized constructor.  

Parameters associations should be obvious.
**/
	SeismicStationLocation(double lat0, double lon0,
		double elev0, double dn0, double de0, string name0,
		string net0, string refsta0);

/*! Standard copy constructor.
**/
	SeismicStationLocation(const SeismicStationLocation& sta);
/*! Standard assignment operator.
**/
	SeismicStationLocation& operator=(const SeismicStationLocation&);
};
/*! \brief Receiver array geometry.

Object to encapsulate concept of a group of stations (array) that have
some generic relationship such that they can be viewed as a useful
entity collectively.  This means what we call seismic networks, seismic
(phased) array, and virtual seismic networks.  
**/
class SeismicArray
{
public:
/*! Name associated with the array entity.  
**/
	string name;
/*!Coordinates of receivers in this array are indexed by a key.

// In the current implementation the key is the station name is used
// as the key alone.  This assumes the current database approach in
// antelope which uses a special table (snetsta) to define mapping
// between the database and foreign naming conventions.  We use 
// the Antelope (css) convention internally.
// This variable thus holds receiver geometry in an the associative
// array defined by the STL map container keyed by station name.  
**/
	map<string,SeismicStationLocation> array;
/*! Default constructor.  

Sets name UNDEFINED and defines an empty receiver geometry.
**/
	SeismicArray();
#ifndef NO_ANTELOPE
/*! Constructs the object from a database.  

Loads all stations marked as
on at the time defined by the time argument.  Note this can be ambiguous
if one is interested in long time spans.  We use only a single time 
time instead of a time interval to avoid the serious complications this
causes.  For analyzing event data this should not be an issue.  This 
object is not for examining geometry changes in time.  

\param dbh is a handle to a database (currently only DatascopeHandle is supported)
   from which the table is to be loaded.
\param time is the epoch time for which geometry is requested.  All stations
   open at the time defined will be loaded.  
\param netname is network name assigned to the result.  It can be a single SEED
   net code if that is appropriate, but it is probably 
  best viewed as a virtual network name.

\exception SeisppError is throw if there are no live stations
  found at the requested time. 
**/
	SeismicArray(DatabaseHandle& dbh,double time, string netname);
/*! Constructs the object from a database, but load only stations from a specified
list.  

This is essential a way to construct a virtual network with the name
  of the result being set to the netname parameter.

\param dbh is a handle to a database (currently only DatascopeHandle is supported)
   from which the table is to be loaded.
\param time is the epoch time for which geometry is requested.  All stations
   open at the time defined will be loaded. 
\param netname is the name that will be assigned to the result.
\param sta_to_use is a list of station names to be loaded.  Only stations in that
   list will be loaded into the object definition.
   It is VERY IMPORTANT to note this list must define the keys used to index
   the station geometry container (see above).  In this implementation that
   means the key must be of the form net_sta. 
**/
	SeismicArray(DatabaseHandle& dbh, double time, string netname, 
		list<string> sta_to_use);
#endif
/*! Constructs the object from an ascii file.

  When a database is not available it is convenient to construct this
  object from a simple ascii file.   This constructor does that for 
  simple ascii file structure that is linked to the object.   Two formats
  are currently supported:   form="ascii_table_with_pf" and 
  form="simple_ascii_table".   They differ only in how the array name and
  valid time window are obtained.   In the pf version these are extracted from
  a required filed derived from the name parameter as "name.pf".  Both formats
  assume the data table is in a file "name.dat".   In the "simple" version the
  name the file name and the time span is assumed to be effectively infinite. 

  \param name - is the base file name that contains data. This constructor
     always looks for a file name+".dat" and with the pf format looks for 
     name+".pf" as well.   
  \parm form - is a name assigned to the format.  Currently only support formats
     noted above.
  \exception - throws a SeisppError exception with an expanatory message if the
     constructor fails.  
  */
        SeismicArray(string name,string form="simple_ascii_table");
/*!
  Standard copy constructor.
**/
	SeismicArray(const SeismicArray& sa);
/*!
  Standard assignment operator
**/
	SeismicArray& operator=(const SeismicArray&);
/*!
 Seismic station geometry is commonly dynamic.  

Stations do not exist for
 all times but only exist for finite time intervals.  This object keeps track
 of the range of times stations it contains are valid.  If any of the stations
 are marked off during any portion of the time interval defined by test this
 method returns false.  In that situation the caller should refresh the object
 with a call to the constructor for a time within the test interval.
**/
	bool GeometryIsValid(TimeWindow test);
/*!
  \brief Extract a subarray by name.

  Subarrays are a useful general concept that covers a lot of issues
  in receiver array geometry.  A subarray is a subset of a larger
  array.  This method retrieves a SeismicArray object that is a 
  subset of the parent.  The subset in this implementation has
  no subsets defined within it.  It might be possible to extend this
  but for now it is not implemented.  Individual array subsets
  are indexed by either a name or an integer index.  This method
  retrieves subarray using the name key.  

  Note stations can be defined in a subarray that are not marked
  as operational at the time this array geometry is defined.  In 
  such a situation the subset will silently be truncated.  i.e.
  the result will not contain coordinates for stations not marked live
  at the current time stamp.

  \param subsetname name of array subset to be retrieved.

  \returns SeismicArray of that subset.  

  \throw SeisppError if requested subarray does not exist.
*/
	SeismicArray subset(string subsetname);
/*!
  \brief Extract a subarray by numerical index.

  Subarrays are a useful general concept that covers a lot of issues
  in receiver array geometry.  A subarray is a subset of a larger
  array.  This method retrieves a SeismicArray object that is a 
  subset of the parent.  The subset in this implementation has
  no subsets defined within it.  It might be possible to extend this
  but for now it is not implemented.  Individual array subsets
  are indexed by either a name or an integer index.  This method
  retrieves subarray using a numerical index number.

  Note stations can be defined in a subarray that are not marked
  as operational at the time this array geometry is defined.  In 
  such a situation the subset will silently be truncated.  i.e.
  the result will not contain coordinates for stations not marked live
  at the current time stamp.

  This method exists because there are times when an algorithm needs
  to loop through a sequence of subarrays.  This is more convenient
  than trying to maintain a list of names.  

  Be aware that the user MUST NOT call the LoadSubset method 
  inside a loop over subarrays or the iterators used to 
  provide this capability will become invalid and chaos is
  sure to follow.

  \param nsub numerical index of subarray to extract.

  \returns SeismicArray of the desired subset.  

  \throw SeisppError if the requested index is outside the range of 
	possible subsets.
*/
	SeismicArray subset(int nsub);
/*! \brief Loads a subarray definition.

Appends a subset to this array definition.  

\param saname unique name to be assigned to this subarray.
\param salist list of station names in this subarray.

*/
	void LoadSubarray(string saname, list<string> salist);
/*! Returns the number of subarrays currently defined for 
 this array.
*/
	int number_subarrays();
private:
	TimeWindow valid_time_interval;
	map<string,list<string> > subarrays;
};
/* ***************End Class Declarations -- Begin helper procedures ***********/
/*!
\brief Define subarrays for a network using a parameter file.

Subarrays are a concept encapsulated in the SeismicArray object.
As the name suggests a subarray is a subset of a larger array of receivers.
The SeismicArray object handles this in a general way.  This helper
function defines subarrays for an input SeismicArray object by
using a parameter file description of the subarray geometry. 
Note the SeismicArray object is altered which is why a reference
to the object is required.

\param master is the larger seismic array for which subarrays are to be 
	defined.
\param pf Antelope parameter file to be parsed to define subarrays.


**/
void load_subarrays_from_pf(SeismicArray& master,Pf *pf);
/*! Computes predicted arrival times for a given phase at all stations
 defined for an input array.  

\param stations object defining station geometry.
\param hypo object defining source coordinates.
\param phase seismic phase to compute predicted arrival times.

\return map keyed by station named containing predicted arrival times
   for requested phase.
\author Gary L. Pavlis
**/
StationTime ArrayPredictedArrivals(SeismicArray& stations,
                Hypocenter& hypo, string phase);

/*! \brief Read a block of data in a fixed absolute time window.


 The algorithm used to define the time window is this:
 start = atmin + data_window.start - tpad while
 end = atmax + data_window.end + tpad.  In this relation atmin and
 atmax are computed minimum and maximum arrival times of all stations
 defined by the input receiver geometry.  data_window defines
 the base time window needed around each arrival time and tpad is a
 necessary fudge factor to allow for errors in the predicted arrival time
 and extra time needed to remove filter endpoint transients.  
 The algorithm keys on a chan string to select data from the database.
 This can be applied in one of three ways.  First, a single channel
 code (e.g. BHZ) can be used, in which case only data matching that
 channel code will be returned.  Second, you can use a Datascope
 style regular expression for character strings.  e.g. ..Z would return
 all 3 character channel codes ending in Z.  Finally, you can use
 one of thee allowed single characters:  Z,N,E.  These are expanded
 to an expression match for all SEED channel codes matching that
 pattern including an expansion of Antelope style loc codes 
 (e.g. if you had BHZ, LHZ, HHZ, BHZ_00, and BHZ_01 at a station
 all five would be returned if you specify Z).  
 Z,N, or E are assumed cardinal directions.  
 It is the callers job to sort out any redundancy this 
 will produce.  

\return a pointer to a newly allocated TimeSeriesEnsemble containing all 
   data requested.  Since this is effectively a constructor this means
   the caller is responsible for handling this memory block.  Save the
   result in an auto_ptr or make sure you manage this memory carefully
   as this object can easily get huge.  This is why we return a pointer
   instead of the object itself.
\exception SeisppError object for one of several possible unrecoverable
   errors.  If this routine ever throws an exception the output is,
   of course, invalid by definition.   

\param stations Receiver geometry definition through SeismicArray object.
\param hypo Source geometry definition through Hypocenter object.
\param phase Theoretical arrival times are computed for this seismic phase name.
\param chan channel code definition (see above for important constraints on this)
\param data_window base time window for required output.  View this as the 
    minimum time window desired around each arrival.
\param tpad extra time pad (in second) on each end of the window to be
    read.  This needs to be large enough to mute out filter startup 
    transients for simple filters and decimation filters when resampling is
    required.
\param dbh generalized database handle to read data.  At present this is immediately
    converted to a DatascopeHandle as the methods used use the Antelope API.
\param ensemble_mdl list of data to be read from the database and placed in
    the metadata area for the full ensemble.  
    WARNING:  part of an interface definition that has not  yet been 
    been implemented.  i.e. this argument is ignored.
\param member_mdl list of attributes to be taken from the database for each
    data member (seismogram).
    WARNING:  part of an interface definition that has not  yet been 
    been implemented.  i.e. this argument is ignored.
\param am AttributeMap object defining mapping from database external namespace
    to internal namespace.  
    WARNING:  part of an interface definition that has not  yet been 
    been implemented.  i.e. this argument is ignored.
**/
TimeSeriesEnsemble *array_get_data(SeismicArray& stations, Hypocenter& hypo,
	string phase, string chan, TimeWindow data_window, double tpad,
	DatabaseHandle& dbh, MetadataList& ensemble_mdl, MetadataList& member_mdl,
	AttributeMap& am);
/*! \brief Assemble a gather with uniform sample rates.
 
 Takes an ensemble of data with potentially irregular sample
 rates and irregular start times and returns a gather with 
 uniform sample rates and in relative time reference frame
 defined by an input map.

 This is a relatively high level function to produce a data gather
 with three useful properties:  (1) all the data can be assumed to
 be at a common sample rate, although not on sample by sample
 common time base like multichannel data (i.e. t0 can vary at least
 within + or - one sample); (2) the input data are assumed on an
 absolute time base, but the output is in a relative time base with
 zero times defined by the StationTimes map object;  and (3) the 
 result is "clean" in the sense that one can assume all data are
 associated with a known station and were cleanly handled in the 
 processing to build this gather.  Clean, however, does NOT 
 mean that the data are gap free.  If there are marked data gaps
 in the raw data they will be quietly copied to the result and
 the user is expected to deal with them as appropriate for a given
 algorithm.  

  This function tries to catch any errors found in processing with
 the when in doubt throw it out attitude.  i.e. the result can be
 expected to frequently be smaller than then input.  Problems will
generally result in a spew of errors to stderr.

\param raw contains the base data.  It is assumed to be in an
  absolute time reference frame.  The metadata attributes  sta 
  and chan must be defined or this function will throw a MetadataError
  exception.
\param times is a map keyed by station.  It's contents are used to
  convert from absolute to relative time.  The most common example
  of this would be measured or predicted arrival times.  For this
  reason the results are written to the input and output gather
  as the field defined by the SeisppKeyword predicted_time_key.
\param phase is the phase name to be assigned to the predicted_time_key
  field.  See LoadPredictedArrivalTimes for how this is used.
\param result_twin is the time window definition for times
  relative to the times contained in the "times" StationTime object.
\param target_dt is the target sample rate for the output data.  All
  data returned will be sampled at this rate.
\param rsdef is the ResamplingDefinitions object passed to the
  resample function used to convert data to a common sample rate.
  see ResampleTimeSeries page or resample(3) for details.
\param trim is a boolean that controls edge transients in resampling.
  if true resample results will be truncated on the left and right
  to eliminate edge transients in using the FIR filters used for 
  decimation.

\exception throws a SeisppError if the resulting gather has no data
after resampling and windowing.
**/
TimeSeriesEnsemble *AssembleRegularGather(TimeSeriesEnsemble& raw,
	StationTime& times,
		string phase,
			TimeWindow result_twin,
				double target_dt,
					ResamplingDefinitions& rsdef,
						bool trim);
/*! \brief Aligns data by a metadata field time and resamples to a common 
sample interval.

This is a core, general method to return a gather that can be equivalenced
to a matrix.  That is, result has all data with common sample rate and
common time base.  i.e. dt, dt, and t0 are the same for all members of 
the output gather.  The input, in contrast, can be arbitrary irregular 
provided the algorithm can cut out and resample something that is 
rational.  
 
The algorithm usd is a cascade of two core algorithms in the SEISPP
library.  First, if the input trace does not have the same 
sample rate as target_dt the resample function is called on the entire
input trace to produce data sampled at target_dt.  The user should
be aware of this as for maximum efficiency the input should have just
enough padding to avoid transients from decimation filter edge effects.
Second, the resample data are changed from absolute to relative time
with time 0 of the new data defined by the time defined in a 
Metadata attribute fetched with key align_mdkey.  If align_mdkey 
is not defined for a given input trace this function will complain 
with a message to stderr and that data will be dropped from the 
output returned by this function.  The user should be aware that this
algorithm is actually very generic and can be applied to a wide
range of algorithms.  For example, it can be used for common source
gathers aligned by some predicted arrival time, common source gathers
aligned on measured arrival times, common receiver gathers with 
data aligned by measured arrival time, or CMP gathers aligned on
some arbitrary time base.  

\param raw contains the base data.  It is assumed to be in an
  absolute time reference frame.  
\param align_mdkey is a string key that MUST reference a real valued
  Metadata attribute that is to be used to define relative time on the 
  output.  That is, t0 of the output is result_twin.start with 0 defined
  relative to the time extracted from this key.
\param result_twin is the time window definition for times
  relative to the times contained in the "times" StationTime object.
\param target_dt is the target sample rate for the output data.  All
  data returned will be sampled at this rate.
\param rsdef is the ResamplingDefinitions object passed to the
  resample function used to convert data to a common sample rate.
  see ResampleTimeSeries page or resample(3) for details.
\param trim is a boolean that controls edge transients in resampling.
  if true resample results will be truncated on the left and right
  to eliminate edge transients in using the FIR filters used for 
  decimation.
*/
TimeSeriesEnsemble *AlignAndResample(TimeSeriesEnsemble& raw,
	string align_mdkey,
		TimeWindow result_twin,
			double target_dt,
				ResamplingDefinitions& rsdef,
					bool trim);

/*! \brief Read a block of three component data in a fixed time window.

 Reads a block of data defined by a time window derived from
 input specifications and a time range computed from a set of theoretical
 arrival times.  

 The algorithm used to define the time window is this:
 start = atmin + data_window.start - tpad while
 end = atmax + data_window.end + tpad.  In this relation atmin and
 atmax are computed minimum and maximum arrival times of all stations
 defined by the input receiver geometry.  data_window defines
 the base time window needed around each arrival time and tpad is a
 necessary fudge factor to allow for errors in the predicted arrival time
 and extra time needed to remove filter endpoint transients.  

 The algorithm first calls the scalar ensemble function with the
 same name (array_get_data).  That routine reads all channels 
 within a specified time window.  To build three-component 
 objects these scalar time series data need to be bundled into
 threes to build the 3c object.  This is accomplished here through
 a special ThreeComponentChannelMap object that defines how this
 process should go.  The ThreeComponentChannelMap allows a heirarchy
 of channels that provide a way to sort out multiple data streams
 from the same station.  The ensemble that is returned by this
 function can have irregular sample rates so beware.

 Note the output of this function is sorted by sta.

\return a pointer to a newly allocated TimeSeriesEnsemble containing all 
   data requested.  Since this is effectively a constructor this means
   the caller is responsible for handling this memory block.  Save the
   result in an auto_ptr or make sure you manage this memory carefully
   as this object can easily get huge.  This is why we return a pointer
   instead of the object itself.
\exception SeisppError object for one of several possible unrecoverable
   errors.  If this routine ever throws an exception the output is,
   of course, invalid by definition.  It can blast out errors to cerr
   for several nonfatal conditions as well. 

\param stations Receiver geometry definition through SeismicArray object.
\param hypo Source geometry definition through Hypocenter object.
\param phase Theoretical arrival times are computed for this seismic phase name.
\param data_window base time window for required output.  View this as the 
    minimum time window desired around each arrival.
\param tpad extra time pad (in second) on each end of the window to be
    read.  This needs to be large enough to mute out filter startup 
    transients for simple filters and decimation filters when resampling is
    required.
\param dbwf generalized database handle to read data.  At present this is immediately
    converted to a DatascopeHandle as the methods used use the Antelope API.
\param scmap StationChannelMap object that defines what channels are to 
    be bundled to produce the three-components that define a 3c object.
\param ensemble_mdl list of data to be read from the database and placed in
    the metadata area for the full ensemble.  
    WARNING:  part of an interface definition that has not  yet been 
    been implemented.  i.e. this argument is ignored.
\param member_mdl list of attributes to be taken from the database for each
    data member (seismogram).
    WARNING:  part of an interface definition that has not  yet been 
    been implemented.  i.e. this argument is ignored.
\param am AttributeMap object defining mapping from database external namespace
    to internal namespace.  
    WARNING:  part of an interface definition that has not  yet been 
    been implemented.  i.e. this argument is ignored.
**/
ThreeComponentEnsemble
  *array_get_data(SeismicArray& stations,
        Hypocenter& hypo,string phase,
        TimeWindow data_window, double tpad,
        DatabaseHandle& dbwf,
        StationChannelMap& scmap,
        MetadataList& ensemble_mdl, MetadataList& member_mdl,
        AttributeMap& am);

/*! Scans an array of times to return the range spanned by the group.

  This is a simple algorithm complicated by the fact that the 
 input is an STL map object with station names keying a set of times.
 This function simply scans the contents of the map to find the maximum
 and mininum times found.  It returns this range as a TimeWindow
 object.

\param times is an STL map containing times keyed by station name.
\return TimeInterval of the range of times found.
**/
TimeWindow StationTimeRange(StationTime& times);

/*! Load a set of predicted arrival times into all members of an ensemble.

Template for use by either a TimeSeries or ThreeComponent Ensemble.
It loads arrival times defined in the times map into the individual
station metadata area using the keyword defined by key.
The phase name is loaded into each trace's metadata area using the 
key "assoc.phase" to be consistent with the common css3.0 database
definitions.  

Function prints a message if any station in the gather has no defined
time in the input.  Caller must be aware of this as the result is 
not set for stations that contain that error.  The result should always
be viewed as potentially incomplete.  

\param ensemble is the input data to be processed
\param times is a map of station names containing times to be inserted
   into the metadata area for each member trace of the input ensemble.
\param key is the keyword used to store the contents of times for each station.
\param phase is the phase name to be stored under the "assoc.time" metadata key.

**/

template <class T> void LoadPredictedTimes(T& ensemble,
	StationTime& times,string key,string phase)
{
	StationTime::iterator it,ite;
	const string phase_key("assoc.phase");
	ite=times.end();
	string sta;
	double atime;
	for(int i=0;i<ensemble.member.size();++i)
	{
		try {
			sta=ensemble.member[i].get_string("sta");
			it=times.find(sta);
			if(it==ite)
			{
				cerr << "LoadPredictedTimes:  station "
					<< sta << " is not defined in station geometry"
					<<endl;
			}
			else
			{
				atime=(*it).second;
				ensemble.member[i].put(key,atime);
				ensemble.member[i].put(phase_key,phase);
			}
		} catch (MetadataGetError mde)
		{
			mde.log_error();
		}
	}
}
} // End Namespace SEISPP declaration
#endif
