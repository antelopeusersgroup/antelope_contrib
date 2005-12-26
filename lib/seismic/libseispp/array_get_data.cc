#include "seispp.h"
#include "seismicarray.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP {

/* This set of procedures use geometry defined in SeismicArray and 
Hypocenter objects to compute appropriate time intervals and build
an ensemble of seismograms in an appropriate time window to contain
data around a defined seismic phase.  */

/* This is a companion needed below.  Builds a vector of 
predicted arrival times for a given phase and input array geometry*/

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
		Hypocenter& hypo, string phase)
{
	StationTime result;
	map<string, SeismicStationLocation>::iterator sta;
	for(sta=stations.array.begin();sta!=stations.array.end();++sta)
	{
		double atime;
		SeismicStationLocation loc=(*sta).second;
		atime=hypo.phasetime(loc.lat,loc.lon,loc.elev,phase);
		atime+=hypo.time;
		result[(*sta).first]=atime;
	}
	return(result);
}
/* Scan the map of arrival times returning a TimeWindow defining
range (max and min) of times contained in the map.  It might
be possible to replace the algorithm here with calls to 
min_element and max_element, but I'm unclear if these algorithms
work on a map.  I don't think so.  A one pass solution like
this is probably faster anyway.*/
TimeWindow StationTimeRange(StationTime& times)
{
	StationTime::iterator stpair;
	double tmin,tmax;
	int i;
	for(i=0,stpair=times.begin();stpair!=times.end();stpair++,i++)
	{
		if(i==0)
		{
			tmin=(*stpair).second;
			tmax=(*stpair).second;
		}
		else
		{
			tmin=min(tmin,(*stpair).second);
			tmax=max(tmax,(*stpair).second);
		}
	}
	return(TimeWindow(tmin,tmax));
}
		
//
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
//@param dbh generalized database handle to read data.  At present this is immediately
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
	AttributeMap& am)
{
	TimeSeriesEnsemble *result=NULL;
	StationTime times;
	times=ArrayPredictedArrivals(stations,hypo,phase);
	TimeWindow arrival_range=StationTimeRange(times);
	TimeWindow read_window(arrival_range.start-tpad+data_window.start,
			arrival_range.end+tpad+data_window.end);
	// When we desire ray coordinates a rotation is required.
	// This demands 3 component data.  In this case we require
	// the 3c seismogram constructor and call the rotation function
	// using the supplied Hypocenter object to compute slowness.
	// The computed slowness vector is used to compute the free surface
	// transformation matrix and that is applied to the data.
	// If the chan code is anything else we fall into the else 
	// block.  See comments there for that algorithm.
	// 
	if( (chan=="L") || (chan=="R") || (chan=="T") )
	{
		throw SeisppError("Support for ray transformation not yet implemented");
		// Algorithm we'll use eventually
		/*
			1) Call a ThreeComponentEnsemble constructor that
				is not finished that will build a 3c ensemble
			2) Call free surface transformation routine on
				each member
			3) Call extract component function to retrieve only 
				desired component 
		ALTERNATIVE:
			Creating a routine of this same name that 
			loads a full 3C ensemble.  Probably should remove
			this functionality from this routine and use that
			approach.
		*/
	}
	else if( (chan=="Z") || (chan=="N") || (chan=="E") )
	{
		/* For the single channel version we depend on the 
		time window based TimeSeriesEnsemble constructor.
		This uses a sta,chan expression to select data.
		Here we simply the interface to Z,N, or E and construct
		a chan expression internally assuming SEED channel codes
		are used (i.e. things like BHZ, HHZ, and LHZ).  After that
		the constructor does almost all the work for us.  The result
		MUST be recognized to contain multiple sample rate data. */
		string chan_expression=string("..")+chan+string(".*");
		try {
			result = new TimeSeriesEnsemble(dbh,
				read_window,"none",chan_expression);
		} catch (...) {throw;}
	}
	else
	{
		throw SeisppError(string("array_get_data: cannot handle channel code ")
			+ chan + string("\n Must be Z,N,E,L,R, or T\n"));
	}
	/* Also not yet implemented:  need to load metadata as
		required here by two metadata list args.  Here is 
		the algorithm I'll eventually want to use:
		1.  assume input handle is a view joining all tables needed
			to load metadata defined in the lists.
		2.  after read foreach member of ensemble find matching
			sta:chan row in view, call Metadata constructor
			with handle pointing at that row, and run 
			copy_selected_metadata to copy result
			(Note cannot dup because window reader posts
			some critical metadata we need to retain).

		First step is that I will now need to build a find
		method for dbpp.

		For now warn the user strongly.
	*/
	cerr << "Warning(array_read_data):  because of incomplete implementation the metadata list was ignored."<<endl;
	return(result);
}
// Common error routine for function immediately following.
void AssembleRegularRatherErrorLog(TimeSeriesEnsemble& raw, int i, 
	SeisppError serr)
{
	cerr << "AssembleRegularGather: Problem processing data for station="
		<< raw.member[i].get_string("sta")
		<< "  and channel="
		<< raw.member[i].get_string("chan")
		<< endl
		<< "Problem function sent this error message:"
		<<endl;

		serr.log_error();

		cerr << "Data from this sta:chan discarded" << endl;
}
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
						bool trim)

{
	// Metadata key used to make predicted arrival time
	const string predicted_time_key("predarr.time");
	// Fractional tolerance for sample rate error
	const double relative_dt_allowed=0.001;
	TimeSeries raw_data_trace;

	// This clones the metadata area for ensemble, but leaves 
	// contents empty.
	TimeSeriesEnsemble *result=new TimeSeriesEnsemble(
			dynamic_cast<Metadata&>(raw),raw.member.size());
			
	LoadPredictedTimes(raw,times,predicted_time_key,phase);
	for(int i=0;i<raw.member.size();++i)
	{
		double rdterr;
		// work with this copy
		rdterr=fabs((raw.member[i].dt-target_dt)/target_dt);
		if(rdterr>relative_dt_allowed)
		{
			try {
				raw_data_trace=ResampleTimeSeries(
					raw.member[i],rsdef,target_dt,trim);
			} catch(SeisppError serr) {
				AssembleRegularRatherErrorLog(raw,i,serr);
				continue;  // This skips when error is thrown
			}
		}
		else
		{
			raw_data_trace=raw.member[i];
		}
		try {
			TimeSeries newtrace=ArrivalTimeReference(raw_data_trace,
						predicted_time_key,
							result_twin);
			result->member.push_back(newtrace);
		} catch (SeisppError serr)
		{
			AssembleRegularRatherErrorLog(raw,i,serr);
			// No need for continue here as result only 
			// saved when ArrivalTimeReference returns 
			// successfully.
		}
	}
	return(result);
}

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
void LoadPredictedTimes(T& ensemble, StationTime& times,string key,string phase)
{
	StationTime::iterator it,ite;
	const string phase_key("assoc.phase");
	ite=times.end();
	for(int i=0;i<ensemble.member.size();++i)
	{
		string sta;
		double atime;
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

} // End declaration of SEISPP namespace
