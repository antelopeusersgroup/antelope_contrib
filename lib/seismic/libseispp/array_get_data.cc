#include "seispp.h"
#include "SeisppKeywords.h"
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
		try {
			atime=hypo.phasetime(loc.lat,loc.lon,loc.elev,phase);
			atime+=hypo.time;
			result[(*sta).first]=atime;
		}
		catch (SeisppError& serr)
		{
			cerr << "ArrayPredictedError(Warning):  phasetime "
				<< "failed computing travel time for "
				<< phase << " for station "<<(*sta).first<<endl;
		}
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
// Only 3 values are allowed for chan: Z,N,E.  If anything
// else is passed through chan the functio will throw a SeisppError exception.
// Z,N, or E are assumed cardinal directions.  They generate subsets of the 
// input defined by SEED channel codes.  The result will contain ALL Z,N, or
// E channels found in the requested time windows.  For example, if a station
// has HHZ, BHZ, and LHZ channels the output will contain three members from
// that station.  It is the callers job to sort out any redundancy this 
// will produce.  
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
//    WARNING:  part of an interface definition that has not  yet been 
//    been implemented.  i.e. this argument is ignored.
//@param member_mdl list of attributes to be taken from the database for each
//    data member (seismogram).
//    WARNING:  part of an interface definition that has not  yet been 
//    been implemented.  i.e. this argument is ignored.
//@param am AttributeMap object defining mapping from database external namespace
//    to internal namespace.  
//    WARNING:  part of an interface definition that has not  yet been 
//    been implemented.  i.e. this argument is ignored.
//@}
/* Modified Oct 2009 to have more flexibility in chan specification.  
   Previous version, as described above, only accepted Z,N, or E.  Now
   a single character code initiates old behaviour, but if chan has
   more than one character it is passed directly to to the channel 
   sort expression */
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
        string chan_expression(chan);
	if( (chan=="Z") || (chan=="N") || (chan=="E") )
	{
		/* For the single channel version we depend on the 
		time window based TimeSeriesEnsemble constructor.
		This uses a sta,chan expression to select data.
		Here we simply the interface to Z,N, or E and construct
		a chan expression internally assuming SEED channel codes
		are used (i.e. things like BHZ, HHZ, and LHZ).  After that
		the constructor does almost all the work for us.  The result
		MUST be recognized to contain multiple sample rate data. */
		chan_expression=string("..")+chan+string(".*");
        }
	try {
		result = new TimeSeriesEnsemble(dbh,
			read_window,"none",chan_expression);
	} catch (...) {throw;}
	// Currently the MetadataList elements are ignored.  This 
	// needs to ultimately be repaired.
	return(result);
}
/* Automatically switches polarity of traces with reversed
polarity.  Works only for SEED channel codes.  Silently does
nothing if the third character of the channel code is anything
but Z, N, or E.  Every trace is tested because we often mix
channel codes in data sets.  Silently ignores any data 
for which chan is not defined.  */
void auto_switch_polarity(TimeSeriesEnsemble *d)
{
	const string z("Z"),n("N"),e("E");
	int i,j;
	double hang,vang;
	for(i=0;i<d->member.size();++i)
	{
		try {
			string chan=d->member[i].get_string("chan");
			// String constructor extracts 3rd char of chan
			string ctest(chan,2,1);
			if(ctest==z)
			{
				vang=d->member[i].get_double("vang");
				if(fabs(vang-180.0)<1.0)
				{
					for(j=0;j<d->member[i].s.size();++j)
						d->member[i].s[j]*=-1.0;
				}
				d->member[i].put("vang",0.0);
			}
			else if (ctest==n)
			{
				hang=d->member[i].get_double("hang");
				if(fabs(hang-180.0)<1.0)
				{
					for(j=0;j<d->member[i].s.size();++j)
						d->member[i].s[j]*=-1.0;
				}
				d->member[i].put("hang",0.0);
			}
			else if (ctest==e)
			{
				hang=d->member[i].get_double("hang");
				if((fabs(hang-270.0)<1.0)
				  || (fabs(hang+90.0)<1.0) )
				{
					for(j=0;j<d->member[i].s.size();++j)
						d->member[i].s[j]*=-1.0;
				}
				d->member[i].put("hang",90.0);
			}
		} catch (MetadataGetError& mde) {};
	}
}

// Common error routine for function immediately following.
void AlignAndResampleErrorLog(TimeSeriesEnsemble& raw, int i, 
	SeisppError& serr)
{
	cerr << "AlignAndResample: Problem processing data for station="
		<< raw.member[i].get_string("sta")
		<< "  and channel="
		<< raw.member[i].get_string("chan")
		<< endl
		<< "Problem function sent this error message:"
		<<endl;

		serr.log_error();

		cerr << "Data from this sta:chan discarded" << endl;
}
/* Core routine for AssembleRegularGather but useful on it's own.  This 
function aligns data on time defined by align_mdkey and resamples all
data in a gather to target_dt.  The result is in relative with time
defined b the result_twin definition. */
TimeSeriesEnsemble *AlignAndResample(TimeSeriesEnsemble& raw,
	string align_mdkey,
		TimeWindow result_twin,
			double target_dt,
				ResamplingDefinitions& rsdef,
					bool trim)
{
	// This clones the metadata area for ensemble, but leaves 
	// contents empty.
	TimeSeriesEnsemble *result=new TimeSeriesEnsemble(
			dynamic_cast<Metadata&>(raw),raw.member.size());
	TimeSeries raw_data_trace;
	for(int i=0;i<raw.member.size();++i)
	{
		// Do nothing to data marked dead
		if(!raw.member[i].live) continue;
		if(SampleIntervalsMatch<TimeSeries>(raw.member[i],target_dt) )
		{
			raw_data_trace=raw.member[i];
		}
		else
		{
			try {
				raw_data_trace=ResampleTimeSeries(
					raw.member[i],rsdef,target_dt,trim);
			} catch(SeisppError& serr) {
				AlignAndResampleErrorLog(raw,i,serr);
				continue;  // This skips when error is thrown
			}
		}
		try {
			auto_ptr<TimeSeries> newtrace(ArrivalTimeReference(raw_data_trace,
						align_mdkey,result_twin));
			result->member.push_back(*newtrace);
		} catch (SeisppError& serr)
		{
			AlignAndResampleErrorLog(raw,i,serr);
			// No need for continue here as result only 
			// saved when ArrivalTimeReference returns 
			// successfully.
		}
	}
	return(result);
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
//  as the field defined by the SeisppKeyword.h string predicted_time_key.
//@param phase is the phase name to be assigned to the predicted_time_key
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
	LoadPredictedTimes(raw,times,predicted_time_key,phase);
	/* This function always returns something.  Throw and exception only if 
	the result is empty */
	TimeSeriesEnsemble *result=AlignAndResample(raw,predicted_time_key,
		result_twin,target_dt,rsdef,trim);
	if(result->member.size()<=0) 
        {
            delete result;
            throw SeisppError(string("AssembleRegularGather:  ")
		+ string("Result after AlignAndResample function contains no data") );
        }
	auto_switch_polarity(result);
	return(result);
}

} // End declaration of SEISPP namespace
