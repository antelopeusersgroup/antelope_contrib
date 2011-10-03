//  Peng Wang, HPC/RAC/UITS
//  Indiana University
//
//  Copyright 2005, The Trustees of Indiana University.
//  Last Modified: 12/1/2005

#ifndef __XCOR_ENGINE_H
#define __XCOR_ENGINE_H

#include <sstream>
#include <vector>

#include "stock.h"
#include "pf.h"
#include "Metadata.h"
#include "resample.h"
#include "ensemble.h"
#include "SeisppKeywords.h"
#include "seismicarray.h"
#include "filter++.h"
#include "MultichannelCorrelator.h"
#include "XcorAnalysisSetting.h"
#include "ArrivalUpdater.h"
#include "SignalToNoise.h"
#include "ProcessingQueue.h"

namespace SEISPP {

using namespace std;
using namespace SEISPP;

/*! \brief Defines overall processing mode for XcorEngine. 

This enum controls the overall processing mode for an XcorProcessingEngine object.
See documentation for the XcorProcessingEngine for a desciption of what thee modes mean.
*/
enum XcorEngineMode {ContinuousDB, EventGathers, GenericGathers};

/* \brief Processing object for multichannel correlator.

This is a processing object that can be used to process a series of gathers
using the MultichannelCorrelator object.  It is an model of a processing engine
that reads in data and produces output like an engine that eat fuel and air 
and produces power and exhaust.  The normal use is this sequence for each
gather to be processed:  (1) read data, (2) run MultichannelCorrelator on
these data, and (3) save results.   The options in running this are huge,
however, and this beast has a lot of variation.  The behaviour is controlled
by two different means.  First, on creation most parameters are set through
an input Antelope parameter file.  Second, this beast has a parallel 
object called XcorAnalysisSetting that contains input to this beast that
are volatile.  To use an automotive analogy, the pf is like parts selected
to create this engine at the factor.  The XcorAnalysisSetting object is like
the engine controls (throttle, choke, etc.).

This beast behaves expects very different data and behaves very differently
for each of these modes.  ContinuousDB assumes event gathers indexed
by an Antelope database using wfdisc. This can be segmented data in spite
of the name, but the model is the same.  That is time windows are carved 
from the input on the fly.  Provided the data segments are larger than requested
time gates there will be no issues.  EventGathers is a generalization of 
this mode.  We still assume event processing, but dbprocess is used to 
define the database view that drives the processing.  In this mode it is
essential that the commands passed to dbprocess contain a dbgroup command
to define ensemble grouping by event.  GenericGathers is similar to EventGathers
in how it expects to see input data, but does not assume the data are 
event gathers.  In the current implementation this somewhat implicitly 
assumes the input data are common receiver gathers used for source array 
processing, but the only place this matter much is in the save method. 
*/
class XcorProcessingEngine {
public:
	bool use_subarrays;
	int current_subarray;  // Index to current subarray 
	XcorProcessingEngine(Pf * global_pf, XcorAnalysisSetting asinitial,
		string waveform_db_name, string result_db_name,string queuefile);
	~XcorProcessingEngine();  // necessary unless we can get rid of mcc raw pointer
        void change_analysis_setting(XcorAnalysisSetting a) {analysis_setting=a; if(!analysis_setting.rw_set) analysis_setting.robust_tw=analysis_setting.beam_tw;}
	XcorAnalysisSetting get_analysis_setting() {return(analysis_setting);};

	void shift_arrivals(double tshift);
	// This function sorts results according to current sort 
	// definition in XcorAnalysisSetting
	void sort_ensemble();
	
	//save the resulting beam to the output database
        void save_results(long evid, long orid,Hypocenter& h);

	void load_data(Hypocenter& hypo);
	/*! Load data from a generic database handle.  

	This method is aimed as generic interface to load data from a database 
	tagged with a queue.  When called the engine will load the next generic
	gather defined through the engine defintion and the database handle.
	This is a very generic way to load data from a database.  
	\param dbh  generic database handle that will be used to load the next gather
		to be processed.
	\param stat is the processing status that will be used to mark the queue
		associated with this database. 

	\return 0 if data loaded successfully.  Nonzero is returned when the queue
		is empty.
	*/
	int load_data(DatabaseHandle& dbh,ProcessingStatus stat);
	/*! \brief Deletes data already processed.

	In source array processing it has been found useful to work 
	through a gather in sections.  The reason is that an ensemble
	may have several common waveforms that can be correlated.  This 
	allows working through all common waveforms until no common members
	remain.  Note this method does something no other method does.
	That is, it clears data from the (normally hidden) raw data 
	gather.  
	*/
	int clear_already_processed();
	// Some public attributes required to implement subarrays
	int number_subarrays();  // Returns count of number of subarrays
	string current_subarray_name;  // name assigned to current subarray
	// Makes the next subarray data current and updates above attributes
	void next_subarray();
	// We should use a shared_ptr here so we keep an internal
	// reference as well as one visible externally.  This
	// avoids copying and uses resource management class to
	// simplify memory management.  I elected to not do this
	// for now as Sun's stock compiler does not seem to have std::tr1
	// For now beware this returns a pointer to the result stored in 
	// the private area.
	MultichannelCorrelator *analyze();

	// this method applies another filter to current data
	// Use change_analysis setting to alter base filter
	void filter_data(TimeInvariantFilter f);
	
	//validate analysis setting
	bool validate_setting(XcorAnalysisSetting & a) { return (a.aw_set && a.bw_set && a.rt_set); } 
	bool arrival_times_are_loaded(){return(load_arrivals);};

	TimeSeries get_beam();
	TimeSeriesEnsemble get_waveforms();
	TimeSeriesEnsemble get_raw_data();
      	TimeSeriesEnsemble *get_waveforms_gui() {return &(waveform_ensemble);}
	MultichannelCorrelator *get_correlation_results();
	// Restores ensemble to raw form
	void restore_original_ensemble();

	//Peng Wang
	Metadata get_data_md() {return data_display_md;}
 	Metadata get_xcor_md() {return xcor_display_md;}
	Metadata get_beam_md() {return beam_display_md;}
	/*! Fetch a handle to one of the internal databases maintained by this object.

	This processing object maintains a number of database handles it uses for the 
	load methods and the save_results methods.  Sometimes an external program 
	needs access to these databases.  In particular, because the load methods 
	were designed to be stateless, the state must be maintained externally.
	In particular, in continuous processing this means the information about 
	events.  In generic gathers it means which gather is to be processed next.

	Note since this method returns a pointer that points to an internal member
	of this object it goes without saying you must NEVER delete this pointer.
	
	\param dbmember is a name used to request which database handle is desired.
		Must be one of two unique strings:  "waveformdb" or "resultdb".  
		The former is the handle used for reading while the second is the handle
		used for writing.  They may or may not be the same database depending
		on parameter settings.  
	\expection SeisppError object is thrown if dbmember is anything but one of the 
		two allowed values.
	\return pointer to a generic DatabaseHandle object.  At present this is an upcast
		from a DatascopeHandle, but the interface was intentionally made more 
		generic anticipating alternatives may exist in the future.
	*/
	DatabaseHandle *get_db(string dbmember);
private:
	DatascopeHandle waveform_db_handle;
	DatascopeHandle result_db_handle;

	//This is the meta data object that stores the global static parameters.
	Metadata global_md;
	// Save the table references for each output table used by this program.
	// This avoids constant lookups.  These are saved as raw datascope Dbptr
	// structures instead of a DatascopeHandle because they are hidden behind
	// the interface anyway.
	Dbptr dbassoc;
	Dbptr dbarrival;
	Dbptr dbxcorarrival;
	Dbptr dbxcorbeam;
	Dbptr dbevlink;
	Dbptr dbwfprocess;
	/*! Handles updating arrival/assoc tables automatically. 
	*  Earlier version had saving arrival/assoc optional.  Now it is 
	*  always required and always updated */
	
	ArrivalUpdater arru;
	auto_ptr<TimeSeriesEnsemble> regular_gather;
	//TimeSeriesEnsemble *regular_gather;
	// This holds the working data.  
	TimeSeriesEnsemble waveform_ensemble;

	XcorAnalysisSetting analysis_setting;
	MultichannelCorrelator * mcc;
	
	//This is the meta data object to hold SeismicPlot display parameters.
	Metadata data_display_md;
	// For now I use raw pointers to hold plot objects.  This will
	// need to change eventually.  Allows me to get past current Xwindow
	// problem in Peng's fancier interface.
	Metadata xcor_display_md;
	Metadata beam_display_md;

	// Updates geometry when needed.  Multiple methods need this so 
	// and require access to the private area so we put it here.
        void UpdateGeometry(TimeWindow twin);
	SeismicArray stations;

        // These are easily wrapped into constructor.  Method exist to build them
        // from a pf.   Their purpose is to define what attributes are extracted from
        // or written to the database.
        MetadataList ensemble_mdl;
        MetadataList trace_mdl;
        MetadataList beam_mdl;  // Used for array trace output
        AttributeMap am;
	string beam_directory;
	string beam_dfile;

	ResamplingDefinitions rdef;
	double target_dt;
        bool trim_resampled;  // set true of edge transients automatically cut in resampling
	// Added to private data 1/2/2006 by GLP.  Part of analysis setting now.
	TimeWindow current_data_window;
	TimeWindow raw_data_twin;
	string netname;
	double xcorpeak_cutoff_default;
	double coherence_cutoff_default;
	double stack_weight_cutoff_default;
	double xcorpeak_cutoff,coherence_cutoff,stack_weight_cutoff;
	double time_lag_cutoff;
	// Added Dec. 2006 by GLP to allow an option for autoscaling of initial plot
	bool autoscale_initial;
	// Had to add this to support subarrays
	Pf *pf_used_by_engine;
	// Needed to support three component data
	bool RequireThreeComponents;
	StationChannelMap stachanmap;
	// Added by GLP June 2007 to allow running without extension tables
	bool save_extensions;
	//Added April 2008:  if true, delete old picks for an event in a 
	// gather not marked as processed
	bool delete_old_arrivals;
	// Added by GLP Nov 2007 to control loading of arrivals from db
	bool load_arrivals;
	// Added Feb. 2008 to allow program to work in source array mode
	XcorEngineMode processing_mode;
	// Necessary metadata key for GenericGathers.  Otherwise ignored
	string time_align_key;
	// This is coming up enough places in this that it is useful to make
	// it a variable.  These are method and model for a travel time calculator
	string ttmethod,ttmodel;
	/* Added to support new queue driven processng Feb 2008 */
	DatascopeProcessingQueue *dpq;
	/* Code used to be in load_data, but since load_data is overloaded this
	method contains common code shared by load_data methods.  It needs to be
	a member to allow access to all the class data. */
	void prep_gather();
};
/*! /brief Function object to sort a set of objects defined by a Metadata double in
increasing order.

The stl sort algorithm can use a general function to define order.  This template
allows sorting a group of objects that inherit metadata in ascending order by 
a real (double) parameter defined by a particular key. 

\param T is the ensemble class type
\param SO is a SortOrder enum value that is used to define the attribute for
	sorting.  This has to be an enum as I've found no way to give a function
	object like this a parameter any other way. 
*/

template <class T, SortOrder SO> struct less_metadata_double
                : public binary_function<T,T,bool> {
        bool operator()(T x, T y)
        {
		string keyword;
		switch (SO)
		{
		case COHERENCE:
			keyword=SEISPP::coherence_keyword;
			break;
		case CORRELATION_PEAK:
			keyword=SEISPP::peakxcor_keyword;
			break;
                case XCORPEAKRATIO:
                        keyword=SEISPP::XcorPeakRatioKeyword;
                        break;
		case AMPLITUDE:
			keyword=SEISPP::amplitude_static_keyword;
			break;
		case LAG:
			keyword=SEISPP::moveout_keyword;
			break;
  		case SITE_LAT:
			keyword=string("sta_lat");
			break;
		case SITE_LON:
			keyword=string("sta_lon");
			break;
		case PREDARR_TIME:
			keyword=predicted_time_key;
			break;
		case ESAZ:
			keyword=string("esaz");;
			break;
		case DISTANCE:
			keyword=string("distance");;
			break;
		case DBARRIVAL_TIME:
			keyword=dbarrival_time_key;
			break;
		case ARRIVAL_TIME:
			keyword=arrival_time_key;
			break;
		case SNR:
			keyword=snr_keyword;
			break;
		case WEIGHT:
		default:
			keyword=SEISPP::stack_weight_keyword;
			break;
		}
		/* the try-catch logic below assure that data that
		return errors on a get act as if they are the smallest possible value */
		double valx,valy;
		try { 
			valx=x.get_double(keyword);
		}catch(...) {return true;};
		try { 
			valy=y.get_double(keyword);
		}catch(...) {return false;};
	
	        if(valx<valy)
	                return true;
	        else
	                return false;
        }
};
/*! /brief Function object to sort a set of objects defined by a Metadata double in decreasing order.

The stl sort algorithm can use a general function to define order.  This template
allows sorting a group of objects that inherit metadata in descending order by 
a real (double) parameter defined by a particular key. 

\param T is the ensemble class type
\param SO is a SortOrder enum value that is used to define the attribute for
	sorting.  This has to be an enum as I've found no way to give a function
	object like this a parameter any other way. 
*/
template <class T, SortOrder SO> struct greater_metadata_double
                : public binary_function<T,T,bool> {
        bool operator()(T x, T y)
        {
		string keyword;
		switch (SO)
		{
		case COHERENCE:
			keyword=SEISPP::coherence_keyword;
			break;
		case CORRELATION_PEAK:
			keyword=SEISPP::peakxcor_keyword;
			break;
		case AMPLITUDE:
			keyword=SEISPP::amplitude_static_keyword;
			break;
		case LAG:
			keyword=SEISPP::moveout_keyword;
			break;
  		case SITE_LAT:
			keyword=string("sta_lat");
			break;
		case SITE_LON:
			keyword=string("sta_lon");
			break;
		case PREDARR_TIME:
			keyword=predicted_time_key;
			break;
		case ESAZ:
			keyword=string("esaz");;
			break;
		case DISTANCE:
			keyword=string("distance");;
			break;
		case DBARRIVAL_TIME:
			keyword=dbarrival_time_key;
			break;
		case ARRIVAL_TIME:
			keyword=arrival_time_key;
			break;
		case SNR:
			keyword=snr_keyword;
			break;
		case WEIGHT:
		default:
			keyword=SEISPP::stack_weight_keyword;
			break;
		}
		/* the try-catch logic below assure that data that
		return errors on a get act as if they are the largest possible value */
		double valx,valy;
		try { 
			valx=x.get_double(keyword);
		}catch(...) {return true;};
		try { 
			valy=y.get_double(keyword);
		}catch(...) {return false;};
	
	        if(valx>valy)
	                return true;
	        else
	                return false;
        }
};

} // End SEISPP namespace declaration
#endif
