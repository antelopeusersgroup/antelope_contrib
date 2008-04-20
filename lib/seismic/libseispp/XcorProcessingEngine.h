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

enum XcorEngineMode {ContinuousDB, EventGathers, GenericGathers};

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
        void save_results(int evid, int orid,Hypocenter& h);

	void load_data(Hypocenter& hypo);
	/*! Load data from a generic gather and set the queue status of the previously processed
	gather to stat.*/
	void load_data(DatabaseHandle& dbh,ProcessingStatus stat);
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

} // End SEISPP namespace declaration
#endif
