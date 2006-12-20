//  Peng Wang, HPC/RAC/UITS
//  Indiana University
//
//  Copyright 2005, The Trustees of Indiana University.
//  Last Modified: 12/1/2005

#include <algorithm>
#include "XcorProcessingEngine.h"


XcorProcessingEngine::XcorProcessingEngine(Pf * global_pf, 
	AnalysisSetting asinitial,
		string waveform_db_name,
			string result_db_name) 
			  : rdef(global_pf),am("css3.0"),analysis_setting(asinitial)
{
	int i, j;
	try {

		global_md=Metadata(global_pf);
		pf_used_by_engine = pfdup(global_pf);
		string schema=global_md.get_string("schema");
		if(schema!="css3.0")
	 		am=AttributeMap(schema);
	
		trace_mdl=pfget_mdlist(global_pf,"trace_mdl");
		ensemble_mdl=pfget_mdlist(global_pf,"trace_mdl");
	
		waveform_db_handle=DatascopeHandle(waveform_db_name,true);
		// deal with possibility that output db is the same
		// as the input db.  This logic depends upon the 
		// current API for Datascope in which the handle is 
		// Datascope like being freely copyable.  
		if (waveform_db_name==result_db_name) 
			result_db_handle=DatascopeHandle(waveform_db_handle);
		else
			result_db_handle=DatascopeHandle(result_db_name,false);
		dbassoc=dblookup(result_db_handle.db,0,"assoc",0,0);
		dbarrival=dblookup(result_db_handle.db,0,"arrival",0,0);
		dbxcorarrival=dblookup(result_db_handle.db,0,"xcorarrival",0,0);
		dbxcorbeam=dblookup(result_db_handle.db,0,"xcorbeam",0,0);
		dbwfprocess=dblookup(result_db_handle.db,0,"wfprocess",0,0);
		dbevlink=dblookup(result_db_handle.db,0,"evlink",0,0);
		if( (dbassoc.table==dbINVALID) 
			|| (dbarrival.table==dbINVALID)
			|| (dbxcorarrival.table==dbINVALID)
			|| (dbxcorbeam.table==dbINVALID)
			|| (dbevlink.table==dbINVALID)
			|| (dbwfprocess.table==dbINVALID) )
		{
			result_db_handle.close();
			waveform_db_handle.close();
			throw SeisppError(string("XcorProcessingEngine:")
				+string("  constructor had problems opening one or more database tables"));
		}
		// Verify site and sitechan are defined and abort if
		// they are empty
		int ntest;
		Dbptr dbtmp=dblookup(waveform_db_handle.db,0,"site",0,0);
		dbquery(dbtmp,dbRECORD_COUNT,&ntest);
		if(ntest<=0) 
			throw SeisppError(string("XcorProcessingEngine:")
				+string(" required site table is empty"));
		dbtmp=dblookup(waveform_db_handle.db,0,"sitechan",0,0);
		dbquery(dbtmp,dbRECORD_COUNT,&ntest);
		if(ntest<=0) 
			throw SeisppError(string("XcorProcessingEngine:")
				+string(" required sitechan table is empty"));
		// We could bypass arrival/assoc dblookup if this
		// were false, but the overhead is so small it is 
		// best to leave it alone.  (glp)
		save_arrival=global_md.get_bool("save_to_arrival");
		// This is needed for wfprocess table saving of array beam
		beam_mdl = pfget_mdlist(global_pf,"BeamMetadataList");
		beam_directory=global_md.get_string("beam_directory");
		beam_dfile=global_md.get_string("beam_dfile");
	
	        string time_stamp=global_md.get_string("initial_time_stamp");
	        double treference=str2epoch(const_cast<char *>
	                        (time_stamp.c_str()));
		netname=global_md.get_string("network_name");
		stations= SeismicArray(dynamic_cast<DatabaseHandle&>(waveform_db_handle),
                                        treference,netname);
		use_subarrays=global_md.get_bool("use_subarrays");
		if(use_subarrays) 
		{
			load_subarrays_from_pf(stations,global_pf);
			current_subarray=0;
			if(stations.number_subarrays()<=0)
				throw SeisppError(string("XcorProcessingEngine: ")
					+string("pf error.  subarrays turned on, but subarray definitions are empty"));
			// A bit inefficient, but useful to test up front to be sure this works for
			// small cost in execution time
			SeismicArray test=stations.subset(0);
			current_subarray_name=test.name;
		}
		else
		{
			current_subarray=-1;
			current_subarray_name=string("");
		}
	
		raw_data_twin.start=global_md.get_double("data_window_start");
		raw_data_twin.end=global_md.get_double("data_window_end");
		double tpad=global_md.get_double("tpad");
		regular_gather_twin.start = global_md.get_double("regular_gather_twin_start");
		regular_gather_twin.end = global_md.get_double("regular_gather_twin_end");
		current_data_window.start=treference+raw_data_twin.start;
		current_data_window.end=treference+raw_data_twin.end;
	        target_dt=global_md.get_double("target_sample_interval");
		// These set up separate display parameters for each 
		// display window (data, beam, xcor result);
		data_display_md=Metadata(global_pf,"data_window_parameters");
		xcor_display_md=Metadata(global_pf,"correlation_window_parameters");
		beam_display_md=Metadata(global_pf,"beam_window_parameters");
		// Some useful cutoff parameters
		xcorpeak_cutoff=global_md.get_double("correlation_peak_cutoff");
		coherence_cutoff=global_md.get_double("coherence_cutoff");
		stack_weight_cutoff=global_md.get_double("stack_weight_cutoff");
		xcorpeak_cutoff_default=xcorpeak_cutoff;
		coherence_cutoff_default=coherence_cutoff;
		stack_weight_cutoff_default=stack_weight_cutoff;
		time_lag_cutoff=global_md.get_double("time_lag_cutoff");
		// For three component data we need to create this 
		// StationChannelMap object that tells us how to map channel
		// codes into components
		RequireThreeComponents=global_md.get_bool("RequireThreeComponents");
		if(RequireThreeComponents)
			stachanmap=StationChannelMap(global_pf);
		else
			stachanmap=StationChannelMap();
		mcc = NULL;   // Need this unless we can convert to a shared_ptr;
		autoscale_initial=global_md.get_bool("AutoscaleInitialPlot");

	} catch (MetadataGetError mderr)
	{
		mderr.log_error();
		throw SeisppError("XcorProcessingEngine construction failed");
	}
	catch (SeisppError serr)
	{
		throw serr;
	}
	catch (...)
	{
		throw SeisppError("XcorProcessingEngine constructor:  Unknown error was thrown");
	}
}
//
// It would be nice to get rid of mcc as a raw pointer here.  We should
// either invoke a copy overhead or use a resource managing pointer shared_ptr.
//  This is a typically dangerous use of wild pointers.
//
XcorProcessingEngine::~XcorProcessingEngine()
{
	if(mcc!=NULL) delete mcc;
	// There is a serious problem here with destruction related to
	// a design flaw in DatascopeHandle.  Because we need to keep
	// several tables open in potentially different databases there
	// is no clear way to handle the destruction.  The important thing
	// this means is an XcorProcessingEngine should be created only
	// once and never destroyed until the program is ready to exit.
	// Multiple create destroys are guaranteed problems.  Should
	// be fixed eventually, but the correct solution is in the 
	// DatascopeHandle code itself, not here.,
}
// Small helper needed below.  Looks for bad moveout flag
// and resets moveout to 0.0.  Needed to keep cross correlation
// plot from crashing.  Kind of a hack solution.  Need a cleaner
// way to handle this perhaps.

void ResetMoveout(TimeSeriesEnsemble& d)
{
	vector<TimeSeries>::iterator dptr;

	for(dptr=d.member.begin();dptr!=d.member.end();++dptr)
	{
		double moveout;
		if(dptr->live)
		{
			moveout=dptr->get_double(moveout_keyword);
			if(fabs(moveout)>MoveoutBadTest)
			{
				moveout=0.0;
				dptr->put(moveout_keyword,moveout);
			}
		}
	}
}
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
		case AMPLITUDE:
			keyword=SEISPP::amplitude_static_keyword;
			break;
		case LAG:
			keyword=SEISPP::moveout_keyword;
			break;
  		case SITE_LAT:
			keyword=lat_keyword;
			break;
		case SITE_LON:
			keyword=lon_keyword;
			break;
		case PREDARR_TIME:
			keyword=predicted_time_key;
			break;
		case WEIGHT:
		default:
			keyword=SEISPP::stack_weight_keyword;
			break;
		}
                double valx=x.get_double(keyword);
                double valy=y.get_double(keyword);
                if(valx<valy)
                        return true;
                else
                        return false;
        }
};


MultichannelCorrelator *XcorProcessingEngine::XcorProcessingEngine :: analyze()
{
   if(mcc!=NULL) delete mcc;
   try {
     UpdateGeometry(current_data_window);
     mcc=	new MultichannelCorrelator(waveform_ensemble,RobustStack,analysis_setting.beam_tw,
   	analysis_setting.robust_tw,time_lag_cutoff,RobustSNR,NULL,
   	analysis_setting.reference_trace); 
   } catch (...) {throw;};
   //
   // We need to reset bad moveout in xcor so it can be plotted properly.
   // 
   ResetMoveout(mcc->xcor);
   // This permanently alters the time base for the xcor result.
   // All users of this must be aware time base for xcor is not longer
   // related to a fixed time base.  Aim is to produce a gather that
   // is aligned to zero lag that can inspected graphically.
   mcc->xcor=MoveoutTimeShift(mcc->xcor);
   // This aligns the ensemble by the time shifts computed by mcc operator.
   // WARNING:  the frozen arrival.time key used here is a long-term
   // maintenance issue and limits future reuse.
   // ALSO very important to realize arrival.time MUST HAVE BEEN INITIALIZED
   // original to a predicted arrival time when running from raw data
   // (the current situation for teleseismic data).
   LagShift(waveform_ensemble,moveout_keyword,arrival_time_key);
   // Auto scale data using computed amplitude set by MultichannelCorrelator
   ScaleEnsemble<TimeSeriesEnsemble,TimeSeries>(waveform_ensemble,
	amplitude_static_keyword,true);
   // reset gain so amplitude statics can be properly accumulated
   // if data are reprocessed
   ScaleCalib <TimeSeriesEnsemble>(waveform_ensemble,
		gain_keyword,amplitude_static_keyword);
   return(mcc);
}

void XcorProcessingEngine::sort_ensemble()
{
   switch(analysis_setting.result_sort_order)
   {
   case COHERENCE:
	sort(waveform_ensemble.member.begin(),waveform_ensemble.member.end(),
		less_metadata_double<TimeSeries,COHERENCE>());
	break;
   case CORRELATION_PEAK:
	sort(waveform_ensemble.member.begin(),waveform_ensemble.member.end(),
		less_metadata_double<TimeSeries,CORRELATION_PEAK>());
	break;
   case AMPLITUDE:
	sort(waveform_ensemble.member.begin(),waveform_ensemble.member.end(),
		less_metadata_double<TimeSeries,AMPLITUDE>());
	break;
   case LAG:
	sort(waveform_ensemble.member.begin(),waveform_ensemble.member.end(),
		less_metadata_double<TimeSeries,LAG>());
	break;
   case WEIGHT:
	sort(waveform_ensemble.member.begin(),waveform_ensemble.member.end(),
		less_metadata_double<TimeSeries,WEIGHT>());
	break;
   case SITE_LAT:
	sort(waveform_ensemble.member.begin(),waveform_ensemble.member.end(),
                less_metadata_double<TimeSeries,SITE_LAT>());
        break;
   case SITE_LON:
        sort(waveform_ensemble.member.begin(),waveform_ensemble.member.end(),
                less_metadata_double<TimeSeries,SITE_LON>());
        break;
   case PREDARR_TIME:
        sort(waveform_ensemble.member.begin(),waveform_ensemble.member.end(),
                less_metadata_double<TimeSeries,PREDARR_TIME>());
        break;
   default:
	cerr << "Illegal sort order.  Original order preserved."<<endl;
   }
}
// The next three functions are somewhat unnecessary with
// this implementation, but are useful as interface routines
// nonetheless.
TimeSeries XcorProcessingEngine::get_beam()
{
    return(mcc->ArrayBeam());
}


MultichannelCorrelator *XcorProcessingEngine::get_correlation_results()
{
    return (mcc);
}
TimeSeriesEnsemble XcorProcessingEngine::get_waveforms()
{
	return(waveform_ensemble);
}
TimeSeriesEnsemble XcorProcessingEngine::get_raw_data()
{
	return(*regular_gather);
}
// Local function to extract a particular component from a 3c ensemble
// returning a scalar time series ensemble that can be passed downstream
// for processing by this program.  
auto_ptr<TimeSeriesEnsemble> Convert3CEnsemble(ThreeComponentEnsemble *tcse,
			string compname,Hypocenter hypo,string phase)
{
	int i;
	TimeSeries *x;
	// Hard wired for now.  May be a parameter eventually
	const double vp0def(6.0),vs0def(3.5);
	auto_ptr<TimeSeriesEnsemble> result(new
		TimeSeriesEnsemble(dynamic_cast<Metadata&> (*tcse),
				tcse->member.size()));
	if(compname.find_first_of("ZNE")!=string::npos)
	{
	    for(i=0;i<tcse->member.size();++i)
	    {
		if(!tcse->member[i].live) continue;
		// First make certain we are in cardinal coordinates
		tcse->member[i].rotate_to_standard();
		if(compname=="E")
			x=ExtractComponent(tcse->member[i],0);
		else if(compname=="N")
			x=ExtractComponent(tcse->member[i],1);
		else
			x=ExtractComponent(tcse->member[i],2);
		result->member.push_back(*x);
		delete x;
	    }
	}
	else if(compname.find_first_of("TRL")!=string::npos)
	{
	    for(i=0;i<tcse->member.size();++i)
            {
		if(!tcse->member[i].live) continue;
		double vp0,vs0;
		double stalat,stalon,staelev;
		tcse->member[i].rotate_to_standard();
		try {
			vp0=tcse->member[i].get_double("vp0");
			vs0=tcse->member[i].get_double("vs0");
		} catch (MetadataGetError mde) {
			cerr << "Warning required vp0 and vs0 not defined. "
				<< "using default="
				<< vp0def << ", "<<vs0def <<endl;
			vp0=vp0def;
			vs0=vs0def;
		}
		try {
			stalat=tcse->member[i].get_double("lat");
			stalon=tcse->member[i].get_double("lon");
			staelev=tcse->member[i].get_double("elev");
		}
		catch (MetadataGetError mde)
		{
			mde.log_error();
			throw SeisppError(string("XcorProcessingEngine:")
				+string("  Failure in three component processing"));
			
		}
		SlownessVector u=hypo.phaseslow(stalat,stalon,staelev,phase);
		try {
			tcse->member[i].free_surface_transformation(u,vp0,vs0);
		} catch (SeisppError serr)
		{
			serr.log_error();
			cerr << "Using simple ray coordinates for station "
				<< tcse->member[i].get_string("sta")
				<<endl;
			SphericalCoordinate sc=PMHalfspaceModel(vp0,vs0,
				u.ux,u.uy);
			tcse->member[i].rotate(sc);
		}
		if(compname=="T")
			x=ExtractComponent(tcse->member[i],0);
		else if(compname=="R")
			x=ExtractComponent(tcse->member[i],1);
		else
			x=ExtractComponent(tcse->member[i],2);
		result->member.push_back(*x);
		delete x;
	    }
	}
	else
		throw SeisppError(string("XcorProcessingEngine:")
			+ string("Cannot handle component name=")
			+ compname);
	return result;
}
void XcorProcessingEngine::load_data(Hypocenter & h)
{
    try {
	current_data_window=TimeWindow(h.time+raw_data_twin.start,
		h.time+raw_data_twin.end);
	UpdateGeometry(current_data_window);
	// Read raw data.  Using an auto_ptr as good practice
	// 3c mode can work for either cardinal directions or
	// applying transformations.  In either case we use
	// the temporary auto_ptr to hold the data read in
	auto_ptr<TimeSeriesEnsemble> tse;
	if(RequireThreeComponents)
	{
		string chan_allowed("ZNELRT");
		if(analysis_setting.component_name
			.find_first_of(chan_allowed,0)<0)
		{
			throw SeisppError(
				string("XcorProcessingEngine::load_data:")
				+ string(" Illegal channel code specified.")
				+ string(" Must be Z,N,E,L,R, or T") );
		}
		ThreeComponentEnsemble *tcse;
		tcse = array_get_data(
  			   stations,
                           h,
			   analysis_setting.phase_for_analysis,
                           raw_data_twin,
                           analysis_setting.tpad,
                           dynamic_cast<DatabaseHandle&>(waveform_db_handle),
			   stachanmap,
                           ensemble_mdl,
                           trace_mdl,
                           am);
		// This specialized function could be generalized, but
		// done in one pass here for efficiency.  Local function
		// to this file found above
		tse=auto_ptr<TimeSeriesEnsemble>(Convert3CEnsemble(tcse,
			analysis_setting.component_name,h,
			analysis_setting.phase_for_analysis));	
		delete tcse;
	}
	else
	{
		tse=auto_ptr<TimeSeriesEnsemble>(array_get_data(
  			   stations,
                           h,
			   analysis_setting.phase_for_analysis,
			   analysis_setting.component_name,
                           raw_data_twin,
                           analysis_setting.tpad,
                           dynamic_cast<DatabaseHandle&>(waveform_db_handle),
                           ensemble_mdl,
                           trace_mdl,
                           am));
	}
	StationTime predarr=ArrayPredictedArrivals(stations,h,
		analysis_setting.phase_for_analysis); 

	// Following not needed because regular_gather is now
	// stored as an auto_ptr.
	//if (regular_gather != NULL) { delete regular_gather; regular_gather=NULL;}
	regular_gather=auto_ptr<TimeSeriesEnsemble>
			(AssembleRegularGather(*tse,predarr,
			analysis_setting.phase_for_analysis,
            		regular_gather_twin,target_dt,rdef,true));
	// This should probably be in the libseispp library, but
	// we'll hard code it here for now.  Need to set defaults
	// on all the computed metadata for all members of the
	// ensemble.  Without this there can be state issues about
	// whether these are loaded that cause metadata related
	// exceptions to be thrown.
	for(int i=0;i<regular_gather->member.size();++i)
	{
		double atime;
		regular_gather->member[i].put(coherence_keyword,0.0);
		regular_gather->member[i].put(stack_weight_keyword,0.0);
		regular_gather->member[i].put(peakxcor_keyword,0.0);
		regular_gather->member[i].put(amplitude_static_keyword,0.0);
		regular_gather->member[i].put(moveout_keyword,MoveoutBad);
		// Load initial arrival times as predicted time
		// This is a bit dogmatic.  may want to make this an if not
		// defined  set this field.  for not just do it. 
		atime=regular_gather->member[i].get_double(predicted_time_key);
		regular_gather->member[i].put(arrival_time_key,atime);
	}
	
	// Load filtered data into waveform_ensemble copy
	// of this data.
	if(use_subarrays)
	{
		int nsubs=stations.number_subarrays();
		auto_ptr<TimeSeriesEnsemble> csub;
		for(current_subarray=0;current_subarray<nsubs;
				++current_subarray)
		{
			SeismicArray subnet=stations.subset(current_subarray);
			csub=ArraySubset(*regular_gather,subnet);
			if((*csub).member.size()>0) break;
		} 
		if(current_subarray>=nsubs) 
			throw SeisppError(
				string("XcorProcessingEngine::load_data: ")
				+ string("  error in subarray definitions.  ")
				+ string("All subarrays have no data for this event"));
		waveform_ensemble=*csub;
	}
	else
	{
		waveform_ensemble=*regular_gather;
	}
	FilterEnsemble(waveform_ensemble,analysis_setting.filter_param);
	if(autoscale_initial)
	{
		MeasureEnsemblePeakAmplitudes<TimeSeriesEnsemble,TimeSeries>
			(waveform_ensemble,gain_keyword);
		ScaleEnsemble<TimeSeriesEnsemble,TimeSeries>
			(waveform_ensemble,gain_keyword,true);
		ScaleCalib<TimeSeriesEnsemble>
			(waveform_ensemble,gain_keyword,amplitude_static_keyword); 
	}
	else
	{
		double initial_gain=1.0;
		InitializeEnsembleAttribute<TimeSeriesEnsemble,double>
			(waveform_ensemble,gain_keyword,initial_gain);
	}
	
	// We need to always reset these
	xcorpeak_cutoff=xcorpeak_cutoff_default;
	coherence_cutoff=coherence_cutoff_default;
	stack_weight_cutoff=stack_weight_cutoff_default;
    }
    catch (...) {throw;}
}
// This applies a second filter to the data beyond the base filter.  
// This is often necessary. e.g. demean followed by integration.
//
void XcorProcessingEngine::filter_data(TimeInvariantFilter f)
{
	FilterEnsemble(waveform_ensemble,f);
}
void XcorProcessingEngine::save_results(int evid, int orid )
{
	// First save the beam
	int pwfid;
	string pchan(analysis_setting.component_name);
	string filter(analysis_setting.filter_param.type_description(true));
	string filter_param;
	try {
		int record;
		// First get and save the array beam
		TimeSeries beam=mcc->ArrayBeam();
		int fold=beam.get_int("fold");
		filter_param=beam.get_string("filter_spec");
		// Set dir and dfile
		beam.put("wfprocess.dir",beam_directory);
		beam.put("dir",beam_directory);
		beam.put("wfprocess.dfile",beam_dfile);
		beam.put("wfprocess.endtime",beam.endtime());
		record=dbsave(beam,dbwfprocess,string("wfprocess"),
				beam_mdl,am);
		dbwfprocess.record=record;
		dbgetv(dbwfprocess,0,"pwfid",&pwfid,0);
		double beam_amplitude=beam.get_double(beam_rms_key);
		//
		// For the present the chan code will be the same
		// as pchan
		record=dbaddv(dbxcorbeam,0,"netname",netname.c_str(),
			"chan",pchan.c_str(),
			"pchan",pchan.c_str(),
			"phase",analysis_setting.phase_for_analysis.c_str(),
			"pwfid",pwfid,
			"filter",filter_param.c_str(),
			"robustt0",analysis_setting.robust_tw.start,
			"robusttwin",analysis_setting.robust_tw.length(),
			"fold",fold,
			"amp",beam_amplitude,
				0);
		if(record<0)
			cerr << "save_results(Warning):  problems adding to xcorbeam table"<<endl;
		dbaddv(dbevlink,0,"evid",evid,"pwfid",pwfid,0);
	}
	catch (MetadataGetError mderr)
	{
		cerr << "Problems getting attributes needed for xcorbeam"<<endl
			
			<< "Coding error or problem in MetadataList parameters"
			<< endl;
		mderr.log_error();
	}
	catch (...) 
	{
		cerr << "Unexpected exception"<<endl;
		throw;
	}
		
	// I think static in this context means they are set once
	// and only once at startup
	static const string auth("dbxcorr");
	// Since this is hidden behind the interface I'm going
	// to use the standard datascope API instead of going
	// through the DatascopeHandle API.  Since this code
	// is locked into Datascope anyway, this should not be
	// an issue.  It should execute faster without the additional
	// overhead of another layer of software.
	// Save arrival and assoc trace by trace.
	//
	vector<TimeSeries>::iterator trace;
	int i;
	for(i=0, trace=waveform_ensemble.member.begin();
		trace!=waveform_ensemble.member.end();++trace,++i)
	{
		string sta;
		string chan;
		string phase;
		double atime;
		double lag;
		double xcorpeak,coh,stack_weight,amplitude;
		double gain;
		int record;
		int arid;
		// Skip data marked dead
		if(trace->live)
		{
		    try {
			lag=trace->get_double(moveout_keyword);
			//
			// Skip data marked with indeterminate moveout
			// This is used by MultichannelCorrelator to 
			// flag data it could not handle
			//
			if(fabs(lag)>MoveoutBadTest) continue;
			sta=trace->get_string("sta");
			chan=trace->get_string("chan");
			atime=trace->get_double(arrival_time_key);
			xcorpeak=trace->get_double(peakxcor_keyword);
			coh=trace->get_double(coherence_keyword);
			stack_weight=trace->get_double(stack_weight_keyword);
			amplitude=trace->get_double(amplitude_static_keyword);
			// We accumulate amplitude changes in gain so we
			// need to get the final amplitude adjustment using
			// the gain attribute
			gain=trace->get_double(gain_keyword);
			amplitude *=gain;
			// Write nothing for events that don't satisfy
			// all of the criteria on xcor, coherence, or weight
			if( (xcorpeak>xcorpeak_cutoff)
				&& (coh>coherence_cutoff)
				&& (stack_weight>stack_weight_cutoff) )
			{
			    filter_param=trace->get_string("filter_spec");
			    record=dbaddv(dbxcorarrival,0,"sta",sta.c_str(),
					"chan",chan.c_str(),
					"phase",analysis_setting.phase_for_analysis.c_str(),
					"pwfid",pwfid,
					"filter",filter_param.c_str(),
					"algorithm",auth.c_str(),
					"pchan",pchan.c_str(),
					"time",atime,
					"twin",analysis_setting.analysis_tw.length(),
					"samprate",1.0/(trace->dt),
					"stackwgt",stack_weight,
					"coherence",coh,
					"relamp",amplitude,
					"xcorpeak",xcorpeak,0);
			    if(record<0)
			    {
				cerr << "save_results(warning):  problems saving xcorarrival table"
					<<endl;
			    }
			    if(save_arrival)
			    {
				record = dbaddv(dbarrival,0,
					"sta",sta.c_str(),
					"chan",chan.c_str(),
					"time",atime,
					"iphase",
					  analysis_setting.phase_for_analysis.c_str(),
					"auth",auth.c_str(),
					0);
				if(record<0)
				{
					cerr<<"save_results:  problems saving data to arrival for station "
						<< sta<<endl;
				}
				else
				{
					dbarrival.record=record;
					dbgetv(dbarrival,0,
						"arid",&arid,0);
					
					// missing for now:  delta, seaz,esaz
					// need these eventually, but for 
					// debugging purposes will leave them
					// aside for a bit
					dbaddv(dbassoc,0,
						"arid",arid,
						"orid",orid,
						"sta",sta.c_str(),
						"phase",
					  	  analysis_setting
						   .phase_for_analysis.c_str(),
						"timeres",lag,
						"timedef","d",
						"wgt",stack_weight,0);
				}
			    }
			}
		    }
		    catch (MetadataGetError mderr)
		    {
			cerr << "save_results:  problem with trace number "
				<< i << endl;
			mderr.log_error();
		    }				
		}
	}
}	
// These are private functions hidden by the interface
void XcorProcessingEngine::UpdateGeometry(TimeWindow twin)
{
	if(stations.GeometryIsValid(twin))
	{
		return;
	}
	else
	{
		stations=SeismicArray(dynamic_cast<DatabaseHandle&>(waveform_db_handle),
			twin.start,netname);
		if(use_subarrays) 
			load_subarrays_from_pf(stations,pf_used_by_engine);
	}
	if(stations.array.size()<=0)
		throw SeisppError(string("XcorProcessingEngine::UpdateGeometry -- network name=")
			+ netname
			+ string(" has no active stations at current time.") );
}
// These are the display functions.  I put them here at the bottom because
// they will change dramatically when Peng returns and has time to clean up
// his enhanced gui.
//
void XcorProcessingEngine::display_data()
{
//	dataplot=new SeismicPlot(waveform_ensemble,data_display_md);
//	dataplot->draw();
//	cout << "Click with MB2 in plot window to continue"<<endl;
//	int junk=dataplot->select_member();
//	delete dataplot;
//	dataplot=NULL;
}
void XcorProcessingEngine::display_correlations()
{
//	xcorplot=new SeismicPlot(mcc->xcor,xcor_display_md);
//	xcorplot->draw();
//	cout << "Click with MB2 in plot window to continue"<<endl;
//	int junk=xcorplot->select_member();
//	delete xcorplot;
//	dataplot=NULL;
}
void XcorProcessingEngine::display_beam()
{
	// A bit more complicated since the beam is not an ensemble
	TimeSeries beam=mcc->ArrayBeam();
	TimeSeriesEnsemble tse(1,beam.ns);
	tse.member[0]=beam;
//	beamplot=new SeismicPlot(tse,beam_display_md);
//	beamplot->draw();
//	cout << "Click with MB2 in plot window to continue"<<endl;
//	int junk=beamplot->select_member();
//	delete beamplot;
//	beamplot=NULL;
}
// Get a relative time from the beam trace to use to a correct 
// time reference for output arrival times
PointPick XcorProcessingEngine::pick_beam()
{
	// A bit more complicated since the beam is not an ensemble
	TimeSeries beam=mcc->ArrayBeam();
	TimeSeriesEnsemble tse(1,beam.ns);
	tse.member[0]=beam;
//	beamplot=new SeismicPlot(tse,beam_display_md);
//	cout << "Pick arrival time on beam trace with MB2"<<endl;
	// This is necessary in current SeismicPlot implementation
	// to get proper initialization.  Eventually probably should
	// allow functions like pick_point to be called without
	// this initialization.
//	beamplot->draw();  
//	PointPick arrival_pick=beamplot->pick_point();
//	delete beamplot;
//	beamplot=NULL;
	return PointPick();
}
// this applies time shift set by pick_beam;
void XcorProcessingEngine::shift_arrivals(double tshift)
{
	vector<TimeSeries>::iterator trace;
	for(trace=waveform_ensemble.member.begin();
                trace!=waveform_ensemble.member.end();++trace)
	{
		//
		// Intentionally not putting a try block here to 
		// catch an exception for moveout not being defined.
		// Shouldn't get here in that condition so I'm
		// intentionally avoiding the overhead of a try block
		// glp 1/13/2006
		//
		try {
		  if(trace->live)
		  {
			double moveout=trace->get_double(moveout_keyword);
			if(fabs(moveout)<MoveoutBadTest)
			{
				moveout+=tshift;
				trace->put(moveout_keyword,moveout);
			}
		  } 
		} catch (MetadataGetError mde) {
		    mde.log_error();
		}
	}
}
	
void XcorProcessingEngine::do_all_picks()
{
	char ctest;
	//if(dataplot!=NULL) delete dataplot;
//	dataplot=new SeismicPlot(waveform_ensemble,data_display_md);
//	dataplot->draw();
	do {
		cout << "Pick reference trace with MB2"<<endl;
//		analysis_setting.reference_trace=dataplot->select_member();
		cout << "Selected trace "<<analysis_setting.reference_trace
		  << "as reference"<<endl;
		cout << "Accept (y or n)?";
		ctest=fgetc(stdin);
		cout << "Read "<<ctest<<endl;
	} while (ctest!='y');
	cout << endl;
	analysis_setting.rt_set=true;
	//
	// Note for future:  for this interface I'm making analysis
	// window invariant.  Final interface should allow it to be
	// changed when desired.  For now it is pretty much the whole
	// data window
	//
	do {
		cout << "Select beam time window with MB3"<<endl;
//		analysis_setting.beam_tw=dataplot->pick_time_window();
		cout << "Beam time window set as "
			<< analysis_setting.beam_tw.start
			<<" to "
			<< analysis_setting.beam_tw.end<<endl;
		cout << "Accept (y or n)?";
		cin >> ctest;
	} while (ctest!='y');
	cout << endl;
	analysis_setting.bw_set=true;
	
	do {
		cout << "Select robust stack time window with MB3"<<endl;
//		analysis_setting.robust_tw=dataplot->pick_time_window();
		cout << "Robust stack time window set as "
			<< analysis_setting.robust_tw.start
			<<" to "
			<< analysis_setting.robust_tw.end<<endl;
		cout << "Accept (y or n)?";
		cin >> ctest;
	} while (ctest!='y');
	cout << endl;
	analysis_setting.rw_set=true;
//	delete dataplot;
//	dataplot=NULL;
}
int XcorProcessingEngine::pick_one_trace()
{
	int result;
//	dataplot=new SeismicPlot(waveform_ensemble,data_display_md);
//	dataplot->draw();
//	cout << "Pick desired trace with MB2"<<endl;
//	result=dataplot->select_member();
//	delete dataplot;
//	dataplot=NULL;
	return(1);
}
void XcorProcessingEngine::pick_cutoff()
{
	int cutoff_trace;

	cout << "Select trace to use as cutoff with MB2"<<endl;
//	dataplot=new SeismicPlot(waveform_ensemble,data_display_md);
//        dataplot->draw();
//	cutoff_trace=dataplot->select_member();
	// Intentionally not aim to catch Metadata errors in this switch block
	// Assumption is we don't get here without being sure this
	// will not cause an exception.
	switch (analysis_setting.result_sort_order)
	{
	case COHERENCE:
		coherence_cutoff=waveform_ensemble
			.member[cutoff_trace].get_double(coherence_keyword);
		cout << "Coherence cutoff set to " 
			<< coherence_cutoff 
			<< " using ensemble member "
			<< cutoff_trace << endl;
	        break;
	case CORRELATION_PEAK:
		xcorpeak_cutoff=waveform_ensemble
			.member[cutoff_trace].get_double(peakxcor_keyword);
		cout << "Peak correlation cutoff set to " 
			<< xcorpeak_cutoff 
			<< " using ensemble member "
			<< cutoff_trace << endl;
	        break;
	case AMPLITUDE:
		cerr << "Amplitude cannot used be used as cutoff attribute."<<endl
			<<"Change sort ensemble sort order"<<endl;
	        break;
	case LAG:
		cerr << "Time lag cannot used be used as cutoff attribute."<<endl
			<<"Change sort ensemble sort order"<<endl;
	        break;
	case WEIGHT:
	default:
		stack_weight_cutoff=waveform_ensemble
			.member[cutoff_trace].get_double(stack_weight_keyword);
		cout << "Stack weight cutoff set to " 
			<< stack_weight_cutoff
			<< " using ensemble member "
			<< cutoff_trace << endl;
	        break;
	}
//	delete dataplot;
//	dataplot=NULL;
	
	
}
void XcorProcessingEngine::edit_data()
{
	cout << "Click on traces to delete with MB2"<<endl
		<< "Double click final trace you want to delete"<<endl;
//	dataplot=new SeismicPlot(waveform_ensemble,data_display_md);
//      dataplot->draw();
//	int kill_trace,last_kill=-1;
//	while( (kill_trace=dataplot->select_member()) != last_kill)
//	{
//			last_kill=kill_trace;
//			waveform_ensemble.member[kill_trace].live=false;
//			waveform_ensemble.member[kill_trace].s.clear();
//			waveform_ensemble.member[kill_trace].ns=0;
//	}
//	delete dataplot;
//	dataplot=NULL;
	// This seems necessary for cleanup
	vector<TimeSeries>::iterator ens_iter;
	// Must traverse the container in reverse order as 
	// erase leaves iterators after the deletion point undefined
	for(ens_iter=waveform_ensemble.member.end();
		ens_iter!=waveform_ensemble.member.begin();--ens_iter)
	{
		if((*ens_iter).live)continue; 
		waveform_ensemble.member.erase(ens_iter);
	}
}
void XcorProcessingEngine::restore_original_ensemble()
{
	if(use_subarrays)
	{
		SeismicArray subnet=stations.subset(current_subarray);
		auto_ptr<TimeSeriesEnsemble> csub(ArraySubset(*regular_gather,subnet));
		waveform_ensemble=*csub;
	}
	else
	{
		waveform_ensemble=*regular_gather;
	}
	FilterEnsemble(waveform_ensemble,analysis_setting.filter_param);
	if(autoscale_initial)
	{
		MeasureEnsemblePeakAmplitudes<TimeSeriesEnsemble,TimeSeries>
			(waveform_ensemble,gain_keyword);
		ScaleEnsemble<TimeSeriesEnsemble,TimeSeries>
			(waveform_ensemble,gain_keyword,true);
		ScaleCalib<TimeSeriesEnsemble>
			(waveform_ensemble,gain_keyword,amplitude_static_keyword); 
	}
	else
		InitializeEnsembleAttribute<TimeSeriesEnsemble,double>
			(waveform_ensemble,gain_keyword,(double)1.0);
}
void XcorProcessingEngine::next_subarray()
{
	// Note this method intentionally does nothing if use_subarrays is false.
	if(use_subarrays)
	{
	    try{
		++current_subarray;
		SeismicArray ss=stations.subset(current_subarray);
		current_subarray_name=ss.name;
		auto_ptr<TimeSeriesEnsemble> csub(ArraySubset(*regular_gather,ss));
		waveform_ensemble=*csub;
		FilterEnsemble(waveform_ensemble,analysis_setting.filter_param);
		if(autoscale_initial)
		{
		    MeasureEnsemblePeakAmplitudes<TimeSeriesEnsemble,TimeSeries>
			(waveform_ensemble,gain_keyword);
		    ScaleEnsemble<TimeSeriesEnsemble,TimeSeries>
			(waveform_ensemble,gain_keyword,true);
		    ScaleCalib<TimeSeriesEnsemble>
			(waveform_ensemble,gain_keyword,amplitude_static_keyword); 
		}
		else
			InitializeEnsembleAttribute<TimeSeriesEnsemble,double>
				(waveform_ensemble,gain_keyword,(double)1.0);
		    
	    } catch (...) {throw;}
	}
	else
	    throw SeisppError(string("XcorProcessingEngine::next_subarray():  ")
			+ string("usage error.  ")
			+ string("This method should be called only when using subarrays"));
}
int XcorProcessingEngine::number_subarrays()
{
	return(stations.number_subarrays());
}

