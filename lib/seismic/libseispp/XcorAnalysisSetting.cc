#include "XcorAnalysisSetting.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP{
XcorAnalysisSetting::XcorAnalysisSetting(Metadata md)
{
	try
	{
		// tpad perhaps should be qualified as this is a generic 
		// name.  Used, for example, in XcorProcessingEngine too
		tpad=md.get_double("tpad");  
		// Similarly these parameters are also used in
		// constructor for XcorProcessingEngine.  Not
		// an ideal design, but workable here.
		gather_twin.start=md.get_double("regular_gather_twin_start");
		gather_twin.end=md.get_double("regular_gather_twin_end");
	        if(gather_twin.end-gather_twin.start
	        			<= 2.0*tpad)
	        {
	        	analysis_tw=gather_twin;
		}
	        else
	        {
	        	analysis_tw
	        		  =TimeWindow(gather_twin.start-tpad,
	        	  		gather_twin.end+tpad);
	        }
	        aw_set=true;

	        // Use a multiplier to reduce to fraction of analysis
	        // window as default
	        double frac=md.get_double("beam_window_fraction");
	        beam_tw
	        		= TimeWindow(frac*gather_twin.start,
	        		    frac*gather_twin.end);
	        bw_set=true;

	        // Use a multiplier to reduce to fraction of analysis
	        // window as default
	        frac=md.get_double("robust_window_fraction");
	        robust_tw
	        		= TimeWindow(frac*gather_twin.start,
	        		    frac*gather_twin.end);
	        rw_set=true;
	        rt_set=false;  // No rational way to default this
	        reference_trace=0;  // best to set this anyway
	        string stype=md.get_string("stacking_method");
	        if((stype=="basic") || (stype=="simple") )
	        	stack_type=BasicStack;
	        else if(stype=="median")
	        	stack_type=MedianStack;
	        else if(stype=="robust")
	        	stack_type=RobustSNR;
	        else
	        {
	        	cerr << "XcorAnalysisSetting Metadata driven constructor:  "
	        		<< "unknown stack type ="
	        		<< stack_type
	        		<<endl
	        		<<"Using robust method as default"<<endl; 
	        	stack_type=RobustSNR;
	        }
	        string default_filter=md.get_string("default_filter");
	        filter_param=TimeInvariantFilter(default_filter);
	        component_name=md.get_string("component_for_analysis");
	        if(component_name=="E")
	        	component_for_analysis=0;
	        else if(component_name=="N")
	        	component_for_analysis=1;
	        else if(component_name=="Z")
	        	component_for_analysis=2;
	        else if(component_name=="T")
	        	component_for_analysis=0;
	        else if(component_name=="R")
	        	component_for_analysis=1;
	        else if(component_name=="L")
	        	component_for_analysis=2;
	        else
	        {
	        	cerr << "XcorAnalysisSetting Metadata driven constructor:  "
	        		<< "cannot handle component = "
	        		<< component_name 
	        		<<endl
	        		<<"Defaulted to vertical"<<endl;
	        	component_for_analysis=2;
	        	component_name=string("Z");
	        }
		arrival_chan_code=md.get_string("arrival_channel");
		// Silently keep only the last character if entry
		// is not a single character.
		if( arrival_chan_code.length()>1)
		{
			string stmp(arrival_chan_code,arrival_chan_code.length()-1,1);
			arrival_chan_code=stmp;
		}
	        phase_for_analysis=md.get_string("phase_for_analysis");
		//
		// Get sort order for analysis output
		//
		string sortname=md.get_string("analysis_sort_order");
		if(sortname=="coherence")
			result_sort_order=COHERENCE;
		else if(sortname=="correlation_peak")
			result_sort_order=CORRELATION_PEAK;
		else if(sortname=="amplitude")
			result_sort_order=AMPLITUDE;
		else if(sortname=="moveout")
			result_sort_order=LAG;
		else if(sortname=="stack_weight")
			result_sort_order=WEIGHT;
		else if(sortname=="database_arrival_time")
			result_sort_order=DBARRIVAL_TIME;
		else if(sortname=="measured_arrival_time")
			result_sort_order=ARRIVAL_TIME;
		else
		{
			cerr << "unknown keyword for analysis_sort_order parameter="
				<< sortname <<endl
				<< "Defaulted to stack_weight"<<endl;
			result_sort_order=WEIGHT;
		}
		// Default on construction is always normal sort order.  User must 
		// explicitly set this true to change the default.
		sort_reverse=false;
		/* We hard code the default as stack and sum as that is the normal operation
		mode and default for MultichannelCorrelator constructor */
		mccmode=CORRELATE_AND_STACK;
	        	                      
	} catch (MetadataGetError mderr)
	{
		mderr.log_error();
		throw SeisppError("XcorAnalysisSetting construction failed");
	}
	catch (SeisppError serr)
	{
		throw serr;
	}
	catch (...)
	{
		throw SeisppError("XcorAnalysisSetting constructor:  Unknown error was thrown");
	}
}
XcorAnalysisSetting::XcorAnalysisSetting(const XcorAnalysisSetting& old)
{
	aw_set=old.aw_set;
	bw_set=old.bw_set;
	rw_set=old.rw_set;
	rt_set=old.rt_set;
	gather_twin=old.gather_twin;
	analysis_tw=old.analysis_tw;
	beam_tw=old.beam_tw;
	robust_tw=old.robust_tw;
	reference_trace=old.reference_trace;
	removed_traces=old.removed_traces;
	stack_type=old.stack_type;
	phase_time_pick=old.phase_time_pick;
	filter_param=old.filter_param;
	component_for_analysis=old.component_for_analysis;
	component_name=old.component_name;
	phase_for_analysis=old.phase_for_analysis;
	tpad=old.tpad;
	result_sort_order=old.result_sort_order;
	arrival_chan_code=old.arrival_chan_code;
	sort_reverse=old.sort_reverse;
	mccmode=old.mccmode;
}
XcorAnalysisSetting& XcorAnalysisSetting::operator=(const XcorAnalysisSetting& old)
{
    if(this!=&old)
    {
	aw_set=old.aw_set;
	bw_set=old.bw_set;
	rw_set=old.rw_set;
	rt_set=old.rt_set;
	beam_tw=old.beam_tw;
	gather_twin=old.gather_twin;
	analysis_tw=old.analysis_tw;
	robust_tw=old.robust_tw;
	reference_trace=old.reference_trace;
	removed_traces=old.removed_traces;
	stack_type=old.stack_type;
	phase_time_pick=old.phase_time_pick;
	filter_param=old.filter_param;
	component_for_analysis=old.component_for_analysis;
	component_name=old.component_name;
	phase_for_analysis=old.phase_for_analysis;
	tpad=old.tpad;
	result_sort_order=old.result_sort_order;
	arrival_chan_code=old.arrival_chan_code;
	sort_reverse=old.sort_reverse;
	mccmode=old.mccmode;
    }
    return(*this);
}
} // End SEISPP namespace declaration
