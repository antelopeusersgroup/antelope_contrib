#include "AnalysisSetting.h"
using namespace std;
using namespace SEISPP;
AnalysisSetting::AnalysisSetting(Metadata md)
{
	try
	{
		// tpad perhaps should be qualified as this is a generic 
		// name.  Used, for example, in XcorProcessingEngine too
		tpad=md.get_double("tpad");  
		// Similarly these parameters are also used in
		// constructor for XcorProcessingEngine.  Not
		// an ideal design, but workable here.
		TimeWindow gather_twin;
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
	        		  =TimeWindow(gather_twin.start+tpad,
	        	  		gather_twin.end-tpad);
	        }
	        aw_set=true;

	        // Use a multiplier to reduce to fraction of analysis
	        // window as default
	        double frac=md.get_double("beam_window_fraction");
	        beam_tw
	        		= TimeWindow(frac*analysis_tw.start,
	        		    frac*analysis_tw.end);
	        bw_set=true;

	        // Use a multiplier to reduce to fraction of analysis
	        // window as default
	        frac=md.get_double("robust_window_fraction");
	        robust_tw
	        		= TimeWindow(frac*analysis_tw.start,
	        		    frac*analysis_tw.end);
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
	        	cerr << "AnalysisSetting Metadata driven constructor:  "
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
	        	cerr << "AnalysisSetting Metadata driven constructor:  "
	        		<< "cannot handle component = "
	        		<< component_name 
	        		<<endl
	        		<<"Defaulted to vertical"<<endl;
	        	component_for_analysis=2;
	        	component_name=string("Z");
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
		else
		{
			cerr << "unknown keyword for analysis_sort_order parameter="
				<< sortname <<endl
				<< "Defaulted to stack_weight"<<endl;
			result_sort_order=WEIGHT;
		}
	        	                      
	} catch (MetadataGetError mderr)
	{
		mderr.log_error();
		throw SeisppError("AnalysisSetting construction failed");
	}
	catch (SeisppError serr)
	{
		throw serr;
	}
	catch (...)
	{
		throw SeisppError("AnalysisSetting constructor:  Unknown error was thrown");
	}
}
// Intentionally defaulted:  copy constructor, operator=, and destructor.
// Current object definition allows default to work correctly

