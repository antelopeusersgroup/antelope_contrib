#include <math.h>
#include "seispp.h"
namespace SEISPP {
using namespace SEISPP;
/* This file is very very similar to a related set of procedures found
in the file three_component_helpers.cc.  Future efforts could merge
them into a template version standardizes what these do.  
At the time of this conversion (Aug 2005) this and it's parent do
only one thing -- they contain procedures to take seismograms in
defined with an absolute time standard and convert them to an
"arrival time reference frame" .  See below for a description.

Author:  Gary L. Pavlis
	Indiana University
	pavlis@indiana.edu
*/


/* This function converts a seismogram from an absolute time standard to 
an arrival time reference frame using an arrival time stored in the 
metadata area of the object and referenced by a variable keyword.  
A time window is relative time units defines the portion to be extracted.
If the window is larger than the data the whole array of data is copied
to the result.  If the window is smaller a subset is returned and 
appropriate parameters are updated.

Arguments:
	tcsi - input data.  Assumed to be in the reference frame that
		matches the arrival time to be used to define time window
	arrival_time_key - metadata keyword string used to extract the
		time used to define time 0.  i.e. this is the keyword
		used to access a metadata field that defines the time
		that will become time 0 o the result.
	tw - TimeWindow in relative time units wrt arrival time defining
		section of data to be extracted.  Time 0 of the result
		will be the arrival time.

*/

TimeSeries& ArrivalTimeReference(TimeSeries& tcsi,
	string arrival_time_key,
		TimeWindow tw)
{
	double atime;
	string base_error_message("ArrivalTimeReference: ");
	try
	{
		atime = tcsi.get_double(arrival_time_key);
	// Intentionally use the base class since the contents are discarded
	// get_double currently would throw a MetadataGetError
	} catch (MetadataError& mde)
	{
		throw SeisppError(base_error_message
				+ arrival_time_key
				+ string(" not found in TimeSeries object"));
	}
	// We have to check this condition because ator will do nothing if
	// time is already relative and this condition cannot be tolerated
	// here as we have no idea what the time standard might be otherwise
	if(tcsi.tref == relative)
		throw SeisppError(string("ArrivalTimeReference:  ")
			+ string("received data in relative time units\n")
			+ string("Cannot proceed as timing is ambiguous"));

	// start with a clone of the original
	TimeSeries *tcso=new TimeSeries(tcsi);
	tcso->ator(atime);  // shifts to arrival time relative time reference

	// Extracting a subset of the data is not needed when the requested
	// time window encloses all the data
	// Note an alternative approach is to pad with zeros and mark ends as 
	// a gap, but here I view ends as better treated with variable
	// start and end times
	if( (tw.start > tcso->t0)  || (tw.end<tcso->time(tcso->ns-1) ) )
	{
		int jstart, jend;
		int ns_to_copy;
		int i,j,jj;
		jstart = tcso->sample_number(tw.start);
		jend = tcso->sample_number(tw.end);
		if(jstart<0) jstart=0;
		if(jend>=tcso->ns) jend = tcso->ns - 1;
		ns_to_copy = jend - jstart + 1;
		tcso->s.reserve(ns_to_copy);
		tcso->ns=ns_to_copy;
		for(j=0,jj=jstart;j<ns_to_copy;++j,++jj)
			tcso->s[j]=tcsi.s[jj];
		// Now we have to update these metadata.  
		// Rather than abort if the entries are missing we just
		// print a message.  Could get verbose, but die is
		// to brutal and not always required in this situation
		if(jstart>0)
		{
			try{
				double stime=tcso->get_double("time");
				stime -= atime;
				tcso->put("time",stime);
			} catch (MetadataError& mde)
			{
				cerr << base_error_message << endl;
				mde.log_error();
			}
		}
		if(jend>=tcsi.ns)
		{
			try{
				double etime=tcso->get_double("endtime");
				etime -= atime;
				tcso->put("endtime",etime);
			} catch (MetadataError& mde)
			{
				cerr << base_error_message << endl;
				mde.log_error();
			}
		}
	}
	return(*tcso);
}
/* Similar function to above but processes an entire ensemble.  The only 
special thing it does is handle exceptions.  When the single object
processing function throws an exception the error is printed and the 
object is simply not copied to the output ensemble */
TimeSeriesEnsemble& ArrivalTimeReference(TimeSeriesEnsemble& tcei,
	string arrival_time_key,
		TimeWindow tw)
{
	int nmembers=tcei.member.size();
	// use the special constructor to only clone the metadata and 
	// set aside slots for the new ensemble.
	TimeSeriesEnsemble *tceo
		=new TimeSeriesEnsemble(dynamic_cast<Metadata&>(tcei),nmembers);
	tceo->member.reserve(nmembers);  // reserve this many slots for efficiency
	// We have to use a loop instead of for_each as I don't see how
	// else to handle errors cleanly here.
	vector<TimeSeries>::iterator indata;
	for(indata=tcei.member.begin();indata!=tcei.member.end();++indata)
	{
		try {
			TimeSeries tcs;
			tcs=ArrivalTimeReference(*indata,arrival_time_key,tw);
			tceo->member.push_back(tcs);
		} catch ( SeisppError& serr)
		{
			serr.log_error();
		}
	}
	return(*tceo);
}
	

} // end SEISPP namespace encapsulation
