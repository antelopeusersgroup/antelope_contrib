#include <math.h>
#include "seispp.h"
namespace SEISPP {
using namespace SEISPP;
/* This file contains helper procedures for Three_Component_Seismogram objects.  Most
are truly procedural and take a Three_Component_Seismogram object or a Three_Component_Seismogram_Ensemble
object and return one or the other.  

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
	tw - Time_Window in relative time units wrt arrival time defining
		section of data to be extracted.  Time 0 of the result
		will be the arrival time.

*/

Three_Component_Seismogram& Arrival_Time_Reference(Three_Component_Seismogram& tcsi,
	string arrival_time_key,
		Time_Window tw)
{
	double atime;
	string base_error_message("Arrival_Time_Reference: ");
	try
	{
		atime = tcsi.get_double(arrival_time_key);
	// Intentionally use the base class since the contents are discarded
	// get_double currently would throw a Metadata_get_error
	} catch (Metadata_error mde)
	{
		throw seispp_error(base_error_message
				+ arrival_time_key
				+ string(" not found in Three_Component_Seismogram object"));
	}
	// We have to check this condition because ator will do nothing if
	// time is already relative and this condition cannot be tolerated
	// here as we have no idea what the time standard might be otherwise
	if(tcsi.tref == relative)
		throw seispp_error(string("Arrival_Time_Reference:  ")
			+ string("received data in relative time units\n")
			+ string("Cannot proceed as timing is ambiguous"));

	// start with a clone of the original
	Three_Component_Seismogram *tcso=new Three_Component_Seismogram(tcsi);
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
		// This is not the fastest way to do this, but it is
		// clearer and the performance hit should not be serious
		// old advice:  make it work before you make it fast
		tcso->u = dmatrix(3,ns_to_copy);
		tcso->ns=ns_to_copy;
		for(i=0;i<3;++i)
			for(j=0,jj=jstart;j<ns_to_copy;++j,++jj)
				tcso->u(i,j)=tcsi.u(i,jj);
		// Now we have to update these metadata.  
		// Rather than abort if the entries are missing we just
		// print a message.  Could get verbose, but die is
		// to brutal and not always required in this situation
		if(jstart>0)
		{
			try{
				double stime=tcso->get_double("starttime");
				stime -= atime;
				tcso->put_metadata("starttime",stime);
			} catch (Metadata_error mde)
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
				tcso->put_metadata("endtime",etime);
			} catch (Metadata_error mde)
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
Three_Component_Ensemble& Arrival_Time_Reference(Three_Component_Ensemble& tcei,
	string arrival_time_key,
		Time_Window tw)
{
	//Use the default constuctor because the output size may not match
	// the input when errors occur
	Three_Component_Ensemble *tceo=new Three_Component_Ensemble();
	tceo->tcse.reserve(tcei.tcse.size());  // reserve this many slots for efficiency
	// We have to use a loop instead of for_each as I don't see how
	// else to handle errors cleanly here.
	vector<Three_Component_Seismogram>::iterator indata;
	for(indata=tcei.tcse.begin();indata!=tcei.tcse.end();++indata)
	{
		Three_Component_Seismogram tcs;
		try {
			tcs = Arrival_Time_Reference(*indata,
				arrival_time_key,tw);
			tceo->tcse.push_back(tcs);
		} catch ( seispp_error serr)
		{
			serr.log_error();
		}
	}
	return(*tceo);
}
	

} // end SEISPP namespace encapsulation
