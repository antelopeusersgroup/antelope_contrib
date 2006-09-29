#include "seispp.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP
{
/* Extracts one component from a 3c seismogram returning the
 result as a pointer to a TimeSeries object.  Any exceptions are
simply rethrown. 

This version clones the entire metadata space of the parent to the 
output TimeSeries.  
*/
TimeSeries *ExtractComponent(ThreeComponentSeismogram& tcs,int component)
{
    try {
	TimeSeries *ts=new TimeSeries(dynamic_cast<Metadata&>(tcs),false);
	// There may be a clever way to do this, but here we manually
	// copy BasicTimeSeries attributes from tcs to result (ts).
	// Note this is a potential maintenance problem is BasicTimeSeries
	// changes
	ts->live=tcs.live;
	ts->t0=tcs.t0;
	ts->ns=tcs.ns;
	ts->dt=tcs.dt;
	ts->tref=tcs.tref;
	if(ts->live)
	    for(int i=0;i<tcs.ns;++i) 
		ts->s.push_back(tcs.u(component,i));
    	return(ts);
    }
    catch (...)
    {
	throw;
    }
}
// Overloaded version to do a selective copy
TimeSeries *ExtractComponent(ThreeComponentSeismogram& tcs,int component,
	MetadataList& mdl)
{
    try {
	Metadata mdclone;
	copy_selected_metadata(dynamic_cast<Metadata &>(tcs),
		mdclone,mdl);
	TimeSeries *ts=new TimeSeries(mdclone,false);
	// As above these from BasicTimeSeries need to be copied
	ts->live=tcs.live;
	ts->t0=tcs.t0;
	ts->ns=tcs.ns;
	ts->dt=tcs.dt;
	ts->tref=tcs.tref;
	for(int i=0;i<tcs.ns;++i) 
		ts->s.push_back(tcs.u(component,i));
	return(ts);
    }
    catch (...)
    {
	throw;
    }
}
} // End namespace SEISPP  definitions
