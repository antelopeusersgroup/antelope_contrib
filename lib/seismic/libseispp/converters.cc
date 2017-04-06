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
        /* This absurd set of gyrations is needed to copy the BasicTimeSeries components of
         * the parent */
        BasicTimeSeries *bts=dynamic_cast<BasicTimeSeries*>(ts);
        BasicTimeSeries *pbts=dynamic_cast<BasicTimeSeries*>(&tcs);
        *bts=bts->BasicTimeSeries::operator=(*pbts);
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
	// Same crazy gyrations as above to copy BasicTimeSeries attributes
        BasicTimeSeries *bts=dynamic_cast<BasicTimeSeries*>(ts);
        BasicTimeSeries *pbts=dynamic_cast<BasicTimeSeries*>(&tcs);
        *bts=bts->BasicTimeSeries::operator=(*pbts);
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
