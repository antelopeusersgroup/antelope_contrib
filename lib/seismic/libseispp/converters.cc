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
TimeSeries *ExtractComponent(const ThreeComponentSeismogram& tcs,const int component)
{
    try {
	TimeSeries *ts=new TimeSeries(dynamic_cast<const BasicTimeSeries&>(tcs),
              dynamic_cast<const Metadata&>(tcs));
        /* Important - need to clear vector or we get nothing */
        ts->s.clear();
        double *ptr;
        dmatrix *uptr;
	if(ts->live)
	    for(int i=0;i<tcs.ns;++i) 
            {
              uptr=const_cast<dmatrix *>(&(tcs.u));
              ptr=uptr->get_address(component,i);
              ts->s.push_back(*ptr);
            }
    	return(ts);
    }
    catch (...)
    {
	throw;
    }
}
// Overloaded version to do a selective copy
TimeSeries *ExtractComponent(const ThreeComponentSeismogram& tcs,const int component,
	const MetadataList& mdl)
{
    try {
	Metadata mdclone;
	copy_selected_metadata(dynamic_cast<const Metadata &>(tcs),
		mdclone,mdl);
	TimeSeries *ts=new TimeSeries(dynamic_cast<const BasicTimeSeries&>(tcs),
            mdclone);
        double *ptr;
        dmatrix *uptr;
	for(int i=0;i<tcs.ns;++i) 
        {
          uptr=const_cast<dmatrix *>(&(tcs.u));
          ptr=uptr->get_address(component,i);
          ts->s.push_back(*ptr);
        }
	return(ts);
    }
    catch (...)
    {
	throw;
    }
}
} // End namespace SEISPP  definitions
