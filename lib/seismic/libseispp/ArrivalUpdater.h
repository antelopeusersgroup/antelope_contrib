#ifndef __ARRIVALUPDATER_H
#define __ARRIVALUPDATER_H

#include "Metadata.h"
#include "dbpp.h"
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
namespace SEISPP {
using namespace std;
using namespace SEISPP;

class ArrivalUpdater
{
public:
	ArrivalUpdater();
	ArrivalUpdater(DatabaseHandle& dbh,
		MetadataList& mdlass,MetadataList& mdlarr,string amname);
	ArrivalUpdater(const ArrivalUpdater& parent);
	ArrivalUpdater& operator=(const ArrivalUpdater& parent);
	int update(Metadata& md);
	int update(TimeSeries& ts);
	int update(ThreeComponentSeismogram& tcs);
	/*! Clears origin and assoc rows not marked as prefor.
	*
	* \return number of origins cleared 
	* \param evid_to_clear is the evid of event to clean up.*/
	int clear_when_not_prefor(int evid_to_clear);
private:
	/*! This is a match handle into the working view.
	This working view is event:origin:assoc:arrival 
	subsetted to orid==prefor */
	DatascopeMatchHandle aaview; 
	/*! Datascope pointers conveniently cached here */
	Dbptr dbassoc,dbarrival,dborigin;
 
	MetadataList mdlassoc,mdlarrival;
	/*! event-origin-assoc grouped by evid:orid.  
	Needed to clear unassociated origin and assoc rows */
	DatascopeMatchHandle eogroup;
	AttributeMap am;
};

} // End SEISPP Namespace declaration
	
#endif
