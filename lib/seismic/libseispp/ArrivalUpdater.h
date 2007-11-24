#ifndef __ARRIVALUPDATER_H
#define __ARRIVALUPDATER_H

#include "Metadata.h"
#include "dbpp.h"
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
namespace SEISPP {
using namespace std;
using namespace SEISPP;
/*! \brief Data processing object to simplify process of updating a database with new 
or revised arrival times.

In passive array processing arrival times are a fundamental seismological measurement.
The css3.0 schema, which this assumes is being used, uses three tables to 
hold arrival time data:  event, origin, assoc, and arrival.  
When a new arrival time is measured and update requires interaction with all
four of thee tables.  This object is designed to simplify that process.
The update methods accomplish the processing.  
*/

class ArrivalUpdater
{
public:
	/*! Default constructor.  Creates a null object that will not be 
	usable.  Exists in the interface only because some compilers seem to 
	require declaration of a default constructor.*/
	ArrivalUpdater();
	/*! Construct this object from the database to be updated.

	This constructor sets up all the internal workings needed to make 
	the update algorithms work.  The current implementation, which works
	only on the Antelope database, sets up match handles to index the key
	tables required for an update. 

	\param dbh database to be updated
	\param mdlass contains the list of metadata to be updated in the assoc table.
	\param dblarr contains the list of metdata to be updated in the arrival table.
	\param amname is the name used to tag the AttributeMap to be used to map from
		the internal namespace (metadata) to database attribute names. This is
		normally a schema name. 

	\exception SeisppError is thrown if there are problems in creation of this object.
	*/
	ArrivalUpdater(DatabaseHandle& dbh,
		MetadataList& mdlass,MetadataList& mdlarr,string amname);
	/*! Standard copy constructor. */
	ArrivalUpdater(const ArrivalUpdater& parent);
	/*! Standard assignment operator. */
	ArrivalUpdater& operator=(const ArrivalUpdater& parent);
	/*! \brief Update database using attributes stored in a Metadata object.
`
	The Metadata object is used extensively in the SEISPP library to store 
	attributes associated with other data objects.  Here it is assumed this
	object contains attributes that can be mapped to db attributes through the
	AttributeMap.  When called this algorithm attempts to find existing rows
	in assoc and arrival that match the contents of md.  If a match is found 
	required attributes (defined through MetadataLists passed to constructor)
	for arrival and assoc are pushed to the database replacing previous values
	or that row (tuple).  If there is no match, which in this case always means
	an exact match of evid:orid:sta:phase in the join of event:origin:assoc:arrival, 
	a new row is appended to arrival and assoc adding the same attributes as 
	would be updated had a match been found.  Note that this can and often will
	create tuples with inadequate information.  Note than in this mode because
	arid is a key in the assoc:arrival join a new arid is created and posted to
	arrival and assoc when appending.

	A final detail of the algorithm used by this method is that it will attempt
	to clean up duplicate arids.  That is, if a match on evid:orid:sta:phase yields
	more than one row, the update will be applied to the first tuple found and 
	the rest will be deleted.  It will complain loudly about this by posting 
	a message to stderr, but this will not lead cause an exception to be thrown.

	\return number of nonfatal errors.  A failure to write one or more attributes
		is considered bad, but not treated as fatal because the seriousness
		depends on the context.  The caller should determine what action to 
		take if a nonzero value is returned. 

	\exception A SeisppError can be thrown in various situations that cause
	an update to fail.  Any exception thrown by this method should normally
	be treated as fatal as multiple failed updates are a usually a recipe
	for disaster. */
	int update(Metadata& md);
	/*! \brief Update using attributes stored with a TimeSeries object.

	In SEISPP a TimeSeries object inherits Metadata so this is largely 
	a convenience method to avoid the dynamic_cast operator that would
	be required otherwise. */
	int update(TimeSeries& ts);
	/*! \brief Update using attributes stored with a ThreeComponenSeismogram object.

	In SEISPP a ThreeComponenSeismoram object inherits Metadata so this is largely 
	a convenience method to avoid the dynamic_cast operator that would
	be required otherwise. */
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
