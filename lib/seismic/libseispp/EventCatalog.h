#ifndef _EVENT_CATALOG_H_
#define _EVENT_CATALOG_H_
#include <map>
#include <memory>
#include "TimeWindow.h"
#include "Hypocenter.h"
#include "Metadata.h"
#include "AttributeMap.h"
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
/*! \brief Function object for weak ordering of Hypocenters in space-time order.

Hypocenter objects encapsulate the concept of earthquake locations on the Earth.  
Because a hypocenter is a four-vector defining locations of an event in space time what
ordering means is a more elaborate concept than people commonly think.  This function 
object defines order using a space-time concept based using P waves as the equivalent
of the speed of light for the Earth.  Time order of events is established by computing
the origin time of each pair of events corrected for the P wave propagation time to 
a standard origin.  Default in this library is lat 0.0, lon 0.0, and sea level.
Network operators who use this code may want to customize the origin as some 
sensible origin in the center of their network.  Since I was writing this with a
global perspective I didn't crack the tough nut of figuring out how to parameterize
a set of constants like this in a function object.  I'm sure it is possible, I just
couldn't figure it out and had to move on for lack of time.  For now the origin
is hard wired as a set of constants.

There is a second hard wired constant in this that is more subtle as it is linked
to the use of this function object inside the EventCatalog object defined below.
That object's implementation uses an STL map of Hypocenters using this compare
function to compare members.  Equality is a nontrivial issue with Hypocenter members
because location errors render this question far from simple.  Two parameters are
hard wired into this code linked to this concept.  vp is the velocity used to convert 
time differences to space differences.  Two events are effectively treated equal if 
their space times positions (using vp) are less than rmax (units of km).  Parameters
here are reasonable for global solution, but network operators using this code
for regional networks may want to change these parameters.
*/
class SpaceTimeCompare : public binary_function<Hypocenter,Hypocenter,bool>
{
public:
        bool operator()(const Hypocenter& h1, const Hypocenter& h2) const
        {
		const double vp(6.2);  // km/s
		const double dtmin(0.01);  // fudge factor cannot be smaller than this
		/* if origin times differ by more than this just use a
		simple inequality test on origin time for speed.*/
		const double dtbig(100.0);  
		double t1,t2;
		t1=h1.time;
		t2=h2.time;
		/* For efficiency use a simple algorithm unless the times 
		are very close.*/
		if(fabs(t1-t2)>dtbig)
		{
			if(t1<t2)
				return true;
			else
				return false;
		}
		else
		{
			double r2,dt;
			Hypocenter htmp=const_cast<Hypocenter&>(h1);
			r2=htmp.distance(h2.lat,h2.lon);
			r2=r2*r2;
			r2+=((h1.z-h2.z)*(h1.z-h2.z));
			dt=r2/(vp*vp);
			dt=sqrt(dt);
			if(dt<dtmin) dt=dtmin;
			if( (h1.time+dt) < (h2.time-dt) )
				return true;
			else
				return false;
		}
	}
};
/*! Data object to define a complete event catalog.

Seismologist define an "event catalog" as a collection of seismic events.
This data object encapsulates this concept and supplies some methods that 
are a convenient means of manipulating such a catalog.  Be aware this is 
object loads the entire catalog into memory so it is ill advised for huge
catalogs.  On the other hand, with modern computers I'm not sure there
is a single catalog large enough to make this a serious issue.  
*/
class EventCatalog
{
public:
	/*! \brief Default constructor.  

	Explicitly set to do nothing. */
	EventCatalog(){};
	/*! \brief Create and empty catalog enabling a restricted list of auxiliary attributes.

	This constructor creates an empty catalog and enables a list of allowed auxiliary attributes
	passed via the MetadataList argument.

	\param mdl list of attributes that can be expected to be loaded with each hypocenter to
		 be added later to the object. 
	*/
	EventCatalog(MetadataList& mdl);
	/*! \brief Database constructor.

	Constructs this object from a generic database.  In the current implementation the 
	generic DatabaseHandle is immediately downcast to a DatascopeHandle and the constructor
	blindly assumes dbh points to a view containing the css3.0 origin table.  

	\param dbh handle to database.  Must be a view containing origin.
	\param ttmethod travel time method used to construct Hypocenter objects that
		are internally used to define this object.
	\param ttmodel earth model used for travel time and slowness vector calculations.
	\param is_view allows optional preprocessing to use a subset of data.
		By default the handle is modified to become a join or origin 
		and event.  If this is false, it is assumed the dbh points to
		a handle that is a join of event and origin.
	\exception will throw a SeisppError exception if this process fail.  Fail in this case
		means any read error on the database.
	*/
	EventCatalog(DatabaseHandle& dbh,
		string ttmethod="tttaup",string ttmodel="iasp91",
			bool is_view=false);
	/*! \brief Database constructor with auxiliary attributes.

	Constructs this object from a generic database.  In the current implementation the 
	generic DatabaseHandle is immediately downcast to a DatascopeHandle and the constructor
	blindly assumes dbh points to a view containing the css3.0 origin table.  
	This differs from a similar constructor without the MetadataList parameter 
	in an important way.  The MetadatList drives attributes loaded from the db that
	are not part of the Hypocenter object itself.  For example, in magnitude estimates,
	css3.0 specific things like evid and orid, or moment tensor estimate could all
	be loaded through this mechanism.  This allows a more general use of this object.

	\param dbh handle to database.  Must be a view containing origin.
	\param mdl MetadataList defining auxiliary attributes to be loaded.
	\param am AttributeMap needed to map db namespace to Metadata names.
	\param ttmethod travel time method used to construct Hypocenter objects that
		are internally used to define this object.
	\param ttmodel earth model used for travel time and slowness vector calculations.
	\param is_view allows optional preprocessing to use a subset of data.
		By default the handle is modified to become a join or origin 
		and event.  If this is false, it is assumed the dbh points to
		a handle that is a join of event and origin.
	\exception will throw a SeisppError exception if this process fail.  Fail in this case
		means any read error on the database.
	*/
	EventCatalog(DatabaseHandle& dbh,MetadataList& mdl,AttributeMap& am,
		string ttmethod="tttaup",string ttmodel="iasp91",
			bool is_view=false);
	/*! \brief Standard copy constructor.
	*/
	EventCatalog(const EventCatalog& parent);
	/*! \brief Return the entire time range spanned by this catalog.  

	It is sometimes useful to know the full time range spanned by a catalog.
	This is a convenient way to get this information.  The result is returned
	as a TimeWindow object which contains epoch times that define the range.*/
	TimeWindow range();
	/*! \brief find an event in the catalog.

	A basic operation to make this useful is to find a match in 
	the catalog to a test hypocenter.  If a match is found the 
	internal pointer is positioned to the the matching entry.
	The matching entry can be retrieved after a successful find
	by calling the current() method.  If a match is not found the
	position of the current event counters should be assumed to
	be undefined.  (It isn't but it probably isn't what you want.)
	
	\param test Hypocenter object to test to see if in catalog.
	\return true if match is found, false if not
	*/
	bool find(Hypocenter& test);
	/*! \brief Add a Hypocenter to the catalog.

	This is a bombproof add. If a match to the Hypocenter to be inserted already 
	exists, the request is ignored and the method returns false.  If it is unique
	and was successfully added returns true.  When successful the current Hypocenter
	pointer will be positioned to the point just added.  

	When an add is successful it also creates a copy of md.   The md object is 
	simply copied without checking against the master list of attributes loaded
	at creation.  
	\param hnew event to be added
	\param md contains auxiliary attributes to be loaded with this Hypocenter.
	\return true if this event was added.  false if this event matched an existing
		entry.  In the later case the previous event data will be retained.
		Use the replace method if you want to force an overwrite.
	*/
	bool add(Hypocenter& hnew,Metadata& md);
	/*! \brief Add/Replace an entry in this catalog.

	This method is complementary to add.  The add method will not overwrite
	an existing entry if it is already in the catalog.  This does the reverse
	in that it will always replace an event with the new data if a match is 
	found.  If there was no event matching the one passed as an argument it
	is simply added.  The return can be tested if it is important to know if
	a replacement was made or the event was just appended.  It returns
	true if something was replaced and false otherwise.

	\param hnew event to be added/replaced
	\param md contains auxiliary attributes to be loaded with this Hypocenter.
	\return true if something was replaced.  If there was no match to this event
		hnew/md will just be added and the method will return false. 
	*/
	bool replace(Hypocenter& hnew,Metadata& md);
	/*! \brief Set the internal pointer to beginning.

	This object maintains the catalog as a time ordered set of Hypocenter objects.
	It maintains an internal pointer that defines a current position in the catalog.
	This method resets this pointer to the beginning of the catalog.*/
	void rewind(); 
	/*! \brief Return the actual Hypocenter defined as the current event.

	This object maintains an internal pointer to a position inside the catalog.
	This method returns the Hypocenter defined as the current one.*/
	Hypocenter current();
	/*! \brief Return aux information related to current hypocenter.

	This object stores arbitrarily complicated auxiliary attributes tagged to 
	each hypocenter int he catalog.  This method is like the current() method
	but returns only these aux attributes. */
	Metadata current_aux();
	/*! Delete current Hypocenter from the catalog.

	This method deletes the event defined as the "current" hypocenter.  The current
	hypocenter is set either by working through the catalog with the ++ operator
	or using the find method.  It is very important to recognize that the current
	hypocenter should be viewed as undefined after calling this method.  This is necessary 
	because deletion invalidates the current pointer and there is no unambiguous
	way to reset it.  One could make a choice, but in the authors view keeping the
	interface consistent is more important and since what is and is not feasible is
	implementation dependent it is best to just say it is undefined.*/
	void delete_current();
	/*! brief Generic subset method. 
	
	It is frequently of interest to produce a subset of an event catalog.  This provides
	a generic method to do this using the STL concept of a type of function object
	called a predicate.  To be instantiated this template requires a function object that
	returns true if a Hypocenter object satisfies a subset condition.  This allows the
	subset specification to be totally generic.  

	\param pred predicate function object to apply to define subset condition.  Must return
		true if the Hypocenter is to be included in the subset.
	\return auto_ptr to new EventCatalog containing the subset.  The current pointer of 
		subset is set to first event in the new catalog (time order).
	*/
	template<class Predicate> auto_ptr<EventCatalog> subset(Predicate pred);
	/*! \brief Return the count of current number of Hypocenters in the catalog.*/
	int size();
	/*! \brief Standard assignment operator.*/
	EventCatalog& operator=(const EventCatalog& parent);
	/*! \brief Increment the current event pointer.

	This object maintains an internal pointer to a position it defines as
	the "current" Hypocenter.  This operator is used to work through the catalog
	in an linear way from beginning to end (time order).  */
	void operator++();
private:
	map<Hypocenter,Metadata,SpaceTimeCompare> catalog;
	map<Hypocenter,Metadata,SpaceTimeCompare>::iterator current_hypo;
	/* There is no guarantee every hypo has all entries in this loaded unless
	the data are loaded from a db.  Otherwise just a guideline */
	MetadataList mdloaded;
};

template <class Predicate>
auto_ptr<EventCatalog> EventCatalog::subset(Predicate pred)
{
	map<Hypocenter,Metadata,SpaceTimeCompare>::iterator cptr;
	auto_ptr<EventCatalog> result(new EventCatalog());
	for(cptr=catalog.begin();cptr!=catalog.end();++cptr)
	{
		if(pred(cptr->first)) result->catalog.insert(*cptr);
	}
	return(result);
}

} // End SEISPP namespace declaration
#endif
