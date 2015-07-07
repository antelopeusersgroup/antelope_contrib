#ifndef _BASICTIMESERIES_H_
#define _BASICTIMESERIES_H_
#include <ostream>
#include <set>
#include <boost/serialization/set.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include "TimeWindow.h"
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
/*!
// Nearest integer function.
// This is the same as the standard intrinsic function by the same name.
// It is included in this library at the moment to support g++ in linux
// which does not seem to have this function in the standard math library.
// Note Sun puts it in sunmath.h.
**/
int nint(double);
/*!
// Type of time standard for time series data.
// Time series data have two common standards.  Absolute time means the
// time is an epoch time.  Relative means time is some other arbitrary
// reference.  An example is an arrival time reference frame where all
// data are set with time zero defined by a set of arrival time picks.  
**/
enum TimeReferenceType {
	absolute, /*!< Use an absolute (usually UTC) time base. */
	relative   /*!<Use a relative time scale (usually t0 is 0 or a small number.*/
};

/*! \brief Base class for time series objects.

This is a mostly abstract class defining data and methods shared by all
data objects that are time series.  To this library time series means
data sampled on a 1d, uniform grid defined by a sample rate, start time,
and number of samples.  Derived types can be scalar, vector, or complex 
but share the common property of being related to a base grid.
This is a classic use of inheritance in OOP.
\author Gary L. Pavlis
**/
class BasicTimeSeries
{
public:
/*!
// Boolean defining if a data object has valid data or is to be ignored.
// Data processing often requires data to be marked bad but keep the original
// data around in case an error was made.  This boolean allows this capability.
**/
	bool live;
/*!
// Sample interval.
**/
	double dt;
/*!
// Data start time.  That is the time of the first sample of data.
**/
	double t0;
/*!
// Number of data samples in this data object.
**/
	int ns;
/*!
// Time reference standard for this data object.  Defined by enum Time_Reference
// this currently is only one of two things.  When set as "absolute" the time
// standard is an epoch time.  When set as "relative" time has no relationship
// to any external standard but are relative to some arbitrary reference that must
// ascertained by the algorithm by some other means (in seispp this is normally
// done through a metadata object).  A classic example is multichannel data where
// channels have a time relative to a shot time.  
**/
	TimeReferenceType tref;
/*!
// Default constructor. Does essentially nothing since a BasicTimeSeries
// object has no data.  Does initialize data to avoid run time checkers 
// bitching about unitialized data, but values are meaningless when this
// constructor is called.
**/
	BasicTimeSeries();
/*!
// Standard copy constructor.
**/
	BasicTimeSeries(const BasicTimeSeries&);
/*! \brief Virtual destructor.

  A base class with virtual members like this requires this 
  incantation to avoid some odd conflicts.  This particular one 
  was added to make the boost::serialization code work properly.
  The geeky details for why this is necessary can be found in 
  Scott Meyers book "Effective C++" */
        virtual ~BasicTimeSeries(){};
/*!
// Checks if a sample defined by an integer offset value is a data gap.
// Calls like seis.is_gap(is) return true if sample is is a data gap.  
// It also returns true if i is outside the range of the data. 
// (i.e. less than 0 or >= ns).
//\param is - sample number to test.
**/
	bool is_gap(int is);  // query by sample number
/*!
// Checks if data at time ttest is a gap or valid data.  
// This function is like the overloaded version with an int argument except 
// it uses a time instead of sample number for the query.
//\param ttest - time to be tested.
**/
	bool is_gap(double ttest);  // query by time
/*!
// Checks if a given data segment has a gap.
// For efficiency it is often useful to ask if a whole segment of data is
// free of gaps.  Most time series algorithms cannot process through data
// gaps so normal practice would be to drop data with any gaps in a 
// requested time segment.
//\return true if time segment has any data gaps
//@param twin time window of data to test defined by a TimeWindow object
**/
	bool is_gap(TimeWindow twin);
/*!
// Global test to see if data has any gaps defined. 
// Gap processing is expensive and we need this simple method to
// test to see if the associated object has any gaps defined.  
//\return true if the associated object has any gaps defined.  
**/
	bool has_gap(){return(!gaps.empty());};
/*!
// Adds a gap to the gap definitions for this data object.
// Sometimes an algorithm detects or needs to create a gap (e.g. a mute,
// or a constructor).
// This function provides a common mechanism to define such a gap in the data.
**/
	void add_gap(TimeWindow tw){gaps.insert(tw);};
/*!
//  \brief Clear gaps.
//
// It is sometimes necessary to clear gap definitions.
// This is particularly important when a descendent of this class
// is cloned and then morphed into something else.
*/
	void clear_gaps(){if(!gaps.empty())gaps.clear();};
/*!
// Force all data inside data gaps to zero.  
// This is a virtual function that makes sense only to a derived type since
// the contents of the data vector depend upon the nature of the data.  
// i.e. this function cannot be called on a BasicTimeSeries.
**/
	virtual void zero_gaps()=0; // pure virtual function
	// inline function to return time of sample i
/*!
// Get the time of sample i.
// It is common to need to ask for the time of a given sample.
// This standardizes this common operation in an obvious way.
//\param i - sample number to compute time for.  
**/
	double time(int i){return(t0+dt*static_cast<double>(i));};
	// inverse of time
/*!
// Inverse of time function.  That is,  it returns the integer position
// of a given time t within a time series.  The returned number is
// not tested for validity compared to the data range.  This is the
// callers responsibility as this is a common error condition that 
// should not require the overhead of an exception.
**/
	int sample_number(double t){return(nint((t-t0)/dt));};
/*!
// Returns the end time (time associated with last data sample) 
// of this data object.
**/
	double endtime(){return(t0+dt*static_cast<double>(ns-1));};
/*!
// Absolute to relative time conversion.  
// Sometimes we want to convert data from absolute time (epoch times)
// to a relative time standard.  Examples are conversions to travel
// time using an event origin time or shifting to an arrival time
// reference frame.  This operation simply switches the tref 
// variable and alters t0 by tshift.
//\param tshift - time shift applied to data before switching data to relative time mode.
**/
	void ator(double tshift);
/*!
// Relative to absolute time conversion.
// Sometimes we want to convert data from relative time to 
// to an absolute time standard.  An example would be converting
// segy shot data to something that could be processed like earthquake
// data in a css3.0 database.
// This operation simply switches the tref 
// variable and alters t0 by tshift.
//\param tshift - time shift applied to data before switching data to absolute time mode.
// 
**/
	void rtoa(double tshift);
/*!
// Outputs the data members of this base class.
// Main use of this function is for ease of output in an ascii format.
// There are not plans for an inverse << function.
//
//\param os output stream to send result to.
//\param y BasicTimeSeries to be dumped.
**/
	friend ostream& operator<<(ostream& os,BasicTimeSeries& y);

protected:
/*! Holds data gap definitions.
* We use an STL set object to define data gaps for any time series 
* object derived from this base class.  The set is keyed by a TimeWindow
* which allows a simple, fast way to define a time range with invalid 
* data. */
	set<TimeWindow,TimeWindowCmp> gaps;
	
private:
    friend class boost::serialization::access;
    template<class Archive>
            void serialize(Archive & ar, const unsigned int version)
    {
        ar & live;
        ar & t0;
        ar & dt;
        ar & tref;
        ar & ns;
        ar & gaps;
    };
};
/*! Convert a TimeWindow to a SampleRange object.

TimeWindow objects define a range as a set of floating point numbers.
Any object that is a subclass of BasicTimeSeries may find it convenient to
convert a time window to a range of sample values (the SampleRange object).
This generic algorithm does this for any class that inherits BasicTimeSeries.
An attempt to use this template for an object that does not inherit
BasicTimeSeries should fail at compile time when templates are 
instantiated.
*/
template <class T> SampleRange get_sample_range(T& d, TimeWindow win)
{
	/* This requires T to be a subclass of BasicTimeSeries.
	The live and sample_number() methods come from BasicTimeSeries. */
	if(!d.live) return(SampleRange(0,0));
	int nstart,nend;
	nstart=d.sample_number(win.start);
	if(nstart<0) nstart=0;
	nend=d.sample_number(win.end);
	if(nend>=d.ns)nend = d.ns - 1;
	return(SampleRange(nstart,nend));
}

} // End namespace declaration SEISPP
#endif
