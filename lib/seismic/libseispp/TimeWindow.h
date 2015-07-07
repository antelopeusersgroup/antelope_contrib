#ifndef _TIMEWINDOW_H_
#define _TIMEWINDOW_H_
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
/*!
\brief Defines a time window.

Time windows are a common concept in time series analysis and seismology
in particular.  The object definition here has no concept of a time
standard.  It simply defines an interval in terms of a pair of 
real numbers.  
**/
class TimeWindow
{
public:
/*!
// Start time of the window.
**/
	double start;
/*!
// End time of the window.
**/
	double  end;  
/*!
// Default constructor.
**/
	TimeWindow(){start=0.0;end=1.0e99;};
/*!
// Parameterized constructor.
//\param ts - start time
//\param te - end time 
**/
	TimeWindow(double ts,double te){start=ts;end=te;};
/*!
// Returns a new time window translated by tshift argument.
**/
	TimeWindow shift(double tshift) 
	{
		TimeWindow newwindow(*this);
		newwindow.start+=tshift;
		newwindow.end += tshift;
		return(newwindow);
	}
/*!
// Returns the window length
**/
	double length()
	{
		return(end-start);
	};
private:
        friend class boost::serialization::access;
        template<class Archive>
            void serialize(Archive & ar, const unsigned int version)
        {
            ar & start;
            ar & end;
        };
};

/* This strange looking function is a C++ function object.
// It is used in the STL container called a set used for gaps below.  
// This function is used as the comparison function for ordering
// the elements of the set.  It makes TimeWindows indexed by
// intervals similar to thw way Datascope uses time:endtime
// Be aware, however, that for the same reason as datascope overlapping 
// time windows will cause ambiguity in indexing times by this
// method.
*/
/*! \brief Function object used for weak comparison to order TimeWindow objects.

// TimeWindow objects are used, among other things, to define real
// or processed induced data gaps.
// The set container requires a weak ordering function like to correctly
// determine if a time is inside a particular time window.
//\author Gary L. Pavlis
**/
class TimeWindowCmp
{
public:
	bool operator()(const TimeWindow ti1,const TimeWindow ti2) const
	{return(ti1.end<ti2.start);};
};

/*! \brief Class to specify time windows in terms of sample index values.

Sometimes it is convenient to convert a time window to a range of sample values.
Any data that inherits BasicTimeSeries can effectively use this concept.  That is,
time series data are defined by sample intervals and number of samples.  Thus 
a time range a time series can be directly converted to a range of sample 
values.  This simple class encapsulates that idea.
*/
class SampleRange
{
public:
	/*! Starting index value of range. */
	int nstart;
	/*! Ending index value of range. */
	int nend;
	/*! \brief Number of samples in range.   

	The number of samples can be readily computed, but it is
	commonly clearer to test if nsamp is less than or equal to 
	zero to mark an invalid range.  Otherwise code needs the
	baggage in the simple constructor or this class any time
	testing or validity of a range is required. */
	int nsamp;
	/*! Parameterized constructor for this simple class. 

	\param ns value for nstart
	\param ne value for nend
	*/
	SampleRange(int ns, int ne)
	{
		nstart=ns;
		nend=ne;
		if(nend<=nstart)
		{
			nend=nstart;
			nsamp=0;
		}
		else
		{
			nsamp = nend - nstart + 1;
		}
	};
}; 
}  // end SEISPP namespace declaration
#endif
