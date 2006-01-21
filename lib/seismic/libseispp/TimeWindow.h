#ifndef _TIMEWINDOW_H_
#define _TIMEWINDOW_H_
namespace SEISPP
{
//@{
// Defines a time window.
// Time windows are a common concept in time series analysis and seismology
// in particular.  The object definition here has no concept of a time
// standard.  It simply defines an interval in terms of a pair of 
// real numbers.  
//@}
class TimeWindow
{
public:
//@{
// Start time of the window.
//@}
	double start;
//@{
// End time of the window.
//@}
	double  end;  
//@{
// Default constructor.
//@}
	TimeWindow(){start=0.0;end=1.0e99;};
//@{
// Parameterized constructor.
//@param ts - start time
//@param te - end time 
//@}
	TimeWindow(double ts,double te){start=ts;end=te;};
//@{
// Returns a new time window translated by tshift argument.
//@}
	TimeWindow shift(double tshift) 
	{
		TimeWindow newwindow(*this);
		newwindow.start+=tshift;
		newwindow.end += tshift;
		return(newwindow);
	}
//@{
// Returns the window length
//@}
	double length()
	{
		return(end-start);
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
//@{
// Function object used for weak comparison to order TimeWindow objects.
// TimeWindow objects are used, among other things, to define real
// or processed induced data gaps.
// The set container requires a weak ordering function like to correctly
// determine if a time is inside a particular time window.
//@author Gary L. Pavlis
//@}
class TimeWindowCmp
{
public:
	bool operator()(const TimeWindow ti1,const TimeWindow ti2) const
	{return(ti1.end<ti2.start);};
};
}  // end SEISPP namespace declaration
#endif
