#ifndef _TIMEVARIABLEWEIGHT_H_
#define _TIMEVARIABLEWEIGHT_H_
#include "TimeWindow.h"
namespace SEISPP {
//@{
// A time window with a functional form overlaid.
// This is essential an object definition of a window function
// for a time window defined by a TimeWindow object.
//@author Gary L. Pavlis
//@}
class TimeVariableWeight : public TimeWindow
{
public:
//@{
// Set true if this defines a data gap and data within it should be ignored.`
//@}
	bool gap; // alternate way to set a gap
//@{
// Default constructor.
//@}
	TimeVariableWeight(){w0=1.0;wgrad=0.0;};
//@{
// Returns the weight (window value) at time t within a time window.
// If the requested point is less than the start time
// of the window function the value at the beginning of the window is
// return.  If the requested points is greater than the end time the
// value at the right edge of the window is returned.
//@}
	double weight(double t)
	{
		if(gap) return(0.0);
		if(t<start) return(w0);
		if(t>end) return(w0+(end-start)*wgrad);
		return(w0+(t-start)*wgrad);
	}
private:
	double w0;  // weight at t0
	double wgrad;  // dw/dt 
};
}  // End SEISPP namespace declaration
#endif
