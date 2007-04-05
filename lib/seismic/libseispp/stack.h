#ifndef _STACK_H_
#define _STACK_H_

#include <vector>
#include "SeisppKeywords.h"
#include "TimeWindow.h"
#include "TimeSeries.h"
#include "ensemble.h"
namespace SEISPP {
enum StackType {BasicStack, MedianStack, RobustSNR};

/*!
\brief Generalized stacking object.

Stacking is a common theme in seismic data processing.  This generalizes the 
concept in a single object.  The approach uses here is decidely not procedural.
The philosphy of this object is that the object is created through the computation
of the stack.  
\par
This is a very general stack operator.  Data are stacked over a specified time 
gate with the window shifted to arbitrary positions on any trace through a 
moveout keyword extracted from input trace metadata.  This any kind of moveout
can be specified.  The same method will work for reflection normal moveout or
plane wave stacks in earthquake seismology array processing.  
\par
This object implements multiple methods for defining the stack.  The methods
available are defined by the StackType enum documented elsewhere.
\par
All methods post an amplitude metric to the stack trace metadata area.  
The norm used is the rms amplitude of the computed beam.  In all cases the amplitude
figure can be accessed by calling the get_double method for the attribute
beam_rms_key on the TimeSeries stack data member of this object.
**/

class Stack
{
public:
	/*! Fold of this stack.  For those not familiar with exploration geophysics 
	jargon that means the number of input traces that went into this stack. */
	int fold;
	/*! Vector of weights used to compute this stack.  Assumed to be consistent with
	input ensemble used to compute the stack.  Note that because in SEISPP an ensemble
	is easily sorted, this vector will be wrong if the input data are sorted after a
	stack is created.  Will be all ones for simple stack or median stack.*/
	vector<double>weights;
	/*! Sum of weights. Sometimes this is necessary to normalize a stack. */
	double sumwt;
	/*! Holds the result of the stack. Intentionally made public to allow flexibility in
	posting user specific Metadata to it.*/
	TimeSeries stack; 
	/*! Default constructor.  Initializes to a null result.  Done for completeness, but useless.*/
	Stack();
	/*! Simplified constructor for stock case of a simple (mean) stack. 
	* The algorithm here is the simplest possible.  The data from the time gate defined by
	* twin shifted by the specified moveout is accumulated and summed trace by trace.
	* The final result is normalized by 1/fold and stored in the stack component of this object.
	*
	* \param d input ensemble.  The moveout keyword must be set in the metadata area and this
	* 	value will be used to determine the time window to extract.  If a trace is marked
	*	dead the data from that trace will be silently ignored and the fold will not be
	*	incremented.  If the requested time gate is outside the range off the data
	*	this constructor will throw an exception.
	* \param twin relative time window defining the time gate relative to the moveout time.
	*	e.g. -1 to 2 would mean a time gate from 1 s before to 2 s after the time defined
	*	by the moveout parameter. 
	* \exception SeisppError is thrown for most potential problems.  The most likely is 
	* 	requesting a time gate outside the range of the input data.
	* \exception MetadataGetError will be thrown if the moveout keyword is not present in all
	*	members of the input ensemble.
	*/
	Stack(TimeSeriesEnsemble& d,TimeWindow twin);
	/*! Constructor that implements robust stacking methods.  
	*  This constructor behaves similar to the simple stack constructor, but the result can
	* be dramatically different with variable signal-to-noise data.  Currently accepts 
	* median and robust methods.  The median is well known.  The robust method is an interative
	* weighted stack method using a penalty function using essentially a form of coherence.
	*
	* \param d input ensemble.  The moveout keyword must be set in the metadata area and this
	* 	value will be used to determine the time window to extract.  If a trace is marked
	*	dead the data from that trace will be silently ignored and the fold will not be
	*	incremented.  If the requested time gate is outside the range off the data
	*	this constructor will throw an exception.
	* \param stack_twin relative time window defining the time gate relative to the moveout time.
	*	e.g. -1 to 2 would mean a time gate from 1 s before to 2 s after the time defined
	*	by the moveout parameter. 
	* \param robust_twin relative time window defining the time gate relative to the moveout time
	*	to use for computing the robust weight loss function.  Ignored for a simple or median
	*	stack.  For the robust method the weight is essentially determined by the coherence of
	*	each member relative to the stack.  Traces similar to the stack are given a high weight
	*	while traces that have low coherence are strongly downweighted.  
	* \param method defines the stack method that should be used to form this stack.
	* \param power in robust method the weight loss function is taken to 
	*    this power (default 1.0).  Weights are 0 to 1 so increasing powers
	*    make the loss function increasingly aggressive at downweighting
	*    outliers.  This parameter is ignored for anything but
	*    the RobustSNR method.
	*/
	Stack(TimeSeriesEnsemble& d,TimeWindow stack_twin, TimeWindow robust_twin, StackType method,double power=1.0);
	/*! Standard copy constructor. */
	Stack(const Stack& old);
	/*! Standard assignment operator. */
	Stack& operator=(const Stack& other);
private:
	StackType stacktype;
};
}
#endif
