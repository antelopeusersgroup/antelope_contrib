#ifndef _MULTICHANNELCORRELATOR_H_
#define _MULTICHANNELCORRELATOR_H_

#include <vector>
#include <math.h>

#include "stock.h"
#include "coords.h"
#include "pf.h"
#include "tt.h"
#include "db.h"
#include "TimeSeries.h"
#include "ensemble.h"
#include "stack.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP 
{
/*! List of implemented MultichannelCrossCorrelation methods.
*/
enum CorrelationMethod 
{
	Basic, /*!< Correlate once and only once with the reference trace. */
	SimpleStack, /*!< Iterative correlation with beam computed by a simple stack.*/
	RobustStack /*!< Iterative correlation with beam computed by robust stacking algorithm.*/
};
/*! List of implemented penalty functions for robust stacking algorithm.
*/
enum PenaltyFunction {
	NoPfunction, /*!< Use a simple mean. */
	StackCoherence /*!< Use the coherence-based weighting method. */
};
/*! Keyword used to post peak cross-correlation value to each trace's Metadata area. */
static const string peakxcor_keyword("peak_xcor");
/*! \brief Cross-correlation procedure for TimeSeries objects.
This is one of two overloaded methods for implementing cross-correlation in the 
time domain between two TimeSeries objects.  
This procedure correlates two time series over the complete range of lags possible
for the size of the two traces.  
In this procedure x is the correlator and y is the trace against which x is correlated.
\par 
This procedure checks for gaps in the correlation window of y.  If any gaps are present
in the window the procedure returns immediately with a default TimeSeries object 
containing no data.  The caller should test for this condition by examining the 
live variable of the output.  The trace will be marked live if the correlation was
successful, but live will be false if gaps were detected and correlation failed. 
\par
Although the procedure does not test this it makes a somewhat implicit assumption the
data have been converted to a relative time reference frame.  This was done to make
the procedure work with both common source and common receiver gathers (receiver
array versus source array processing to those not as familiar with the exploration
geophysics jargon).  Have the correlator defined on an absolute time base is 
particularly problematic. Hence, before calling this function create a relative 
time reference data set using something like ArrivalTimeReference or force each 
trace passed through this procedure to have a relative time base using the 
ator method of the TimeSeries object.

\return A new TimeSeries object containing the cross-correlation function.  
	The correlation trace contains a copy of the Metadata of the correlated (y)
	trace.  Be warned that this may leave some relics in this object's 
	Metadata that are not correct.
\param x correlator function (i.e. this is the trace that is shifted in the operation).
\param y data to correlate x against.  The length of y must be more than x or an error
	will be thrown.
\param normalize if true the output is normalized by the L2 norm of x.  

\exception SeisppError is thrown if sample rates of x and y do not match 
*/
TimeSeries correlation(TimeSeries& x, TimeSeries& y,bool normalize=false);

/*! \brief Cross-correlation procedure for TimeSeries objects with specified lag range.
This is one of two overloaded methods for implementing cross-correlation in the 
time domain between two TimeSeries objects.  
This procedure correlates two time series over a specified range of time lags.
In this procedure x is the correlator and y is the trace against which x is correlated.
If therequest range of lags is shorter than the data allow, the output will be silently
truncated without warning.  This assumes the caller will always check the time range
defined by the data for consistency.  The view here is that a shortened window may be
valid and should not be consider an unrecoverable error.
\par 
This procedure checks for gaps in the correlation window of y.  If any gaps are present
in the window the procedure returns immediately with a default TimeSeries object 
containing no data.  The caller should test for this condition by examining the 
live variable of the output.  The trace will be marked live if the correlation was
\par
A more subtle problem with gaps arises if on of the inputs (x or y) has a gap at
the beginning or end of the time window to be processed.  In the seispp library
this type of situation is not always flagged as a gap because it can happen 
naturally when, for example, dealing with segmented data derived either from
a triggered instrument or extraction from continuous data.  This situation is 
handled by testing the feasibility of the correlation by examining the size
of x and y.  If x is shorter than y this function returns a null seismogram
with no data and marked dead.  Thus, just as with an internal gap the caller
must handle this situation and always test for a return marked dead.
successful, but live will be false if gaps were detected and correlation failed. 
If SEISPP_verbose is true an error announcing this will be pushed to stderr.
Otherwise this potential error situation is done silently.
\par
Although the procedure does not test this it makes a somewhat implicit assumption the
data have been converted to a relative time reference frame.  This was done to make
the procedure work with both common source and common receiver gathers (receiver
array versus source array processing to those not as familiar with the exploration
geophysics jargon).  Have the correlator defined on an absolute time base is 
particularly problematic. Hence, before calling this function create a relative 
time reference data set using something like ArrivalTimeReference or force each 
trace passed through this procedure to have a relative time base using the 
ator method of the TimeSeries object.

\return A new TimeSeries object containing the cross-correlation function.  
	The correlation trace contains a copy of the Metadata of the correlated (y)
	trace.  Be warned that this may leave some relics in this object's 
	Metadata that are not correct.  To test for truncation of the correlation 
	function verify that the length of the output is the same as that requested
	for the cwin input.
\param x correlator function (i.e. this is the trace that is shifted in the operation).
\param y data to correlate x against.  The length of y must be more than x or an error
	will be thrown.
\param cwin defines the range of lags for which the correlation function is to be computed.
	This, of course, is always in relative time units measured from the 0 time 
	position of y.  
\param normalize if true the output is normalized by the L2 norm of x.  

\exception SeisppError is thrown if sample rates of x and y do not match or if the 
	lengths of the two traces are inconsistent (i.e. we require y.ns>x.ns).
*/
TimeSeries correlation(TimeSeries& x, TimeSeries& y,
		TimeWindow cwin, bool normalize=false);
/*! \brief Cross-correlation procedure for ThreeComponentSeismogram  objects.

This is procedure computes the cross correlation in a vector sense between
two ThreeComponentSeismogram object.  It is conceptually similar to the 
similar procedures in defined in this library for TimeSeries objects.
In fact the implementation was produced by simply editing the algorithm for
TimeSeries objects.   Vector correlation is computed as the dot product
between x and y at variable lag summed for all three components.   
The correlation interval is defined by the length of the two components.
The input x MUST be shorter than y or the procedure will throw and 
exception.  The range of lags is defined by the samples x can fit in y.
\par 
This procedure checks for gaps in the correlation window of y.  
It also checks for any gaps in x  If any gaps are present
the procedure returns immediately with a default TimeSeries object 
containing no data and marked dead.  The caller should test for this condition by examining the 
live variable of the output.  The trace will be marked live if the correlation was
successful, but live will be false if gaps were detected and correlation failed. 
\par
Although the procedure does not test this it makes a somewhat implicit assumption the
data have been converted to a relative time reference frame.  This was done to make
the procedure work with both common source and common receiver gathers (receiver
array versus source array processing to those not as familiar with the exploration
geophysics jargon).  Have the correlator defined on an absolute time base is 
particularly problematic. Hence, before calling this function create a relative 
time reference data set using something like ArrivalTimeReference or force each 
trace passed through this procedure to have a relative time base using the 
ator method of the TimeSeries object.

\return A new TimeSeries object containing the cross-correlation function.  
	The correlation trace contains a copy of the Metadata of the correlated (y)
	trace.  Be warned that this may leave some relics in this object's 
	Metadata that are not correct.
\param x correlator function (i.e. this is the trace that is shifted in the operation).
\param y data to correlate x against.  The length of y must be more than x or an error
	will be thrown.
\param normalize if true the output is normalized by the L2 norm of x and y in the
    correlation window (3C L2 norm means sum of a global sum of squares across components)..  

\exception SeisppError is thrown if sample rates of x and y do not match 
*/
TimeSeries correlation(ThreeComponentSeismogram& x, 
        ThreeComponentSeismogram& y,bool normalize=false);

/*! \brief Encapsulates data defining the peak of a cross-correlation function.
*
* Peak cross-correlation is a commonly required statistic.  The correlation 
* has both a value and a time lag associated with it.  This simple class
* contains these in one simple object. 
*/
class TimeSeriesMaximum
{
public:
	//
	// The destructor and copy constructor for this class were intentionally
	// defaulted.  Contains only simple data so let the compiler handle it.
	// Probably could have default constructor and assignment operator as well
	// but these are implemented in the source code so we'll use them.
	//
	/*! Lag time of position of peak.*/
        double lag;
	/*! Value of the correlation function at the peak. */
        double peak;
	/*! Default constructor. */
        TimeSeriesMaximum(){lag=0.0;peak=0.0;};
	/*! Compute the lag and peak from a correlation function stored in a TimeSeries object.*/
        TimeSeriesMaximum(TimeSeries& d);
	/*! Standard assignment operator. */
        TimeSeriesMaximum& operator=(const TimeSeriesMaximum& other);
};
/*!
*  \brief Generalized array cross correlation object.
* 
*  Cross correlation is a common signal processing algorithm.  The standard
* equation defines the correlation between two time series.  In earthquake 
* seismology people have tended to focus on pairwise combinations of correlations
* resolving variations through a least squares fitting procedure.  Correlation
* in the exploration geophysics work is also common in multiple contexts, but
* the focus there is always multichannel.  The most common application is 
* deconvolution by correlation with a vibrator sweep in land seismic data.  
* Correlation is also use for other reflection processing.  For example, most
* systems allow first breaks to be picked by a cross-correlation algorithm.
* All exploration algorithms I know of are not pair-wise but multichannel. 
* That is, a single correlator is defined for the whole ensemble that is to
* be correlated (e.g. a measured or theoretical vibrator sweep) and this 
* function is correlated with each member of the ensemble.  This is the 
* appoach used by this processing object.  If you need to do pair-wise 
* correlations, use the overloaded correlation procedures.
* \par
*  This object implements multiple methods of multichannel cross-correlation.  
*  It uses a model foreign to most of us old procedural guys in that the creation
*  of the object involves the actual calculations behind the concept.  That is,
*  it isn't arguments in, data out.  It is arguments create the object that is
*  the result of the calculations;  in this case multichannel cross correlation.
*  Although foreign it makes for a clean design for this concept as it cleanly
*  encapsulates the output of the calculations.  Details of the algorithm used
*  are best left to other forms of documentation.  Usage is better described by
*  reading documentation for the member functions of the object.
* 
* \author Gary L. Pavlis
**/

class MultichannelCorrelator
{
public:
	/*!
	*  \brief Contains an array of lag values for peak correlation.
	*
	* If the input ensemble has n members this vector will be of length 
	* n and will contain a set of lag times for of the maximum value of 
	* the computed correlation function for each of the n members off the
	* ensemble.  The order of this vector is the same as the peakxcor vector.
	*  Be warned if you sort the xcor ensemble member of this object this vector
	*  order will not be preserved.
	*/
	vector<double>lag;
	/*!
	*  \brief Contains an array of peak cross-correlation function values.
	*
	* f the input ensemble has n members this vector will be of length 
	* n and will contain a set of numbers that are the peak values of the 
	* cross-correlation functions computed for each member of the ensemble.
	* The order of this vector is the same as the lag vector.
	*  Be warned if you sort the xcor ensemble member of this object this vector
	*  order will not be preserved.
	*/
	vector<double>peakxcor;
	/*!
	*  \brief Contains an array of weights used in computing the stack for this ensemble.
	*
	*  The algorithm this object implements always computes an ensemble stack
	*  (beam in earthquake seismology jargon, but not necessarily a beam in
	*  the context of this algorithm) that is used as the correlation function
	*  for the ensemble.  A simple stack will leave this vector all ones while
	*  the robust stacking method will have variable weights stored in this vector.
	*  The order of this vector is the same as lag and peakxcor vector which are
	*  original grace order.  
	*  Be warned if you sort the xcor ensemble member of this object this vector
	*  order will not be preserved.
	*/
	vector<double>weight;
	/*!
	*  \brief Contains an array of amplitude scale factors for each member trace.
	*
	*  A useful side benefit of an array correlation method is that the relative size
	*  of different signals that correlate can be measured accurately.  This vector
	*  will contain a set of amplitudes factors relative to the array beam.  The 
	*  amplitude factor for the ith trace in the ensemble is computed using the following formula:
		\f[
			A_i = { { {\bf b} \cdot {\bf y}_i (\tau_{max} )  }
				\over
				{ || {\bf b} || } }
		\f]
	*  where \f$ {\bf b} \f$ is the computed stack,
	*  \f$ \dot \f$ is the standard vector dot product computed for the length of 
	* \f$ {\bf b} \f$, 
	* \f$ {\bf y_i (\tau_{max} ) } \f$ is the ith member of the ensemble shifted to
	* so sample 0 is at the peak correlation value, and \f$ || {\bf b} || \f$ is
	* the L2 norm of the ensemble stack (beam).  
	* \par
	*  Be warned if you sort the xcor ensemble member of this object this vector
	*  order will not be preserved.
	*/
	vector<double>amplitude_static;
	/*!
	*  \brief Defines the cross-correlation method used in computing this result.
	*/
	CorrelationMethod method_used;
	/*!
	*  \brief Defines the penalty function used in computing this result.
	*/
	PenaltyFunction pfunction_used;
	/*!
	*  \brief An ensemble containing the computed cross-correlation functions.
	*
	*  This quantity is essentially the verbose content of this object. It contains
	*  the complete set of cross-correlation functions computed for each member 
	*  used in it's creation.  The number of elements should be equal to the parent
	*  data ensemble used for it's creation.  Note, however, that any use of this
	*  ensemble should test for member's marked as dead.  Data gaps within the
	*  requested correlation window will cause the associated correlation functions
	*  to be marked dead.
	*/
	TimeSeriesEnsemble xcor;
	
	/*!
	\brief Default constructor.  Does nothing and should not be used.
	
	Created explicitly to make sure key quantities are initialized.  This
	would not happen here with a compiler generated version of this as this
	object has complicated contents that require careful initialization
	or they will contain garbage.
	*/
	MultichannelCorrelator();  
	/*!
	*  \brief Construct this object from a TimeSeriesEnsemble.
	*
	* This constructor takes the contents of in input ensemble and performs
	* a multichannel correlation with the results depending on the choice of
	* algorithm and, in anything but the "Basic" method, the choice of the
	* initial estimate of the array beam.
	*
	* The "Basic" method does a simple cross correlation of each trace with a
	* reference trace passed either through the initial_beam parameter or the
	* reference_member parameter.  Other methods involve an iterative procedure
	* using a beam recomputed iteratively until convergence.  The basic algorithm
	* is correlate with beam, align stack using current lags, sum, repeat until
	* time shifts do not change.
	*
	* The robust methods differ from the simpler methods in the use of a penalty
	* function in forming the stack.  Currently two robust stacks are supported:
	* (a) a median stack and (b) a SNR weighting method.  The later is of greatest
	* value for highly variable signal to noise conditions.  The most significant
	* example is source correlations where different magnitude events can enter in
	* the same stack.  Note the SNR scheme is slightly more complicated than a signal
	* to noise estimate.  It is more coherence-like using a ratio of L2 norm of the
	* original data to the L2 norm of the residuals (amplitude adjusted stack) to
	* define the SNR. The formula used to compute the weight is:
		\f[
			w_i = { { {\bf b} \cdot {\bf y}_i (\tau_{max} ) }
				\over
				{ || {\bf y}_i ||   || {\bf r}_i || } }
		\f]
	*  where \f$ {\bf b} \f$ is the computed stack,
	*  \f$ \dot \f$ is the standard vector dot product computed for the length of 
	* \f$ {\bf b} \f$, 
	* \f$ {\bf y_i (\tau_{max} ) } \f$ is the ith member of the ensemble shifted to
	* so sample 0 is at the peak correlation value,  \f$ {\bf r}_i  \f$ is
	* the (amplitude corrected) residual for the ith member over this time gate, and
	* \f$ || || \f$ denotes the L2 vector norm.
	*
	* \param d data ensemble to use for this algorithm.
	* \param meth Cross-correlation method to be used.
	* \param beam_window time gate to compute stack (assumed in units consistent with d).
	* \param robust_window time gate to compute penalty function for robust method.  
	*	Must be shorter than beam_window.  Ignored for anything but robust method.
	* \param lag_cutoff compute lags only within + or - this range or data range, which
	*	ever is shorter.
	* \param stack_type Method to use to compute stack.
	* \param initial_beam optional initial estimate of stack.
	*   If NULL (default) this is ignored and reference_member is used as the 
	*   initial beam estimate.
	* \param reference_member optional reference trace to use as starting estimate of
	*	stack for robust method.  Ignored for other simple or median stack.
	*	(default false)
	* \param normalize if true cross-correlation traces will be normalized. (default false)
	* \param parallel use a parallel processing algorithm (currently not implemented so 
	*       default is false).
	* \param correlate_only when true cross-correlation with the reference trace is performed
	*       but the data are not stacked and the beam attribute of the object is a copy
	*       of the input reference trace.  Trace header (Metadata) attributes normally set by this
	*       object, however, set in the input ensmble, d. 
	* \param freeze is somewhat the reverse of correlate_only.  When true a stack is computed
	*       but lag estimates are all set to zero.  In this situation the cross-correlation 
	*       functions are still computed and stored in the xcor attribute but the peak 
	*       correlation will not normally be aligned on zero.  The peakxcor and amplitude_static
	*       vectors will be properly filled out and are defined relative to the computed stack.
	*       In short, this pretty much behaves like the default behaviour except no time shifting
	*       done.  (Default is false)  Note since this is the opposite of correlate_only
	*       setting this and correlate_only both true will lead to a SeisppError exception
	*       being thrown.
	* \exception SeisppError will be thrown for a variety of conditions.  These include:  (1) no
	*	data in input ensemble, (2) no reference trace defined, (3) cascading errors thrown
	*       by stacker or other seispp procedures, and (4) input inconsistencies.
	*/
	MultichannelCorrelator(TimeSeriesEnsemble& d,
     	      CorrelationMethod meth,
	        TimeWindow beam_window,
	          TimeWindow robust_window=TimeWindow(),
	           double lag_cutoff=5.0,
	            StackType stack_type=BasicStack,
	              TimeSeries *initial_beam=NULL,
	               int reference_member=0,
	                bool normalize=false,
	                  bool parallel=false,
	                    bool correlate_only=false,
                      	      bool freeze=false);
    /*!
	*  \brief Construct this object from a ThreeComponentEnemble.
	*
	*  This constructor differs very little from it's close cousin that
	*  creates the object from a TimeSeriesEnsemble.  All it really does is
	*  extract the data component defined by the "component" argument into
	*  an ensemble of scalar time series data and then calls the TimeSeriesEnsemble
	*  constructor.  See the documentation for the TimeSeriesEnsemble based
	*  constructor for details on usage of other parameters.  They are identical
	*  here.
	*/
	MultichannelCorrelator(ThreeComponentEnsemble d,
		CorrelationMethod meth,
			TimeWindow beam_window,
				int component,
					TimeWindow robust_window=TimeWindow(),
				    double lag_cutoff=5.0,
						StackType stacktye=BasicStack,
							TimeSeries *intial_beam=NULL,
							    int reference_member=0,
								bool normalize=false,
									bool parallel=false);
	/*!
	*  \brief Standard copy constructor.
	*/
	MultichannelCorrelator(const MultichannelCorrelator& co);
	/*!
	*  \brief Standard assignment operator.
	*/
	MultichannelCorrelator& operator=(const MultichannelCorrelator& co);


	/*!
	*  \brief Return the stack trace used for this estimate.
	*/
	TimeSeries ArrayBeam() {return beam;}  // needs to test against method a beam not always computed
	/*!
	*  \brief Return the stack trace use for this estimate and post m to the result.
	*
	* Metadata provide a useful way to keep ancillary processing parameters with data.
	* this allows posting such results to the result in a single call without hiding
	* what is posted behind the interface.
	*/
	TimeSeries ArrayBeam(Metadata m);  // alternate appends m to beam metadata
private:
	TimeSeries beam;
};
}  // End SEISPP namespace declaration

#endif
