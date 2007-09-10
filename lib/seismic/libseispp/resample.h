#ifndef _RESAMPLE_H_
#define _RESAMPLE_H_
#include <map>
#include <vector>
#include "stock.h"
#include "pf.h"
#include "TimeSeries.h"

// Need this when I add this to seispp
namespace SEISPP {
using namespace std;
using namespace SEISPP;


/*! \brief Internal object used for decimation operators.

This object is closely related to decimation/resampling operators.
It is used internally to hold decimated data during resampling.
It can be viewed as a simplified TimeSeries object.
It is probably of minimal use to anyone but the author.
I probably should have hidden it.
*/
class DecimatedVector
{
public:
	/*! Lag in samples of first sample relative to parent vector (always positive).*/
	int lag; 
	/*! holds decimatged data.*/
	vector<double>d;
	/*! Default constructor. Useless but is an explicit initializer.*/
	DecimatedVector(){lag=0;d.resize(0);};
	/*! Allocate slots for data but do not initialize vector.*/
	DecimatedVector(int ns);
	/*! Standard copy constructor.*/
	DecimatedVector(const DecimatedVector&);
	/*! Standard assignment operator. */
	DecimatedVector& operator=(const DecimatedVector&);
};

/*! \brief A decimation operator for time series data.
*
*  Decimation and/or resampling is implemented with this operator object.
* The object uses an "apply" approach to data processing as it would 
* commonly be applied to many similar data traces on a single run.  
* This is a low level processing object used internally in the higher
* level resampling procedures.  The operator defined by this object
* will handle both upsampling and downsampling.  Downsampling normally
* requires an FIR filter operator to be applied as an antialiasing operator.
*/	
class Decimator
{
public:
	/*! Decimation factor for this operator.  

	Greater than 1.0 means downsampling.
	If greater 1.0 means upsampling.  e.g. decfac=0.5 means sample rate will be halved.
	*/
	double decfac;
	/*! Default constructor.  

	Produces none operator which do nothing to data. */
	Decimator();
	/*! Construct this operator from a CSS format response file. 

	Antelope uses a set of standard files to describe filter responses following
	the CSS specification.  These constructor reads such and uses it to construct
	a decimation operator.  Currently the filter this file specifies is required
	to be an FIR filter.  This prejudice came from the fact that FIR filters are
	now universally used on modern data loggers and the most sensible choices for
	decimators are generally those used by various digitizers.  
	\param fname file name of response file to be used to define this operator.
	\param decfac_expected expected decimation factor for this response file.
	
	\exception Throws a SeisppError exception if the response file can't be found or
		is inconsistent with the decfac_expected argument.,
	*/
	Decimator(string fname, double decfac_expected); 
	/*! Standard copy constructor. */
	Decimator(const Decimator&);
	/*! Standard assignment operator. */
	Decimator& operator=(const Decimator&);
	/*! Apply operator to a vector of data using default trim.

	This is an overloaded method that calls the 3 argument version with the trim
	variable defaulted to false.   It is a shortcut. 
	\return DecimatedVector object containing decimated data.  Note a pointer is
		returned for efficiency, which means the caller needs to delete this 
		pointer before it goes out of scope or is overwritten.
	
	\param ns number of samples in input vector.
	\param di pointer to array of length ns holding data to which the operator should
		be applied.
	*/
	DecimatedVector *apply(int ns, double *di);
	/*! Apply operator to an array of data.

	\return DecimatedVector object containing decimated data.  Note a pointer is
		returned for efficiency, which means the caller needs to delete this 
		pointer before it goes out of scope or is overwritten.
	
	\param ns number of samples in input vector.
	\param di pointer to array of length ns holding data to which the operator should
		be applied.
	\param trim if true the output is trimmed on the right and left according to the
		size of the FIR filter used for a decimator (usually 1/2 the number of 
		coefficients in the filter).  Ignored for upsampling. 
	*/	  
	DecimatedVector *apply(int ns, double *di,bool trim); 
	/*! Apply operator to an STL vector of data. 

	\return DecimatedVector object containing decimated data.  Note a pointer is
		returned for efficiency, which means the caller needs to delete this 
		pointer before it goes out of scope or is overwritten.
	
	\param di data vector to which the operator should be applied.
	\param trim if true the output is trimmed on the right and left according to the
		size of the FIR filter used for a decimator (usually 1/2 the number of 
		coefficients in the filter).  Ignored for upsampling. 
	*/	  
	DecimatedVector *apply(vector<double>di,bool trim);
	
private:
	vector<double>coefs;
	int lag;  // position in coefs of zero lag point
};
/*! \brief A general resampling operator for time series data. 

Defines an operator that can be applied to resample data within a range of
target sample rates.  The range property was used to allow for digitizers
with slippery clocks that skew the sample rate to get a target number of samples
in a specific time period.  Thus we can, for example, specify and operator that
is valid for data with a sample rate between 39 and 41 Hz to allow for nominal
40 Hz data.
\par
This operator uses the "apply" model for object oriented data processing.
That means you build the operator once and then apply it to lots of data during
processing.  
*/
class ResampleOperator
{
public:
	/*! Low limit of sampling frequency for which this operator is appropriate.*/
	double low;
	/*! High limit of sampling frequency for which this operator is appropriate. */
	double high;
	/*! Exact sample interval that is the target of this operator.*/
	double exact;
	/*! Decimation stages to use to get to target sample rate.*/
	list <Decimator> declist;

	/*! Partial constructor.  Defines a do nothing operator for the specified
	sampling interval range.  That is, it creates a do nothing operator for this
	sample interval range. 
	\param e Set exact attribute to this value.
	\param l Set low attribute to this value.
	\param h Set high attribute to this value.
	*/
	ResampleOperator(double e, double l, double h);
	/*! Construct from a parameter file.  This is the most common method for
	constructing this operator in the current implementation.  Antelope users
	should read man resample(3) for the format of the parameter file.
	 \param e Set exact attribute to this value.
	 \param pf Antelope Pf to parse to create this object.
	*/
	ResampleOperator(double e, Pf *pf);
	/*! Standard copy constructor. */
	ResampleOperator(const ResampleOperator& ro);
	/*! Standard assignment operator. */
	ResampleOperator& operator= (const ResampleOperator&);
	/*! Apply this operator to an array of data.
	This method will apply this operator to produce a vector of new data
	with the resampling operator applied to produce the result.  This is
	done blindly assuming the sample rate of the input is consistent with
	the input data.
	\param ns length of input data array.
	\param s array of length ns containing data to be resampled.  
		As a raw array as usual be warned this assumes that this 
		pointer references a block of memory of that size.
	\param dtin exact input sample rate of data.
	\param dtout target output sample rate.  
	\param trim if true the output is trimmed on the right and left according to the
		size of the FIR filter used for a decimator (usually 1/2 the number of 
		coefficients in the filter).  Ignored for upsampling.
	*/
	DecimatedVector *apply(int ns,double *s,double dtin, double dtout,
		bool trim);
};
/*! \brief Interval class used internally to provide an interval sample rate.

Often we want to use a loose match for a real number.  An example is the
resampling operator that can work over a fairly loose range of sample intervals.
*/
class Interval
{
public:
	/*! upper limit of interval. */
	double high;
	/*! Lower limit of interval. */
	double low;
};
/*! \brief Function object for interval comparisons for weak order.
*/
class IntervalCompare
{
public:
	bool operator()(const Interval r1, const Interval r2) const
	{return(r1.high<r2.low);};
};
/*! \brief Container to hold a set of recipes for resampling data.
*
* This object uses an STL map container to hold a set of ResampleOperator
* objects. It provides a high level object to contain a complete set of
* recipes to resample an complete dataset.  It is normally constructed
* early in the life of a program from a parameter file and used for
* on the fly resampling as data are read and processed.  
*/
class ResamplingDefinitions
{
public:
	/*! Default constructor.  Builds an empty map.*/
	ResamplingDefinitions(){
		decset=map<Interval,ResampleOperator,IntervalCompare>();
	};
	/*! Construct from a parameter file.

	This is the primary constructor for this object.  Uses
	a complicated parameter file structure described in resample(3).
	*/
	ResamplingDefinitions(Pf *pf);
        /*! Container that holds the ResampleOperator objects.

	Note the map is keyed by an interval definition.  This allows us
	to look up a sample rate to match an interval and avoids the mess
	of an exact floating point match. */
	map<Interval,ResampleOperator,IntervalCompare> decset;
};


// function prototypes that use the objects defined above
/*! \brief High level procedure to resample a time series automatically
to a target sample rate. 

This procedure will take an input time series with an arbitrary sample rate
and attempt to upsample or downsample the data to a target sample rate.  
Multiple stage can be defined for this operation through the ResamplingDefinitions
object handed to the procedure.  The recipes allowed by ResamplingDefinitions
are very general (read also that this means complicated) allowing both 
decimations and interpolation to mismatched sampling schemes.  

\param ts input data to be resampled.
\param rd object containing recipes for resampling different input sample rates.
\param dtout output target sample rate.  Result will aim to match this as closely
	as possible.  
\param trim if true the output will be trimmed on the right and left to remove 
	FIR filter transient edge effects.  

\return New TimeSeries object resampled to dtout.  The Metadata area of this 
	new trace is a clone of the parent except for sample rate.
*/ 

TimeSeries ResampleTimeSeries(TimeSeries& ts, 
		ResamplingDefinitions& rd,
			double dtout,
				bool trim);


} // end namespace encapsulation
#endif
