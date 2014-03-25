#ifndef _FILTER_PP_H_
#define _FILTER_PP_H_
#include <string>
#include "seispp.h"

namespace SEISPP
{
using namespace std;
using namespace SEISPP;
/*! \brief List of valid filter types for TimeInvariantFilter processing object.
*  The current list is pretty much those supported by the Antelope trfilter 
*  procedures, but the interface should easily support extensions.
*/
enum Filter_Type {highpass, /*!< highpass filter */
	lowpass,	/*!< lowpass filter */
	bandpass,	/*!< bandpass filter */
	WAA,		/*!< Convert to Wood-Anderson acceleration */
	WAV,		/*!< Convert to Wood-Anderson velocity */
	WAD,		/*!< Convert to Wood-Anderson displacement */
	DIF,		/*!< First derivative operator. */
	DIF2,		/*!< Second derivative operator. */
	INT,		/*!< Integrate in time once. */
	INT2,		/*!< Integrate in time twice. */
	DEMEAN,		/*!< Remove mean. */
	none		/*!< do nothing. */
};
/*! \brief Data processing object to implement standard signal processing filters.
*
* This object can be used to filter time series data by what is commonly called
* a time invariant filter.  That means the filter operator does not change with time.
* Such filters can be implemented by convolution as FIR filters, IIR filters through
* fixed coefficient ARMA operators, or through FFT methods.  The key concept is that 
* the filter properties never change and the same operator is applied uniformly 
* across a set of time series data.  
* \par
* This operator uses one scheme for data processing I call the "apply" method 
* model.  The constructor builds an object containing the recipe for the filter,
* but an "apply" method uses the parameters stored in the object to alter data. 
* The apply function itself is overloaded to allow an application to multiple
* base data types. 
* \par
* The current implementation leans heavily on the Antelope trfilter routines.
* The apply methods simple call that procedure directly with wrappers to handle
* the different data types. 
*
*\author Gary L. Pavlis
*/
class TimeInvariantFilter
{
public:
	/*! Defines the type of filter this object implements. */
	Filter_Type type;
	/*! Return the low (minimum frequency) corner. */
	double fmin();
	/*! Return the high (maximum frequency) corner. */
	double fmax();
	/*! Return the number of poles that define the low frequency rolloff.*/
	int fmin_poles();
	/*! Return the number of poles that define the high frequency rolloff.*/
	int fmax_poles();
	/*! Return the Antelope character string descriptor for this operator.
	* Antelope filters are defined by some keywords defined in trfilter (3).
	* For example, "BW 0.01 5 1.0 5" describes a 0.01 to 1 Hz Butterworth
	* bandpass filter with 5 poles on the high and low corners.  
	*/
	string type_description(bool verbose=true);
	/*! Default constructor.  Sets to null (do nothing) filter. */
	TimeInvariantFilter(){type=none,f1=0.0;f2=0.0;npole1=0;npole2=0;};
	/*! Use the Antelope keyword string method to define the filter properties.*/
	TimeInvariantFilter(string);
	/*! Fully parameterized constructor.
	\param flow low frequency corner.
	\param npl number of poles for low corner rolloff.
	\param fhigh high frequency corner.
	\param nph number of poles for high corner rolloff.
	*/
	TimeInvariantFilter(double flow, int npl, double fhigh, int nph);
	/*! Standard copy constructor. */
	TimeInvariantFilter(const TimeInvariantFilter&);
	/*! Standard assignment operator. */
	TimeInvariantFilter& operator=(const TimeInvariantFilter&);
	/*! Apply this filter to a vector of doubles. 
	*  The data are replaced by filtered data on output.
	*	\param ns number of samples in vector to be filtered.
	*	\param s pointer to vector holding sample data.
	*	\param dt sample interval of data (assumes uniform sampling.)
	* \exception SeisppError if the filter operator fails.  
	*/
	void apply(int ns, double *s,double dt);
	/*! Apply this filter to a vector of single precision floating point numbers. 
	*  The data are replaced by filtered data on output.
	*	\param ns number of samples in vector to be filtered.
	*	\param s pointer to vector holding sample data.
	*	\param dt sample interval of data (assumes uniform sampling.)
	* \exception SeisppError if the filter operator fails.  
	*/
	void apply(int ns, float *s,double dt);
	/*! Apply this filter to a TimeSeries object.
	* A TimeSeries is a data object holding time series data.  This filters
	* the data vector in a TimeSeries object.  The filter alters the data 
	* and posts the filter parameters as string Metadata accessible with the 
	* key "filter_spec".  It does this by a smart append operator that allows
	* the accumulation of multiple filter stages separated by semicolons.  
	* (e.g. "DEMEAN; BW 0.5 5 2.5 2").
	*/
	void apply(TimeSeries& ts);
	/*! Apply this filter to a ThreeComponentSeismogram object.
	* A ThreeComponentSeismogram is a data object holding 
	* three-component seismic data stored in a matrix.  This filters
	* the data vector in a TimeSeries object.  The filter is applied to 
	* each "channel" of the data matrix.  
	* The filter alters the data 
	* and posts the filter parameters as string Metadata accessible with the 
	* key "filter_spec".  It does this by a smart append operator that allows
	* the accumulation of multiple filter stages separated by semicolons.  
	* (e.g. "DEMEAN; BW 0.5 5 2.5 2").
	*/
	void apply(ThreeComponentSeismogram& tce);
#ifndef NO_ANTELOPE
	/*! Apply this filter to a trace database. */
	void apply(Dbptr tr);
#endif

private:
	string filter_spec;
	// keep these here to avoid having to parse this 
	double f1,f2;  // low and high corner respectively
	int npole1,npole2;
};
/*! \brief Filter an entire ensemble of TimeSeries objects.
* This procedure filters an ensemble of TimeSeries objects using
* a common TimeInvariantFilter object.  The apply() method is 
* applied to each member.  
*
* \exception SeisppError is thrown if filtering of any member 
*	fails.  If this happens the ensemble may be left in a 
*	partially processed state.
*/
void FilterEnsemble(TimeSeriesEnsemble& ensemble,
		TimeInvariantFilter& filter);
/*! \brief Filter an entire ensemble of ThreeComponentSeismogram objects.
* This procedure filters an ensemble of ThreeComponentSeismogram objects using
* a common TimeInvariantFilter object.  The apply() method is 
* applied to each member.  
*
* \exception SeisppError is thrown if filtering of any member 
*	fails.  If this happens the ensemble may be left in a 
*	partially processed state.
*/
void FilterEnsemble(ThreeComponentEnsemble& ensemble,
		TimeInvariantFilter& filter);
}  // End namespace SEISPP


#endif
