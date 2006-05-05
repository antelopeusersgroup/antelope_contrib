#ifndef _COMPLEX_TIMESERIES_H_
#define _COMPLEX_TIMESERIES_H_
#include <vector>
#include <complex>
// Assumes TimeSeries.h includes BasicTimeSeries.h
#include "TimeSeries.h"
#include "dbpp.h"
#include "Metadata.h"
namespace SEISPP {
using namespace std;
using namespace SEISPP;
typedef std::complex<double> Complex;
//@{
// Complex valued time series data object.
// This data object extends BasicComplexTimeSeries mainly by adding a vector of
// complex data.  It uses a Metadata object to contain auxiliary parameters
// that aren't essential to define the data object, but which are necessary
// for some algorithms.  
//@author Gary L. Pavlis
//@}
class ComplexTimeSeries : public BasicTimeSeries, public Metadata 
{
public:
//@{
// Holds actual time series data.  
//@}
	vector<Complex> s;
	
//@{
// Default constructor.  Initializes all data but sets data vector
// to be zero length.
//@}
	ComplexTimeSeries();
//@{
// Space allocating constructor.  
// Calls reserve on vector container to set aside nsin slots but 
// not data is loaded.  The ns variable is set to nsin, but other
// data are initialized to defaults.  Default Metadata constructor
// is called.
//@param nsin expected size of vector data.
//@}
	ComplexTimeSeries(int nsin);
//@{
// Metadata driven constructor.  
// Copies Metadata from argument mdheader.  Optionally loads data
// when the load_data is set true.  Note that data load is fairly 
// restrictive.  To be successful mdheader MUST contain all of the 
// following attributes:  samprate,TimeReferenceType,time, and nsamp.
// This constructor throws an exception if these variables are not
// present.  In addtion, 
// when load_data is set true the constructor also requires attributes
// dir, dfle, and datatype.  
// Currently datatype is REQUIRED to be cx.  If it is anything else 
// a SeisppError exception will be thrown.  
//
//@throws MetadataError if any of the required attributes is missing
//   from the input Metadata.
//@throws SeisppError if load_data is set true and there are any
//   i/o errors.  
//
//@param mdheader Metadata object to be copied to the header for this
//   object.
//@param load_data when set true constructor will attempt to read data
//   from the file dir+"/"+dfile.
//@}
	ComplexTimeSeries(const Metadata& mdheader, bool load_data);
//@{
// Antelope database driven constructor.
// The basic model here is that this constructor 
// builds a ComplexTimeSeries object
// from one row of a database table (normally wfdisc, but the intent is to 
// to be totally general).  
//
//  To be successful the Metadata loaded from the 
// database  MUST contain all of the 
// following attributes:  samprate,TimeReferenceType,time, nsamp,
// dir, dfile, and dataype.  
// This constructor throws an exception if any of these variables are not
// present.  
// Currently datatype is REQUIRED to be cx.  If it is anything else 
// a SeisppError exception will be thrown.  
//
//@throws MetadataError if any of the required attributes is missing
//   from the input Metadata.
//@throws SeisppError if load_data is set true and there are any
//   i/o errors.  
//
//@param db DatabaseHandle object that is assumed to point at one row of a database view.
//@param mdl A MetadataList contains a list of internal names that tells the 
//   constructor which attributes it will need to extract from the database and place in
//   the Metadata area of the object.  
//@param am This object is used to map internal names to an external namespace.  It is
//  normally created once at the early stage of a program's execution.  
//@}
	ComplexTimeSeries(DatabaseHandle& rdb,MetadataList& md_to_extract,
                        AttributeMap& am);
//@{
// Standard copy constructor.
//@}
        ComplexTimeSeries(const ComplexTimeSeries&);
//@{
// Standard assignment operator.
//@}
        ComplexTimeSeries& operator=(const ComplexTimeSeries&);
//@{
// Summation operator.  Simple version of stack.  
// That is, alters contents to this->s + d.s.
// If right hand side does not overlap with left
// hand side (*this to operator) it will be silently be accumulated
// to produce a potentially ragged edge on left or right.  
//@}
        void operator+=(const ComplexTimeSeries& d);
//@{
// Vector subtraction accumulation operator.  
// That is, it does x-=y when x and y are complex valued vectors.
// Aligns data before
// subtracting d.  If right hand side does not overlap with left
// hand side (*this to operator) it will be silently be accumulated
// to produce a potentially ragged edge on left or right.  
//
//@}
        void operator-=(const ComplexTimeSeries& d);
//@{
// Multiplies each sample in data by scale factor zr.  
// Returns rescaled copy of parent.  
//@}
	ComplexTimeSeries operator*(const double zr);
//@{
// Multiplies each sample in data by complex scale factor zr.  
// Returns rescaled copy of parent.  
//@}
	ComplexTimeSeries operator*(const Complex z);
//@{
// Initializes all data to constant z.
//@}
	void initialize(const Complex z);
//@{
// Returns a copy of parent with each vector element replaced by 
// it's complex conjugate.
//@}
	ComplexTimeSeries conj();
//@{
// Extract the real part of this object to a TimeSeries object.
// Often we want the real or imaginary part of a complex vector.
// This method function copies the Metadata area of this object 
// and the BasicTimeSeries attributes and then copies the real
// part of each vector component to the output TimeSeries object.
//@}
	TimeSeries real();
//@{
// Extract the imaginary part of this object to a TimeSeries object.
// Often we want the real or imaginary part of a complex vector.
// This method function copies the Metadata area of this object 
// and the BasicTimeSeries attributes and then copies the imaginary
// part of each vector component to the output TimeSeries object.
//@}
	TimeSeries imag();
//@{
// Get the modulus (magnitude) of a complex vector and return it as
// a scalar time series.  
// This method function copies the Metadata area of this object 
// and the BasicTimeSeries attributes and then computes the modulus
// of part of each vector component and places the 
// result in the output TimeSeries object.
//@}
	TimeSeries mag();
//@{
// Get the phase angles of each sample of a complex vector and return it as
// a scalar time series.  
// This method function copies the Metadata area of this object 
// and the BasicTimeSeries attributes and then computes the phase
// each vector component and places the 
// result in the output TimeSeries object.
//@}
	TimeSeries phase();
//@{
// Zeros data in sections marked as gaps.
//@}
	void zero_gaps();
//@{
// Overloaded ascii output stream operator.  
//@}
	friend ostream& operator<<(ostream& os,ComplexTimeSeries& z);
};
ostream& operator<<(ostream& os,ComplexTimeSeries& z);
} // End SEISPP Namespace declaration
#endif
