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
/*! Use the stdlib complex template to define complex quantities.
*  For a complex valued time series we need to define complex numbers
*  since complex is not native to C/C++.  Here we use the stdlib
*  complex template.  We make the values double to be consistent with
*  other time series data in this library. */
typedef std::complex<double> Complex;
/*! \brief Complex valued time series data object.

// This data object extends BasicComplexTimeSeries mainly by adding a vector of
// complex data.  It uses a Metadata object to contain auxiliary parameters
// that aren't essential to define the data object, but which are necessary
// for some algorithms.  It uses an STL vector container of Complex data
// objects to hold it's contents.  This does not mesh well with FORTRAN
// oriented algorithms like the BLAS, but makes a lot of other things 
// cleaner.   The intention is that if an algorithm needs to utilize
// a vector algorithm that is FORTRANish one can simply copy data to and
// from a work vector.  
//\author Gary L. Pavlis
**/
class ComplexTimeSeries : public BasicTimeSeries, public Metadata 
{
public:
/*!
* \brief Holds actual time series data.  
* We use an STL vector container to hold the data that define this time
* series.  Note because Complex is an alias for std::complex the contents 
* of the data vector cannot be easily handled with FORTRANish vector algorithms
* like those used in the BLAS.  If vector processing is important for 
* efficiency an algorithm would be advised to copy the contents to a work
* vector.  
**/
	vector<Complex> s;
	
/*!
// Default constructor.  Initializes all data but sets data vector
// to be zero length.
**/
	ComplexTimeSeries();
/*!
// Space allocating constructor.  
// Calls reserve on vector container to set aside nsin slots but 
// not data is loaded.  The ns variable is set to nsin, but other
// data are initialized to defaults.  Default Metadata constructor
// is called.
//\param nsin expected size of vector data.
**/
	ComplexTimeSeries(int nsin);
/*!
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
//\exception MetadataError if any of the required attributes is missing
//   from the input Metadata.
//\exception SeisppError if load_data is set true and there are any
//   i/o errors.  
//
//\param mdheader Metadata object to be copied to the header for this
//   object.
//\param load_data when set true constructor will attempt to read data
//   from the file dir+"/"+dfile.
**/
	ComplexTimeSeries(const Metadata& mdheader, bool load_data);
/*!
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
//\exception MetadataError if any of the required attributes is missing
//   from the input Metadata.
//\exception SeisppError if load_data is set true and there are any
//   i/o errors.  
//
//\param rdb DatabaseHandle object that is assumed to point at one row of a database view.
//\param mdl A MetadataList contains a list of internal names that tells the 
//   constructor which attributes it will need to extract from the database and place in
//   the Metadata area of the object.  
//\param am This object is used to map internal names to an external namespace.  It is
//  normally created once at the early stage of a program's execution.  
**/
	ComplexTimeSeries(DatabaseHandle& rdb,MetadataList& mdl,
                        AttributeMap& am);
/*!
// Standard copy constructor.
**/
        ComplexTimeSeries(const ComplexTimeSeries&);
/*!
// Standard assignment operator.
**/
        ComplexTimeSeries& operator=(const ComplexTimeSeries&);
/*!
// Summation operator.  Simple version of stack.  
// That is, alters contents to this->s + d.s.
// If right hand side does not overlap with left
// hand side (*this to operator) it will be silently be accumulated
// to produce a potentially ragged edge on left or right.  
**/
        void operator+=(const ComplexTimeSeries& d);
/*!
// Vector subtraction accumulation operator.  
// That is, it does x-=y when x and y are complex valued vectors.
// Aligns data before
// subtracting d.  If right hand side does not overlap with left
// hand side (*this to operator) it will be silently be accumulated
// to produce a potentially ragged edge on left or right.  
//
**/
        void operator-=(const ComplexTimeSeries& d);
/*!
// Multiplies each sample in data by scale factor zr.  
// Returns rescaled copy of parent.  
**/
	ComplexTimeSeries operator*(const double zr);
/*!
// Multiplies each sample in data by complex scale factor zr.  
// Returns rescaled copy of parent.  
**/
	ComplexTimeSeries operator*(const Complex z);
/*!
// Extract a sample from data vector with range checking.
// Because the data vector is public in this interface
// this operator is simply an alterative interface to this->s[sample].  
// There are two primary differences.  First, this is a simpler interface
// to the data.  That is, if we have the object ComplexTimeSeries x, we could 
// either use:  a=x[i]; or a=x.s[i].   The later, which is this 
// operator, is clearly simpler.  It comes at a nontrivial cost,
// which is the second major difference.  That is, this is a range
// checking operator.  If the requested sample is outside the range
// of the data this operator will throw a SeisppError exception.
//
//\exception SeisppError exception if the requested sample is outside
//    the range of the data.  Note this includes an implicit "outside"
//    defined when the contents are marked dead.  
//
//\param sample is the integer sample number of data desired.
**/
	Complex operator[](int sample);
/*!
// Initializes all data to constant z.
**/
	void initialize(const Complex z);
/*!
// Returns a copy of parent with each vector element replaced by 
// it's complex conjugate.
**/
	ComplexTimeSeries conj();
/*!
// Extract the real part of this object to a TimeSeries object.
// Often we want the real or imaginary part of a complex vector.
// This method function copies the Metadata area of this object 
// and the BasicTimeSeries attributes and then copies the real
// part of each vector component to the output TimeSeries object.
**/
	TimeSeries real();
/*!
// Extract the imaginary part of this object to a TimeSeries object.
// Often we want the real or imaginary part of a complex vector.
// This method function copies the Metadata area of this object 
// and the BasicTimeSeries attributes and then copies the imaginary
// part of each vector component to the output TimeSeries object.
**/
	TimeSeries imag();
/*!
// Get the modulus (magnitude) of a complex vector and return it as
// a scalar time series.  
// This method function copies the Metadata area of this object 
// and the BasicTimeSeries attributes and then computes the modulus
// of part of each vector component and places the 
// result in the output TimeSeries object.
**/
	TimeSeries mag();
/*!
// Get the phase angles of each sample of a complex vector and return it as
// a scalar time series.  
// This method function copies the Metadata area of this object 
// and the BasicTimeSeries attributes and then computes the phase
// each vector component and places the 
// result in the output TimeSeries object.
**/
	TimeSeries phase();
/*!
// Zeros data in sections marked as gaps.
**/
	void zero_gaps();
/*!
// Overloaded ascii output stream operator.  
**/
	friend ostream& operator<<(ostream& os,ComplexTimeSeries& z);
};
ostream& operator<<(ostream& os,ComplexTimeSeries& z);
/*!
// Save the data in a ComplexTimeSeries object to a database.
// This function works only with an Antelope (Datascope) database but the
// design is aimed to be schema and database independdent.  
// It does this by assuming each object will generate one row in 
// some table.  What is written to the row that is associated with
// this object is assumed to be present in the Metadata area of the
// object.  The attributes to be written are controlled by the
// contents of the MetadataList passed as an argument.  
// The internal names in the MetadataList are translated 
// translated to the database namespace using the AttributeMap
// object am and pushed to an output record using the Datascope dbputv
// function one attribute at a time.  The data are saved to files
// whose name and location are driven by two (frozen) standard names
// extracted from the metadata area:  dir and dfile.  The filename
// for output is created as dir+"/"+dfile or simply dfile if dir
// is not defined (assumes current directory).  
//
// This function is dogmatic about four database output names.  
// It always translates it's internal t0 to be a "time" database
// attribute,  the ns variable is saved as "nsamp", the sample
// interval (dt) is always converted to 1/dt and called "samprate",
// and an endtime (computed as this->endtime()) is computed and
// saved as the database attribute "endtime".   These are the css3.0
// name conventions and I chose to leave them as frozen names.  
// The current implemenation ALWAYS saves the result as a 
// host-specific (i.e. not portable across platform) 
// vector of double complex values (real,imag).  i.e. real and 
// imaginary parts are multiplexed and stored externally as 
// doubles.  This will NOT work if data like this are passed 
// between different hosts with incompatible binary double
// formats (infamous endian problem).
// The "datatype" is ALWAYS saved and is ALWAYS set to cx.  
// Finally note that if the "live" boolean in the object is set false
// this function silently returns immediately doing nothing.
//
//\exception SeisppError object if there are any problems saving the data or 
//    writing attributes into the database.
//
//\return -1 if live is false, record number of added row otherwise
//
//\param ts is the ComplexTimeSeries object to be saved.
//\param db is a Datascope database pointer.  It need only point at a valid
//    open database.
//\param table is the name of the table to index this time series data
//   (generally wfprocess for this procedure)
//\param md  is the list of metadata to be dumped to the database as described above.
//\param am is a mapping operator that defines how internal names are to be mapped
//    to database attribute names and tables.  
*/
long dbsave(ComplexTimeSeries& ts,Dbptr db,
        string table, MetadataList& md, 
        AttributeMap& am);
/*! Measure peak amplitude (a modulus) of ComplexTimeSeries. */
double PeakAmplitude(ComplexTimeSeries *p);
/*! Scales a ComplexTimeSeries object by scale. */
void ScaleMember(ComplexTimeSeries *p,double scale);
} // End SEISPP Namespace declaration
#endif
