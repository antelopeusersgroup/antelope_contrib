#ifndef _TIMESERIES_H_
#define _TIMESERIES_H_
#include <vector>
#include <boost/serialization/serialization.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include "BasicTimeSeries.h"
#ifndef NO_ANTELOPE
#include "dbpp.h"
#endif
#include "Metadata.h"
namespace SEISPP {
using namespace std;
using namespace SEISPP;
/*! \brief Scalar time series data object.

// This data object extends BasicTimeSeries mainly by adding a vector of
// scalar data.  It uses a Metadata object to contain auxiliary parameters
// that aren't essential to define the data object, but which are necessary
// for some algorithms.  
//\author Gary L. Pavlis
**/
class TimeSeries: public BasicTimeSeries , public Metadata
{
public:
/*!
// Actual data stored as an STL vector container.  
// Note the STL guarantees the data elements of vector container are 
// contiguous in memory like FORTRAN vectors.  As a result things
// like the BLAS can be used with data object by using a syntax
// like this: if d is a TimeSeries object, the address of the first sample of 
// the data is &(d.s[0]).  
**/
	vector<double>s;
/*!
// Default constructor.  Initializes object data to zeros and sets the
// initial STL vector size to 0 length.
**/
	TimeSeries();
/*!
// Similar to the default constructor but creates a vector of data 
// with nsin samples and initializes all samples to 0.0.  
// This vector can safely be accessed with the vector index 
// operator (i.e. operator []).  A corollary is that push_back 
// or push_front applied to this vector will alter it's length
// so use this only if the size of the data to fill the object is
// already known.
**/
	TimeSeries(int nsin);
/*! \brief Partial constructor for a TimeSeries object driven by a Metadata object.
// This is essentially uses the Metadata object as a way to make a parameterized
// constructor with a potentially large and variable number of parameters. 
// The following parameters must exist in the Metadata object or the constructor
// will throw an exception:  samprate, time, and nsamp.  If the load_data
// boolean is true the function will attempt to read data using an additional
// set of keywords that must be in the metadata:  TimeReferenceType, datatype,
// dir, dfile, and foff.  TimeReferenceType is a string that must be either "relative"
// or "absolute".  datatype, dir, dfile, and foff are exactly as in a wfdisc record.
// An important current limitation is that only host 4 byte float datatype (t4 or u4)
// are allowed by this constructor.  If datatype is anything else the constructor will
// throw an exception.  
//
//\exception MetadataError object is thrown if any of the metadata associated with the 
//           keywords noted above are not defined.  
//\exception SeisppError is thrown for read errors when load_data is true.  
//
//\param md - Metadata object to drive construction.
//\param load_data - if true tries to read data in an antelope style ala wfdisc but using
//                     attributes derived from the Metadata object (see above).
**/
	TimeSeries(const Metadata& md,bool load_data);
/*! \brief Partial constructor with explicit core time series data.

  This is partial constructor that is useful to deal with type conversion
from higher level time series type object.   Metadata is copied verbatim 
from parent, which would typically be sent with a dynamic_cast from a 
child of Metadata.   The BasicTimeSeries component would normally be
from the same parent, although that isn't required.   The contents of the 
BasicTimeSeries are used to initialize the data vector to ns zeros.  
*/
        TimeSeries(const BasicTimeSeries& bd,const Metadata& md);
#ifndef NO_ANTELOPE

/*!
// Antelope database driven constructor.
// The basic model here is that this constructor builds a TimeSeries object
// from one row of a database table (normally wfdisc, but the intent is to 
// to be totally general).  
//\param db DatabaseHandle object that is assumed to point at one row of a database view.
//\param mdl A MetadataList contains a list of internal names that tells the 
//   constructor which attributes it will need to extract from the database and place in
//   the Metadata area of the object.  
//\param am This object is used to map internal names to an external namespace.  It is
//  normally created once at the early stage of a program's execution.  
**/
	TimeSeries(DatabaseHandle& db, MetadataList& mdl, AttributeMap& am);
#endif
/*!
// Standard copy constructor.
**/
	TimeSeries(const TimeSeries&);
/*!
// Standard assignment operator.
**/
	TimeSeries& operator=(const TimeSeries&);
/*!
// Summation operator.  Simple version of stack.  Aligns data before
// summing.
**/
	void operator+=(const TimeSeries& d);

/*!
// Extract a sample from data vector with range checking.
// Because the data vector is public in this interface
// this operator is simply an alterative interface to this->s[sample].  
// There are two primary differences.  First, this is a simpler interface
// to the data.  That is, if we have the object TimeSeries x, we could 
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
	double operator[](int sample);
/*!
// Scans for defined gaps and sets the data to zero in time periods defined by gaps.
**/
	void zero_gaps();
private:
        friend class boost::serialization::access;
        template<class Archive>void serialize(Archive & ar, const unsigned int version)
        {
            ar & boost::serialization::base_object<Metadata>(*this);
            ar & boost::serialization::base_object<BasicTimeSeries>(*this);
            ar & s;
        };
};
/*!
// Output a TimeSeries as ascii data to an output stream.
**/
ostream& operator << (ostream& os, TimeSeries& y);
}  // End SEISPP namespace declaration
#endif
