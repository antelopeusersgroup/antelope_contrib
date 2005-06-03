//@{
// Seismic C++ Library (SEISPP).
// This is a collection of C++ objects used for processing seismic data.  
// Objects define some common seismological concepts including time series
// data, three component seismograms, ensembles of seismograms, velocity
// models, hypocenters, slowness vectors, and others.  
// The library has a strong dependence on Antelope in implementation, but
// the API design was intended to be more general.  
//@}
#ifndef _SEISPP_H_
#define _SEISPP_H_
#include <vector>
#include <set>
#ifdef sun
#include <sunmath.h>
#endif
#include <limits.h>
// antelope routines
#include "db.h"
#include "tr.h"
#include "pf.h"
// glp routines
#include "metadata.h"
#include "dbpp.h"
#include "pfstream.h"
#include "dmatrix.h"

//@{
// The SEISPP namespace encapsulates the library functions and 
// classes that defined the SEISPP seismic processing library in C++.
// Almost all applications using this library will need a 
// "using namespace SEISPP" line to make the package visible to 
// the compiler and linker.
//@}
namespace SEISPP 
{
//@{
// Turns verbose mode on and off.  
//@}
extern bool SEISPP_verbose;
using namespace std;
//@{
// Base class for error object thrown by seispp library routines.
// This is the generic error object thrown by the seispp library. 
// it is similar in concept to basic error objects described in various
// books by Stroustrup.  The base object contains only a simple 
// generic message and a virtual log_error method common to all 
// seispp error objects that are it's descendents.
//@}
class SeisppError
{
public:
//@{
// Holds error message that can be printed with log_error method.
//@}
	string message;
//@{
// Default constructor built inline.
//@}
	SeisppError(){message="seispp library error\n";};
//@{
// Copy constructor.
//@}
	SeisppError(const string mess){message=mess;};
//@{
// Sends error message thrown by seispp library functions to standard error.
//@}
	virtual void log_error(){cerr << "seispp error: "<<message<<endl;};
};

//@{
// Defines severity code for errors thrown by database routines.
//@}
enum ErrorSeverity {fault, fatal, complain, notify, log, unknown};
//@{
// Error object thrown by routines accessing an Antelope database.  
// This object normally leads to errors written by Antelope elog functions
// and extra messages attached to the error object.
//@}
class SeisppDberror : public SeisppError
{
public:
//@{
// Antelope (Datascope) database pointer when error occurred.
//@}
        Dbptr db;
//@{
// Error severity level code.  
//@}
	ErrorSeverity error_type;
//@{
// Standard constructor for this object.
//@}
        SeisppDberror(const string mess,Dbptr dbi);
//@{
// Copy constructor.
//@}
	SeisppDberror(const string mess, 
		Dbptr dbi, ErrorSeverity et);
//@{ 
// Sends error message thrown by seispp library functions to standard error.
// This version writes errors from elog functions posted by Antelope libraries.
//@}

	void log_error();
};
//@{
// Special error object thrown by SAC file reader.
//@}

class SACdataError : public SeisppError
{
public:
//@{
// Standard constructor for this object.
//@}
	SACdataError(const string mess){message=mess;};
//@{
// Sends error message thrown by seispp library functions to standard error.
//@}
	void log_error()
	{
		cerr<<"Error processing SAC format time series"<<endl;
		cerr<<"Error message = "<<message;
	}
};
//@{
// Slowness vector object.  
// Slowness vectors are a seismology concept used to describe wave propagation.
// A slowness vector points in the direction of propagation of a wave with a
// magnitude equal to the slowness (1/velocity) of propagation.  
//@}

class SlownessVector
{
public:
//@{
// East-west component of slowness vector.
//@}
	double ux;
//@{
// North-south component of slowness vector.
//@}
	double uy;
//@{
// Default constructor.
//@}
	SlownessVector();
//@{
// Copy constructor.
//@}
	SlownessVector(const SlownessVector&);
//@{
// Computes the magntitude of the slowness vector.
// Value returned is in units of seconds/kilometer.  
//@}
	double mag();
//@{
// Returns the propagation direction defined by a slowness vector.
// Azimuth is a direction clockwise from north in the standard geographic
// convention.  Value returned is in radians.
//@}
	double azimuth();
//@{
// Returns the back azimuth direction defined by a slowness vector.
// A back azimuth is 180 degrees away from the direction of propagation and 
// points along the great circle path directed back to the source point 
// from a given position.  The value returned is in radians.
//@}
	double baz();
};
//@{
// This object defines a uniform grid of points in slowness space.
// In array processing it is common to need a grid of feasible slowness 
// vectors and data are commonly stacked to define all of this family 
// of plane wave stacks.  The object used here grids up slowness space
// in uniform spacing in ux and uy components.  
// The gridding is defined by the lower left corner (uxlow, uylow), 
// spacing in EW and NS directions (dux, duy), and the number of 
// points in the x (EW) and y (NS) directions.  
// Note the grid and all methods assume units of s/km, although 
// this application is largely blind to units.
//@}
class RectangularSlownessGrid
{
public:
//@{
// Name assigned to this grid object.
//@}
	string name;
//@{
// Minimum ux (East-west) component of grid.
// The location of the lower left corner of the grid defined by this object is
// at coordinates (uxlow,uylow).
//@}
	double uxlow;
//@{
// Minimum uy (North-south) component of grid.
// The location of the lower left corner of the grid defined by this object is
// at coordinates (uxlow,uylow).
//@}
	double uylow;
//@{
// Grid size (s/km) of slowness grid in EW component.  
//@}
	double dux;
//@{
// Grid size (s/km) of slowness grid in NS component.  
//@}
	double duy;
//@{
// Number of grid points in x (EW) direction of slowness grid.
//@}
	int nux; 
//@{
// Number of grid points in y (NW) direction of slowness grid.
//@}
	int nuy;
//@{
// Default constructor.
//@}
	RectangularSlownessGrid();  // generic default is defined
//@{
// Fully parameterized constructor.
//
// @param nm - name to be assigned to grid.
// @param uxl - lower left corner ux.
// @param uyl - lower left corner uy.
// @param du1 - dux (increment in x direction)
// @param du2 - duy (increment in y direction)
// @param n1 - number of grid points in x direction.
// @param n2 - number of grid points in y direction.
//@}
	RectangularSlownessGrid(string nm, double uxl, double uxh,
		double du1,double du2,int n1, int n2);
//@{
// Parameter file driven constructor.
// @param pf - Antelope pf pointer normally produced by earlier call to pfread
// @param tag - name to search in pf to describe this grid object.`
//              The parameters to describe the object are assumed encased in an 
//              &Arr{ } construct with this tag.  This allows multiple grids to 
//              be defined in a single parameter file with different tags. 
//@}
	RectangularSlownessGrid(Pf *pf,string tag);
//@{
// Standard copy constructor.
//@}
	RectangularSlownessGrid(const  RectangularSlownessGrid&);
//@{
// Returns x component of slowness grid at index position i.
//@}
	double ux(int i) {return(uxlow+i*dux);};
//@{
// Returns y component of slowness grid at index position j.
//@}
	double uy(int i) {return(uylow+i*duy);};
//@{
// Returns a slowness grid object for grid position (i,j).
// @throws SeisppError object if i and j are outside range.
//@}
	SlownessVector slow(int i, int j);
};

// This is used to define gaps and/or time variable weights
// The later is not implemented, but we 
//
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
};
//@{
// A time window with a functional form overlaid.
// This is essential an object definition of a window function
// for a time window defined by a TimeWindow object.
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
//@}
class TimeWindowCmp
{
public:
	bool operator()(const TimeWindow ti1,const TimeWindow ti2) const
	{return(ti1.end<ti2.start);};
};


//@{
// Type of time standard for time series data.
// Time series data have two common standards.  Absolute time means the
// time is an epoch time.  Relative means time is some other arbitrary
// reference.  An example is an arrival time reference frame where all
// data are set with time zero defined by a set of arrival time picks.  
//@}
enum TimeReferenceType {absolute,relative};

//@{
// Nearest integer function.
// This is the same as the standard intrinsic function by the same name.
// It is included in this library at the moment to support g++ in linux
// which does not seem to have this function in the standard math library.
// Note Sun puts it in sunmath.h.
//@}
int nint(double);
//@{
// Base class for time series objects.  
// This is a mostly abstract class defining data and methods shared by all
// data objects that are time series.  To this library time series means
// data sampled on a 1d, uniform grid defined by a sample rate, start time,
// and number of samples.  Derived types can be scalar, vector, or complex 
// but share the common property of being related to a base grid.
// This is a classic use of inheritance in OOP.
//@}
class BasicTimeSeries
{
public:
//@{
// Boolean defining if a data object has valid data or is to be ignored.
// Data processing often requires data to be marked bad but keep the original
// data around in case an error was made.  This boolean allows this capability.
//@}
	bool live;
//@{
// Sample interval.
//@}
	double dt;
//@{
// Data start time.  That is the time of the first sample of data.
//@}
	double t0;
//@{
// Number of data samples in this data object.
//@}
	int ns;
//@{
// Time reference standard for this data object.  Defined by enum Time_Reference
// this currently is only one of two things.  When set as "absolute" the time
// standard is an epoch time.  When set as "relative" time has no relationship
// to any external standard but are relative to some arbitrary reference that must
// ascertained by the algorithm by some other means (in seispp this is normally
// done through a metadata object).  A classic example is multichannel data where
// channels have a time relative to a shot time.  
//@}
	TimeReferenceType tref;
//@{
// Default constructor. Does essentially nothing since a BasicTimeSeries
// object has no data.  Does initialize data to avoid run time checkers 
// bitching about unitialized data, but values are meaningless when this
// constructor is called.
//@}
	BasicTimeSeries::BasicTimeSeries();
//@{
// Standard copy constructor.
//@}
	BasicTimeSeries::BasicTimeSeries(const BasicTimeSeries&);
//@{
// Checks if a sample defined by an integer offset value is a data gap.
// Calls like seis.is_gap(is) return true if sample is is a data gap.  
// It also returns true if i is outside the range of the data. 
// (i.e. less than 0 or >= ns).
// @param is - sample number to test.
//@}
	bool is_gap(int is);  // query by sample number
//@{
// Checks if data at time ttest is a gap or valid data.  
// This function is like the overloaded version with an int argument except 
// it uses a time instead of sample number for the query.
// @param ttest - time to be tested.
//@}
	bool is_gap(double ttest);  // query by time
//@{
// Checks if a given data segment has a gap.
// For efficiency it is often useful to ask if a whole segment of data is
// free of gaps.  Most time series algorithms cannot process through data
// gaps so normal practice would be to drop data with any gaps in a 
// requested time segment.
// @returns true if time segment has any data gaps
// @parm twin time window of data to test defined by a TimeWindow object
//@}
	bool is_gap(TimeWindow twin);
//@{
// Adds a gap to the gap definitions for this data object.
// Sometimes an algorithm detects or needs to create a gap (e.g. a mute,
// or a constructor).
// This function provides a common mechanism to define such a gap in the data.
//@}
	void add_gap(TimeWindow tw){gaps.insert(tw);};
//@{
// Force all data inside data gaps to zero.  
// This is a virtual function that makes sense only to a derived type since
// the contents of the data vector depend upon the nature of the data.  
// i.e. this function cannot be called on a BasicTimeSeries.
//@}
	virtual void zero_gaps()=0; // pure virtual function
	// inline function to return time of sample i
//@{
// Get the time of sample i.
// It is common to need to ask for the time of a given sample.
// This standardizes this common operation in an obvious way.
// @param i - sample number to compute time for.  
//@}
	double time(int i){return(t0+dt*static_cast<double>(i));};
	// inverse of time
//@{
// Inverse of time function.  That is,  it returns the integer position
// of a given time t within a time series.  The returned number is
// not tested for validity compared to the data range.  This is the
// callers responsibility as this is a common error condition that 
// should not require the overhead of an exception.
//@}
	int sample_number(double t){return(nint((t-t0)/dt));};
//@{
// Returns the end time (time associated with last data sample) 
// of this data object.
//@}
	double endtime(){return(t0+dt*static_cast<double>(ns-1));};
//@{
// Absolute to relative time conversion.  
// Sometimes we want to convert data from absolute time (epoch times)
// to a relative time standard.  Examples are conversions to travel
// time using an event origin time or shifting to an arrival time
// reference frame.  This operation simply switches the tref 
// variable and alters t0 by tshift.
//@param tshift - time shift applied to data before switching data to relative time mode.
//@}
	void ator(double tshift);
//@{
// Relative to absolute time conversion.
// Sometimes we want to convert data from relative time to 
// to an absolute time standard.  An example would be converting
// segy shot data to something that could be processed like earthquake
// data in a css3.0 database.
// This operation simply switches the tref 
// variable and alters t0 by tshift.
//@param tshift - time shift applied to data before switching data to absolute time mode.
// 
//@}
	void rtoa(double tshift);
protected:
	set<TimeWindow,TimeWindowCmp> gaps;
};
//@{
// Scalar time series data object.
// This data object extends BasicTimeSeries mainly by adding a vector of
// scalar data.  It uses a Metadata object to contain auxiliary parameters
// that aren't essential to define the data object, but which are necessary
// for some algorithms.  
//@}
class TimeSeries: public BasicTimeSeries , public Metadata
{
public:
//@{
// Actual data stored as an STL vector container.  
// Note the STL guarantees the data elements of vector container are 
// contiguous in memory like FORTRAN vectors.  As a result things
// like the BLAS can be used with data object by using a syntax
// like this: if d is a TimeSeries object, the address of the first sample of 
// the data is &(d.s[0]).  
//@}
	vector<double>s;
//@{
// Default constructor.  Initializes object data to zeros and sets the
// initial STL vector size to 0 length.
//@}
	TimeSeries();
//@{
// Similar to the default constructor but calls reserve to set aside
// memory for nsin elements in the vector container that holds sample data.
//@}
	TimeSeries(int nsin);
//@{
// Partial constructor for a TimeSeries object driven by a Metadata object.
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
// @throws MetadataError object is thrown if any of the metadata associated with the 
//           keywords noted above are not defined.  
// @throws SeisppError is thrown for read errors when load_data is true.  
//
// @param md - Metadata object to drive construction.
// @param load_data - if true tries to read data in an antelope style ala wfdisc but using
//                     attributes derived from the Metadata object (see above).
//@}
	TimeSeries(Metadata& md,bool load_data);
//@{
// Antelope database driven constructor.
// The basic model here is that this constructor builds a TimeSeries object
// from one row of a database table (normally wfdisc, but the intent is to 
// to be totally general).  
// @param db DatabaseHandle object that is assumed to point at one row of a database view.
// @param mdl A MetadataList contains a list of internal names that tells the 
//   constructor which attributes it will need to extract from the database and place in
//   the Metadata area of the object.  
// @param am This object is used to map internal names to an external namespace.  It is
//  normally created once at the early stage of a program's execution.  
//@}
	TimeSeries(DatabaseHandle& db, MetadataList& mdl, AttributeMap& am);
//@{
// Standard copy constructor.
//@}
	TimeSeries(const TimeSeries&);
//@{
// Standard assignment operator.
//@}
	TimeSeries& operator=(const TimeSeries&);
//@{
// Scans for defined gaps and sets the data to zero in time periods defined by gaps.
//@}
	void zero_gaps();
};

//@{
// Encapsulates spherical coordinates in a data structure.
// Spherical coordinates come up in a lot of contexts in Earth Science data
// processing.  Note actual coodinate system can depend on context.
// For whole Earth models it can define global coordinates, but in three component
// seismograms the normal convention of geographical coordinates is always assumed.
//@}
typedef struct SphericalCoordinate_{
//@{
// Radius from center.
//@}
        double radius;
//@{
// Zonal angle (from z) of spherical coordinates.  Units always assumed to be radians.
//@}
        double theta;
//@{
// Azimuthal angle (from x) of spherical coordinates.  Units always assumed to be radians.
//@}
        double phi;
} SphericalCoordinate;
/* A ThreeComponentSeismogram is viewed as a special collection of Time Series 
type data that is essentially a special version of a vector time series. 
It is "special" as the vector is 3D with real components.  One could produce a
similar inherited type for an n vector time series object.  

The structure of a vector time series allows the data to be stored in a matrix
*/
 

//@{
// Vector (three-component) seismogram data object.
// A three-component seismogram is a common concept in seismology. The concept  
// used here is that a three-component seismogram is a time series with a 3-vector
// as the data at each time step.  As a result the data are stored internally as
// a matrix with row defining the component number (C indexing 0,1,2) and 
// the column defining the time variable.
// The object inherits common concepts of a time series through the 
// BasicTimeSeries object.  Auxiliary parameters are defined for the object
// through inheritance of a Metadata object.
//@}
class ThreeComponentSeismogram : public BasicTimeSeries , public Metadata
{
public:
//@{
// Defines if the contents of this object are components of an orthogonal basis.
// Most raw 3c seismic data use orthogonal components, but this is not universal.
// Furthermore, some transformations (e.g. the free surface transformation operator)
// define transformations to basis sets that are not orthogonal.  Because
// detecting orthogonality from a transformation is a nontrivial thing 
// (rounding error is the complication) this is made a part of the object to 
// simplify a number of algorithms. 
//@}
	bool components_are_orthogonal;  
//@{
// Defines of the contents of the object are in Earth cardinal coordinates.
// Cardinal means the cardinal directions at a point on the earth.  That is,
// x1 is positive east, x2 is positive north, and x3 is positive up.
// Like the components_are_orthogonal variable the purpose of this variable is
// to simplify common tests for properties of a given data series.
//@}
	bool components_are_cardinal;  // true if x1=e, x2=n, x3=up
	// This is the transformation matrix applied relative to standard
//@{
// Transformation matrix. 
// This is a 3x3 transformation that defines how the data in this object is
// produced from cardinal coordinates.  That is, if u is the contents of this 
// object the data in cardinal directions can be produced by tmatrix^-1 * u.
//@}
	double tmatrix[3][3]; 
//@{
// Holds the actual data.  Matrix is 3xns.  Thus the rows are the component number
// and columns define time position.NEEDS A LINK TO DMATRIX DEFINTION
//@}
	dmatrix u;
//@{
// Default constructor.  Sets ns to zero and builds an empty matrix.
//@}
	ThreeComponentSeismogram();
//@{
// Simplest parameterized constructor. Initializes data and sets aside memory for
// matrix of size 3xnsamples.  The data matrix is not initialized.  
//@param nsamples number of samples expected for holding data.
//@}
	ThreeComponentSeismogram(int nsamples);

//@{
// Partial constructor for a ThreeComponentSeismogram object driven by a Metadata object.
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
// @throws MetadataError object is thrown if any of the metadata associated with the 
//           keywords noted above are not defined.  
// @throws SeisppError is thrown for read errors when load_data is true.  
//
// @param md - Metadata object to drive construction.
// @param load_data - if true tries to read data in an antelope style ala wfdisc but using
//                     attributes derived from the Metadata object (see above).
//@}
	ThreeComponentSeismogram(Metadata& md,bool load_data);
//@{
// Antelope database driven constructor.
// The basic model here is that this constructor builds a ThreeComponentSeismogram object
// from a database table.  This is complicated by a fundamental format issue.  Raw seismic
// data is traditionally stored in channel-oriented blocks.  In CSS3.0 wfdisc tables index
// one waveform segment per row.  Three component seismic data requires three channels of 
// data.  This constructor works in two fundamentally different ways.  First, in normal
// data mode it assumes the DatabaseHandle defines a group of rows in the database
// (formed in Antelope with dbgroup).  The groupings are assumed to be in blocks of 
// three channels. The constructor will throw an exception if the count of rows in the 
// group is anything but 3.  
//
// The second mode this constructor can operate in is triggered by a test for a group
// definition for the DatabaseHandle.  If the handle is not a group pointer the 
// Metadata constructor is called and the attribute "datatype" is checked.  If datatype
// is not "3c" and exception is thrown.  If it is "3c" it is assumed the data are stored 
// in a simple binary dump of the u matrix contents.  
//
// The following attributes must be present in MetadataList structure passed to the
// constructor or the routine wil throw a MetadataError exception:  time, endtime,
// nsamp, samprate, datatype, dir, dfile, and foff. If the datatype is not 3c the
// attributes hang and vang (normally from sitechan) must be defined.  
// 
// Finally irregular start and end times for data read in single channel mode will
// cause gaps to be defined at the beginning and/or end of the data.  
//
// @param db DatabaseHandle object that is assumed to point at one row of a database view.
// @param mdl A MetadataList contains a list of internal names that tells the 
//   constructor which attributes it will need to extract from the database and place in
//   the Metadata area of the object.  
// @param am This object is used to map internal names to an external namespace.  It is
//  normally created once at the early stage of a program's execution.  
//@}
	ThreeComponentSeismogram(DatabaseHandle& db, 
		MetadataList& mdl, AttributeMap& am);
//@{
// Standard copy constructor.
//@}

	ThreeComponentSeismogram(const ThreeComponentSeismogram&);
//@{
// Standard assignment operator.
//@}
	ThreeComponentSeismogram& operator 
		= (const ThreeComponentSeismogram&);
	// Default destructor is acceptable
	//~ThreeComponentSeismogram(); 
//@{
// Sets the data values in all defined gaps to zero.
//@}
	void zero_gaps();
//@{
// Apply inverse transformation matrix to return data to cardinal direction components.
// It is frequently necessary to make certain as set of three component data are oriented
// to the standard reference frame (EW, NS, Vertical).  This function does this.
// For efficiency it checks the components_are_cardinal variable and does nothing if 
// it is set true.  Otherwise, it applies the transformation and then sets this variable true.
//@}
	void rotate_to_standard() throw(SeisppError);
	// This overloaded pair do the same thing for a vector
	// specified as a unit vector nu or as spherical coordinate angles
//@{
// Rotate data using a P wave type coordinate definition.  
// In seismology the longitudinal motion direction of a P wave defines a direction 
// in space.  This method rotates the data into a coordinate system defined by a 
// direction passed through the argument.  The data are rotated such that x1 becomes 
// the transverse component, x2 becomes radial, and x3 becomes longitudinal.  In the 
// special case for a vector pointing in the x3 direction the data are not altered.
//
//@param sc defines final x3 direction (longitudinal) in a spherical coordinate structure.
//@}
	void rotate(SphericalCoordinate sc);

//@{
// Rotate data using a P wave type coordinate definition.  
// In seismology the longitudinal motion direction of a P wave defines a direction 
// in space.  This method rotates the data into a coordinate system defined by a 
// direction passed through the argument.  The data are rotated such that x1 becomes 
// the transverse component, x2 becomes radial, and x3 becomes longitudinal.  In the 
// special case for a vector pointing in the x3 direction the data are not altered.
//
//@param nu defines direction of x3 direction (longitudinal) as a unit vector with three components.
//@}
	void rotate(double nu[3]);
	// This applies a general transform with a 3x3 matrix.  
	// User should set components_are_orthogonal true if they
	// are so after this transformation.  The default assumes not
	// Note this is routine does NOT return to standard before
	// applying the transformation so this is accumulative.
//@{
// Applies an arbitrary transformation matrix to the data.
//
//@param a is a C style 3x3 matrix.
//@}
	void apply_transformation_matrix(double a[3][3]);
//@{
// Computes and applies the Kennett [1991] free surface transformation matrix.
// Kennett [1991] gives the form for a free surface transformation operator 
// that reduces to a nonorthogonal transformation matrix when the wavefield is
// not evanescent.  
//
//@param u slowness vector off the incident wavefield
//@param vp0 Surface P wave velocity
//@param vs0 Surface S wave velocity.
//@}
	void free_surface_transformation(SlownessVector u, double vp0, double vs0);
};
// Note for ensembles the lengths of each trace (3ctrace) and
// should be allowed to be variable.  The number of elements in
// the ensemble can be obtained using the size() function for the STL
// vector container.
//@{
// Object to contain a group (ensemble) of time series objects (seismograms).
// It is common in seismic data processing to have a group (ensemble) of
// seismograms that have some association and hence belong together to
// define a useful group.  This object is a general way to hold a group of
// time series data using an STL vector container to hold the members.  
// The ensemble has metadata associated with the group acquired by 
// inheritance, but which contructors have to deal with to load the
// right attributes into the ensemble metadata area.  
//@}
class TimeSeriesEnsemble : public Metadata
{
public:  
//@{
// Standard Template Library vector container holding the members of the ensemble.
// This is the main data area for the ensemble.  We use a vector container
// as it is common to want to use the indexing operator to ask for a member
// and it is common to want to sort the ensemble. 
//@}
	vector <TimeSeries> member;

//@{
// Default constructor.  Does little, but is not defaulted.  
//@}
	TimeSeriesEnsemble();
//@{
// Space allocating constructor.  Sets aside slots in the ensemble
// for ntsin members with a typical size of nsampin samples.
//@}
	TimeSeriesEnsemble(int ntsin, int nsampin);
//@{
// Space allocating constructor with metadata.  Sets aside slots in
// the ensemble for ntsin ensemble members with a typical size of nsampin
// samples.  Defines global metadata elements by mdl.
//@}
	TimeSeriesEnsemble(int ntsin, int nsampin, MetadataList& mdl);
//@{
// Construct an ensemble from a parameter file description.  This
// constructor is useful in combination with the pfstream library.
// In that context blocks of data can be parsed to produce the Pf_ensemble
// regular C data object.  See man pfstream(3).
//@}
	TimeSeriesEnsemble(Pf_ensemble *pfe);
//@{
// Standard copy constructor. 
//@}
	TimeSeriesEnsemble(TimeSeriesEnsemble& tseold);
//@{
// Partial copy constructor. 
// Sometimes it is useful to copy only the Metadata from an ensemble 
// and leave slots open for the container to hold the data objects for
// the ensemble.  
// @param md is the metadata object to copy to the metadata area for the ensemble.
// @param nmembers is the number of slots to reserve in the new ensemble vector.
//@}
	TimeSeriesEnsemble(Metadata& md,int nmembers);
//@{
// Standard assignment operator.
//@}
	TimeSeriesEnsemble& operator=(const TimeSeriesEnsemble& tseold);
	friend void set_global_metadata_list(TimeSeriesEnsemble& te,MetadataList&);
private:
	// This list contains metadata copied as global to the ensemble
	MetadataList mdlist;
};

//@{
// Object to contain a group (ensemble) of three component seismogram objects. 
// It is common in seismic data processing to have a group (ensemble) of
// seismograms that have some association and hence belong together to
// define a useful group.  This object is a general way to hold a group of
// three component seismogram data stored as as ThreeComponentSeismogram
// objects.  This object uses an STL vector container to hold the members.  
// The ensemble has metadata associated with the group acquired by 
// inheritance, but which contructors have to deal with to load the
// right attributes into the ensemble metadata area.  
//@}
class ThreeComponentEnsemble : public Metadata
{
public:
//@{
// Standard Template Library vector container holding the members of the ensemble.
// This is the main data area for the ensemble.  We use a vector container
// as it is common to want to use the indexing operator to ask for a member
// and it is common to want to sort the ensemble. 
//@}
	vector <ThreeComponentSeismogram> member;
//@{
// Default constructor.  Does little, but is not defaulted.  
//@}
	ThreeComponentEnsemble();
//@{
// Space allocating constructor.  Sets aside slots in the ensemble
// for ntsin members with a typical size of nsampin samples.
//@}
	ThreeComponentEnsemble(int nsta, int nsamp);
//@{
// Space allocating constructor with metadata.  Sets aside slots in
// the ensemble for ntsin ensemble members with a typical size of nsampin
// samples.  Defines global metadata elements by mdl.
//@}
	ThreeComponentEnsemble(int nsta, int nsamp,
				MetadataList& mdl);
//@{
// Database-driven constructor.  
// Seismic data are often stored today using a database to index the raw
// waveforms.  The database contains attributes loaded here as metadata.
// Attributes that are global to the ensemble are loaded driven by the
// ensemble_mdl list while single 3c seismogram metadata is controlled
// by station_mdl.  Note in both cases the list of names is the internal
// name not external database names.  The AttributeMap object 
// defines the mapping from the internal name space to database
// names.

// @param db is a generalized handle to a database.  In the current
// implementation it is assumed to be a DatascopeHandle.
// @param station_mdl is a list of metadata attributes to be loaded 
//   into each 3c seismogram's metadata area.  
// @param ensemble_mdl is a list of metadata attributes to be loaded
//   into the global metadata area for the whole ensmeble.
// @param am is the mapping operator used to translate internal names
//   to database attribute names (e.g. wfdisc.time).
//@}
	ThreeComponentEnsemble(DatabaseHandle& db,
		MetadataList& station_mdl,
		MetadataList& ensemble_mdl,
		 AttributeMap& am);
//@{
// Standard copy constructor.
//@}
	ThreeComponentEnsemble(ThreeComponentEnsemble& tseold);
//@{
// Partial copy constructor. 
// Sometimes it is useful to copy only the Metadata from an ensemble 
// and leave slots open for the container to hold the data objects for
// the ensemble.  
// @param md is the metadata object to copy to the metadata area for the ensemble.
// @param nmembers is the number of slots to reserve in the new ensemble vector.
//@}
	ThreeComponentEnsemble(Metadata& md,int nmembers);
//@{
// Standard assignment operator.
//@}
	ThreeComponentEnsemble& operator=(const ThreeComponentEnsemble& tseold);
	friend void set_global_metadata_list(ThreeComponentEnsemble&,
			MetadataList&);
private:
	MetadataList mdlist;
};
//
//  Mute definitions
//
//@{
// Defines a top mute zone. 
// Top Mutes are a common concept in reflection processing.  They are used to 
// zero out sections of data with an optional taper of a specified width.  
// This object encapsulates the idea of a top mute in a simplified package.
//@}
class TopMute
{
public:
//@{
// Time of end of zero mute region.  The start of the zero region of a top
// mute is always assumed to be the start of data.  Data will be zeroed from 
// start to this time.  
//@}
	double t0e;
//@{
// End of taper region.  This top mute object defines a linear taper from
// 0 to 1 between t0e and t1.  
//@}
        double 	t1;  
//@{
// Time reference type.  Defined by an enum in seispp as absolute or relative.  
// If relative time is used the t0e and t1 times are assumed to be computed relative
// to the first sample of data.  If absolute the actual value of t0 for the data file
// is referenced and times are presumed to be relative to that standard.
//@}
	TimeReferenceType reftype;   
	//* Default constructor */
	TopMute(){t0e=1.0; t1=2.0; reftype=relative;};
//@{
// Parameter file driven constructor.  
// Looks for three keyword strings to set the three data parameters
// that define the object.  They are:  Zero_End_Time, End_Time, and TimeReferenceType 
// which reference t0, t1e, and reftype respectively.  
//
// @param pf Antelope parameter file pf object.
// @param tag is a string that defines an &Tbl{} enscapsulation of the parameters
//   parsed for the mute definition. The nesting of an &Tbl{ } with the parameters
//   between the curly brackets allows the same keywords to be used in multiple
//   constructors for different mute definitions.
//@}
	TopMute(Pf *pf,string tag);
};

//@{
// Defines a source position and a set of useful methods with which a source
// position can be associated.
// Seismic processing is commonly revolved around geometry defined by source and
// receiver positions.  This object encapsulates information about a source and 
// provides a set of methods useful for waveform processing that depends on
// source information.  
//@}
class Hypocenter
{
public:
	//@{
	// Latitude of the source in radians.
	//@}
	double lat;
	//@{
	// Longitude of the source in radians.
	//@}
	double lon;
	//@{
	// Depth below sea level of the source in kilometers.
	//@}
	double z;
	//@{
	// Origin time of the source in seconds.  If absolute time is being used
	// this is an epoch time.
	//@}
	double time;
	//* Default constructor.  Defines travel time interface, but not source coordinates.*/
	Hypocenter(){method=string("tttaup"); model=string("iasp91");};  // default
	//@{
	// Metadata object driven constructor.  Looks for these keywords:
	//  origin.lat, origin.lon, origin.depth, and origin.time.  
	// Note it is assumed the latitude and longitude values contained in 
	// the parent metadata are given already in radians.  
	//@}
	Hypocenter(Metadata& );
	//* Standard copy constructor */
	Hypocenter(const Hypocenter&);
	//* Standard assignment operator.*/
	Hypocenter& operator=(const Hypocenter&);
	//* Compute distance from source to surface position (lat0,lon0).*/
	double distance(double lat0, double lon0);
	//@{
	// Compute event to station azimuth.  
	// Event to source azimuth is defined as the azimuth of the great 
	// circle path joining event and station at the position of the 
	// source and looking toward the station.
	// @param lat0 latitude of the station
	// @param lat0 longitude of the station
	//@}
	double esaz(double lat0, double lon0);
	//@{
	// Compute station to event azimuth.
	// Station to event azimuth is defined as the azimuth of the
	// great circle path joining event and station but evaluated
	// at the position of the station and looking back toward 
	// the source.  
	// @param lat0 latitude of the station
	// @param lat0 longitude of the station
	//@}
	double seaz(double lat0, double lon0);
	//@{
	// Compute the P wave arrival time using the currently defined travel time
	// calculator.  
	// @throws SeisppError object when travel time calculator fails for any reason
	//
	// @param lat0 latitude of the station
	// @param lat0 longitude of the station
	// @param elev elevation of the station relative to sea level
	//@}
	double ptime(double lat0, double lon0, double elev)
		throw(SeisppError);
	//@{
	// Compute the P wave slowness vector using the currently defined travel time
	// calculator.  
	// @throws SeisppError object when travel time calculator fails for any reason
	//
	// @param lat0 latitude of the station
	// @param lat0 longitude of the station
	// @param elev elevation of the station relative to sea level
	//@}
	SlownessVector pslow(double lat0, double lon0, double elev)
		throw(SeisppError);
	//@{
	// Compute the predicted arrival time of a requested phase
	// using the currently defined travel time
	// calculator.  
	// @throws SeisppError object when travel time calculator fails for any reason
	//
	// @param lat0 latitude of the station
	// @param lat0 longitude of the station
	// @param elev elevation of the station relative to sea level
	// @param phase name of phase for which arrival time is requested.
	//@}
	double phasetime(double lat0, double lon0, double elev, string phase)
		throw(SeisppError);
	//@{
	// Compute the predicted slowness vector for a 
	// specified seismic phase using the currently defined travel time
	// calculator.  
	// @throws SeisppError object when travel time calculator fails for any reason
	//
	// @param lat0 latitude of the station
	// @param lat0 longitude of the station
	// @param elev elevation of the station relative to sea level
	// @param phase name of phase for which slowness estimate is requested.
	//@}
	SlownessVector phaseslow(double lat0, double lon0, double elev, 
			string phase) throw(SeisppError);
	//@{
	// Initialize the travel time calculator or change travel time model or method.
	// This function can be used to alter the model or method used to 
	// compute travel times and/or slowness vectors with this
	// object.  The object uses the Antelope ttime library that allows
	// runtime changes in the method and Earth model.  This 
	// function is used to invoke that type of change in the object's \
	// behaviour.
	//
	// @param meth set the travel time method to meth
	// @param mod set the earth model to that with the name mod.
	//@}
	void tt_setup(string meth, string mod); // change default method:model
private:
	string method;
	string model;
};

//@{
// Error object thrown by 1d velocity model objects.
//@}
class VelocityModel_1d_Error
{
public:
	//* Contains error message. */
	string message;
	//* Dump error message to stderr. */
	virtual void log_error(){cerr<<"VelocityModel_1d object error"<<endl;};
};

//@{
// Error object thrown by 1d velocity model objects.
// This version is thrown by database driven constructor.
//@}
class VelocityModel_1d_Dberror : public VelocityModel_1d_Error
{
public:
	//* Holds name of velocity model that generated error.*/
	string name;
	//* Basic constructor.*/
	VelocityModel_1d_Dberror(string modname,string mess){
		name=modname;  message=mess;};
	//* Dump error message to stderr. */
	virtual void log_error(){
		cerr<<"Database error accessing velocity model mod1d table "<<name<<endl;
		cerr<<message;
	};
};
//@{
// Error object thrown by 1d velocity model object for i/o error.
//@}
class VelocityModel_1d_IOerror : public VelocityModel_1d_Error
{
public:
	//* Holds i/o error message.*/
	string ioerr;
	//* Basic constructor */
	VelocityModel_1d_IOerror(string mess, string ioe){
		message = mess; ioerr = ioe;};
	//* Dump error message to stderr.*/
	virtual void log_error(){
		cerr<<"Velocity i/o error" << endl
			<< message << endl
			<< ioerr << endl;
	};
};

//@{
// Object to encapsulate concept of a 1d (layered Earth) velocity model.
// Many seismology algorithms utilized layered earth velocity models. 
// This object provides methods to ask for the velocity at any given depth
// without concerns about anything about how the model is stored.  
// Thus it can hold constant velocity layers or continuous models 
// specified on irregular depth grids all through the same interface.
//@}
class VelocityModel_1d
{
public:
	//* Number of points used to define the model.*/
	int nlayers;
	//@{
	// The model is stored as a trip of depth (z), velocity(v), and 
	// gradient (grad) between points.  These are stored in three 
	// parallel vectors of length nlayers.  The nlayers data is not
	// essential as it could be obtained by calling the size() method
	// on any of these vectors, but this was considered a simpler
	// interface.  
	// This vector holds the depths of each point that specifies the model.
	//@}
	vector<double> z;
	//@{
	// The model is stored as a trip of depth (z), velocity(v), and 
	// gradient (grad) between points.  These are stored in three 
	// parallel vectors of length nlayers.  The nlayers data is not
	// essential as it could be obtained by calling the size() method
	// on any of these vectors, but this was considered a simpler
	// interface.  
	// This vector holds the velocity of each point that specifies the model.
	//@}
	vector<double> v;
	//@{
	// The model is stored as a trip of depth (z), velocity(v), and 
	// gradient (grad) between points.  These are stored in three 
	// parallel vectors of length nlayers.  The nlayers data is not
	// essential as it could be obtained by calling the size() method
	// on any of these vectors, but this was considered a simpler
	// interface.  
	// This vector holds the gradient from point i to point i+1.
	// For last point it is the gradient to use to extrapolate downward
	// below the depth of the last point.
	//@}
	vector<double> grad;
	//* Default constructor.  Initializes all to zero.*/
	VelocityModel_1d(){z.reserve(0);v.reserve(0);grad.reserve(0);nlayers=0;};
	//@{
	// Allocating constructor.  Sets aside space for n layers, but leaves
	// contents empty.  
	//@}
	VelocityModel_1d(int n){nlayers=n;
                z.reserve(nlayers);
                v.reserve(nlayers);
                grad.reserve(nlayers);};
	//@{
	// Database driven constructor.
	// Builds a velocity model from database tables.
	// Depends on a models database as used in tt1dcvl travel time calculator.
	//
	// @throws SeisppError if there are problems constructing the object
	//    from the database.
	//
	// @param db Datascope database pointer.  
	// @param name unique name to identify requested Earth model.
	// @param property defines property attribute of models database table.  
	//    Normall Pvelocity or Svelocity.
	//@}
	
	VelocityModel_1d(Dbptr db,string name, string property)
		throw(VelocityModel_1d_Dberror);
	//@{
	// Ascii file constructor.  
	// Reads a velocity model from file fname using a simple ascii format
	// file.  Data are assumed in free format lines of the form
	// (z,vp,vs) where z is depth, vp is P velocity at z, and vs is
	// S velocity at z.  
	//
	// @throws SeisppError if is an i/o problem of any kind.
	//
	// @param fname is file name to be read.
	// @param form is either rbh or plain.  Anything else will cause
	//   an exception to be thrown.  plain is just a set of lines as 
	//   described above.  rbh format is from Bob Herrmann's velocity
	//   model format.  The main difference is that his format has 7 
	//   header lines before the velocity model parameters begin.
	//@}
	VelocityModel_1d(string fname, string form, string property)
		throw(VelocityModel_1d_IOerror);
	//@{
	// Return interpolated velocity at depth zin.
	// If z is above the first point the first point velocity is returned.
	// If z is below the last point the value is computed from the last
	// point velocity and the last point gradient.
	//@}
	double getv(double zin);
};


//@{
// Applies a top mute to a TimeSeries object.
//@}
void ApplyTopMute(TimeSeries &ts,TopMute& mute);
//@{
// Applies a top mute to a ThreeComponentSeismogram object.
//@}
void ApplyTopMute(ThreeComponentSeismogram& ts,TopMute& mute);
//@{
// Applies a single top mute definition to all members of a TimeSeriesEnsemble.
//@}
void ApplyTopMute(TimeSeriesEnsemble& t, TopMute& mute);
//@{
// Applies a single top mute definition to all members of a ThreeComponentEnsemble.
//@}
void ApplyTopMute(ThreeComponentEnsemble &t3c, TopMute& mute);
//@{
// Applies a time shift called a geometric static to TimeSeries object ts 
// using velocity vel and elevation elev.  This is a simple elev/vel correction.
//@}
void ApplyGeometricStatic(TimeSeries *ts, double vel, double elev);
//@{
// Applies a time shift called a geometric static to TimeSeries object ts 
// using velocity and elevation extracted from the metadata area of ts.  
// The function requires that attributes "elevation" and "surface_velocity"
// be defined in the object.  If these attributes are not defined the 
// data are not altered but a diagnostic is issued to stderr.
//@}
void ApplyGeometricStatic(TimeSeries *ts);
//@{
// Pfstream method for getting a time series object from an input stream.
// Useful only in a pfstream environment which is currently not well developed
// for this application.
//@}
TimeSeries* GetNextTimeSeries(Pfstream_handle *pfh);
//@{
// Companion to GetNextTimeSeries.  
// Useful only in a pfstream environment which is currently not well developed
// for this application.
//@}
TimeSeries *LoadTimeSeriesUsingPf(Pf *pf);
//@{
// Load an ensemble through a pfstream.
//@}
TimeSeriesEnsemble *GetNextEnsemble(Pfstream_handle *pfh,
	 char *tag,MetadataList& mdlist) throw(SeisppError);
//@{
// Load a 3c ensemble through a pfstream.
//@}
ThreeComponentEnsemble *GetNext3cEnsemble(Pfstream_handle *pfh,
	 char *tag,MetadataList& mdlist) throw(SeisppError);
//@{
// Save a 3c seismgram using a pfstream output.
//@}
void PfstreamSave3cseis(ThreeComponentSeismogram *seis,string tag,
	string dir, string dfile, Pfstream_handle *pfh) throw(SeisppError);
//@{
//  Used by TimeSeries constructors to set data gaps using Antelope methods.
//@}
void SetGaps(TimeSeries&,Trsample *,int, string)
		throw(SeisppError);
//@{
// Returns a SphericalCoordinate data structure equivalent to one
// define dby a unit vector nu.
//@}
SphericalCoordinate UnitVectorToSpherical(double nu[3]);
//@{
// Returns a unit vector (vector of 3 doubles) equivalent to
// direction defined in sphereical coordinates.
//@}
double *SphericalToUnitVector(SphericalCoordinate& sc);
//@{
// Return direction of particle motion for a P wave with 
// slowness (ux,uy) at a surface with P velocity vp0 and 
// S velocity vs0.
//@}
SphericalCoordinate PMHalfspaceModel(double vp0,double vs0,
	double ux,double uy);
//@{
// Extract one component from a ThreeComponentSeismogram and 
// create a TimeSeries object from it.  
//
// @param tcs is the ThreeComponentSeismogram to convert.
// @param component is the component to extract (0, 1, or 2)
// @param mdl list of metadata to copy to output from input object.
//@}
TimeSeries *ExtractComponent(ThreeComponentSeismogram& tcs,int component,
        MetadataList& mdl);
//@{
// Extract one component from a ThreeComponentSeismogram and 
// create a TimeSeries object from it.  
// Similar to overloaded function of same name, but all metadata from
// parent is copied to the output.
//
// @param tcs is the ThreeComponentSeismogram to convert.
// @param component is the component to extract (0, 1, or 2)
//@}
TimeSeries *ExtractComponent(ThreeComponentSeismogram& tcs,int component);
//@{
// Returns a new seismogram in an arrival time reference.
// An arrival time reference means that the time is set to relative and 
// zero is defined as an arrival time extracted from the metadata area of
// the object.
//
// @throws SeisppError for errors in extracting required information from metadata area.
//
// @param din  is input seismogram
// @param key is the metadata key used to find the arrival time to use as a reference.
// @param tw is a TimeWindow object that defines the window of data to extract around
//    the desired arrival time.
//@}
ThreeComponentSeismogram& Arrival_Time_Reference(ThreeComponentSeismogram& din,
	string key, TimeWindow tw);
//@{
// Returns a gather of ThreeComponentSeismograms in an arrival time reference fram.
// An arrival time refernce means that the time is set to relative and 
// zero is defined as an arrival time extracted from the metadata area of
// each member object.
//
// @throws SeisppError for errors in extracting required information from metadata area.
//
// @param din  is input gather
// @param key is the metadata key used to find the arrival time to use as a reference.
// @param tw is a TimeWindow object that defines the window of data to extract around
//    the desired arrival time.
//@}
ThreeComponentEnsemble& Arrival_Time_Reference(ThreeComponentEnsemble& din,
	string key, TimeWindow tw);


//@{
// Bombproof low level write routine for a vector of doubles.  
// Uses fwrite to write vector x to the file dir+"/"+dfile.
//
// @throws SeisppError object if there are problems saving data to requested file.
// @param x vector of data to be saved.
// @param n length of vector x
// @param dir directory to place file.  If empty assumes current directory.
// @param dfile file name 
//@}
long int vector_fwrite(double *x,int n, string dir, string dfile) throw(SeisppError);
//@{
// Bombproof low level write routine for a vector of doubles.  
// Uses fwrite to write vector x to the file fname
//
// @throws SeisppError object if there are problems saving data to requested file.
// @param x vector of data to be saved.
// @param n length of vector x
// @param fname file name 
//@}
long int vector_fwrite(double *x,int n, string fname) throw(SeisppError);
//@{
// Bombproof low level write routine for a vector of floats.  
// Uses fwrite to write vector x to the file dir+"/"+dfile.
//
// @throws SeisppError object if there are problems saving data to requested file.
// @param x vector of data to be saved.
// @param n length of vector x
// @param dir directory to place file.  If empty assumes current directory.
// @param dfile file name 
//@}
long int vector_fwrite(float *x,int n, string dir, string dfile) throw(SeisppError);
//@{
// Bombproof low level write routine for a vector of floats.  
// Uses fwrite to write vector x to the file fname
//
// @throws SeisppError object if there are problems saving data to requested file.
// @param x vector of data to be saved.
// @param n length of vector x
// @param fname file name 
//@}
long int vector_fwrite(float *x,int n, string fname) throw(SeisppError);
//@{
// Save the data in a TimeSeries object to a database.
// This function works only with an Antelope (Datascope) database but the
// design is aimed to be schema independent.  That is, most raw 
// earthquake seismology data is indexed with a table defined in css3.0
// called wfdisc.  This function will work with a css3.0 wfdisc, but it
// will work with any other table as well provided you set up the
// interface correctly.  This is done through the MetadataList object
// which tells the function what attributes are to be saved to the
// output database along with the time series data.  
//
// A TimeSeries object contains a Metadata object it acquires by 
// inheritance.  The Metadata area is assumed to contain attributes
// listed in the MetadataList object passed to this function.  The
// basic algorithm is that the list of metadata in mdl are processed in order.
// They are translated to the database namespace using the AttributeMap
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
// Note also that if the "live" boolean in the object is set false
// this function silently returns immediately doing nothing.
//
// @throws SeisppError object if there are any problems saving the data or 
//    writing attributes into the database.
//
// @returns -1 if live is false, record number of added row otherwise
//
// @param ts is the TimeSeries object to be saved.
// @param db is a Datascope database pointer.  It need only point at a valid
//    open database.
// @param table is the name of the table to index this time series data
//   (e.g. "wfdisc").
// @param md  is the list of metadata to be dumped to the database as described above.
// @param am is a mapping operator that defines how internal names are to be mapped
//    to database attribute names and tables.  
//@}
int dbsave(TimeSeries& ts,Dbptr db,string table, MetadataList& md, AttributeMap& am)
		throw(SeisppError);
//@{
// Save the data in a ThreeComponentSeismogram object to a database.
// This function works only with an Antelope (Datascope) database but the
// design is aimed to be schema independent.  That is, most raw 
// earthquake seismology data is indexed with a table defined in css3.0
// called wfdisc.  This function will work with a css3.0 wfdisc, but it
// will work with any other table as well provided you set up the
// interface correctly.  This is done through the MetadataList object
// which tells the function what attributes are to be saved to the
// output database along with the time series data.  
//
// A ThreeComponentSeismogram object contains a Metadata object it acquires by 
// inheritance.  The Metadata area is assumed to contain attributes
// listed in the MetadataList object passed to this function.  The
// basic algorithm is that the list of metadata in mdl are processed in order.
// They are translated to the database namespace using the AttributeMap
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
// Note also that if the "live" boolean in the object is set false
// this function silently returns immediately doing nothing.
//
// This function differs in a significant way from an overloaded function
// with the same name.  The other has a "chanmap" argument to tell the 
// function how to split up the 3 components into channel codes.  This
// function takes a very different approach and saves data by dumping
// the internal 3xns matrix as the basic output data series. As a result
// this function will write ONE AND ONLY ONE DATABASE ROW PER OBJECT.
// This means somewhat by definition that the output table CANNOT be
// wfdisc if this function is called.  Consequently, this routine will
// throw an exception and do nothing if table=="wfdisc".
//
// @throws SeisppError object if there are any problems saving the data or 
//    writing attributes into the database.
//
// @returns -1 if live is false, record number of added row otherwise
//
// @param ts is the TimeSeries object to be saved.
// @param db is a Datascope database pointer.  It need only point at a valid
//    open database.
// @param table is the name of the table to index this time series data
//   (e.g. "wfdisc").
// @param md  is the list of metadata to be dumped to the database as described above.
// @param am is a mapping operator that defines how internal names are to be mapped
//    to database attribute names and tables.  
//@}
int dbsave(ThreeComponentSeismogram& ts,Dbptr db,string table, 
	MetadataList& md, AttributeMap& am);
//@{
// Save the data in a ThreeComponentSeismogram object to a database.
// This function works only with an Antelope (Datascope) database but the
// design is aimed to be schema independent.  That is, most raw 
// earthquake seismology data is indexed with a table defined in css3.0
// called wfdisc.  This function will work with a css3.0 wfdisc, but it
// will work with any other table as well provided you set up the
// interface correctly.  This is done through the MetadataList object
// which tells the function what attributes are to be saved to the
// output database along with the time series data.  
//
// A ThreeComponentSeismogram object contains a Metadata object it acquires by 
// inheritance.  The Metadata area is assumed to contain attributes
// listed in the MetadataList object passed to this function.  The
// basic algorithm is that the list of metadata in mdl are processed in order.
// They are translated to the database namespace using the AttributeMap
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
// Note also that if the "live" boolean in the object is set false
// this function silently returns immediately doing nothing.
//
// The chanmap and output_to_standard variables control how the 
// data are saved externally.  If output_as_standard is set true
// (highly recommended in general)  the data are restored (if necessary)
// to standard 3c data geometry (ew,ns,z) before being written to 
// output.  In that case vang and hang are set accordingly in 
// case the output algorithm requires these to be stored.  
// The components are then extraced from the 3c object one by 
// one and written in three successive database rows with the
// channel code ("chan" attribute in css3.0) derived from the
// chanmap array (chanmap[0]=channel name for component 0,
// chanmap[1]=component 1, and chanmap[2]=component 2).
//
// @throws SeisppError object if there are any problems saving the data or 
//    writing attributes into the database.
//
// @returns -1 if live is false, record number of added row otherwise
//
// @param ts is the TimeSeries object to be saved.
// @param db is a Datascope database pointer.  It need only point at a valid
//    open database.
// @param table is the name of the table to index this time series data
//   (e.g. "wfdisc").
// @param md  is the list of metadata to be dumped to the database as described above.
// @param am is a mapping operator that defines how internal names are to be mapped
//    to database attribute names and tables.  
// @param chanmap is a set of channel names to map each component to channel code (see above)
// @param output_as_standard when true forces data to be converted to ew,ns, z system
//@}
int dbsave(ThreeComponentSeismogram& ts,Dbptr db,
	string table, MetadataList& md, 
	AttributeMap& am, vector<string>chanmap,bool output_as_standard);
}
#endif
