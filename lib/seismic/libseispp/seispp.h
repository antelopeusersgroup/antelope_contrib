#ifndef _SEISPP_H_
#define _SEISPP_H_
#include <vector>
#include <set>
#ifdef sun
#include <sunmath.h>
#endif
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
// Seismic C++ Library (SEISPP).
// This is a collection of C++ objects used for processing seismic data.  
// Objects define some common seismological concepts including time series
// data, three component seismograms, ensembles of seismograms, velocity
// models, hypocenters, slowness vectors, and others.  
// The library has a strong dependence on Antelope in implementation, but
// the API design was intended to be more general.  
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
class seispp_error
{
public:
//@{
// Holds error message that can be printed with log_error method.
//@}
	string message;
//@{
// Default constructor built inline.
//@}
	seispp_error(){message="seispp library error\n";};
//@{
// Copy constructor.
//@}
	seispp_error(const string mess){message=mess;};
//@{
// Sends error message thrown by seispp library functions to standard error.
//@}
	virtual void log_error(){cerr << "seispp error: "<<message<<endl;};
};

//@{
// Defines severity code for errors thrown by database routines.
//@}
enum error_severity {fault, fatal, complain, notify, log, unknown};
//@{
// Error object thrown by routines accessing an Antelope database.  
// This object normally leads to errors written by Antelope elog functions
// and extra messages attached to the error object.
//@}
class seispp_dberror : public seispp_error
{
public:
//@{
// Antelope (Datascope) database pointer when error occurred.
//@}
        Dbptr db;
//@{
// Error severity level code.  
//@}
	error_severity error_type;
//@{
// Standard constructor for this object.
//@}
        seispp_dberror(const string mess,Dbptr dbi);
//@{
// Copy constructor.
//@}
	seispp_dberror(const string mess, 
		Dbptr dbi, error_severity et);
//@{ 
// Sends error message thrown by seispp library functions to standard error.
// This version writes errors from elog functions posted by Antelope libraries.
//@}

	void log_error();
};
//@{
// Special error object thrown by SAC file reader.
//@}

class SAC_data_error : public seispp_error
{
public:
//@{
// Standard constructor for this object.
//@}
	SAC_data_error(const string mess){message=mess;};
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

class Slowness_vector
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
	Slowness_vector();
//@{
// Copy constructor.
//@}
	Slowness_vector(const Slowness_vector&);
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
class Rectangular_Slowness_Grid
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
	Rectangular_Slowness_Grid();  // generic default is defined
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
	Rectangular_Slowness_Grid(string nm, double uxl, double uxh,
		double du1,double du2,int n1, int n2);
//@{
// Parameter file driven constructor.
// @param pf - Antelope pf pointer normally produced by earlier call to pfread
// @param tag - name to search in pf to describe this grid object.`
//              The parameters to describe the object are assumed encased in an 
//              &Arr{ } construct with this tag.  This allows multiple grids to 
//              be defined in a single parameter file with different tags. 
//@}
	Rectangular_Slowness_Grid(Pf *pf,string tag);
//@{
// Standard copy constructor.
//@}
	Rectangular_Slowness_Grid(const  Rectangular_Slowness_Grid&);
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
// @throws seispp_error object if i and j are outside range.
//@}
	Slowness_vector slow(int i, int j);
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
class Time_Window
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
	Time_Window(){start=0.0;end=1.0e99;};
//@{
// Parameterized constructor.
//@param ts - start time
//@param te - end time 
//@}
	Time_Window(double ts,double te){start=ts;end=te;};
};
//@{
// A time window with a functional form overlaid.
// This is essential an object definition of a window function
// for a time window defined by a Time_Window object.
//@}
class Time_Variable_Weight : public Time_Window
{
public:
//@{
// Set true if this defines a data gap and data within it should be ignored.`
//@}
	bool gap; // alternate way to set a gap
//@{
// Default constructor.
//@}
	Time_Variable_Weight(){w0=1.0;wgrad=0.0;};
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
// the elements of the set.  It makes Time_Windows indexed by
// intervals similar to thw way Datascope uses time:endtime
// Be aware, however, that for the same reason as datascope overlapping 
// time windows will cause ambiguity in indexing times by this
// method.
*/
//@{
// Function object used for weak comparison to order Time_Window objects.
// Time_Window objects are used, among other things, to define real
// or processed induced data gaps.
// The set container requires a weak ordering function like to correctly
// determine if a time is inside a particular time window.
//@}
class Time_Window_Cmp
{
public:
	bool operator()(const Time_Window ti1,const Time_Window ti2) const
	{return(ti1.end<ti2.start);};
};


//@{
// Type of time standard for time series data.
// Time series data have two common standards.  Absolute time means the
// time is an epoch time.  Relative means time is some other arbitrary
// reference.  An example is an arrival time reference frame where all
// data are set with time zero defined by a set of arrival time picks.  
//@}
enum Time_Reference_Type {absolute,relative};

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
class Basic_Time_Series
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
	Time_Reference_Type tref;
//@{
// Default constructor. Does essentially nothing since a Basic_Time_Series
// object has no data.  Does initialize data to avoid run time checkers 
// bitching about unitialized data, but values are meaningless when this
// constructor is called.
//@}
	Basic_Time_Series::Basic_Time_Series();
//@{
// Standard copy constructor.
//@}
	Basic_Time_Series::Basic_Time_Series(const Basic_Time_Series&);
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
// Adds a gap to the gap definitions for this data object.
// Sometimes an algorithm detects or needs to create a gap (e.g. a mute,
// or a constructor).
// This function provides a common mechanism to define such a gap in the data.
//@}
	void add_gap(Time_Window tw){gaps.insert(tw);};
//@{
// Force all data inside data gaps to zero.  
// This is a virtual function that makes sense only to a derived type since
// the contents of the data vector depend upon the nature of the data.  
// i.e. this function cannot be called on a Basic_Time_Series.
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
	set<Time_Window,Time_Window_Cmp> gaps;
};
//@{
// Scalar time series data object.
// This data object extends Basic_Time_Series mainly by adding a vector of
// scalar data.  It uses a Metadata object to contain auxiliary parameters
// that aren't essential to define the data object, but which are necessary
// for some algorithms.  
//@}
class Time_Series: public Basic_Time_Series , public Metadata
{
public:
//@{
// Actual data stored as an STL vector container.  
// Note the STL guarantees the data elements of vector container are 
// contiguous in memory like FORTRAN vectors.  As a result things
// like the BLAS can be used with data object by using a syntax
// like this: if d is a Time_Series object, the address of the first sample of 
// the data is &(d.s[0]).  
//@}
	vector<double>s;
//@{
// Default constructor.  Initializes object data to zeros and sets the
// initial STL vector size to 0 length.
//@}
	Time_Series();
//@{
// Similar to the default constructor but calls reserve to set aside
// memory for nsin elements in the vector container that holds sample data.
//@}
	Time_Series(int nsin);
//@{
// Partial constructor for a Time_Series object driven by a Metadata object.
// This is essentially uses the Metadata object as a way to make a parameterized
// constructor with a potentially large and variable number of parameters. 
// The following parameters must exist in the Metadata object or the constructor
// will throw an exception:  samprate, time, and nsamp.  If the load_data
// boolean is true the function will attempt to read data using an additional
// set of keywords that must be in the metadata:  Time_Reference_Type, datatype,
// dir, dfile, and foff.  Time_Reference_Type is a string that must be either "relative"
// or "absolute".  datatype, dir, dfile, and foff are exactly as in a wfdisc record.
// An important current limitation is that only host 4 byte float datatype (t4 or u4)
// are allowed by this constructor.  If datatype is anything else the constructor will
// throw an exception.  
//
// @throws Metadata_error object is thrown if any of the metadata associated with the 
//           keywords noted above are not defined.  
// @throws seispp_error is thrown for read errors when load_data is true.  
//
// @param md - Metadata object to drive construction.
// @param load_data - if true tries to read data in an antelope style ala wfdisc but using
//                     attributes derived from the Metadata object (see above).
//@}
	Time_Series(Metadata& md,bool load_data);
//@{
// Antelope database driven constructor.
// The basic model here is that this constructor builds a Time_Series object
// from one row of a database table (normally wfdisc, but the intent is to 
// to be totally general).  
// @param db Database_Handle object that is assumed to point at one row of a database view.
// @param mdl A Metadata_list contains a list of internal names that tells the 
//   constructor which attributes it will need to extract from the database and place in
//   the Metadata area of the object.  
// @param am This object is used to map internal names to an external namespace.  It is
//  normally created once at the early stage of a program's execution.  
//@}
	Time_Series(Database_Handle& db, Metadata_list& mdl, Attribute_Map& am);
//@{
// Standard copy constructor.
//@}
	Time_Series(const Time_Series&);
//@{
// Standard assignment operator.
//@}
	Time_Series& operator=(const Time_Series&);
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
typedef struct Spherical_Coordinate_{
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
} Spherical_Coordinate;
/* A Three_Component_Seismogram is viewed as a special collection of Time Series 
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
// Basic_Time_Series object.  Auxiliary parameters are defined for the object
// through inheritance of a Metadata object.
//@}
class Three_Component_Seismogram : public Basic_Time_Series , public Metadata
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
	Three_Component_Seismogram();
//@{
// Simplest parameterized constructor. Initializes data and sets aside memory for
// matrix of size 3xnsamples.  The data matrix is not initialized.  
//@param nsamples number of samples expected for holding data.
//@}
	Three_Component_Seismogram(int nsamples);

//@{
// Partial constructor for a Three_Component_Seismogram object driven by a Metadata object.
// This is essentially uses the Metadata object as a way to make a parameterized
// constructor with a potentially large and variable number of parameters. 
// The following parameters must exist in the Metadata object or the constructor
// will throw an exception:  samprate, time, and nsamp.  If the load_data
// boolean is true the function will attempt to read data using an additional
// set of keywords that must be in the metadata:  Time_Reference_Type, datatype,
// dir, dfile, and foff.  Time_Reference_Type is a string that must be either "relative"
// or "absolute".  datatype, dir, dfile, and foff are exactly as in a wfdisc record.
// An important current limitation is that only host 4 byte float datatype (t4 or u4)
// are allowed by this constructor.  If datatype is anything else the constructor will
// throw an exception.  
//
// @throws Metadata_error object is thrown if any of the metadata associated with the 
//           keywords noted above are not defined.  
// @throws seispp_error is thrown for read errors when load_data is true.  
//
// @param md - Metadata object to drive construction.
// @param load_data - if true tries to read data in an antelope style ala wfdisc but using
//                     attributes derived from the Metadata object (see above).
//@}
	Three_Component_Seismogram(Metadata& md,bool load_data);
//@{
// Antelope database driven constructor.
// The basic model here is that this constructor builds a Three_Component_Seismogram object
// from a database table.  This is complicated by a fundamental format issue.  Raw seismic
// data is traditionally stored in channel-oriented blocks.  In CSS3.0 wfdisc tables index
// one waveform segment per row.  Three component seismic data requires three channels of 
// data.  This constructor works in two fundamentally different ways.  First, in normal
// data mode it assumes the Database_Handle defines a group of rows in the database
// (formed in Antelope with dbgroup).  The groupings are assumed to be in blocks of 
// three channels. The constructor will throw an exception if the count of rows in the 
// group is anything but 3.  
//
// The second mode this constructor can operate in is triggered by a test for a group
// definition for the Database_Handle.  If the handle is not a group pointer the 
// Metadata constructor is called and the attribute "datatype" is checked.  If datatype
// is not "3c" and exception is thrown.  If it is "3c" it is assumed the data are stored 
// in a simple binary dump of the u matrix contents.  
//
// The following attributes must be present in Metadata_list structure passed to the
// constructor or the routine wil throw a Metadata_error exception:  time, endtime,
// nsamp, samprate, datatype, dir, dfile, and foff. If the datatype is not 3c the
// attributes hang and vang (normally from sitechan) must be defined.  
// 
// Finally irregular start and end times for data read in single channel mode will
// cause gaps to be defined at the beginning and/or end of the data.  
//
// @param db Database_Handle object that is assumed to point at one row of a database view.
// @param mdl A Metadata_list contains a list of internal names that tells the 
//   constructor which attributes it will need to extract from the database and place in
//   the Metadata area of the object.  
// @param am This object is used to map internal names to an external namespace.  It is
//  normally created once at the early stage of a program's execution.  
//@}
	Three_Component_Seismogram(Database_Handle& db, 
		Metadata_list& mdl, Attribute_Map& am);
//@{
// Standard copy constructor.
//@}

	Three_Component_Seismogram(const Three_Component_Seismogram&);
//@{
// Standard assignment operator.
//@}
	Three_Component_Seismogram& operator 
		= (const Three_Component_Seismogram&);
	// Default destructor is acceptable
	//~Three_Component_Seismogram(); 
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
	void rotate_to_standard() throw(seispp_error);
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
	void rotate(Spherical_Coordinate sc);

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
	void free_surface_transformation(Slowness_vector u, double vp0, double vs0);
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
class Time_Series_Ensemble : public Metadata
{
public:  
//@{
// Standard Template Library vector container holding the members of the ensemble.
// This is the main data area for the ensemble.  We use a vector container
// as it is common to want to use the indexing operator to ask for a member
// and it is common to want to sort the ensemble. 
//@}
	vector <Time_Series> tse;

//@{
// Default constructor.  Does little, but is not defaulted.  
//@}
	Time_Series_Ensemble();
//@{
// Space allocating constructor.  Sets aside slots in the ensemble
// for ntsin members with a typical size of nsampin samples.
//@}
	Time_Series_Ensemble(int ntsin, int nsampin);
//@{
// Space allocating constructor with metadata.  Sets aside slots in
// the ensemble for ntsin ensemble members with a typical size of nsampin
// samples.  Defines global metadata elements by mdl.
//@}
	Time_Series_Ensemble(int ntsin, int nsampin, Metadata_list& mdl);
//@{
// Construct an ensemble from a parameter file description.  This
// constructor is useful in combination with the pfstream library.
// In that context blocks of data can be parsed to produce the Pf_ensemble
// regular C data object.  See man pfstream(3).
//@}
	Time_Series_Ensemble(Pf_ensemble *pfe);
//@{
// Standard copy constructor. 
//@}
	Time_Series_Ensemble(Time_Series_Ensemble& tseold);
//@{
// Partial copy constructor. 
// Sometimes it is useful to copy only the Metadata from an ensemble 
// and leave slots open for the container to hold the data objects for
// the ensemble.  
// @param md is the metadata object to copy to the metadata area for the ensemble.
// @param nmembers is the number of slots to reserve in the new ensemble vector.
//@}
	Time_Series_Ensemble(Metadata& md,int nmembers);
//@{
// Standard assignment operator.
//@}
	Time_Series_Ensemble& operator=(const Time_Series_Ensemble& tseold);
	friend void set_global_metadata_list(Time_Series_Ensemble& te,Metadata_list&);
private:
	// This list contains metadata copied as global to the ensemble
	Metadata_list mdlist;
};

//@{
// Object to contain a group (ensemble) of three component seismogram objects. 
// It is common in seismic data processing to have a group (ensemble) of
// seismograms that have some association and hence belong together to
// define a useful group.  This object is a general way to hold a group of
// three component seismogram data stored as as Three_Component_Seismogram
// objects.  This object uses an STL vector container to hold the members.  
// The ensemble has metadata associated with the group acquired by 
// inheritance, but which contructors have to deal with to load the
// right attributes into the ensemble metadata area.  
//@}
class Three_Component_Ensemble : public Metadata
{
public:
//@{
// Standard Template Library vector container holding the members of the ensemble.
// This is the main data area for the ensemble.  We use a vector container
// as it is common to want to use the indexing operator to ask for a member
// and it is common to want to sort the ensemble. 
//@}
	vector <Three_Component_Seismogram> tcse;
//@{
// Default constructor.  Does little, but is not defaulted.  
//@}
	Three_Component_Ensemble();
//@{
// Space allocating constructor.  Sets aside slots in the ensemble
// for ntsin members with a typical size of nsampin samples.
//@}
	Three_Component_Ensemble(int nsta, int nsamp);
//@{
// Space allocating constructor with metadata.  Sets aside slots in
// the ensemble for ntsin ensemble members with a typical size of nsampin
// samples.  Defines global metadata elements by mdl.
//@}
	Three_Component_Ensemble(int nsta, int nsamp,
				Metadata_list& mdl);
//@{
// Database-driven constructor.  
// Seismic data are often stored today using a database to index the raw
// waveforms.  The database contains attributes loaded here as metadata.
// Attributes that are global to the ensemble are loaded driven by the
// ensemble_mdl list while single 3c seismogram metadata is controlled
// by station_mdl.  Note in both cases the list of names is the internal
// name not external database names.  The Attribute_Map object 
// defines the mapping from the internal name space to database
// names.

// @param db is a generalized handle to a database.  In the current
// implementation it is assumed to be a Datascope_Handle.
// @param station_mdl is a list of metadata attributes to be loaded 
//   into each 3c seismogram's metadata area.  
// @param ensemble_mdl is a list of metadata attributes to be loaded
//   into the global metadata area for the whole ensmeble.
// @param am is the mapping operator used to translate internal names
//   to database attribute names (e.g. wfdisc.time).
//@}
	Three_Component_Ensemble(Database_Handle& db,
		Metadata_list& station_mdl,
		Metadata_list& ensemble_mdl,
		 Attribute_Map& am);
//@{
// Standard copy constructor.
//@}
	Three_Component_Ensemble(Three_Component_Ensemble& tseold);
//@{
// Partial copy constructor. 
// Sometimes it is useful to copy only the Metadata from an ensemble 
// and leave slots open for the container to hold the data objects for
// the ensemble.  
// @param md is the metadata object to copy to the metadata area for the ensemble.
// @param nmembers is the number of slots to reserve in the new ensemble vector.
//@}
	Three_Component_Ensemble(Metadata& md,int nmembers);
//@{
// Standard assignment operator.
//@}
	Three_Component_Ensemble& operator=(const Three_Component_Ensemble& tseold);
	friend void set_global_metadata_list(Three_Component_Ensemble&,
			Metadata_list&);
private:
	Metadata_list mdlist;
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
class Top_Mute
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
	Time_Reference_Type reftype;   
	//* Default constructor */
	Top_Mute(){t0e=1.0; t1=2.0; reftype=relative;};
//@{
// Parameter file driven constructor.  
// Looks for three keyword strings to set the three data parameters
// that define the object.  They are:  Zero_End_Time, End_Time, and Time_Reference_Type 
// which reference t0, t1e, and reftype respectively.  
//
// @param pf Antelope parameter file pf object.
// @param tag is a string that defines an &Tbl{} enscapsulation of the parameters
//   parsed for the mute definition. The nesting of an &Tbl{ } with the parameters
//   between the curly brackets allows the same keywords to be used in multiple
//   constructors for different mute definitions.
//@}
	Top_Mute(Pf *pf,string tag);
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
	// @throws seispp_error object when travel time calculator fails for any reason
	//
	// @param lat0 latitude of the station
	// @param lat0 longitude of the station
	// @param elev elevation of the station relative to sea level
	//@}
	double ptime(double lat0, double lon0, double elev)
		throw(seispp_error);
	//@{
	// Compute the P wave slowness vector using the currently defined travel time
	// calculator.  
	// @throws seispp_error object when travel time calculator fails for any reason
	//
	// @param lat0 latitude of the station
	// @param lat0 longitude of the station
	// @param elev elevation of the station relative to sea level
	//@}
	Slowness_vector pslow(double lat0, double lon0, double elev)
		throw(seispp_error);
	//@{
	// Compute the predicted arrival time of a requested phase
	// using the currently defined travel time
	// calculator.  
	// @throws seispp_error object when travel time calculator fails for any reason
	//
	// @param lat0 latitude of the station
	// @param lat0 longitude of the station
	// @param elev elevation of the station relative to sea level
	// @param phase name of phase for which arrival time is requested.
	//@}
	double phasetime(double lat0, double lon0, double elev, string phase)
		throw(seispp_error);
	//@{
	// Compute the predicted slowness vector for a 
	// specified seismic phase using the currently defined travel time
	// calculator.  
	// @throws seispp_error object when travel time calculator fails for any reason
	//
	// @param lat0 latitude of the station
	// @param lat0 longitude of the station
	// @param elev elevation of the station relative to sea level
	// @param phase name of phase for which slowness estimate is requested.
	//@}
	Slowness_vector phaseslow(double lat0, double lon0, double elev, 
			string phase) throw(seispp_error);
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
class Velocity_Model_1d_error
{
public:
	//* Contains error message. */
	string message;
	//* Dump error message to stderr. */
	virtual void log_error(){cerr<<"Velocity_Model_1d object error"<<endl;};
};

//@{
// Error object thrown by 1d velocity model objects.
// This version is thrown by database driven constructor.
//@}
class Velocity_Model_1d_dberror : public Velocity_Model_1d_error
{
public:
	//* Holds name of velocity model that generated error.*/
	string name;
	//* Basic constructor.*/
	Velocity_Model_1d_dberror(string modname,string mess){
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
class Velocity_Model_1d_ioerror : public Velocity_Model_1d_error
{
public:
	//* Holds i/o error message.*/
	string ioerr;
	//* Basic constructor */
	Velocity_Model_1d_ioerror(string mess, string ioe){
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
class Velocity_Model_1d
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
	Velocity_Model_1d(){z.reserve(0);v.reserve(0);grad.reserve(0);nlayers=0;};
	//@{
	// Allocating constructor.  Sets aside space for n layers, but leaves
	// contents empty.  
	//@}
	Velocity_Model_1d(int n){nlayers=n;
                z.reserve(nlayers);
                v.reserve(nlayers);
                grad.reserve(nlayers);};
	//@{
	// Database driven constructor.
	// Builds a velocity model from database tables.
	// Depends on a models database as used in tt1dcvl travel time calculator.
	//
	// @throws seispp_error if there are problems constructing the object
	//    from the database.
	//
	// @param db Datascope database pointer.  
	// @param name unique name to identify requested Earth model.
	// @param property defines property attribute of models database table.  
	//    Normall Pvelocity or Svelocity.
	//@}
	
	Velocity_Model_1d(Dbptr db,string name, string property)
		throw(Velocity_Model_1d_dberror);
	//@{
	// Ascii file constructor.  
	// Reads a velocity model from file fname using a simple ascii format
	// file.  Data are assumed in free format lines of the form
	// (z,vp,vs) where z is depth, vp is P velocity at z, and vs is
	// S velocity at z.  
	//
	// @throws seispp_error if is an i/o problem of any kind.
	//
	// @param fname is file name to be read.
	// @param form is either rbh or plain.  Anything else will cause
	//   an exception to be thrown.  plain is just a set of lines as 
	//   described above.  rbh format is from Bob Herrmann's velocity
	//   model format.  The main difference is that his format has 7 
	//   header lines before the velocity model parameters begin.
	//@}
	Velocity_Model_1d(string fname, string form, string property)
		throw(Velocity_Model_1d_ioerror);
	//@{
	// Return interpolated velocity at depth zin.
	// If z is above the first point the first point velocity is returned.
	// If z is below the last point the value is computed from the last
	// point velocity and the last point gradient.
	//@}
	double getv(double zin);
};


//@{
// Applies a top mute to a Time_Series object.
//@}
void apply_top_mute(Time_Series &ts,Top_Mute& mute);
//@{
// Applies a top mute to a Three_Component_Seismogram object.
//@}
void apply_top_mute(Three_Component_Seismogram& ts,Top_Mute& mute);
//@{
// Applies a single top mute definition to all members of a Time_Series_Ensemble.
//@}
void apply_top_mute(Time_Series_Ensemble& t, Top_Mute& mute);
//@{
// Applies a single top mute definition to all members of a Three_Component_Ensemble.
//@}
void apply_top_mute(Three_Component_Ensemble &t3c, Top_Mute& mute);
//@{
// Applies a time shift called a geometric static to Time_Series object ts 
// using velocity vel and elevation elev.  This is a simple elev/vel correction.
//@}
void apply_geometric_static(Time_Series *ts, double vel, double elev);
//@{
// Applies a time shift called a geometric static to Time_Series object ts 
// using velocity and elevation extracted from the metadata area of ts.  
// The function requires that attributes "elevation" and "surface_velocity"
// be defined in the object.  If these attributes are not defined the 
// data are not altered but a diagnostic is issued to stderr.
//@}
void apply_geometric_static(Time_Series *ts);
//@{
// Pfstream method for getting a time series object from an input stream.
// Useful only in a pfstream environment which is currently not well developed
// for this application.
//@}
Time_Series* get_next_time_series(Pfstream_handle *pfh);
//@{
// Companion to get_next_time_series.  
// Useful only in a pfstream environment which is currently not well developed
// for this application.
//@}
Time_Series *Load_Time_Series_Using_Pf(Pf *pf);
//@{
// Load an ensemble through a pfstream.
//@}
Time_Series_Ensemble *get_next_ensemble(Pfstream_handle *pfh,
	 char *tag,Metadata_list& mdlist) throw(seispp_error);
//@{
// Load a 3c ensemble through a pfstream.
//@}
Three_Component_Ensemble *get_next_3c_ensemble(Pfstream_handle *pfh,
	 char *tag,Metadata_list& mdlist) throw(seispp_error);
//@{
// Save a 3c seismgram using a pfstream output.
//@}
void pfstream_save_3cseis(Three_Component_Seismogram *seis,string tag,
	string dir, string dfile, Pfstream_handle *pfh) throw(seispp_error);
//@{
//  Used by Time_Series constructors to set data gaps using Antelope methods.
//@}
void set_gaps(Time_Series&,Trsample *,int, string)
		throw(seispp_error);
//@{
// Returns a Spherical_Coordinate data structure equivalent to one
// define dby a unit vector nu.
//@}
Spherical_Coordinate unit_vector_to_spherical(double nu[3]);
//@{
// Returns a unit vector (vector of 3 doubles) equivalent to
// direction defined in sphereical coordinates.
//@}
double *spherical_to_unit_vector(Spherical_Coordinate& sc);
//@{
// Return direction of particle motion for a P wave with 
// slowness (ux,uy) at a surface with P velocity vp0 and 
// S velocity vs0.
//@}
Spherical_Coordinate pm_halfspace_model(double vp0,double vs0,
	double ux,double uy);
//@{
// Extract one component from a Three_Component_Seismogram and 
// create a Time_Series object from it.  
//
// @param tcs is the Three_Component_Seismogram to convert.
// @param component is the component to extract (0, 1, or 2)
// @param mdl list of metadata to copy to output from input object.
//@}
Time_Series *Extract_Component(Three_Component_Seismogram& tcs,int component,
        Metadata_list& mdl);
//@{
// Extract one component from a Three_Component_Seismogram and 
// create a Time_Series object from it.  
// Similar to overloaded function of same name, but all metadata from
// parent is copied to the output.
//
// @param tcs is the Three_Component_Seismogram to convert.
// @param component is the component to extract (0, 1, or 2)
//@}
Time_Series *Extract_Component(Three_Component_Seismogram& tcs,int component);
//@{
// Returns a new seismogram in an arrival time reference.
// An arrival time reference means that the time is set to relative and 
// zero is defined as an arrival time extracted from the metadata area of
// the object.
//
// @throws seispp_error for errors in extracting required information from metadata area.
//
// @param din  is input seismogram
// @param key is the metadata key used to find the arrival time to use as a reference.
// @param tw is a Time_Window object that defines the window of data to extract around
//    the desired arrival time.
//@}
Three_Component_Seismogram& Arrival_Time_Reference(Three_Component_Seismogram& din,
	string key, Time_Window tw);
//@{
// Returns a gather of Three_Component_Seismograms in an arrival time reference fram.
// An arrival time refernce means that the time is set to relative and 
// zero is defined as an arrival time extracted from the metadata area of
// each member object.
//
// @throws seispp_error for errors in extracting required information from metadata area.
//
// @param din  is input gather
// @param key is the metadata key used to find the arrival time to use as a reference.
// @param tw is a Time_Window object that defines the window of data to extract around
//    the desired arrival time.
Three_Component_Ensemble& Arrival_Time_Reference(Three_Component_Ensemble& din,
	string key, Time_Window tw);


// low level i/o routines
long int vector_fwrite(double *x,int n, string dir, string dfile) throw(seispp_error);
long int vector_fwrite(double *x,int n, string fname) throw(seispp_error);
long int vector_fwrite(float *x,int n, string dir, string dfile) throw(seispp_error);
long int vector_fwrite(float *x,int n, string fname) throw(seispp_error);
// Antelope database output routine
int dbsave(Time_Series& ts,Dbptr db,string table, Metadata_list& md, Attribute_Map& am)
		throw(seispp_error);
int dbsave(Three_Component_Seismogram& ts,Dbptr db,string table, 
	Metadata_list& md, Attribute_Map& am);
int dbsave(Three_Component_Seismogram& ts,Dbptr db,
	string table, Metadata_list& md, 
	Attribute_Map& am, vector<string>chanmap,bool output_as_standard);
}
#endif
