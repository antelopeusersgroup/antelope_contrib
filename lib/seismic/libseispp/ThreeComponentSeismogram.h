#ifndef _THREECOMPONENTSEISMOGRAM_H_
#define _THREECOMPONENTSEISMOGRAM_H_
#include "Metadata.h"
#include "BasicTimeSeries.h"
#include "TimeSeries.h"
#include "dmatrix.h"
#include "SeisppError.h"
#include "SphericalCoordinate.h"
#include "slowness.h"
namespace SEISPP {
using namespace std;
using namespace SEISPP;
/* A ThreeComponentSeismogram is viewed as a special collection of Time Series 
type data that is essentially a special version of a vector time series. 
It is "special" as the vector is 3D with real components.  One could produce a
similar inherited type for an n vector time series object.  

The structure of a vector time series allows the data to be stored in a matrix.
Here we use a lightweight matrix object I call dmatrix.
*/
 

/*! \brief Vector (three-component) seismogram data object.

 A three-component seismogram is a common concept in seismology. The concept  
 used here is that a three-component seismogram is a time series with a 3-vector
 as the data at each time step.  As a result the data are stored internally as
 a matrix with row defining the component number (C indexing 0,1,2) and 
 the column defining the time variable.
 The object inherits common concepts of a time series through the 
 BasicTimeSeries object.  Auxiliary parameters are defined for the object
 through inheritance of a Metadata object.
\author Gary L. Pavlis
**/
class ThreeComponentSeismogram : public BasicTimeSeries , public Metadata
{
public:
/*!
 Defines if the contents of this object are components of an orthogonal basis.

 Most raw 3c seismic data use orthogonal components, but this is not universal.
 Furthermore, some transformations (e.g. the free surface transformation operator)
 define transformations to basis sets that are not orthogonal.  Because
 detecting orthogonality from a transformation is a nontrivial thing 
 (rounding error is the complication) this is made a part of the object to 
 simplify a number of algorithms. 
**/
	bool components_are_orthogonal;  
/*!
 Defines of the contents of the object are in Earth cardinal coordinates.

 Cardinal means the cardinal directions at a point on the earth.  That is,
 x1 is positive east, x2 is positive north, and x3 is positive up.
 Like the components_are_orthogonal variable the purpose of this variable is
 to simplify common tests for properties of a given data series.
**/
	bool components_are_cardinal;  // true if x1=e, x2=n, x3=up
/*!
 Transformation matrix. 

 This is a 3x3 transformation that defines how the data in this object is
 produced from cardinal coordinates.  That is, if u is the contents of this 
 object the data in cardinal directions can be produced by tmatrix^-1 * u.
**/
	double tmatrix[3][3]; 
/*!
 Holds the actual data.  

Matrix is 3xns.  Thus the rows are the component number
 and columns define time position.  Note there is a redundancy in
 these definitions that must be watched if you manipulate the 
 contents of this matrix.  That is, BasicTimeSeries defines ns, but
 the u matrix has it's own internal size definitions.  Currently no
 tests are done to validate this consistency.  All constructors handle
 this, but again because u is public be very careful in altering u.
**/
	dmatrix u;
/*!
 Default constructor.  

Sets ns to zero and builds an empty matrix.  The live variable
in BasicTimeSeries is also set false.
**/
	ThreeComponentSeismogram();
/*!
 Simplest parameterized constructor. 

Initializes data and sets aside memory for
 matrix of size 3xnsamples.  The data matrix is not initialized
 and the object is marked as not live.
\param nsamples number of samples expected for holding data.
**/
	ThreeComponentSeismogram(int nsamples);
/*!
 Construct a three component seismogram from three TimeSeries objects.

 A three component seismogram is commonly assembled from individual 
 single channel components.  This constructor does the process taking
 reasonable care to deal with (potentially) irregular start and end 
 times of the individual components.  If the start and end times are
 all the same it uses a simple copy operation.  Otherwise it runs a
 more complicated  (read much slower) algorithm that handles the ragged
 start and stop times by adding a marked gap.  That is, the object is
 allocated with space for the earliest start and last end time.  Areas
 at front and back with one or two channels missing are marked as a 
 gap.  

 This constructor handles gaps in the three components correctly as the
 union of the gaps found in all three.  The current algorithm for doing
 this is slow but running a sample by sample test on each component and
 marking gaps with the BasicTimeSeries add_gap methods.  

 Note this constructor requires variables hang and vang, which are 
 orientation angles defined in the CSS3.0 schema (NOT spherical 
 coordinates by the way), by set for each component.  This is used to 
 construct the transformation matrix for the object that allows,
 for example, removing raw data orientation errors using rotate_to_standard.
 The constructor will throw an exception if any component does not have 
 these attributes set in their Metadata area. 

\exception SeisppError exception can be throw for a variety of serious
    problems. 
\param ts vector of 3 TimeSeries objects to be used to assemble
  this ThreeComponentSeismogram.  Input vector order could be 
  arbitrary because a transformation matrix is computed, but for
  efficiency standard order (E,N,Z) is advised.
\param component_to_clone the auxiliary parameters (Metadata and 
   BasicTimeSeries common parameters)
   from one of the components is cloned to assure common required 
   parameters are copied to this object.  This argument controls which
   of the three components passed through ts is used.  Default is 0.

**/
	ThreeComponentSeismogram(vector<TimeSeries>& ts,
		int component_to_clone=0);
/*!
 Partial constructor for a ThreeComponentSeismogram object driven by a Metadata object.

 This is essentially uses the Metadata object as a way to make a parameterized
 constructor with a potentially large and variable number of parameters. 
 The following parameters must exist in the Metadata object or the constructor
 will throw an exception:  samprate, time, and nsamp.  If the load_data
 boolean is true the function will attempt to read data using an additional
 set of keywords that must be in the metadata:  TimeReferenceType, datatype,
 dir, dfile, and foff.  TimeReferenceType is a string that must be either "relative"
 or "absolute".  datatype, dir, dfile, and foff are exactly as in a wfdisc record.
 An important current limitation is that only host 8 byte float datatype in mulitplexed
 or trace order are allowed by this constructor.  If datatype is anything else the constructor will
 throw an exception.  

 Be aware this constructor has not been as heavily tested as it probably should be.  
 It is known to have some limitations.  In particular, byte swapping has not been implemented.

\exception MetadataError object is thrown if any of the metadata associated with the 
           keywords noted above are not defined.  
\exception SeisppError is thrown for read errors when load_data is true.  

\param md - Metadata object to drive construction.
\param load_data - if true tries to read data in an antelope style ala wfdisc but using
                     attributes derived from the Metadata object (see above).
**/
	ThreeComponentSeismogram(Metadata& md,bool load_data);
#ifndef NO_ANTELOPE
/*!
 Antelope database driven constructor.

 The basic model here is that this constructor builds a ThreeComponentSeismogram object
 from a database table.  This is complicated by a fundamental format issue.  Raw seismic
 data is traditionally stored in channel-oriented blocks.  In CSS3.0 wfdisc tables index
 one waveform segment per row.  Three component seismic data requires three channels of 
 data.  This constructor works in two fundamentally different ways.  First, in normal
 data mode it assumes the DatabaseHandle defines a group of rows in the database
 (formed in Antelope with dbgroup).  The groupings are assumed to be in blocks of 
 three channels. The constructor will throw an exception if the count of rows in the 
 group is anything but 3.  

 The second mode this constructor can operate in is triggered by a test for a group
 definition for the DatabaseHandle.  If the handle is not a group pointer the 
 Metadata constructor is called and the attribute "datatype" is checked.  If datatype
 is not "3c" and exception is thrown.  If it is "3c" it is assumed the data are stored 
 in a simple binary dump of the u matrix contents.  

 The following attributes must be present in MetadataList structure passed to the
 constructor or the routine will throw a MetadataError exception:  time, endtime,
 nsamp, samprate, datatype, dir, dfile, and foff. If the datatype is not 3c or c3 the
 attributes hang and vang (normally from sitechan) must be defined in each row of the
 input view.  3c (Sun byte order) and c3 (intel byte order) implies the data are 
 raw binary doubles in a particular byte order multiplexed by 3 channels and in
 cardinal directions (x1=E, x2=N, and x3=Z)..  
 
 Finally irregular start and end times for data read in single channel mode will
 cause gaps to be defined at the beginning and/or end of the data.  

\param db DatabaseHandle object that is assumed to point at one row of a database view.
\param mdl A MetadataList contains a list of internal names that tells the 
   constructor which attributes it will need to extract from the database and place in
   the Metadata area of the object.  
\param am This object is used to map internal names to an external namespace.  It is
  normally created once at the early stage of a program's execution.  
**/
	ThreeComponentSeismogram(DatabaseHandle& db, 
		MetadataList& mdl, AttributeMap& am);
#endif
/*!
 Standard copy constructor.
**/

	ThreeComponentSeismogram(const ThreeComponentSeismogram&);
/*!
 Standard assignment operator.
**/
	ThreeComponentSeismogram& operator 
		= (const ThreeComponentSeismogram&);
/*!
 Extract a sample from data vector. 

 A sample in this context means a three-vector at a requested
 sample index.  Range checking is implicit because 
 of the internal use of the dmatrix to store the samples of
 data.  This operator is an alternative to extracting samples
 through indexing of the internal dmatrix u that holds the data.

 Note that the operator returns a simple pointer into the
 internal array that stores the data.  In this implementation
 this is a dmatrix which stores data in a FORTRAN like array
 (FORTRAN order, but C indexing starting at 0).  
 For this reason DO NOT EVER free this pointer or the results
 will, as usual for memory problems, be undefined and almost
 certain to cause bad things to happen downstream.

\exception SeisppError if the requested sample is outside
    the range of the data.  Note this includes an implicit "outside"
    defined when the contents are marked dead.  
    Note the code does this by catching an error thrown by dmatrix
    in this situation, printing the error message from the dmatrix
    object, and then throwing a new SeisppError with a shorter 
    message.  
\return pointer to 3-vector of requested sample.  Caller should
    assume only the 3 consecutive samples after the value pointed to by
    result is valid.  You could use the result as a pointer to 
    this internal 3xns matrix that stores the data internally 
    in this function, but this is a bad idea.  There is a nonzero
    probability the implementation couldd change invalidating 
    an algorithm that tried to utilize the result in that way.  

\param sample is the integer sample number of data desired.
**/
	double *operator[](int sample);
/*! Standard destructor. */
	~ThreeComponentSeismogram(); 
/*!
 Sets the data values in all defined gaps to zero.
**/
	void zero_gaps();
/*!
 Apply inverse transformation matrix to return data to cardinal direction components.

 It is frequently necessary to make certain a set of three component data are oriented
 to the standard reference frame (EW, NS, Vertical).  This function does this.
 For efficiency it checks the components_are_cardinal variable and does nothing if 
 it is set true.  Otherwise, it applies the inverse transformation and then sets this variable true.
 Note even if the current transformation matrix is not orthogonal it will be put back into 
 cardinal coordinates. 
 \exception SeisppError thrown if the an inversion of the transformation matrix is required and that
 matrix is singular.  This can happen if the transformation matrix is incorrectly defined or the
 actual data are coplanar.
**/
	void rotate_to_standard() throw(SeisppError);
	// This overloaded pair do the same thing for a vector
	// specified as a unit vector nu or as spherical coordinate angles
/*!
 Rotate data using a P wave type coordinate definition.  

 In seismology the longitudinal motion direction of a P wave defines a direction 
 in space.  This method rotates the data into a coordinate system defined by a 
 direction passed through the argument.  The data are rotated such that x1 becomes 
 the transverse component, x2 becomes radial, and x3 becomes longitudinal.  In the 
 special case for a vector pointing in the x3 direction the data are not altered.
 The transformation matrix is effectively the matrix product of two coordinate rotations:
 (1) rotation around x3 by angle phi and (2) rotation around x1 by theta.  

The sense of this transformation is confusing because of a difference in 
convention between spherical coordinates and standard earth coordinates.
In particular, orientation on the earth uses a convention with x2 being
the x2 axis and bearings are relative to that with a standard azimuth
measured clockwise from north.  Spherical coordinate angle phi (used here)
is measured counterclockwise relative to the x1 axis, which is east in
standard earth coordinates. This transformation is computed using a phi
angle.   To use this then to compute a transformation to standard ray 
coordinates with x2 pointing in the direction of wavefront advance, 
phi should be set to pi/2-azimuth which gives the phi angle needed to rotate
x2 to radial.  This is extremely confusing because in spherical coordinates
it would be more intuitive to rotate x1 to radial, but this is NOT the 
convention used here.  In general to use this feature the best way to avoid
this confusion is to use the PMHalfSpaceModel procedure to compute a 
SphericalCoordinate object consistent with given propagation direction
defined by a slowness vector.  Alternatively, use the free_surface_transformation 
method defined below.  

\param sc defines final x3 direction (longitudinal) in a spherical coordinate structure.
**/
	void rotate(SphericalCoordinate sc);

/*!
 Rotate data using a P wave type coordinate definition.  

 In seismology the longitudinal motion direction of a P wave defines a direction 
 in space.  This method rotates the data into a coordinate system defined by a 
 direction passed through the argument.  The data are rotated such that x1 becomes 
 the transverse component, x2 becomes radial, and x3 becomes longitudinal.  In the 
 special case for a vector pointing in the x3 direction the data are not altered.

 This method effectively turns nu into a SphericalCoordinate object and calles the
 related rotate method that has a SphericalCoordinate object as an argument.  The 
 potential confusion of orientation is not as extreme here.  After the transformation
 x3prime will point in the direction of nu, x2 will be in the x3-x3prime plane (rotation by
 theta) and orthogonal to x3prime, and x1 will be horizontal and perpendicular to x2prime
 and x3prime.

\param nu defines direction of x3 direction (longitudinal) as a unit vector with three components.
**/
	void rotate(double nu[3]);
	// This applies a general transform with a 3x3 matrix.  
	// User should set components_are_orthogonal true if they
	// are so after this transformation.  The default assumes not
	// Note this is routine does NOT return to standard before
	// applying the transformation so this is accumulative.
/*!
 Applies an arbitrary transformation matrix to the data.
 i.e. after calling this method the data will have been multiplied by the matrix a
 and the transformation matrix will be updated.  The later allows cascaded 
 transformations to data.

\param a is a C style 3x3 matrix.
**/
	void apply_transformation_matrix(double a[3][3]);
/*!
 Computes and applies the Kennett [1991] free surface transformation matrix.

 Kennett [1991] gives the form for a free surface transformation operator 
 that reduces to a nonorthogonal transformation matrix when the wavefield is
 not evanescent.  On output x1 will be transverse, x2 will be SV (radial),
 and x3 will be longitudinal.  

\param u slowness vector off the incident wavefield
\param vp0 Surface P wave velocity
\param vs0 Surface S wave velocity.
**/
	void free_surface_transformation(SlownessVector u, double vp0, double vs0);
};
//
////////////////////////////////////////////////////
//  Start helper function prototypes
///////////////////////////////////////////////////
//
/*! Rotate data in the horizontal plane by a specified angle.

A common case for rotation is a rotation around the vertical (x3) axis.
This procedure this to modify a ThreeComponentSeismogram object in place
an properly update the transformation matrix.  This is more or less
a front end to the rotate by tranformation matrix.  This procedure perhaps
should have been part of the interface definition.  It is necessary because
the angle based methods that are in the interface become undefined for 
this geometry. 
\param d is the seismogram to be rotated
\param phi is the angle (radians) by which the data are to be rotate.  
It is VERY IMPORTANT to recognize phi is spherical coordinate angle NOT
a geographical azimuth.  

*/
void HorizontalRotation(ThreeComponentSeismogram& d, double phi);
/*!
 Extract one component from a ThreeComponentSeismogram and 
 create a TimeSeries object from it.  

\param tcs is the ThreeComponentSeismogram to convert.
\param component is the component to extract (0, 1, or 2)
\param mdl list of metadata to copy to output from input object.
**/
TimeSeries *ExtractComponent(ThreeComponentSeismogram& tcs,int component,
        MetadataList& mdl);
/*!
 Extract one component from a ThreeComponentSeismogram and 
 create a TimeSeries object from it. 
 
 Similar to overloaded function of same name, but all metadata from
 parent is copied to the output.

\param tcs is the ThreeComponentSeismogram to convert.
\param component is the component to extract (0, 1, or 2)
**/
TimeSeries *ExtractComponent(ThreeComponentSeismogram& tcs,int component);
} // End namespace SEISPP declaration
#endif
