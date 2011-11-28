#ifndef _GCLGRID_H_
#define _GCLGRID_H_
#include <stdlib.h>
#include <string>
#include <vector>
#include "stock.h" 
#include "db.h"
#include "perf.h"
#include "coords.h"
#undef min
#undef max
#include "dmatrix.h"

//#ifdef	__cplusplus
//==================================================================
// @{
// Geographical Curvilinear Grid library.  This is an object oriented
// library for distorted grid objects.  These are common in 3d visualization
// but the distinguishing factor here is that a GCLgrid is 
// geographically referenced.  Points in a grid have both a Cartesian and
// Geographical equivalent.  The data are actually stored in a Cartesian
// format to allow common distorted grid algorithms to work.  Geographical
// elements are added through member functions that convert to a 
// geographical reference frame.  The concepts of the library are described in
// a paper by Fan and Pavlis (in review, 2005 or 2006 publication date).  
// @pkgdoc @root
// @author Gary L. Pavlis
//@}
//==================================================================
//

//@{
// This data structure is used to encapsulate data to describe a point on
// or inside the earth using a geographical (spherical geometry) reference
// system.  Note that geographical angles are always assumed to be radians
// in any internal use of this package.
//@}

typedef struct geo_{
//@{
//  Latitude of the point (radians).
//@}
	double lat;
//@{
// Longitude of point (radians).
//@}
	double lon;
//@{
// Radius of point from Earth's center (kilometers).
//@}
	double r;
} Geographic_point;
//@{
// GCLgrid objects hold points internally in a Cartesian reference fram.
// This data structure encapsulates such a coordinate.  It perhaps should
// be a class with a member to return a 3 vector alternative to the 
// verbose naming.
//@}
typedef struct cart_ {
//@{
// The coordinate in the internal x1 direction.
//@}
	double x1;
//@{
// The coordinate in the internal x2 direction.
//@}
	double x2;
//@{
// The coordinate in the internal x3 direction.
//@}
	double x3;
} Cartesian_point;

//@{
// This is a base class that contains common attributes and virtual 
// declarations for higher level objects that are derived from this base.  
// Note that both 2d and 3d grid and field objects are derived from here.
// @author Gary L. Pavlis
//@}
class BasicGCLgrid
{
public:
//@{
// Name assigned to this object
//@}
	string name;
//@{
// Latitude (radians) of origin of the grid's Cartesian coordinate system.
//@}
	double lat0;
//@{
// Longitude (radians) of origin of the grid's Cartesian coordinate system.
//@}
	double lon0;
//@{
// Radial distance of origin of the grid's Cartesian coordinate system from Earth's center.
//@}
	double r0;
//@{
// Nominal azimuth (radians) of positive y axis of grid line at origin location.
//@}
	double azimuth_y;
//@{
// Nominal spacing (km) of grid lines along the 1 position gridlines.  
//@}
	double dx1_nom;
//@{
// Nominal spacing (km) of grid lines along the 2 position gridlines.  
//
//@}
	double dx2_nom;
//@{
// Number of grid points in generalized coordinate index 1 direction (first array index).
//@}
	int n1;
//@{
// Number of grid points in generalized coordinate index 2 direction (second array index).
//@}
	int n2;
//@{
// Offset in first index to origin grid point.  i.e. origin is at first coordinate index i0.
//@}
	int i0;
//@{
// Offset in second index to origin grid point.  i.e. origin is at second coordinate index j0. ([i0][j0])
//@}
	int j0;
//@{
// Cartesian x1 coordinate of lower limit of bounding box for grid.  
// This edge of the  bounding box is defined as the smallest x1 value minus dx1_nom.
//@}
	double x1low;
//@{
// Cartesian x1 coordinate of upper limit of bounding box for grid.  
// This edge of the  bounding box is defined as the largest x1 value plus dx1_nom.
//@}
	double x1high;
//@{
// Cartesian x2 coordinate of lower limit of bounding box for grid.  
// This edge of the  bounding box is defined as the smallest x2 value minus dx2_nom.
//@}
	double x2low;
//@{
// Cartesian x2 coordinate of upper limit of bounding box for grid.  
// This edge of the  bounding box is defined as the largest x2 value plus dx2_nom.
//@}
	double x2high;
//@{
// Cartesian x3 coordinate of lower limit of bounding box for grid.  
// This edge of the  bounding box is defined as the smallest x3 value minus dx3_nom.
//@}
	double x3low;
//@{
// Cartesian x3 coordinate of upper limit of bounding box for grid.  
// This edge of the  bounding box is defined as the largest x3 value plus dx3_nom.
//@}
	double x3high;
//@{
// Default Constructor.
//
// Implemented to initialize all base attributes explicitly.
//@}
	BasicGCLgrid();
//** Copy constructor.*/
	BasicGCLgrid(const BasicGCLgrid& old);
//@{
// This is a rotation matrix that defines the transformation from standard spherical 
// coordinates (the geographical reference frame) to the local Cartesian coordinate
// system used in a GCLgrid.  Was private in an earlier incarnation, but this is
// is messy when we depend on inheritance so it is public.  Users should not manipulate
// this directly, however, but if it is desired they should use the fetch_transformation_matrix
// member function. 
//@}
	double gtoc_rmatrix[3][3];
//@{
// This is a close companion to the gtoc_rmatrix. The full transformation used to 
// define the Cartesian system in a GCLgrid is a translation from the earth's center
// to the grid coordinate system origin.  This is followed by a rotation by gtoc_rmatrix.
// The translation_vector attribute defines what the name implies.
//@}
	double translation_vector[3];
//@{
// Set or reset the transformation properties for the grid.  The transformation properties 
// are uniquely defined by the coordinate system origin and azimuth_y so if these are
// all that are known this low level member function can be called to set the transformation
// properties.  It is of minimal use to most users and should be used cautiously and only
// if you thoroughly understand the way this all works.  
//
//@}
	void set_transformation_matrix();
//@{
// Returns the transformation matrix for this grid as a 3x3 dmatrix object.
//@}
	dmatrix fetch_transformation_matrix();
//@{
// Returns a newly allocated 3 vector of double containing a copy of the translation 
// vector defining the GCLgrid transformation property.  The user must be sure to 
// call delete [] after using this vector to avoid a memory leak.
//@}
	double *fetch_translation_vector();
//@{
// Convert Cartesian coordinates to geographical coordinates.
// @returns Geographic_point data structure (see @link Geographic_point @endlink )
// @param x1p - Cartesian x1 coordinate of point to convert
// @param x2p - Cartesian x2 coordinate of point to convert
// @param x3p - Cartesian x3 coordinate of point to convert
//@}
	Geographic_point ctog(double x1p, double x2p, double x3p);
//@{
// Convert from Cartesian coordinates to geographical coordinates.
// @returns Geographic_point data structure (see @link Geographic_point @endlink )
// @param p point to convert stored in a Cartesian_point data structure
//@}
	Geographic_point ctog(Cartesian_point p);
//@{
// Convert from geographical to Cartesian coordinates in the GCLgrid 
// coordinate system.
//
// @returns Cartesian_point data structure (see @link Cartesian_point @endlink)
// @param plat Latitude (radians) of point to convert.
// @param plon Longitude (radians) of point to convert.
// @param pr Earth radius (km) of point to convert.
//@}
	Cartesian_point gtoc(double plat, double plon, double pr);
//@{
// Convert from geographical to Cartesian coordinates in the GCLgrid 
// coordinate system.
//
// @returns Cartesian_point data structure (see @link Cartesian_point @endlink)
// @param p point to convert stored in a @link Geographic_point @endlink data structure.
//@}
	Cartesian_point gtoc(Geographic_point p);
	double depth(Cartesian_point p);
	double depth(Geographic_point p);
//@{
// This member function sets the x1min, x1max, x2min, x2max, x3min, and x3max bounding
// box attribute.  These define the "extents" of the name.  Should be needed only if one
// builds a custom grid from user defined coordinates.  Said a different way if you build a 
// GCLgrid from lower level routines be sure to call this function to set the bounding box
// correctly.
//@}
	virtual void compute_extents()=0;
//@{
//Reset lookup index to origin.
//The lookup functions used in the GCLgrid library keeps an index of 
//the previous lookup results under an assumption the next point 
//requested will be nearby.  This can cause convergence problems in
//some grids, however, if that position is a poor place to start
//(e.g. sudden jump to a completely different location).  This function 
//should be called if a lookup fails to try to recover.  Internal methods
//like the += operator does this automatically.
//@}
	virtual void reset_index()=0;
//@{
//Query for the current lookup index position.  The lookup functions used
//in the GCLgrid library keep and index of the previous lookup result under
//an assumption that the next point requested will be nearby.  This method
//is used to ask what the current index position.  
//@param ind vector of ints of length sufficient to hold the index  (2 for 2d and 3 for 3d grids)
//@}
	virtual void get_index(int *ind)=0;
//@{
// Comparison of two grids for equality.  Equality in this context is NOT the obvious.  
// Equality means the transformation properties of the two grids being compared are the same.
// That is, the operator tests only the transformation matrix and translation vector for
// equality.  This is a necessary condition to allow to grids to be mapped onto each other
// for higher level operations like +=.  
//@}
	bool operator==(const BasicGCLgrid&);
//@{
// Comparison of two grids for inequality.  Equality in this context is NOT the obvious.  
// Equality means the transformation properties of the two grids being compared are the same.
// That is, the operator tests only the transformation matrix and translation vector for
// equality.  This is a necessary condition to allow to grids to be mapped onto each other
// for higher level operations like +=.  Returns true if the transformation properties of 
// two matrices do not match.
//@}
	bool operator!=(const BasicGCLgrid&);
};

//@{
// This is the working two-dimensional version of a GCLgrid.  A GCLgrid defines a two-dimensional
// surface in Earth coordinates.  This could be any surface, but the lookup method used in 
// this version will not work if the surface is multivalued or even strongly warped. 
// See the algorithm description in the paper by Fan and Pavlis (in review) for details.
//
// Positions of grid points are stored in three, two-dimensional arrays. Each of these
// arrays define cartesian coordinates for a grid point at a particular index position.
// The topology is that the location in the Cartesian coordinate system of the grid 
// point defined by index positions i and j are (x1[i][j], x2[i][j], x3[i][j]). 
// The coordinates are thus matrices indexing each position in grid.  
//
// Note the object inherits most data attributes from the BasicGCLgrid object.
//@}


class GCLgrid : public BasicGCLgrid
{
public:
//@{
// n1 x n2 matrix of the x1 coordinate values for 
// grid elements.  
//@}
	double **x1;
//@{
// n1 x n2 matrix of the x2 coordinate values for 
// grid elements.  
//@}
	double **x2;
//@{
// n1 x n2 matrix of the x3 coordinate values for 
// grid elements.  
//@}
	double **x3;
//@{
// Default constructor.  Note it sets the x1,x2, and x3 pointers to NULL
// which is used as a test to avoid duplicate free calls on these potentially 
// large arrays.
//@}
	GCLgrid(){
		n1=0;n2=0;x1=NULL;x2=NULL;x3=NULL;
	};
//@{
// Simple constructor.  Allocates space for x1, x2, and x3 arrays and initializes
// other data attributes to zero.  
//
//  @param n1 number of grid points on generalized coordinate axis 1.
//  @param n2 number of grid points on generalized coordinate axis 2.
//@}
	GCLgrid(int n1size, int n2size);
//@{
//  Constructor for what we call a "regular" GCLgrid in the Fan and Pavlis (in review) paper.  
//  The object this constructs is a constant geoid elevation surface (Follows the reference
//  ellipsoid at elevation set by the origin radius, r0.) with approximately regular spacing
//  between grid points (within the limits of what is possible on a spherical surface).  
//  Note that the makegclgrid program is little more than a wrapper around this and the 3d 
//  version of this constructor.
//
//  @param n1 number of grid points on generalized coordinate axis 1.
//  @param n2 number of grid points on generalized coordinate axis 2.
//  @param n name to assign this grid.
//  @param la0 latitude to use for origin.
//  @param lo0 longitude to use for origin.
//  @param radius0 Earth radius to use for origin.  If 0 or negative will use 
//		r0_ellipse at la0.
//  @param az azimuth of great circle path through the origin that defines the generalized coordinate 2 axis direction.
//  @param dx1n nominal spacing of grid points on the generalized coordinate 1 axis.
//  @param dx2n nominal spacing of grid points on the generalized coordinate 2 axis.
//  @param iorigin 1 axis grid index of the origin in generalized coordinate grid frame.
//  @param jorigin 2 axis grid index of the origin in generalized coordinate grid frame.
//
//@}
	GCLgrid(int n1size, int n2size, string n, double la0, double lo0,
		double radius0, double az, double dx1n, double dx2n, 
		int iorigin, int jorigin);
//@{
//  Antelope database driven constructor.  This is a specialized constructor that
//  loads a grid tagged with the name nm from a database using a specialized table.
//  Users who do not wish to connect with Antelope should remove this constructor
//  from the object definition.
//
//  @param db Antelope database pointer.
//  @param nm name of grid to be loaded from the database.
//@}
	GCLgrid(Dbptr db, string nm);  
//@{
// Standard copy constructor.
//@}
	GCLgrid(const GCLgrid&);  //standard copy constructor
//@{
// Destructor.  Nontrivial destructor has to destroy the coordinate arrays correctly
// and handle case when they are never defined.  Handles this by checking for 
// NULL pointers for these arrays.  If the pointers are NULL the free routines
// are not called.  This is important to know if you try to create a GCLgrid
// object by the default constructor.  
//@}
	~GCLgrid();
//@{
// Standard assignment operator.
//@}
	GCLgrid& operator=(const GCLgrid& );
	//@{
	// Save grid to an Antelope (Datascope) database.  
	// This routine writes the object attributes to a special table and 
	// writes the grid coordinate data an output file.  Note the file
	// name used to store the grid is the same as the name parameter passed 
	// to this function.
	//
	// @throws int 
	// Exception is thrown if save failed.  
	// A simple int exception is used because errors are posted 
	// to the Antelope elog mechanism. 
	// If this exception is caught, call elog_complain to flush the 
	// elog buffer.
	//
	// @param db Antelope database pointer.
	// @param nm name used to save the grid.
	//@}
	void dbsave(Dbptr db, string nm) throw(int);
	//@{
	// Find the index position of a point in a GCLgrid.  
	// This is a low level function to find the location of a point
	// specified as the Cartesian, ordered pair (x1p,x2p) in a grid.
	// It does not return the actual index positions, but only sets the
	// internal index.  The routine is very procedural returning an 
	// integer code (see below) indicating success or failure of the lookup.
	// This was intentional as this routine is called millions of times in 
	// some contexts so efficiency is critical.  The alternative would be 
	// to throw an exception when a lookup failed, but since this is viewed
	// as a common problem that could happen millions of times this was 
	// a potential efficiency problems (the books all say throwing exceptions
	// is an expensive operation).  
	// @returns 2 when point is in gray area within on nominal grid spacing of the edge
	// @returns 1 when the point is outside the bounding box.
	// @returns 0 on success.\
	// @returns -1 if the lookup function did not converge.
	//
	// @param x1p - Cartesian x1 coordinate of point to find within the grid
	// @param x2p - Cartesian x2 coordinate of point to find within the grid
	//@}
	int lookup(double x1p, double x2p);
	/** See {@link BasicGCLgrid::reset_index} */
	void reset_index() {ix1=i0; ix2=j0;};
	/** See {@link BasicGCLgrid::get_index} */
	void get_index(int *ind) {ind[0]=ix1; ind[1]=ix2;};
	//@{
	// Returns the geographical coordinates of a point in the grid specified by grid index positions.
	//
	// If you need the actual coordinates of the points that define the grid 
	// converted to geographic coordinates use this function.  Use the 
	// ctog function to convert an arbitrary ordered triplet.  
	//
	// @returns grid point requested in an @link Geographic_point @endlink data structure.
	// @param i1 Value of grid index 1 for point desired.
	// @param i2 Value of grid index 2 for point desired.
	//@}
	Geographic_point geo_coordinates(int i1,int i2);
	//@{
	// Get the latitude (in radians) of a grid point.
	//
	// @returns latitude in radians of the requested grid point.
	// @param i1 Value of grid index 1 for point desired.
	// @param i2 Value of grid index 2 for point desired.
	//@}
	double lat(int i1, int i2);
	//@{
	// Get the longitude (in radians) of a grid point.
	//
	// @returns longitude in radians of the requested grid point.
	// @param i1 Value of grid index 1 for point desired.
	// @param i2 Value of grid index 2 for point desired.
	//@}
	double lon(int i1, int i2);
	//@{
	// Get the radius from the center of the Earth of a grid point.
	//
	// @returns Earth radius in kilometers of the requested grid point.
	// @param i1 Value of grid index 1 for point desired.
	// @param i2 Value of grid index 2 for point desired.
	//@}
	double r(int i1, int i2);
	//@{
	// Get the depth below the standard reference ellipsoid of a grid point.
	//
	// @returns depth in kilometers of the requested grid point.
	// @param i1 Value of grid index 1 for point desired.
	// @param i2 Value of grid index 2 for point desired.
	//@}
	double depth(int i1, int i2);

	//** Sets extents attributes based on min and max values */
	void compute_extents();
	friend class GCLscalarfield;
	friend class GCLvectorfield;
private:
	int ix1, ix2;
};
//3d version is identical except it requires 3 indexes instead of 2 for
//coordinates.  We use inheritance to simply this description.
//@{
// Three-dimensional version of a GCLgrid object.  
//
// Adds additional attributes required to deal with an added dimension and 
// uses a higher dimensional array to contain points.
//@}
class GCLgrid3d : public BasicGCLgrid
{
public:
//@{
// Nominal spacing (km) of grid lines along the 3 position gridlines.  
//
//@}
	double dx3_nom;
//@{
// Number of grid points in generalized coordinate index 3 direction (second array index).
//@}
	int n3;
//@{
// Offset in third index to origin grid point.  i.e. origin is at third coordinate index k0. ([i0][j0][k0])
//@}
	int k0;
//@{
// n1 x n2 x n3 Three-dimensional array that holds the Cartesian
// x1 component of the positions of each grid point.  
//@}
	double ***x1;
//@{
// n1 x n2 x n3 Three-dimensional array that holds the Cartesian
// x2 component of the positions of each grid point.  
//@}
	double ***x2;
//@{
// n1 x n2 x n3 Three-dimensional array that holds the Cartesian
// x3 component of the positions of each grid point.  
//@}
	double ***x3;

	//@{
	// Default constructor. 
	// Note sets pointers to NULL to make destructor work correctly 
	// when a grid is created by this constructor.
	//@} 
	GCLgrid3d(){
		n1=0;n2=0;n3=0;
		x1=NULL;x2=NULL;x3=NULL;
	};
//@{
// Simple constructor.  Allocates space for x1, x2, and x3 arrays and initializes
// other data attributes to zero.  
//
//  @param n1size number of grid points on generalized coordinate axis 1.
//  @param n2size number of grid points on generalized coordinate axis 2.
//  @param n3size number of grid points on generalized coordinate axis 3.
//@}
	GCLgrid3d(int n1size, int n2size, int n3size);
//@{
//  Constructor for what we call a "regular" GCLgrid in 3D in the Fan and Pavlis (in review) paper.  
//  The object this constructs is spherical shell, boxlike object built up of elemental spherical
//  shell cube-like grid components.  The top surface of the box is defined by the reference
//  ellipsoid.  The bottom is a constant depth below this.  Note that the grid this creates 
//  is oriented with the third generalized coordinate index running from the bottom (deeper
//  inside the Earth) upward as the grid index increases.  Note no index is allowed for 
//  index 3 for the origin.  This constuctor always puts the origin at the bottom of the grid.
//  This isn't necessary, but something that is just frozen into this implementation.
//  For the same reason r0 is ignored and just set internally by this constructor.
//
//  Note that the makegclgrid program is little more than a wrapper around this and the 2d 
//  version of this constructor.
//
//  @param n1 number of grid points on generalized coordinate axis 1.
//  @param n2 number of grid points on generalized coordinate axis 2.
//  @param n3 number of grid points on generalized coordinate axis 3.
//  @param n name to assign this grid.
//  @param la0 latitude to use for origin.
//  @param lo0 longitude to use for origin.
//  @param radius0 Earth radius to use for origin.  If 0 or negative will use 
//		r0_ellipse at la0.
//  @param az azimuth of great circle path through the origin that defines the generalized coordinate 2 axis direction.
//  @param dx1n nominal spacing of grid points on the generalized coordinate 1 axis.
//  @param dx2n nominal spacing of grid points on the generalized coordinate 2 axis.
//  @param dx3n nominal spacing of grid points on the generalized coordinate 3 axis.
//  @param iorigin 1 axis grid index of the origin in generalized coordinate grid frame.
//  @param jorigin 2 axis grid index of the origin in generalized coordinate grid frame.
//
//@}
	GCLgrid3d(int n1size, int n2size, int n3size, string n, 
		double la0, double lo0, double az, double radius0,
		double dx1n, double dx2n, double dx3n,
		int iorigin, int jorigin);
//@{
//  Antelope database driven constructor.  This is a specialized constructor that
//  loads a grid tagged with the name nm from a database using a specialized table.
//  Users who do not wish to connect with Antelope should remove this constructor
//  from the object definition.
//
//  @param db Antelope database pointer.
//  @param nm name of grid to be loaded from the database.
//@}
	GCLgrid3d(Dbptr db, string nm); 
	/** Standard copy constructor. */
	GCLgrid3d(const GCLgrid3d&); 
	/** Standard assignment operator. */
	GCLgrid3d& operator=(const GCLgrid3d& );
	//@{
	// Save grid to an Antelope (Datascope) database.  
	// This routine writes the object attributes to a special table and 
	// writes the grid coordinate data an output file.  Note the file
	// name used to store the grid is the same as the name parameter passed 
	// to this function.
	//
	// @throws int 
	// Exception is thrown if save failed.  
	// A simple int exception is used because errors are posted 
	// to the Antelope elog mechanism. 
	// If this exception is caught, call elog_complain to flush the 
	// elog buffer.
	//
	// @param db Antelope database pointer.
	// @param nm name used to save the grid.
	// @}
	void dbsave(Dbptr db, string nm) throw(int);
	//@{
	// Find the index position of a point in a GCLgrid3d object.  
	// This is a low level function to find the location of a point
	// specified as the Cartesian, ordered triplet (x1p,x2p,x3p) in a grid.
	// It does not return the actual index positions, but only sets the
	// internal index.  The routine is very procedural returning an 
	// integer code (see below) indicating success or failure of the lookup.
	// This was intentional as this routine is called millions of times in 
	// some contexts so efficiency is critical.  The alternative would be 
	// to throw an exception when a lookup failed, but since this is viewed
	// as a common problem that could happen millions of times this was 
	// a potential efficiency problems (the books all say throwing exceptions
	// is an expensive operation).  
	// @returns 2 when point is in gray area within on nominal grid spacing of the edge
	// @returns 1 when the point is outside the bounding box.
	// @returns 0 on success.\
	// @returns -1 if the lookup function did not converge.
	//
	// @param x1p - Cartesian x1 coordinate of point to find within the grid
	// @param x2p - Cartesian x2 coordinate of point to find within the grid
	// @param x3p - Cartesian x3 coordinate of point to find within the grid
	//@}
	int lookup(double, double, double);
	/** See {@link BasicGCLgrid::reset_index} */
	void reset_index() {ix1=i0; ix2=j0; ix3=k0;};
	/** See {@link BasicGCLgrid::get_index} */
	void get_index(int *ind) {ind[0]=ix1; ind[1]=ix2; ind[2]=ix3;};
	//@{
	// Returns the geographical coordinates of a point in the grid specified by grid index positions.
	//
	// If you need the actual coordinates of the points that define the grid 
	// converted to geographic coordinates use this function.  Use the 
	// ctog function to convert an arbitrary ordered triplet.  
	//
	// @returns grid point requested in an @link Geographic_point @endlink data structure.
	// @param i1 Value of grid index 1 for point desired.
	// @param i2 Value of grid index 2 for point desired.
	// @param i3 Value of grid index 3 for point desired.
	//@}
	Geographic_point geo_coordinates(int i1,int i2,int i3);
	//@{
	// Get the latitude (in radians) of a grid point.
	//
	// @returns latitude in radians of the requested grid point.
	// @param i1 Value of grid index 1 for point desired.
	// @param i2 Value of grid index 2 for point desired.
	// @param i3 Value of grid index 3 for point desired.
	//@}
	double lat(int,int,int);
	//@{
	// Get the longitude (in radians) of a grid point.
	//
	// @returns latitude in radians of the requested grid point.
	// @param i1 Value of grid index 1 for point desired.
	// @param i2 Value of grid index 2 for point desired.
	// @param i3 Value of grid index 3 for point desired.
	//@}
	double lon(int,int,int);
	//@{
	// Get the radius from the center of the Earth of a grid point.
	//
	// @returns Earth radius in kilometers of the requested grid point.
	// @param i1 Value of grid index 1 for point desired.
	// @param i2 Value of grid index 2 for point desired.
	// @param i3 Value of grid index 3 for point desired.
	//@}
	double r(int,int,int);
	//@{
	// Get the depth below the standard reference ellipsoid of a grid point.
	//
	// @returns depth in kilometers of the requested grid point.
	// @param i1 Value of grid index 1 for point desired.
	// @param i2 Value of grid index 2 for point desired.
	// @param i3 Value of grid index 3 for point desired.
	//@}
	double depth(int,int,int);
	//** Sets extents attributes based on min and max values */
	void compute_extents();
//@{
// Destructor.  Nontrivial destructor has to destroy the coordinate arrays correctly
// and handle case when they are never defined.  Handles this by checking for 
// NULL pointers for these arrays.  If the pointers are NULL the free routines
// are not called.  This is important to know if you try to create a GCLgrid
// object by the default constructor.  
//@}
	~GCLgrid3d();
private:
	int ix1, ix2, ix3;
};	  		
/** Two-dimensional scalar field defined on a GCLgrid framework. */
class GCLscalarfield :  public GCLgrid
{
public:
	//@{
	// Scalar field variable stored in a two-dimensional C array.
	// Index of the field is parallel with coordinate arrays x1, x2, and x3
	// that define grid point positions in space.
	//@}
	double **val;

	/** Default constructor */
	GCLscalarfield();
	//@{
	// Simple constructor.  Allocates space for all arrays but loads nothing.
	// Do not assume anything is initialized.
	//
	//  @param n1size number of grid points on generalized coordinate axis 1.
	//  @param n2size number of grid points on generalized coordinate axis 2.
	//@}
	GCLscalarfield(int n1size, int n2size);
	/** Standard copy constructor */
	GCLscalarfield(const GCLscalarfield&);
	//@{
	// Partial copy constructor cloning grid but not setting field variable.
	//
	// A common situation is to have a grid that is already defined that
	// needs to be cloned and have field variables set through some other
	// mechanism.  For example, one might create a standard grid and then
	// plan to load it with values from a different grid or compute values
	// of a field variable at the grid points.  
	//
	// @param g Grid to be cloned.
	//@}
	GCLscalarfield(GCLgrid& );
	//@{
	//  Antelope database driven constructor.  This is a specialized constructor that
	//  loads a field tagged with two names:  grid name and the field name (see below).
	//  The two components are stored separately in the external database using 
	//  two different tables.  This constructor has a fair amount of memory overhead
	//  as it loads a copy of the parent GCLgrid object before creating the field and
	//  then loading field variable data from the database.
	//
	// @throws int 
	// Exception is thrown if there are any i/o problems.
	// A simple int exception is used because errors are posted 
	// to the Antelope elog mechanism. 
	// If this exception is caught, call elog_complain to flush the 
	// elog buffer.
	//
	//  @param db Antelope database pointer.
	//  @param grdnm name of grid to be loaded from the database.
	//  @param fn name tag for the field to be loaded from the database.
	//@}
	GCLscalarfield(Dbptr db, string grdnm, string fn);
	//@{
	// Destructor.  
	// Nontrivial destructor as it has to destroy components
	// stored in plain C arrays.
	//@}
	~GCLscalarfield();
	/** Zeros the field variable */
	void zero();
	/** Standard assignment operator */
	GCLscalarfield& operator=(const GCLscalarfield&);
	//@{
	// Save field to an Antelope (Datascope) database.  
	// 
	// This routine writes the object attributes to a special table and 
	// writes the grid coordinate data to an output file.  Note the file
	// name used to store the grid is the same as the name parameter passed 
	// to this function.
	// The field variable data are written to a different file and 
	// stored indexed with a different database table.  
	// An odd feature of this routine is that if the directory name 
	// for the field passed to the function is empty (the string
	// equivalent of NULL) the grid is not saved.  This is a necessary
	// feature for saving multiple fields on the same parent grid
	// structure.  
	//
	// @throws int 
	// Exception is thrown if save failed.  
	// A simple int exception is used because errors are posted 
	// to the Antelope elog mechanism. 
	// If this exception is caught, call elog_complain to flush the 
	// elog buffer.
	//
	// @param db Antelope database pointer.
	// @param gclgdir directory to save gclgrid data.  If zero length, 
	// 	    grid data is not saved.
	// @param fielddir directory where field (val) data will be saved.  Can be the same as 
	//          gclgdir, but should not be zero length.
	// @param fieldname name assigned to this field.
	// @param dfile name of file to be used to save field (val) data.  Note the grid data 
	// 	    is always saved to a name based in the name attribure of the grid.
	//@}
	void dbsave(Dbptr db, string gclgdir, string fielddir, string fieldname,
			string dfile) throw(int);
	//@{
	// Add one field to another.  
	//
	// This acts like the += operator for simple types, but does so
	// for an entire field defined on a grid.  As for simple types the
	// field on the right hand side is accumulated to the field on
	// the left had side of the operator.  This works even if the
	// the two fields have different transformation properties and 
	// do not match in positions.  The most important limitation to 
	// understand, however, is basic sampling.  The field to contain
	// the accumulation needs to be as denser or more densely sampled
	// as the field on the right hand side to avoid aliasing effects.
	// This is exactly like time series sampling -- upsampling can just be
	// done by interpolation but downsampling requires an antialiasing
	// (smoother) filter.  
	//@}
	void operator+=(GCLscalarfield&);
	//@{
	// Multiply all field values by a constant scale factor.
	//
	// @param c constant by which the field is to be scaled.
	//@}
	void operator*=(double c);
	//@{
	// Linear interpolation function for a field.  
	//
	// This is one of the core algorithms described in detail in the
	// Fan and Pavlis (in review) paper.  It is intended as a low level
	// routine accessible to the user if desired.  Most applications, however,
	// will likely prefer to use a higher level application of this method
	// through something like the += operator.  It is CRITICAL to recognize
	// that this function must be called AFTER a previous call to lookup on
	// the same point.  This routine blindly uses the index stored in the 
	// object and will return total garbage if the lookup was not called or 
	// returned an error condition that was not handled.  i.e. for efficieny
	// this function simply assumes the interpolation requested will work.
	// For this reason it will never throw an exception.
	// @returns interpolated value of the field at the requested point.
	// @param x1p - Cartesian x1 coordinate of point to where the field is to be interpolated
	// @param x2p - Cartesian x2 coordinate of point to where the field is to be interpolated
	// @param x3p - Cartesian x3 coordinate of point to where the field is to be interpolated
	//@}
	double interpolate(double x1p, double x2p, double x3p);
	//@{
	//  stream output operator for a field.  
	//  Format is:
	//  <pre>
	//@@line 1:  n1 n2
	//@@line 2+:  x1, x2, x3, lat(deg), lon(deg), r(km), val in C output order 
	//@@		(index 2 most rapidly varying).
	//</pre>
	//@}
	friend ostream& operator << (ostream&,GCLscalarfield&);
};

/** Two-dimensional vector field defined on a GCLgrid framework. */
class GCLvectorfield : public GCLgrid
{
public:
	/** Number of components to vectors stored in this field */
	int nv;
	//@{
	// Vector field variable stored in a three-dimensional C array.
	// Index of the field is parallel with coordinate arrays x1, x2, and x3
	// that define grid point positions in space.  The last index is the 
	// vector component.  
	//@}
	double ***val;

	/** Default constructor.  */
	GCLvectorfield();
	//@{
	// Simple constructor.  Allocates space for x1, x2, and x3 arrays and initializes
	// other data attributes to zero.  
	//
	//  @param n1size number of grid points on generalized coordinate axis 1.
	//  @param n2size number of grid points on generalized coordinate axis 2.
	//  @param n3size number of grid points on generalized coordinate axis 3.
	//@}
	GCLvectorfield(int,int,int);
	/** Standard copy constructor. */
	GCLvectorfield(const GCLvectorfield&);
	//@{
	// Partial copy constructor cloning grid but not setting field variable.
	//
	// A common situation is to have a grid that is already defined that
	// needs to be cloned and have field variables set through some other
	// mechanism.  For example, one might create a standard grid and then
	// plan to load it with values from a different grid or compute values
	// of a field variable at the grid points.  
	//
	// @param g Grid to be cloned.
	//@}
	GCLvectorfield(GCLgrid &,int);
	//@{
	//  Antelope database driven constructor.  This is a specialized constructor that
	//  loads a field tagged with two names:  grid name and the field name (see below).
	//  The two components are stored separately in the external database using 
	//  two different tables.  This constructor has a fair amount of memory overhead
	//  as it loads a copy of the parent GCLgrid object before creating the field and
	//  then loading field variable data from the database.
	//
	//  The nvsize parameter has a complicated behaviour.  Most applications should 
	//  simply not use this parameter and allow the default specification of 0.
	//  When nzsize is 0 it is expected nv will be extracted from the database.
	//  The special case is if the field name is null.  In that case only the grid 
	//  geometry is extracted from the data base.  The val array is created and
	//  set to all zeros.  In this case nvsize determines the size of the vector field
	//  created.  In all other cases it is ignored.  
	//
	// @throws int 
	// Exception is thrown if there are any input problems.
	// A simple int exception is used because errors are posted 
	// to the Antelope elog mechanism. 
	// If this exception is caught, call elog_complain to flush the 
	// elog buffer.
	//  @param db Antelope database pointer.
	//  @param grdnm name of grid to be loaded from the database.
	//  @param fn name tag for the field to be loaded from the database.
	//  @param nvsize expected number of components for vectors in field.
	//           (default 0)
	//@}
	GCLvectorfield(Dbptr db, string grdnm, string fn,int nvsize=0); 
	/** Standard assignment operator. */
	GCLvectorfield& operator=(const GCLvectorfield&);
	//@{
	// Destructor.  
	// Note the same precautions about application of the default constructor as noted 
	// in @link GCLgrid::~GCLgrid @endlink
	//@}
	~GCLvectorfield();
	/** Zeros the field variable */
	void zero();
	//@{
	// Save field to an Antelope (Datascope) database.  
	// 
	// This routine writes the object attributes to a special table and 
	// writes the grid coordinate data to an output file.  Note the file
	// name used to store the grid is the same as the name parameter passed 
	// to this function.
	// The field variable data are written to a different file and 
	// stored indexed with a different database table.  
	// An odd feature of this routine is that if the directory name 
	// for the field passed to the function is empty (the string
	// equivalent of NULL) the grid is not saved.  This is a necessary
	// feature for saving multiple fields on the same parent grid
	// structure.  
	//
	// @throws int 
	// Exception is thrown if save failed.  
	// A simple int exception is used because errors are posted 
	// to the Antelope elog mechanism. 
	// If this exception is caught, call elog_complain to flush the 
	// elog buffer.
	//
	// @param db Antelope database pointer.
	// @param gclgdir directory to save gclgrid data.  If zero length, 
	// 	    grid data is not saved.
	// @param fielddir directory where field (val) data will be saved.  Can be the same as 
	//          gclgdir, but should not be zero length.
	// @param fieldname name assigned to this field.
	// @param dfile name of file to be used to save field (val) data.  Note the grid data 
	// 	    is always saved to a name based in the name attribure of the grid.
	//@}
	void dbsave(Dbptr db, string gclgdir, string fielddir, string fiendname, string dfile)
				throw(int);
	//@{
	// Add one field to another.  
	//
	// This acts like the += operator for simple types, but does so
	// for an entire field defined on a grid.  As for simple types the
	// field on the right hand side is accumulated to the field on
	// the left had side of the operator.  This works even if the
	// the two fields have different transformation properties and 
	// do not match in positions.  The most important limitation to 
	// understand, however, is basic sampling.  The field to contain
	// the accumulation needs to be as denser or more densely sampled
	// as the field on the right hand side to avoid aliasing effects.
	// This is exactly like time series sampling -- upsampling can just be
	// done by interpolation but downsampling requires an antialiasing
	// (smoother) filter.  
	//@}
	void operator+=(GCLvectorfield&);
	//@{
	// Multiply all field values by a constant scale factor.
	//
	// @param c constant by which the field is to be scaled.
	//@}
	void operator*=(double);
	//@{
	// Linear interpolation function for a field.  
	//
	// This is one of the core algorithms described in detail in the
	// Fan and Pavlis (in review) paper.  It is intended as a low level
	// routine accessible to the user if desired.  Most applications, however,
	// will likely prefer to use a higher level application of this method
	// through something like the += operator.  It is CRITICAL to recognize
	// that this function must be called AFTER a previous call to lookup on
	// the same point.  This routine blindly uses the index stored in the 
	// object and will return total garbage if the lookup was not called or 
	// returned an error condition that was not handled.  i.e. for efficieny
	// this function simply assumes the interpolation requested will work.
	// For this reason it will never throw an exception.
	// @returns pointer to (freshly allocated with new) interpolated value of the 
	// vector field at the requested point.  The user should call delete [] to 
	// release the memory allocated for this vector to avoid a memory leak.
	//
	// @param x1p - Cartesian x1 coordinate of point to where the field is to be interpolated
	// @param x2p - Cartesian x2 coordinate of point to where the field is to be interpolated
	// @param x3p - Cartesian x3 coordinate of point to where the field is to be interpolated
	//@}
	double *interpolate(double x1p, double x2p, double x3p);
	//@{
	//  stream output operator for a vector field.  
	//  Format is:
	//  <pre>
	//@@line 1:  n1 n2 nv
	//@@line 2+:  x1, x2, x3, lat(deg), lon(deg), r(km), val[0..nv-1] in C output order 
	//@@		(index 2 most rapidly varying).
	//</pre>
	//@}
	friend ostream& operator << (ostream&,GCLvectorfield&);
};
/** Three-dimensional scalar field defined on a GCLgrid framework. */
class GCLscalarfield3d : public GCLgrid3d 
{
public:
	//@{
	// Scalar field variable stored in a three-dimensional C array.
	// Index of the field is parallel with coordinate arrays x1, x2, and x3
	// that define grid point positions in space.
	//@}
	double ***val;

	/** Default constructor. */
	GCLscalarfield3d();
	//@{
	// Simple constructor.  Allocates space for x1, x2, x3, and val arrays and initializes
	// object data attributes to zero.  
	//
	//  @param n1size number of grid points on generalized coordinate axis 1.
	//  @param n2size number of grid points on generalized coordinate axis 2.
	//  @param n3size number of grid points on generalized coordinate axis 3.
	//@}
	GCLscalarfield3d(int n1size, int n2size, int n3size);
	/** Standard copy constructor. */
	GCLscalarfield3d(const GCLscalarfield3d&);
	//@{
	// Partial copy constructor cloning grid but not setting field variable.
	//
	// A common situation is to have a grid that is already defined that
	// needs to be cloned and have field variables set through some other
	// mechanism.  For example, one might create a standard grid and then
	// plan to load it with values from a different grid or compute values
	// of a field variable at the grid points.  
	//
	// @param g Grid to be cloned.
	//@}
	GCLscalarfield3d(GCLgrid3d &g);
	//@{
	//  Antelope database driven constructor.  This is a specialized constructor that
	//  loads a field tagged with two names:  grid name and the field name (see below).
	//  The two components are stored separately in the external database using 
	//  two different tables.  This constructor has a fair amount of memory overhead
	//  as it loads a copy of the parent GCLgrid object before creating the field and
	//  then loading field variable data from the database.
	//
	// @throws int 
	// Exception is thrown if there are any input problems.
	// A simple int exception is used because errors are posted 
	// to the Antelope elog mechanism. 
	// If this exception is caught, call elog_complain to flush the 
	// elog buffer.
	//
	//  @param db Antelope database pointer.
	//  @param grdnm name of grid to be loaded from the database.
	//  @param fn name tag for the field to be loaded from the database.
	//@}
	GCLscalarfield3d(Dbptr db, string grdnm, string fn);
	//@{
	// Destructor.  
	// Note the same precautions about application of the default constructor as noted 
	// in @link GCLgrid3d::~GCLgrid3d @endlink
	//@}
	~GCLscalarfield3d();
	/** Zeros field variable array */
	void zero();
	/** Standard assignment operator. */
	GCLscalarfield3d& operator=(const GCLscalarfield3d&);
	//@{
	// Save field to an Antelope (Datascope) database.  
	// 
	// This routine writes the object attributes to a special table and 
	// writes the grid coordinate data to an output file.  Note the file
	// name used to store the grid is the same as the name parameter passed 
	// to this function.
	// The field variable data are written to a different file and 
	// stored indexed with a different database table.  
	// An odd feature of this routine is that if the directory name 
	// for the field passed to the function is empty (the string
	// equivalent of NULL) the grid is not saved.  This is a necessary
	// feature for saving multiple fields on the same parent grid
	// structure.  
	//
	// @throws int 
	// Exception is thrown if save failed.  
	// A simple int exception is used because errors are posted 
	// to the Antelope elog mechanism. 
	// If this exception is caught, call elog_complain to flush the 
	// elog buffer.
	//
	// @param db Antelope database pointer.
	// @param gclgdir directory to save gclgrid data.  If zero length, 
	// 	    grid data is not saved.
	// @param fielddir directory where field (val) data will be saved.  Can be the same as 
	//          gclgdir, but should not be zero length.
	// @param fieldname name assigned to this field.
	// @param dfile name of file to be used to save field (val) data.  Note the grid data 
	// 	    is always saved to a name based in the name attribure of the grid.
	//@}
	void dbsave(Dbptr db, string gclgdir, 
			string fielddir, string fieldname, 
			string dfile) throw(int);
	//@{
	// Add one field to another.  
	//
	// This acts like the += operator for simple types, but does so
	// for an entire field defined on a grid.  As for simple types the
	// field on the right hand side is accumulated to the field on
	// the left had side of the operator.  This works even if the
	// the two fields have different transformation properties and 
	// do not match in positions.  The most important limitation to 
	// understand, however, is basic sampling.  The field to contain
	// the accumulation needs to be as denser or more densely sampled
	// as the field on the right hand side to avoid aliasing effects.
	// This is exactly like time series sampling -- upsampling can just be
	// done by interpolation but downsampling requires an antialiasing
	// (smoother) filter.  
	//@}
	void operator+=(GCLscalarfield3d&);
	//@{
	// Multiply all field values by a constant scale factor.
	//
	// @param c constant by which the field is to be scaled.
	//@}
	void operator*=(double c);
	//@{
	// Interpolate a 3d scalar field.
	//
	// Usage and caveats are the same as described in 
	//     @link GCLscalarfield::interpolate @endlink
	//@}
	double interpolate(double,double,double);
	//@{
	//  stream output operator for a 3d scalar field.  
	//  Format is:
	//  <pre>
	//@@line 1:  n1 n2 n3
	//@@line 2+:  x1, x2, x3, lat(deg), lon(deg), r(km), val in C output order 
	//@@		(index 3 most rapidly varying).
	//</pre>
	//@}
	friend ostream& operator << (ostream&,GCLscalarfield3d&);
};
/** Three-dimensional vector field defined on a GCLgrid framework. */
class GCLvectorfield3d : public GCLgrid3d
{
public:
	/** Number of components to each vector of the field. */
	int nv;
	//@{
	// Scalar field variable stored in a four-dimensional C array.
	// Index of the field is parallel with coordinate arrays x1, x2, and x3
	// that define grid point positions in space.  The fourth index defines
	// the component of each vector of the field.
	//@}
	double ****val;

	/** Default constructor. */
	GCLvectorfield3d();
	//@{
	// Simple constructor.  Allocates space for x1, x2, x3, and val arrays and initializes
	// object data attributes to zero.  
	//
	//  @param n1size number of grid points on generalized coordinate axis 1.
	//  @param n2size number of grid points on generalized coordinate axis 2.
	//  @param n3size number of grid points on generalized coordinate axis 3.
	//  @param nvsize number of components of vectors in this field
	//@}
	GCLvectorfield3d(int n1size, int n2size, int n3size, int nvsize);
	/** Standard copy constructor. */
	GCLvectorfield3d(const GCLvectorfield3d&);
	//@{
	// Partial copy constructor cloning grid but not setting field variable.
	//
	// A common situation is to have a grid that is already defined that
	// needs to be cloned and have field variables set through some other
	// mechanism.  For example, one might create a standard grid and then
	// plan to load it with values from a different grid or compute values
	// of a field variable at the grid points.  
	//
	// @param g Grid to be cloned.
	// @param nvsize number of vector components for vector field val array.
	//@}
	GCLvectorfield3d(GCLgrid3d &,int nvsize);
	//@{
	//  Antelope database driven constructor.  This is a specialized constructor that
	//  loads a field tagged with two names:  grid name and the field name (see below).
	//  The two components are stored separately in the external database using 
	//  two different tables.  This constructor has a fair amount of memory overhead
	//  as it loads a copy of the parent GCLgrid object before creating the field and
	//  then loading field variable data from the database.
	//
	//  The nvsize parameter has a complicated behaviour.  Most applications should 
	//  simply not use this parameter and allow the default specification of 0.
	//  When nzsize is 0 it is expected nv will be extracted from the database.
	//  The special case is if the field name is null.  In that case only the grid 
	//  geometry is extracted from the data base.  The val array is created and
	//  set to all zeros.  In this case nvsize determines the size of the vector field
	//  created.  In all other cases it is ignored.  
	//
	//
	// @throws int 
	// Exception is thrown if there are any input problems.
	// Also will throw an int exception if the nvsize requested does not match
	// tabulated number of vector components in database table.  
	// A simple int exception is used because errors are posted 
	// to the Antelope elog mechanism. 
	// If this exception is caught, call elog_complain to flush the 
	// elog buffer.
	//
	//  @param db Antelope database pointer.
	//  @param grdnm name of grid to be loaded from the database.
	//  @param fn name tag for the field to be loaded from the database.
	//  @param nvsize expected number of components for vectors in field.
	//@}
	GCLvectorfield3d(Dbptr db, string grdnm, string fn, int nvsize=0);
	//@{
	// Destructor.  
	// Note the same precautions about application of the default constructor as noted 
	// in @link GCLgrid3d::~GCLgrid3d @endlink
	//@}
	~GCLvectorfield3d();
	/** Zeros the field variable */
	void zero();
	/** Standard assignment operator. */
	GCLvectorfield3d& operator=(const GCLvectorfield3d&);
	//@{
	// Save field to an Antelope (Datascope) database.  
	// 
	// This routine writes the object attributes to a special table and 
	// writes the grid coordinate data to an output file.  Note the file
	// name used to store the grid is the same as the name parameter passed 
	// to this function.
	// The field variable data are written to a different file and 
	// stored indexed with a different database table.  
	// An odd feature of this routine is that if the directory name 
	// for the field passed to the function is empty (the string
	// equivalent of NULL) the grid is not saved.  This is a necessary
	// feature for saving multiple fields on the same parent grid
	// structure.  
	//
	// @throws int 
	// Exception is thrown if save failed.  
	// A simple int exception is used because errors are posted 
	// to the Antelope elog mechanism. 
	// If this exception is caught, call elog_complain to flush the 
	// elog buffer.
	//
	// @param db Antelope database pointer.
	// @param gclgdir directory to save gclgrid data.  If zero length, 
	// 	    grid data is not saved.
	// @param fielddir directory where field (val) data will be saved.  Can be the same as 
	//          gclgdir, but should not be zero length.
	// @param fieldname name assigned to this field.
	// @param dfile name of file to be used to save field (val) data.  Note the grid data 
	// 	    is always saved to a name based in the name attribure of the grid.
	//@}
	void dbsave(Dbptr db, string gclgdir, string fielddir, 
			string fieldname, string dfile) throw(int);
	//@{
	// Add one field to another.  
	//
	// This acts like the += operator for simple types, but does so
	// for an entire field defined on a grid.  As for simple types the
	// field on the right hand side is accumulated to the field on
	// the left had side of the operator.  This works even if the
	// the two fields have different transformation properties and 
	// do not match in positions.  The most important limitation to 
	// understand, however, is basic sampling.  The field to contain
	// the accumulation needs to be as denser or more densely sampled
	// as the field on the right hand side to avoid aliasing effects.
	// This is exactly like time series sampling -- upsampling can just be
	// done by interpolation but downsampling requires an antialiasing
	// (smoother) filter.  
	//@}
	void operator+=(GCLvectorfield3d&);
	//@{
	// Multiply all field values by a constant scale factor.
	//
	// @param c constant by which the field is to be scaled.
	//@}
	void operator*=(double);
	//@{
	// Interpolate a 3d scalar field.
	//
	// Usage and caveats are the same as described in 
	//     @link GCLvectorfield::interpolate @endlink
	//  @returns pointer to vector of doubles allocated with new []. 
	//  User must take caution to free this array to avoid memory leaks.
	//@}
	double *interpolate(double,double,double);
	//@{
	//  stream output operator for a 3d scalar field.  
	//  Format is:
	//  <pre>
	//@@line 1:  n1 n2 n3 nv
	//@@line 2+:  x1, x2, x3, lat(deg), lon(deg), r(km), val[0..nv-1] in C output order 
	//@@		(index 3 most rapidly varying).
	//</pre>
	//@}
	friend ostream& operator << (ostream&,GCLvectorfield3d&);
};
// Disabled for now:
// verbosity level.  0 terse, 1 verbose, 2 very verbose
// extern int GCLverbose;  
//@{
// Error object throw by some GCLgrid functions.  
//
// Any function that does not use Antelope routines in the GCLgrid library 
// will throw this object in the event of an exception.  
//@}
class GCLgrid_error
{
public:
/** This will contain the error message from the function which threw this error. */
string message;
/** Default constructor with generic message. */
GCLgrid_error(){message="GCLgrid library error\n";};
/** copy constructor */
GCLgrid_error(const string mess){message=mess;};
/** Routine to print the error message to stderr. */
virtual void log_error(){cerr<<"GCLgrid_error: "<<message<<endl;};
};
/*
//
//C++ helpers
//
*/
//@{
// Returns distance from the center of the Earth (km) of the standard ellipsoid at a specified latitude.
//
// The reference ellipsoid depends only on latitude.  A GCLgrid uses the reference ellipsoid
// as the reference datum to define a depth.  
//
// @returns distance (in kilometers) from the center of the Earth to the sea level geoid surface at the requested latitude.
// @param lat latitude (in radians) for which the standard ellipse radius is requested.  
//@}
double r0_ellipse(double);
//@{
//  retrieves a path along a gridline of a GCLgrid3d object.
//
//  see man(3) extract_gridline.
//@}
dmatrix *extract_gridline(GCLgrid3d& grid, int ix1, int ix2, int ix3, 
int comp, bool reverse) throw(GCLgrid_error);
//@{
//  Integrate a 3D field variable along a predefined path.
//
//  see man(3) pathintegral.
//@}
vector<double> pathintegral(GCLscalarfield3d& field,dmatrix& path)
                                throw(GCLgrid_error);
//@{
// Transformation from standard spherical to local coordinates.
//
//  see man(3) ustrans.
//@}
dmatrix ustrans(GCLgrid& g, double lat, double lon);
//@{
// Initialize a field with a layered structure.
//
// It is often useful to initialize a 3d field to a depth dependent 
// function.  This function uses 1d interpolation to take a 1d function 
// specified in a depth format and project it into all parts of a field.
// A typical example would be initializing a 3d velocity model in tomography
// to a 1d starting model.  
//
// @param field field to to initialized.
// @param val1d vector containing field variables at 1d grid points.
// @param z1d parallel vector to val1d containing depths (NOT Radius) of the 
//    grid points with values stored in the val1d vector.
// @param grad vector of gradients (forward looking) of field between 1d grid points.
//    (Use 0.0 for all values for constant val layer models).
//@}
void initialize_1Dscalar(GCLscalarfield3d& field, 
vector<double> val1d,vector<double> z1d,vector<double>grad);
//@{
// Overloaded version of function by same name.  
// 
// See long form for full description.  This form is used for block models
// where the gradient is forced to zero between 1d grid points.
//
// @see initialize_1Dscalar[0]
//@}
void initialize_1Dscalar(GCLscalarfield3d& field, 
vector<double> val1d,vector<double> z1d);
//@{
// Map a path from one grid coordinate system to another.
//
// Sometimes one has a line (path) in one grid that one wants to map into
// another.  If the two grids have identical transformation properties
// (can be determined with == or != operators) this is not necessary, but 
// if they are not congruent the points need to be converted between the
// two coordinate systems.  This function simplifies that process.
//
// @param parentgrid grid the path to be converted was originally defined in.
// @param path path defined as an 3 x n array of points in the coordinate 
// 	system of parentgrid.
// @param newpathgrid grid onto which path is to be mapped.
//
// @returns a 3 X n matrix of points in the newpathgrid coordinate system.
//@}
dmatrix remap_path(GCLgrid3d& parentgrid, dmatrix& path, GCLgrid3d& newpathgrid);
//@{
// Saves a 3d scalarfield to a stream in Data Explorer's native forma (dx).
//
// @param g grid to be written to output stream.
// @param out output stream.
//@}
void dx_output(GCLscalarfield3d& g, ostream& out);

extern "C" {


/*
//
// plain C helper function prototypes 
//
*/
//@{
// Allocate memory for a four dimensional array.
//
// The GCLgrid library uses a contiguous block of memory to hold 
// arrays and uses C pointers to create the indexing needed for
// standard C style subscripting.  
// A contiguous block is used in preference to a potentially 
// millions of malloc calls if the array were built in pieces.
// This function creates a 4d array in this manner.
// @param n1 size of index 1 of array.
// @param n2 size of index 2 of array.
// @param n3 size of index 3 of array.
// @param n4 size of index 4 of array.
//@}
double ****create_4dgrid_contiguous(int n1, int n2, int n3, int n4);
//@{
// Allocate memory for a three dimensional array.
//
// The GCLgrid library uses a contiguous block of memory to hold 
// arrays and uses C pointers to create the indexing needed for
// standard C style subscripting.  
// A contiguous block is used in preference to a potentially 
// millions of malloc calls if the array were built in pieces.
// This function creates a 3d array in this manner.
// @param n1 size of index 1 of array.
// @param n2 size of index 2 of array.
// @param n3 size of index 3 of array.
//@}
double ***create_3dgrid_contiguous(int n1, int n2, int n3);
//@{
// Allocate memory for a two dimensional array.
//
// The GCLgrid library uses a contiguous block of memory to hold 
// arrays and uses C pointers to create the indexing needed for
// standard C style subscripting.  
// This follows the pattern for 3 and 4d arrays for consistency,
// although the reasons for needing a contiguous block in this 
// case are not as important.
// This function creates a 2d array in this manner.
// @param n1 size of index 1 of array.
// @param n2 size of index 2 of array.
//@}
double **create_2dgrid_contiguous(int n1, int n2);
//@{
// Plain C destructor for a four-dimensional array.
//
// This a companion free function to destroy an array 
// created by @link create_4dgrid_contiguous @endlink .
// The the GCLgrid library it is hidden in the destructor
// for different objects.
//
// @param n1 size of index 1 of array.
// @param n2 size of index 2 of array.
// @param n3 size of index 3 of array.
//@}
void free_4dgrid_contiguous(double ****array,int n1, int n2, int n3);
//@{
// Plain C destructor for a three-dimensional array.
//
// This a companion free function to destroy an array 
// created by @link create_3dgrid_contiguous @endlink .
// The the GCLgrid library it is hidden in the destructor
// for different objects.
//
// @param n1 size of index 1 of array.
// @param n2 size of index 2 of array.
//@}
void free_3dgrid_contiguous(double ***array,int n1, int n2);
//@{
// Plain C destructor for a two-dimensional array.
//
// This a companion free function to destroy an array 
// created by @link create_2dgrid_contiguous @endlink .
// The the GCLgrid library it is hidden in the destructor
// for different objects.
//
// @param n1 size of index 1 of array.
//@}
void free_2dgrid_contiguous(double **array,int n1);
//@{
// FORTRAN function that does interpolation for a distorted cube.
//
// This is a core function for interpolation of a field using shape
// functions for a distorted cube element.  Computes a Jacobian to map
// actual geometry into a standard space and computes weights using the
// standard 8 point (cube) serendipity shape functions.
//
// @author Kagan Tuncay
// @param xx 3 vector defining point in space interpolation is requested
// @param coord  array holding actual coordinates of 8 corner points
// 		of distorted cube.  xx assumed to be inside this element.
// @param fun  return vector of length 8.  On return holds weights of 
// 		to used in sum of values of function on 8 corners.
//@}
void fme_weights_ (double *xx, double *coord, double *fun);
//@{
// Converts a true depth to depth in a flattened coordinate system.
//
// Some packages use the flattening transformation as an approximation
// map spherical shells into a Cartesian reference frame.  This function
// converts depths to flattened depths.
//
// @author Steve Roecker translation to C by Pavlis
// @param z - actual depth
// @returns depth with the flattening transformation applied to z
//@}
double flatz(double z);
//@{
// Inverse flattening transformation of depth.
//
// Some packages use the flattening transformation as an approximation
// map spherical shells into a Cartesian reference frame.  This function
// depths in the flattened earth coordinate system back to true depth.
//
// @author Steve Roecker translation to C by Pavlis
// @param z - flattened coordinate depth
// @returns actual depth in the earth
//@}
double uflatz(double z);
//@{
// Applies flattening transformation to a velocity.
//
// Some packages use the flattening transformation as an approximation
// map spherical shells into a Cartesian reference frame.  This function
// converts velocity at a given depth to the value with the flattening
// transformation applied.
//
// @author Steve Roecker translation to C by Pavlis
// @param v - actual velocity at depth z
// @param z - actual depth
// @returns velocity with the flattening transformation applied to v at z
//@}
double flatvel(double v,double z);
//@{
// Inverse flattening transformation of a velocity.
//
// Some packages use the flattening transformation as an approximation
// map spherical shells into a Cartesian reference frame.  This function
// converts velocity in a flattened coordinate system back to the true
// value at a depth that has to be determined from applying uflatz to z.  
//
// @author Steve Roecker translation to C by Pavlis
// @param v - flattened coordinate velocity
// @param z - flattened coordinate depth
// @returns true velocity with flattening correction removed
//@}
double uflatvel(double v, double z);
}
//@{
// Extract one component from a 2d vector field into a parallel scalar.
//
// It is often useful to extract one component from a vector field
// and treat that quantity as a scalar field.  This function 
// encapsulates that idea.  Note that it returns a pointer to a newly
// allocated scalarfield object that the caller must deal with.
// Use an auto_ptr to contain this pointer of just deal with the
// usual rules of handling dynamically allocated objects to avoid
// memory leaks.  
//
// @author Gary L. Pavlis
// @throws int exception and posts and error to elog if the requested
//      component is outside the range of the field.
//
// @param f input vector field to be converted.
// @param component component number to extract.  Assumes C convention
//      of 0 being the first component.  
//@}
GCLscalarfield *extract_component(GCLvectorfield& f,int component);
//@{
// Extract one component from a 2d vector field into a parallel scalar.
//
// It is often useful to extract one component from a vector field
// and treat that quantity as a scalar field.  This function 
// encapsulates that idea.  Note that it returns a pointer to a newly
// allocated scalarfield object that the caller must deal with.
// Use an auto_ptr to contain this pointer of just deal with the
// usual rules of handling dynamically allocated objects to avoid
// memory leaks.  
//
// @author Gary L. Pavlis
// @throws int exception and posts and error to elog if the requested
//      component is outside the range of the field.
//
// @param f input vector field to be converted.
// @param component component number to extract.  Assumes C convention
//      of 0 being the first component.  
//@}
GCLscalarfield3d *extract_component(GCLvectorfield3d& f,int component);
//@{
// Remap one grid to coordinate system of another.
//
// Sometimes it is useful when dealing with multiple grids to 
// allow an algorithm to make an assumption that all grids in the
// set have a common coordinate system.  This can avoid the
// overhead of conversion of points to and from geographical
// coordinates.  Experience has shown this is a nontrivial
// calculation and needs to be minimized for algorithms that
// might require large numbers of such conversions.  
// This function maps one grid to the coordinate system of the
// one passed as "pattern".  This transformation is defined
// in the BasicGCLgrid, lowest member of the heirarchy so it
// higher levels should be cast to a BasicGCLgrid to be used
// as a pattern.
//
// Note that remap_grid can be called on field objects derived
// from this one with no effect as the grid geometry is not
// altered.  Only the coordinate system changes.
// Note the grid is altered in place so the grid object passed will
// be modified after this function completes.  The exception is if
// the grids are already congruent in which case it silently does nothing.
//
// @author Gary L. Pavlis
//
// @param g grid to be remapped.
// @param pattern grid whose coordinate system is to be used for
//    new version of grid.  
//@}
void remap_grid(GCLgrid& g, BasicGCLgrid& pattern);
//@{
// Remap one grid to coordinate system of another.
//
// Sometimes it is useful when dealing with multiple grids to 
// allow an algorithm to make an assumption that all grids in the
// set have a common coordinate system.  This can avoid the
// overhead of conversion of points to and from geographical
// coordinates.  Experience has shown this is a nontrivial
// calculation and needs to be minimized for algorithms that
// might require large numbers of such conversions.  
// This function maps one grid to the coordinate system of the
// one passed as "pattern".  This transformation is defined
// in the BasicGCLgrid, lowest member of the heirarchy so it
// higher levels should be cast to a BasicGCLgrid to be used
// as a pattern.
//
// Note that remap_grid can be called on field objects derived
// from this one with no effect as the grid geometry is not
// altered.  Only the coordinate system changes.
// Note the grid is altered in place so the grid object passed will
// be modified after this function completes.  The exception is if
// the grids are already congruent in which case it silently does nothing.
//
// @author Gary L. Pavlis
//
// @param g grid to be remapped.
// @param pattern grid whose coordinate system is to be used for
//    new version of grid.  
//@}
void remap_grid(GCLgrid3d& g, BasicGCLgrid& pattern);
/*! \brief Decimate a GCLgrid3d.

We sometimes want to decimate a grid. This procedure does this for
a 3D grid with variable decimation for each generalized coordinate 
axis.  An added compliation is the fact that because GCLgrids hae
the x3 direction directed upward in earth coordinates and the free
surface is so special, we work the x3 coordinate backward compared
to the others.  i.e. we for the n3-1 point to be the n3-1 point in
the result, not the 0 points.  

\param g parent grid that is to be decimated
\param dec1 decimation factor for x1
\param dec2 decimation factor for x2
\param dec3 decimamtion factor for x3

\return pointer to decimated grid object 
*/
GCLgrid3d *decimate(GCLgrid3d& g,int dec1, int dec2, int dec3);
#endif

#  if (__STDC_VERSION__ >= 199901L) /* C99 */ || \
                               (defined(__SUNPRO_C) && defined(__C99FEATURES__))
#   define __func__ __func__
#  else
#   define __func__ "<unknown>"
#  endif
