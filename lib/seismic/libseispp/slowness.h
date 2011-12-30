#ifndef _SLOWNESS_H_
#define _SLOWNESS_H_
#include <string>
#include "stock.h"
#include "pf.h"
#include "Metadata.h"
namespace SEISPP
{
using namespace std;
/*! \brief Slowness vector object.  

 Slowness vectors are a seismology concept used to describe wave propagation.
 A slowness vector points in the direction of propagation of a wave with a
 magnitude equal to the slowness (1/velocity) of propagation.  
\author Gary L. Pavlis
**/

class SlownessVector
{
public:
/*!
 East-west component of slowness vector.
**/
	double ux;
/*!
 North-south component of slowness vector.
**/
	double uy;
/*!
 Default constructor.
**/
	SlownessVector();
/*! \brief Fully parameterized constructor.

A slowness vector is defined by it's components.  There is one ambiguity, however,
with a zero slowness vector.  That is, normally direction of propagation is
inferred from the vector azimuth.  A zero slowness vector has physical significance
(normal incidence) but presents and ambiguity in this regard.  We use a defaulted
az0 parameter to specify the azimuth that should be used if the magnitude of
slowness vector is 0.

\param ux0 - set x (EW) component to this value.
\param uy0 - set y (NS) component to this value.
\param az0 - use this as azimuth (radians) if this is a zero slowness vector
	(default 0.0)
*/
	SlownessVector(double ux0, double uy0, double az0=0.0);
/*!
 Copy constructor.
**/
	SlownessVector(const SlownessVector&);
/*!
 Computes the magntitude of the slowness vector.
 Value returned is in units of seconds/kilometer.  
**/
	double mag();
/*!
 Returns the propagation direction defined by a slowness vector.
 Azimuth is a direction clockwise from north in the standard geographic
 convention.  Value returned is in radians.
**/
	double azimuth();
/*!
 Returns the back azimuth direction defined by a slowness vector.
 A back azimuth is 180 degrees away from the direction of propagation and 
 points along the great circle path directed back to the source point 
 from a given position.  The value returned is in radians.
**/
	double baz();
/*! \brief Standard assignment operator. */
	SlownessVector& operator=(const SlownessVector& parent);
/* \brief Standard accumulation operator. */
	SlownessVector& operator+=(const SlownessVector& other);
/* \brief Standard subtract from  operator. */
	SlownessVector& operator-=(const SlownessVector& other);
/* \brief Standard addition  operator. */
        const SlownessVector operator+(const SlownessVector& other) const;
/* \brief Standard subraction  operator. */
        const SlownessVector operator-(const SlownessVector& other) const;
private:
	double azimuth0;
};
/*! \brief This object defines a uniform grid of points in slowness space.

 In array processing it is common to need a grid of feasible slowness 
 vectors and data are commonly stacked to define all of this family 
 of plane wave stacks.  The object used here grids up slowness space
 in uniform spacing in ux and uy components.  
 The gridding is defined by the lower left corner (uxlow, uylow), 
 spacing in EW and NS directions (dux, duy), and the number of 
 points in the x (EW) and y (NS) directions.  
 Note the grid and all methods assume units of s/km, although 
 this application is largely blind to units.
\author Gary L. Pavlis
**/
class RectangularSlownessGrid
{
public:
/*!
 Name assigned to this grid object.
**/
	string name;
/*!
 Minimum ux (East-west) component of grid.
 The location of the lower left corner of the grid defined by this object is
 at coordinates (uxlow,uylow).
**/
	double uxlow;
/*!
 Minimum uy (North-south) component of grid.
 The location of the lower left corner of the grid defined by this object is
 at coordinates (uxlow,uylow).
**/
	double uylow;
/*!
 Grid size (s/km) of slowness grid in EW component.  
**/
	double dux;
/*!
 Grid size (s/km) of slowness grid in NS component.  
**/
	double duy;
/*!
 Number of grid points in x (EW) direction of slowness grid.
**/
	int nux; 
/*!
 Number of grid points in y (NW) direction of slowness grid.
**/
	int nuy;
/*!
 Default constructor.
**/
	RectangularSlownessGrid();  // generic default is defined
/*!
 Fully parameterized constructor.

\param nm - name to be assigned to grid.
\param uxl - lower left corner ux.
\param uyl - lower left corner uy.
\param dux - dux (increment in x direction)
\param duy - duy (increment in y direction)
\param nx - number of grid points in x direction.
\param ny - number of grid points in y direction.
**/
	RectangularSlownessGrid(string nm, double uxl, double uyl,
		double dux,double duy,int nx, int ny);
/*!
 Parameter file driven constructor.

 The following parameters are fetched:
  "Slowness_Grid_Name" (string), "uxlow" (real), "uylow" (real),
  "nux" (int), "nuy" (int), "dux" (real), and "duy" (real).
  They define an nux by nuy regular grid in slowness space with 
  spacing dux and duy respectively with the lower left corner of the
  grid at (uxlow,uylow).  The name is just a tag.

\param pf - Antelope pf pointer normally produced by earlier call to pfread
\param tag - name to search in pf to describe this grid object.`
              The parameters to describe the object are assumed encased in an 
              &Arr{ } construct with this tag.  This allows multiple grids to 
              be defined in a single parameter file with different tags. 
\exception MetadataGetError (child of SeisppError) is thrown if the 
  required attributes are not defined in the Metadata object passed.
**/
	RectangularSlownessGrid(Pf *pf,string tag);
/* \brief Metadata driven constructor.

This is similar to the Pf driven method except the data is passed
through a more generic object in SEISPP called Metadata.  Keywords
defined in the Metadata object are identical to those in the pf version.
As for the pf constructor the required parameters are:
  "Slowness_Grid_Name" (string), "uxlow" (real), "uylow" (real),
  "nux" (int), "nuy" (int), "dux" (real), and "duy" (real).
  They define an nux by nuy regular grid in slowness space with 
  spacing dux and duy respectively with the lower left corner of the
  grid at (uxlow,uylow).  The name is just a tag.

\param mdin is the Metadata object with attributes set to build this object.
\exception MetadataGetError (child of SeisppError) is thrown if the 
  required attributes are not defined in the Metadata object passed.
  */
        RectangularSlownessGrid(Metadata& mdin);
/*!
 Standard copy constructor.
**/
	RectangularSlownessGrid(const  RectangularSlownessGrid&);
/*!
 Returns x component of slowness grid at index position i.
**/
	double ux(int i) {return(uxlow+i*dux);};
/*!
 Returns y component of slowness grid at index position j.
**/
	double uy(int i) {return(uylow+i*duy);};
/*!
 Returns a slowness grid object for grid position (i,j).
\exception SeisppError object if i and j are outside range.
**/
	SlownessVector slow(int i, int j);
};

} // End SEISPP namespace declaration
#endif
