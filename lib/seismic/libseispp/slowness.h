#ifndef _SLOWNESS_H_
#define _SLOWNESS_H_
#include <string>
#include "stock.h"
#include "pf.h"
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
\param pf - Antelope pf pointer normally produced by earlier call to pfread
\param tag - name to search in pf to describe this grid object.`
              The parameters to describe the object are assumed encased in an 
              &Arr{ } construct with this tag.  This allows multiple grids to 
              be defined in a single parameter file with different tags. 
**/
	RectangularSlownessGrid(Pf *pf,string tag);
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
