#ifndef _REGIONALCOORDINATES_H_
#define _REGIONALCOORDINATES_H_

#include "gclgrid.h"
/* \brief Define a properly georefrenced regional coordinate system.

   At local and regional scales in the earth it is normally more convenient
to define a set of local, Cartesian coordinates than constantly deal with 
spherical geometry.  This object provides a general solution to this problem
using a set of local coordinates that can implicitly be switched at any time
to geographic coordinates.  The coordinates and the underlying algorithms
are identical to that used in the GCLgrid library. 

Note the geometry of the coordinate system generated is defined only by
two quantities (4 parameters):  (1) point on the globe that is to be the 
origin, and (2) an orientation angle.  This works because the x1-x2 plane is 
always made to be the local horizontal at the origin.  An angle is used to 
define how the standard x1=+East and x2=+North axes are rotated to define the
local coordinate frame.  Many applications will not want to rotate the 
axes, but this is convenient, for example, in the GCLgrid library to define
a volume not oriented to the cardinal directions. 
*/
class RegionalCoordinates
{
public:
    /* Default constructor.  Place holder you should not use. */
    RegionalCoordinates();
    /*! \brief Primary constructor.

       The local coordinate system requires only a definition of the point that
    on the earth to use as the origin of the coordinate system and a 
    direction to define one of the axes. This constructor uses those to build
    the object.

    \param olat is the origin latitude in radians
    \param olon is the origin longitude in radians
    \param oradius is the radius (in km) for the origin.  (Use the r0_ellipse
    procedure in libgclgrid to make this the surface with ellipticity).
    \param azn is the angle to rotate x2 from north to define the coordinate 
      system.  azn=0 means x2 will point north.  azn=M_PI/4 points the x2 
      axis at north 45 degrees east.  Thus, azn uses the geographic azimuth
      convention and is in radians.
    */
    RegionalCoordinates(double olat, double olon, double oradius, double azn);
    /*! Standard copy constructor.*/
    RegionalCoordinates(const RegionalCoordinates& parent);
    /*! Standard assignment operator. */
    RegionalCoordinates& operator=(const RegionalCoordinates& parent);
    /*! Return cartesian coordinates of a geographic point.
       \param lat is latitude in radians
       \param lon is longitude in radians
       \param r is radius in km.
       */
    Cartesian_point  cartesian(double lat,double lon, double r);
    /* Return cartesian coordinates of a geographic point.
       Point is specified by Geographic_point object. (radian and km units).
       Cartesian coordinates always have units of km.*/
    Cartesian_point  cartesian(Geographic_point gp);
    /*! \brief Return cartesian coordinates of a geographic point.

      This routine is an overloaded version of using a 3 vector to
      pass the geographic points.
      \param x - is a 3 vector holding Geographic point data to 
        be converted. Must be this order: t, x[1]=lon, x[2]=r.
        Angle must be in radians and r must be radius vector in km.
        */
    Cartesian_point cartesian(double x[3]);
    /* \brief Return geographic point description of cartesian input.

       This routine is one of several overloaded methods that convert
    a cartesian coordinate 3 vector to standard earth coordinates.  
    This and its siblings are the inverse of the cartesian methods. 
    This version passes the point as 3 distinct scalar arguments.

    \param x1 is the x1 coordinate of the point to convert.
    \param x2 is the x2 coordinate of the point to convert.
    \param x3 is the x3 coordinate of the point to convert.

    \return converted point in Geographic_point struct (radians and km units)
    */
    Geographic_point geographic(double x1,double x2, double x3);
    /* \brief Return geographic point description of cartesian input.

       This routine is one of several overloaded methods that convert
    a cartesian coordinate 3 vector to standard earth coordinates.  
    This and its siblings are the inverse of the cartesian methods. 
    This version passes the point as a standard C vector.

    \param x is a 3-vector containing Cartesian coordinates to convert.
    \return converted point in Geographic_point struct (radians and km units)
    */
    Geographic_point geographic(double x[3]);
    /* \brief Return geographic point description of cartesian input.

       This routine is one of several overloaded methods that convert
    a cartesian coordinate 3 vector to standard earth coordinates.  
    This and its siblings are the inverse of the cartesian methods. 
    This version uses a struct input through the Cartesian_point object.

    \param cp contains Cartesian coordinates to be converted.
    \return converted point in Geographic_point struct (radians and km units)
    */
    Geographic_point geographic(Cartesian_point cp);
    /*! Return the origin of the coordinate system. */
    Geographic_point origin();
    /*! Return the azimuth of the coordinate system which is the angle
      (in radians) of the azimuth (geographic NOT the spherical coordinate
      azimuth convention) where the y axis points at the origin.  (0 
      means the y (x2) axis points north at the origin)
      */
    double aznorth_angle(){return(azimuth_y);};
    /*! \brief Return transformation matrix to convert local azimuth to cartesian system.

      Standard local coordinates use a local east direction for x, a local
      north direction as y, and up as z.   Geologic examples include strike
      vectors, gps motion vectors, and fault normal vectors.   This 
      method returns a 3x3 transformation matrix that will convert a local
      x (E), y (N), z (up) vector to a vector in the cartesian system
      defined by this object.  Note this was intended for use on data
      measured at the surface so the matrix is always computed at a 
      sea level datum.   Elevation effects on real data should be totally
      negligible.

      \param lat - latitude in radians of the point to compute the
        transformation matrix.
      \param lon - longidude in radians of the point to compute the
              transformation matrix.
              */
    dmatrix l2rtransformation(double lat, double lon);
private:
    double gtoc_rmatrix[3][3];
    double translation_vector[3];
    double lat0;
    double lon0;
    double r0;
    double azimuth_y;
};
#endif
