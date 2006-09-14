#ifndef _SPHERICALCOORDINATE_H_
#define _SPHERICALCOORDINATE_H_
namespace SEISPP {
/*!
\brief  Encapsulates spherical coordinates in a data structure.

 Spherical coordinates come up in a lot of contexts in Earth Science data
 processing.  Note actual coodinate system can depend on context.
 For whole Earth models it can define global coordinates, but in three component
 seismograms the normal convention of geographical coordinates is always assumed.
\author Gary L. Pavlis
**/
typedef struct {
/*!
 Radius from center.
**/
        double radius;
/*!
 Zonal angle (from z) of spherical coordinates.  Units always assumed to be radians.
**/
        double theta;
/*!
 Azimuthal angle (from x) of spherical coordinates.  Units always assumed to be radians.
**/
        double phi;
} SphericalCoordinate;
/*!
 Returns a SphericalCoordinate data structure equivalent to one
 define dby a unit vector nu.
**/
SphericalCoordinate UnitVectorToSpherical(double nu[3]);
/*!
 Returns a unit vector (vector of 3 doubles) equivalent to
 direction defined in sphereical coordinates.
**/
double *SphericalToUnitVector(SphericalCoordinate& sc);
} // end namespace SEISPP declaration
#endif
