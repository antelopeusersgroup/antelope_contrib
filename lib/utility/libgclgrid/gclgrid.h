#include "db.h"
/* This include file contains a set of typedef definitions that
define objects used for what I call geographical curvilinear 
coordinates.  That means coordinates defined wrt a geographical
reference frame, but which are generalized coordinates (i.e. not
cartesian and not everywhere defining orthogonal basis vectors.)

Author:  G Pavlis
June, 2000
*/

/* This structure defines a geographical curvilinear grid.  
(gclgrid)  Geometrically this object should be thought of 
as a distorted box filled with distorted parallelepipeds (bricks).
The geographic part enters because the grid is referenced to 
points on the earth through two different mechanisms.  First,
the lat0, lon0, and r0 parameters define the position on the 
earth of the coordinate system origin defined for this grid.
One should imagine a cartesian coordinate system at this point
on the earth with the + yaxis of that coordinate system pointing
in the local direction azimuth_y.  The x axis is assumed to 
rotate by teh same angle and the z axis is always assumed to be 
the local vertical.  The 3d arrays (x1,x2,x3) then define the
mapping of points in the 3d grid onto this local cartesian
coordinate sytem.  A simple example is correctly mapping a 
spherical shell for say the crust onto a grid.  The locations 
in x,y,z are not found directly on the trajectories of the 
x y an z axes because of earth curvature.  The defining objects
can be more complex, however, and the cells may not necessarily
be at all parallelpiped like objects.  For example, ray paths in
z define a complex mapping in the vertical direction.  

The other way the mapping is defined geographically is through the
parallel arrays lat, lon, and r.  These define the location on the
earth in standard coordinates for each point in the grid defined
in the cartesian mapping x1, x2, and x3.  A program using this 
object should allow use of the cartesian or geographical form
or both.  It should handle gracefully one or the other missing 
by something simple like initializing the *** pointer to NULL.
However, to make this clearer I've added the variables 
cartesian_defined and geographic_defined that should be set to 
1 if the respective arrays are filled and 0 otherwise.  
(i.e. they are logical variables to be used in a simple, fast test
to see if they are defined.)

There are 2d and 3d versions of this structure.  The 2d form is
assumed to have the depth coordinate of the earth removed.
It can be thought of as the case with n3=1.  Datum for the 2d
grid is defined by the r0 variable in the structure rather than
giving it a special name.  That is, the origin r0 defines depth
datum for this grid.   The Cartesian form is allowed to be defined,
but these are now only 2d arrays instead of 3 because they are
only mapping a surface.  We have to keep all three components 
of the cartesian space, however, since the surface is expected
to normally define a regular grid on a spherical surface.
We define these two structures side by side because a common
method could be to extract a 2d slice from a 3d grid.  Note
that r0 should only be thought of as radius at the origin.
If a full ellipsoid correction is used r will vary with 
latitude.  This is allowed by letting r be a 2 matrix of 
radii values for the 2d case.

Note all angle terms are stored internally in radians.
e.g. lat0, lon0, etc.
*/
typedef struct gcl3dgrid_ {
	char name[10];  /* name assigned to this coordinate grid*/
	double lat0, lon0, r0;  /* geographical location of origin */
	double azimuth_y;  /* Azimuth of positive y axis */
	double dx1_nom, dx2_nom, dx3_nom;  /* nominal grid spacing */
	int n1,n2,n3;  /* grid size in each component direction*/
	int i0, j0, k0;  /* origin location in grid */
	double xlow, xhigh, ylow, yhigh, zlow, zhigh;/* bounding box */
	double ***x1, ***x2, ***x3;
	double ***lat, ***lon, ***r;
	int cartesian_defined, geographic_defined;  
} GCL3Dgrid;
typedef struct gcl2dgrid_ {
	char name[10];  /* name assigned to this coordinate grid*/
	double lat0, lon0, r0;  /* geographical location of origin */
	double azimuth_y;  /* Azimuth of positive y axis */
	double dx1_nom, dx2_nom;  /* nominal grid spacing */
	int n1,n2;  /* grid size in each component direction*/
	int i0, j0;  /* origin location in grid */
	double xlow, xhigh, ylow, yhigh, zlow, zhigh;/* bounding box */
	double **x1, **x2, **x3;
	double **lat, **lon, **r;
	int cartesian_defined, geographic_defined;  
} GCL2Dgrid;

/* function prototypes */
double ***create_3dgrid_contiguous(int n1, int n2, int n3);
void free_3dgrid_contiguous(double ***x,int n1, int n2);
double **create_2dgrid_contiguous(int n1, int n2);
void free_2dgrid_contiguous(double **x,int n1);
GCL3Dgrid *GCL3Dgrid_load_db(Dbptr db,char *gridname);
GCL2Dgrid *GCL2Dgrid_load_db(Dbptr db,char *gridname);
int save_3dgclgrid(GCL3Dgrid *g,Dbptr db,char *dir);
int save_2dgclgrid(GCL2Dgrid *g,Dbptr db,char *dir);
int GCL3Dgrid_index_lookup(GCL3Dgrid *, double, double, double, 
	int *, int *, int *);
int GCL3Dgrid_interpolate(double *,GCL3Dgrid *,int, int, int, 
	double ****, double *, int);
int map_full_grid(GCL3Dgrid *,double ****,GCL3Dgrid *,double ****,
	int, int ***);
void GCL3D_grid_stack(double ***, double ***, int ***, int ***,
	int, int, int);
double r0_ellipse(double);
double r_to_depth(double, double);
