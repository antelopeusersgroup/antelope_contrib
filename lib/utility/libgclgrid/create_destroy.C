#include <string.h>
#include <math.h>
#include "gclgrid.h"
/* These are create and free routines for 2d, 3d, and 4d arrays in plain C.
They are used below for C++ constructors and destructors.  The MOST 
IMPORTANT thing to note about the constructions used here is that I have
implemented these built on contiguous blocks of memory.  This is NOT necessarily
the way one would always build multidimension arrays in C.  This is probably a bad
idea for future maintainability of this code as someone might be tempted to
write their own destructor or add a constructor that used a different memory model.
Future users beware on this point.  I did this on purpose, however, under
and assumption it would improve performance.*/


/* Many gcl functions work with 3d C grids.  C implements multidimensioned
arrays pointer arrays, which makes this kind of ugly.  Anyway, this
function will create the F90 equivalent of  x[0:n1-1][0:n2-1][0:n3-1]
and return a ***double pointer for the 3d array.  The approach 
used here allocates the full block of memory in one single call to 
guarantee the result is in a contiguous block of memory. 
This is useful for reading and writing this type of entity because
then a single call to fread and fwrite can be used.  
Note, however, that the actual order in memory remains the C convention
not the FOTRAN form.  i.e. the rightmost index defines members contingous
in memory.
Author:  GAry Pavlis
Written:  June 2000
*/

double ***create_3dgrid_contiguous(int n1, int n2, int n3)
{
	double ***ptr3d;
	double **ptr2ptr;
	double *ptr;
	int i,j;

	allot(double *,ptr,n1*n2*n3);
	allot(double ***,ptr3d,n1);
	for(i=0;i<n1;++i)
	{
		allot(double **,ptr2ptr,n2);
		ptr3d[i] = ptr2ptr;
	}
	for(i=0;i<n1;++i)
	{
		for(j=0;j<n2;++j)
		{
			ptr3d[i][j] = ptr + n2*n3*i + n3*j;
		}
	}
	return(ptr3d);
}		
/* Free routine for a 3d array.  pointers arrays require extra attention
to avoid a memory leak.   This function assumes standard C indexing
*/
void free_3dgrid_contiguous(double ***x,int n1, int n2)
{
	int i;
	double *ptr;

	/* this clears the work space*/
	ptr = x[0][0];
	free((void *)ptr);

	/* The pointer arrays are still then and have to be freed 
	seperately */
	for(i=0;i<n1;++i)  free((void *)x[i]);
	free((void *)x);
}
/* parallel routines to the above for 2d */
double **create_2dgrid_contiguous(int n1, int n2)
{
	double **ptr2ptr;
	double *ptr;
	int i;

	allot(double *,ptr,n1*n2);
	allot(double **,ptr2ptr,n1);
	for(i=0;i<n1;++i)
		ptr2ptr[i] = ptr + n2*i;
	return(ptr2ptr);
}
void free_2dgrid_contiguous(double **x,int n1)
{
	double *ptr;

	ptr = x[0];
	free((void *)ptr);
	free((void *)x);
}
//
// A bit out of order, but now similar construction for 4D
//
double ****create_4dgrid_contiguous(int n1, int n2, int n3, int n4)
{
	double ****ptr4d;
	double ***ptr3d;
	double **ptr2ptr;
	double *ptr;
	int i,j,k;

	allot(double *,ptr,n1*n2*n3*n4);
	allot(double ****,ptr4d,n1);
	for(i=0;i<n1;++i)
	{
		allot(double ***,ptr3d,n2);
		ptr4d[i] = ptr3d;
		for(j=0;j<n2;++j)
		{
			allot(double **,ptr2ptr,n3);
			ptr4d[i][j]=ptr2ptr;
		}
	}
	for(i=0;i<n1;++i)
	{
		for(j=0;j<n2;++j)
		{
			for(k=0;k<n3;++k)
			{
				ptr4d[i][j][k] = ptr + n2*n3*n4*i 
						+ n3*n4*j + n4*k;
			}
		}
	}
	return(ptr4d);
}
void free_4dgrid_contiguous(double ****x,int n1, int n2, int n3)
{
	int i,j;
	double *ptr;

	/* this clears the work space*/
	ptr = x[0][0][0];
	free((void *)ptr);

	/* unwind and release the pointer arrays -- pretty ugly this deep */
	for(i=0;i<n1;++i)
	{
		for(j=0;j<n2;++j)
		{
			free((void *)x[i][j]);
		}
		free((void *)x[i]);
	}
	free((void *)x);
}		
//
// C++ constructors 
//
GCLgrid::GCLgrid (int n1size, int n2size)
{
	name[0]='\0';
	lat0=0.0; lon0=0.0; r0=0.0;
	azimuth_y=0.0;  dx1_nom=0.0;  dx2_nom=0.0;
	i0=0;  j0=0;
	n1=n1size;
	n2=n2size;
	x1=create_2dgrid_contiguous(n1size,n2size);
	x2=create_2dgrid_contiguous(n1size,n2size);
	x3=create_2dgrid_contiguous(n1size,n2size);
	lat=create_2dgrid_contiguous(n1size,n2size);
	lon=create_2dgrid_contiguous(n1size,n2size);
	r=create_2dgrid_contiguous(n1size,n2size);
	cartesian_defined=0;  // these are only set high when valid data loaded
	geographic_defined=0;
}

GCLgrid3d::GCLgrid3d (int n1size, int n2size, int n3size)
{
	name[0]='\0';
	lat0=0.0; lon0=0.0; r0=0.0;
	azimuth_y=0.0;  dx1_nom=0.0;  dx2_nom=0.0; dx3_nom=0.0;
	i0=0;  j0=0;  k0=0;
	n1=n1size;
	n2=n2size;
	n3=n3size;
	x1=create_3dgrid_contiguous(n1size,n2size,n3size);
	x2=create_3dgrid_contiguous(n1size,n2size,n3size);
	x3=create_3dgrid_contiguous(n1size,n2size,n3size);
	lat=create_3dgrid_contiguous(n1size,n2size,n3size);
	lon=create_3dgrid_contiguous(n1size,n2size,n3size);
	r=create_3dgrid_contiguous(n1size,n2size,n3size);
	cartesian_defined=0;
	geographic_defined=0;
}
//  Copy constructor is far more complex and here it is
GCLgrid::GCLgrid(const GCLgrid& g)
{
	int i,j;
	//
	//I think the books officially say copying the scalars defaulted
	//I, however, have found depending on subtle standard features
	//dangerous so  I'll do this explicitly.
	//
	strcpy(name, g.name);
	lat0=g.lat0;
	lon0=g.lon0;
	r0=g.r0;
	azimuth_y=g.azimuth_y;
	dx1_nom=g.dx1_nom;
	dx2_nom=g.dx2_nom;
	n1=g.n1;
	n2=g.n2;
	i0=g.i0;
	j0=g.j0;
	x1low=g.x1low;
	x1high=g.x1high;
	x2low=g.x2low;
	x2high=g.x2high;
	x3low=g.x3low;
	x3high=g.x3high;
	cartesian_defined=g.cartesian_defined;
	geographic_defined=g.geographic_defined;
	x1=create_2dgrid_contiguous(n1,n2);
	x2=create_2dgrid_contiguous(n1,n2);
	x3=create_2dgrid_contiguous(n1,n2);
	lat=create_2dgrid_contiguous(n1,n2);
	lon=create_2dgrid_contiguous(n1,n2);
	r=create_2dgrid_contiguous(n1,n2);
	//
	//I use separate loops for each array here as this is highly
	//optimized on most compilers 
	//
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) x1[i][j]=g.x1[i][j];
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) x2[i][j]=g.x2[i][j];
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) x3[i][j]=g.x3[i][j];
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) lat[i][j]=g.lat[i][j];
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) lon[i][j]=g.lon[i][j];
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) r[i][j]=g.r[i][j];

}
GCLgrid3d::GCLgrid3d(const GCLgrid3d& g)
{
	int i,j,k;
	strcpy(name,g.name);
	lat0=g.lat0;
	lon0=g.lon0;
	r0=g.r0;
	azimuth_y=g.azimuth_y;
	dx1_nom=g.dx1_nom;
	dx2_nom=g.dx2_nom;
	dx3_nom=g.dx3_nom;
	n1=g.n1;
	n2=g.n2;
	n3=g.n3;
	i0=g.i0;
	j0=g.j0;
	x1low=g.x1low;
	x1high=g.x1high;
	x2low=g.x2low;
	x2high=g.x2high;
	x3low=g.x3low;
	x3high=g.x3high;
	cartesian_defined=g.cartesian_defined;
	geographic_defined=g.geographic_defined;
	x1=create_3dgrid_contiguous(n1,n2,n3);
	x2=create_3dgrid_contiguous(n1,n2,n3);
	x3=create_3dgrid_contiguous(n1,n2,n3);
	lat=create_3dgrid_contiguous(n1,n2,n3);
	lon=create_3dgrid_contiguous(n1,n2,n3);
	r=create_3dgrid_contiguous(n1,n2,n3);
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) x1[i][j]=g.x1[i][j];
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) x2[i][j]=g.x2[i][j];
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) x3[i][j]=g.x3[i][j];
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) lat[i][j]=g.lat[i][j];
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) lon[i][j]=g.lon[i][j];
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) r[i][j]=g.r[i][j];

}
//
//  assignment operator is almost like the copy constructor but 
//  we protect against self assignment.
//
GCLgrid& GCLgrid::operator=(const GCLgrid& g)
{
	if(this != &g)  // avoid self assignment
	{
		int i,j;
		strcpy(name,g.name);
		lat0=g.lat0;
		lon0=g.lon0;
		r0=g.r0;
		azimuth_y=g.azimuth_y;
		dx1_nom=g.dx1_nom;
		dx2_nom=g.dx2_nom;
		n1=g.n1;
		n2=g.n2;
		i0=g.i0;
		j0=g.j0;
		x1low=g.x1low;
		x1high=g.x1high;
		x2low=g.x2low;
		x2high=g.x2high;
		x3low=g.x3low;
		x3high=g.x3high;
		cartesian_defined=g.cartesian_defined;
		geographic_defined=g.geographic_defined;
		x1=create_2dgrid_contiguous(n1,n2);
		x2=create_2dgrid_contiguous(n1,n2);
		x3=create_2dgrid_contiguous(n1,n2);
		lat=create_2dgrid_contiguous(n1,n2);
		lon=create_2dgrid_contiguous(n1,n2);
		r=create_2dgrid_contiguous(n1,n2);
		//
		//I use separate loops for each array here as this is highly
		//optimized on most compilers 
		//
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) x1[i][j]=g.x1[i][j];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) x2[i][j]=g.x2[i][j];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) x3[i][j]=g.x3[i][j];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) lat[i][j]=g.lat[i][j];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) lon[i][j]=g.lon[i][j];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) r[i][j]=g.r[i][j];
	
	}
	return *this;
}
//
//Same for 3D grid
//
GCLgrid3d& GCLgrid3d::operator=(const GCLgrid3d& g)
{
	if(this != &g)  // avoid self assignment
	{
		int i,j,k;
		strcpy(name,g.name);
		lat0=g.lat0;
		lon0=g.lon0;
		r0=g.r0;
		azimuth_y=g.azimuth_y;
		dx1_nom=g.dx1_nom;
		dx2_nom=g.dx2_nom;
		dx3_nom=g.dx3_nom;
		n1=g.n1;
		n2=g.n2;
		n3=g.n3;
		i0=g.i0;
		j0=g.j0;
		x1low=g.x1low;
		x1high=g.x1high;
		x2low=g.x2low;
		x2high=g.x2high;
		x3low=g.x3low;
		x3high=g.x3high;
		cartesian_defined=g.cartesian_defined;
		geographic_defined=g.geographic_defined;
		x1=create_3dgrid_contiguous(n1,n2,n3);
		x2=create_3dgrid_contiguous(n1,n2,n3);
		x3=create_3dgrid_contiguous(n1,n2,n3);
		lat=create_3dgrid_contiguous(n1,n2,n3);
		lon=create_3dgrid_contiguous(n1,n2,n3);
		r=create_3dgrid_contiguous(n1,n2,n3);
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<n3;++k) x1[i][j][k]=g.x1[i][j][k];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<n3;++k) x2[i][j][k]=g.x2[i][j][k];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<n3;++k) x3[i][j][k]=g.x3[i][j][k];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<n3;++k) lat[i][j][k]=g.lat[i][j][k];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<n3;++k) lon[i][j][k]=g.lon[i][j][k];
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j) 
				for(k=0;k<n3;++k) r[i][j][k]=g.r[i][j][k];
	
	}
	return *this;
}

/* This small function builds a baseline vector of latitude and
longitude values along a specified azimuth with a prescribed 
origin.

Arguments:
	lat0, lon0 - origin in radians
	phi - angle to rotate the x axis (+ east) baseline.  That is
		the baseline is established as a great circle passing
		through the origin point with an angle phi measured positive
		northward from east.  (counterclock looking from above)
		Note that away from the origin this angle relative to north
		will change.
	dx - distance spacing between points on the baseline (in radians)
	n - number of points along the baseline.
	i0 - position of the origin point along the baseline vector. 
		NOTE:  we use C convention so 0 is the first point in
		the vector.
	lat, lon - hold n vectors of latitude, longitude pairs of the 
		baseline points.  Note that lat[i0], lon[i0] should 
		equal lat0, lon0 respectively.
Author:  G Pavlis
Written:  Aug 2000
*/

void build_baseline(double lat0, double lon0, double phi, 
			double dx,int n,int i0,
			double *lat, double *lon)
{
	double azimuth,az;
	int i;
	double delta;
	
	/* We want azimuth between 0 and 2*pi */
	azimuth = M_PI_2 - phi;
	if(azimuth < 0.0 ) azimuth += (2.0*M_PI);

	for(i=0,delta=dx*((double)-i0);i<n;++i,delta+=dx)
	{
		if(i<i0)
			latlon(lat0,lon0,fabs(delta),azimuth-M_PI,lat+i,lon+i);
		else if(i==i0)
		{
			lat[i] = lat0;
			lon[i] = lon0;
		}
		else
		{
			latlon(lat0,lon0,delta,azimuth,lat+i,lon+i);
		}
	}
}
//
// This is the C++ constructor that uses a pf and coord routines to compute
// a GCLgrid on the fly using the description coming from pf
//
GCLgrid::GCLgrid(int n1in, int n2in, 
	char *namein, 
	double lat0in, double lon0in, double r0in,
	double azimuth_yin, double dx1_nomin, double dx2_nomin, 
	int i0in, int j0in)

{		

	/* pole to baseline */
	double pole_lat, pole_lon;
	int i,j,k;
	double deltax, deltay, delta;  
	double z;
	double x[3],x0[3];
	double xwork[3];
	double rotation_angle,azimuth_i;
	double dx1_rad,dx2_rad;

	strcpy(name,namein);
	lat0=lat0in;
	lon0=lon0in;
	r0 = r0_ellipse(lat0);
	azimuth_y=azimuth_yin;
	rotation_angle=-azimuth_y;
	dx1_nom=dx1_nomin;
	dx2_nom=dx2_nomin;
	dx1_rad = dx1_nom/r0;
	dx2_rad = dx2_nom/r0;
	n1=n1in;
	n2=n2in;
	i0=i0in;
	j0=j0in;

	/* These are the vector of lat and lons used to define the
	baseline the effectively forms the equator of of the rotated
	coordinates passing through the origin*/
	double *baseline_lat=new double[n1in];
	double *baseline_lon=new double[n1in];

	build_baseline(lat0,lon0,
		rotation_angle,dx1_rad,n1,i0,baseline_lat,baseline_lon);
	/* We need to compute the pole to our base line to use as a 
	target for grid lines that are locally perpendicular along
	the grid lines -- like longitude lines at the equator */
	latlon(lat0,lon0,M_PI_2,-rotation_angle,&pole_lat,&pole_lon);

	/* We now allocate memory for the large coordinate arrays
	themselves and set the elements of the structure */
	lat = create_2dgrid_contiguous(n1,n2);
	lon = create_2dgrid_contiguous(n1,n2);
	r = create_2dgrid_contiguous(n1,n2);
	x1 = create_2dgrid_contiguous(n1,n2);
	x2 = create_2dgrid_contiguous(n1,n2);
	x3 = create_2dgrid_contiguous(n1,n2);
	cartesian_defined = 1;
	geographic_defined = 1;

	/* We now complete the latitude/longitude grid by projecting
	lines from the baseline toward the computed pole.  Grid points 
	will be equally spaced along the gcp toward the pole, but the
	lines they form will converge.  This is complicated greatly by
	the grid origin parameters.  We could make this a function,
	but it is simpler to just write it inline.*/
	for(i=0;i<n1;++i)
	{
		dist(baseline_lat[i],baseline_lon[i],pole_lat, pole_lon,
			&delta,&azimuth_i);
		if(azimuth_i < 0.0) azimuth_i += (2.0*M_PI);
		delta = dx2_rad*(-(double)j0);
		for(j=0;j<n2;++j,delta+=dx2_rad)
		{
			if(j<j0)
				latlon(baseline_lat[i],baseline_lon[i],
					fabs(delta),azimuth_i-M_PI,
					lat[i]+j,lon[i]+j);
			else if(j==j0)
			{
				lat[i][j] = baseline_lat[i];
				lon[i][j] = baseline_lon[i];
			}
			else
				latlon(baseline_lat[i],baseline_lon[i],
					fabs(delta),azimuth_i,
					lat[i]+j,lon[i]+j);
			r[i][j] = r0_ellipse(lat[i][j]);
		}
	}
	//
	// This function standarizes setting the tranformation from geo to cartesian
	// This then sets the Cartesian section
	//
	set_transformation_matrix();
	Cartesian_point cpt;
	for(i=0;i<n1;++i) 
	    for(j=0;j<n2;++j)
	    {
			cpt = gtoc(lat[i][j],lon[i][j],
					r[i][j]);

			x1[i][j] = cpt.x1;
			x2[i][j] = cpt.x2;
			x3[i][j] = cpt.x3;
	    }
	/* We have to compute the extents parameters as the minimum 
	and maximum in each cartesian direction */
	x1low=x1[0][0];
	x1high=x1[0][0];
	x2low=x2[0][0];
	x2high=x2[0][0];
	x3low=x3[0][0];
	x3high=x3[0][0];
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j)
		{
			x1low = MIN(x1[i][j],x1low);
			x1high = MAX(x1[i][j],x1high);
			x2low = MIN(x2[i][j],x2low);
			x2high = MAX(x2[i][j],x2high);
			x3low = MIN(x3[i][j],x3low);
			x3high = MAX(x3[i][j],x3high);
		}
	x1low = x1low;
	x2low = x2low;
	x3low = x3low;
	x1high = x1high;
	x2high = x2high;
	x3high = x3high;

	delete baseline_lat;
	delete baseline_lon;
}
//
// This is the C++ constructor that uses a pf and coord routines to compute
// a GCLgrid on the fly using the description coming from pf
//
GCLgrid3d::GCLgrid3d(int n1in, int n2in, int n3in,
	char *namein, 
	double lat0in, double lon0in, double r0in,
	double azimuth_yin, double dx1_nomin, double dx2_nomin,double dx3_nomin, 
	int i0in, int j0in)
{		

	/* pole to baseline */
	double pole_lat, pole_lon;
	int i,j,k;
	double deltax, deltay,delta;  
	double z0,z;
	double x[3],x0[3];
	double xwork[3];
	double dx1_rad,dx2_rad;
	double rotation_angle, azimuth_i;

	strcpy(name,namein);
	lat0=lat0in;
	lon0=lon0in;
	r0 = r0_ellipse(lat0);
	azimuth_y=azimuth_yin;
	rotation_angle=-azimuth_y;
	dx1_nom=dx1_nomin;
	dx2_nom=dx2_nomin;
	dx3_nom=dx3_nomin;
	dx1_rad = dx1_nom/r0;
	dx2_rad = dx2_nom/r0;
	n1=n1in;
	n2=n2in;
	n3=n3in;
	i0=i0in;
	j0=j0in;
//
// We force the origin of the r coordinate to be at the bottom of the grid
//
	k0=0;
	z0 = ((double)(n3-1))*dx3_nom;
	/* These are the vector of lat and lons used to define the
	baseline the effectively forms the equator of of the rotated
	coordinates passing through the origin*/
	double *baseline_lat=new double[n1in];
	double *baseline_lon=new double[n1in];

	build_baseline(lat0,lon0,
		rotation_angle,dx1_rad,n1,i0,baseline_lat,baseline_lon);
	/* We need to compute the pole to our base line to use as a 
	target for grid lines that are locally perpendicular along
	the grid lines -- like longitude lines at the equator */
	latlon(lat0,lon0,M_PI_2,-rotation_angle,&pole_lat,&pole_lon);

	/* We now allocate memory for the large coordinate arrays
	themselves and set the elements of the structure */
	cartesian_defined = 1;
	geographic_defined = 1;
	lat = create_3dgrid_contiguous(n1,n2,n3);
	lon = create_3dgrid_contiguous(n1,n2,n3);
	r = create_3dgrid_contiguous(n1,n2,n3);
	x1 = create_3dgrid_contiguous(n1,n2,n3);
	x2 = create_3dgrid_contiguous(n1,n2,n3);
	x3 = create_3dgrid_contiguous(n1,n2,n3);

	r0 = r0-z0;
	azimuth_y = -rotation_angle;

	/* We now complete the latitude/longitude grid by projecting
	lines from the baseline toward the computed pole.  Grid points 
	will be equally spaced along the gcp toward the pole, but the
	lines they form will converge.  This is complicated greatly by
	the grid origin parameters.  We could make this a function,
	but it is simpler to just write it inline. Because this function
	descended from the a C program call makegclgrid, which built both
	a 2d and 3d grid in one pass, I'm stealing that algorithm.  That is
	the top of the grid (at n3-1) is built first, then the remainder is
	filled in with same lat lons but with varying depth.  So, first this
	loop creates the top sheet of the grid */
	int top_of_grid=n3-1;
	for(i=0;i<n1;++i)
	{
		dist(baseline_lat[i],baseline_lon[i],pole_lat, pole_lon,
			&delta,&azimuth_i);
		if(azimuth_i < 0.0) azimuth_i += (2.0*M_PI);
		delta = dx2_rad*(-(double)j0);
		for(j=0;j<n2;++j,delta+=dx2_rad)
		{
			if(j<j0)
				latlon(baseline_lat[i],baseline_lon[i],
					fabs(delta),azimuth_i-M_PI,
					&(lat[i][j][top_of_grid]),&(lon[i][j][top_of_grid]));
			else if(j==j0)
			{
				lat[i][j][top_of_grid] = baseline_lat[i];
				lon[i][j][top_of_grid] = baseline_lon[i];
			}
			else
				latlon(baseline_lat[i],baseline_lon[i],
					fabs(delta),azimuth_i,
					&(lat[i][j][top_of_grid]),&(lon[i][j][top_of_grid]));
			r[i][j][top_of_grid] = r0_ellipse(lat[i][j][top_of_grid]);
		}
	}
//
//Now we fill in the rest of the grid keeping the lat lon of the top for all
//levels but varying r -- i.e. these are spherical shells.
//
	for(k=0,z=z0;k<n3-1;++k,z-=dx3_nom)
	{
		for(i=0;i<n1;++i)
			for(j=0;j<n2;++j)
			{
				lat[i][j][k]=lat[i][j][top_of_grid];
				lon[i][j][k]=lon[i][j][top_of_grid];
				r[i][j][k] = r0_ellipse(lat[i][j][top_of_grid]) - z;
			}
	}
	//
	// This function standarizes setting the tranformation from geo to cartesian
	//
	set_transformation_matrix();
	Cartesian_point cpt;
	for(i=0;i<n1;++i) 
	    for(j=0;j<n2;++j)
		for(k=0;k<n3;++k) 
		{
			cpt = gtoc(lat[i][j][k],lon[i][j][k],
					r[i][j][k]);

			x1[i][j][k] = cpt.x1;
			x2[i][j][k] = cpt.x2;
			x3[i][j][k] = cpt.x3;
		}
	/* We have to compute the extents parameters as the minimum 
	and maximum in each cartesian direction */

	x1low=x1[0][0][0];
	x1high=x1[0][0][0];
	x2low=x2[0][0][0];
	x2high=x2[0][0][0];
	x3low=x3[0][0][0];
	x3high=x3[0][0][0];

	for(i=1;i<n1;++i)
	    for(j=1;j<n2;++j)
		for(k=1;k<n3;++k)
		{
			x1low = MIN(x1[i][j][k],x1low);
			x1high = MAX(x1[i][j][k],x1high);
			x2low = MIN(x2[i][j][k],x2low);
			x2high = MAX(x2[i][j][k],x2high);
			x3low = MIN(x3[i][j][k],x3low);
			x3high = MAX(x3[i][j][k],x3high);
		}

	delete baseline_lat;
	delete baseline_lon;
}
//
// constructors for derived types for scalar and vector fields defined on
// a gcl grid.  Makes extensive use of inheritance.
//
GCLscalarfield::GCLscalarfield() : GCLgrid()
{
	val=NULL;
}

GCLscalarfield::GCLscalarfield(int n1size, int n2size)
	: GCLgrid(n1size, n2size)
{
	val=create_2dgrid_contiguous(n1size, n2size);
}
GCLvectorfield::GCLvectorfield() : GCLgrid()
{
	val=NULL;
}

GCLvectorfield::GCLvectorfield(int n1size, int n2size, int n3size)
	: GCLgrid(n1size, n2size)
{
	nv=n3size;
	val=create_3dgrid_contiguous(n1size, n2size, n3size);
}
//
// Some of the repetition below may be avoidable with virtual functions
// if I understood how to do that better I'd try it.
//
GCLscalarfield::GCLscalarfield(GCLgrid& g) : GCLgrid(g.n1, g.n2)
{
	int i,j;
	strcpy(name, g.name);
	lat0=g.lat0;
	lon0=g.lon0;
	r0=g.r0;
	azimuth_y=g.azimuth_y;
	dx1_nom=g.dx1_nom;
	dx2_nom=g.dx2_nom;
	n1=g.n1;
	n2=g.n2;
	i0=g.i0;
	j0=g.j0;
	x1low=g.x1low;
	x1high=g.x1high;
	x2low=g.x2low;
	x2high=g.x2high;
	x3low=g.x3low;
	x3high=g.x3high;
	cartesian_defined=g.cartesian_defined;
	geographic_defined=g.geographic_defined;
	dcopy(3,g.gtoc_rmatrix[0],1,gtoc_rmatrix[0],1);
	dcopy(3,g.gtoc_rmatrix[1],1,gtoc_rmatrix[1],1);
	dcopy(3,g.gtoc_rmatrix[2],1,gtoc_rmatrix[2],1);
	dcopy(3,g.translation_vector,1,translation_vector,1);
	// We don't waste this effort unless these arrays contain something
	if(cartesian_defined)
	{
            for(i=0;i<n1;++i)
                    for(j=0;j<n2;++j) x1[i][j]=g.x1[i][j];
            for(i=0;i<n1;++i)
                    for(j=0;j<n2;++j) x2[i][j]=g.x2[i][j];
            for(i=0;i<n1;++i)
                    for(j=0;j<n2;++j) x3[i][j]=g.x3[i][j];
	}
	if(geographic_defined)
	{
            for(i=0;i<n1;++i)
                    for(j=0;j<n2;++j) lat[i][j]=g.lat[i][j];
            for(i=0;i<n1;++i)
                    for(j=0;j<n2;++j) lon[i][j]=g.lon[i][j];
            for(i=0;i<n1;++i)
                    for(j=0;j<n2;++j) r[i][j]=g.r[i][j];

	}
	val=create_2dgrid_contiguous(g.n1, g.n2);
}
GCLvectorfield::GCLvectorfield(GCLgrid& g, int n3) : GCLgrid(g.n1, g.n2)
{
	int i,j;
	strcpy(name, g.name);
	lat0=g.lat0;
	lon0=g.lon0;
	r0=g.r0;
	azimuth_y=g.azimuth_y;
	dx1_nom=g.dx1_nom;
	dx2_nom=g.dx2_nom;
	n1=g.n1;
	n2=g.n2;
	i0=g.i0;
	j0=g.j0;
	x1low=g.x1low;
	x1high=g.x1high;
	x2low=g.x2low;
	x2high=g.x2high;
	x3low=g.x3low;
	x3high=g.x3high;
	cartesian_defined=g.cartesian_defined;
	geographic_defined=g.geographic_defined;
	dcopy(3,g.gtoc_rmatrix[0],1,gtoc_rmatrix[0],1);
	dcopy(3,g.gtoc_rmatrix[1],1,gtoc_rmatrix[1],1);
	dcopy(3,g.gtoc_rmatrix[2],1,gtoc_rmatrix[2],1);
	dcopy(3,g.translation_vector,1,translation_vector,1);
	// We don't waste this effort unless these arrays contain something
	if(cartesian_defined)
	{
            for(i=0;i<n1;++i)
                    for(j=0;j<n2;++j) x1[i][j]=g.x1[i][j];
            for(i=0;i<n1;++i)
                    for(j=0;j<n2;++j) x2[i][j]=g.x2[i][j];
            for(i=0;i<n1;++i)
                    for(j=0;j<n2;++j) x3[i][j]=g.x3[i][j];
	}
	if(geographic_defined)
	{
            for(i=0;i<n1;++i)
                    for(j=0;j<n2;++j) lat[i][j]=g.lat[i][j];
            for(i=0;i<n1;++i)
                    for(j=0;j<n2;++j) lon[i][j]=g.lon[i][j];
            for(i=0;i<n1;++i)
                    for(j=0;j<n2;++j) r[i][j]=g.r[i][j];

	}
	nv=n3;
	val=create_3dgrid_contiguous(g.n1, g.n2,n3);
}
//
//3d versions
//
GCLscalarfield3d::GCLscalarfield3d() : GCLgrid3d()
{
	val=NULL;
}

GCLscalarfield3d::GCLscalarfield3d(int n1size, int n2size, int n3size)
	: GCLgrid3d(n1size, n2size, n3size)
{
	val=create_3dgrid_contiguous(n1size, n2size, n3size);
}
GCLvectorfield3d::GCLvectorfield3d() : GCLgrid3d()
{
	val=NULL;
}

GCLvectorfield3d::GCLvectorfield3d(int n1size, int n2size, int n3size, int n4)
	: GCLgrid3d(n1size, n2size, n3size)
{
	nv=n4;
	val=create_4dgrid_contiguous(n1size, n2size, n3size, n4);
}
GCLscalarfield3d::GCLscalarfield3d(GCLgrid3d& g) : GCLgrid3d(g.n1, g.n2, g.n3)
{
	int i,j,k;
	strcpy(name,g.name);
	lat0=g.lat0;
	lon0=g.lon0;
	r0=g.r0;
	azimuth_y=g.azimuth_y;
	dx1_nom=g.dx1_nom;
	dx2_nom=g.dx2_nom;
	dx3_nom=g.dx3_nom;
	n1=g.n1;
	n2=g.n2;
	n3=g.n3;
	i0=g.i0;
	j0=g.j0;
	x1low=g.x1low;
	x1high=g.x1high;
	x2low=g.x2low;
	x2high=g.x2high;
	x3low=g.x3low;
	x3high=g.x3high;
	cartesian_defined=g.cartesian_defined;
	geographic_defined=g.geographic_defined;
	dcopy(3,g.gtoc_rmatrix[0],1,gtoc_rmatrix[0],1);
	dcopy(3,g.gtoc_rmatrix[1],1,gtoc_rmatrix[1],1);
	dcopy(3,g.gtoc_rmatrix[2],1,gtoc_rmatrix[2],1);
	dcopy(3,g.translation_vector,1,translation_vector,1);
	if(cartesian_defined)
	{
	    for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) x1[i][j][k]=g.x1[i][j][k];
	    for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) x2[i][j][k]=g.x2[i][j][k];
	    for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) x3[i][j][k]=g.x3[i][j][k];
	}
	if(geographic_defined)
	{
	    for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) lat[i][j][k]=g.lat[i][j][k];
	    for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) lon[i][j][k]=g.lon[i][j][k];
	    for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) r[i][j][k]=g.r[i][j][k];

	}
	val=create_3dgrid_contiguous(g.n1, g.n2, g.n3);
}
GCLvectorfield3d::GCLvectorfield3d(GCLgrid3d& g, int n4) 
	: GCLgrid3d(g.n1, g.n2, g.n3)
{
	int i,j,k;
	strcpy(name,g.name);
	lat0=g.lat0;
	lon0=g.lon0;
	r0=g.r0;
	azimuth_y=g.azimuth_y;
	dx1_nom=g.dx1_nom;
	dx2_nom=g.dx2_nom;
	dx3_nom=g.dx3_nom;
	n1=g.n1;
	n2=g.n2;
	n3=g.n3;
	i0=g.i0;
	j0=g.j0;
	x1low=g.x1low;
	x1high=g.x1high;
	x2low=g.x2low;
	x2high=g.x2high;
	x3low=g.x3low;
	x3high=g.x3high;
	cartesian_defined=g.cartesian_defined;
	geographic_defined=g.geographic_defined;
	dcopy(3,g.gtoc_rmatrix[0],1,gtoc_rmatrix[0],1);
	dcopy(3,g.gtoc_rmatrix[1],1,gtoc_rmatrix[1],1);
	dcopy(3,g.gtoc_rmatrix[2],1,gtoc_rmatrix[2],1);
	dcopy(3,g.translation_vector,1,translation_vector,1);
	if(cartesian_defined)
	{
	    for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) x1[i][j][k]=g.x1[i][j][k];
	    for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) x2[i][j][k]=g.x2[i][j][k];
	    for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) x3[i][j][k]=g.x3[i][j][k];
	}
	if(geographic_defined)
	{
	    for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) lat[i][j][k]=g.lat[i][j][k];
	    for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) lon[i][j][k]=g.lon[i][j][k];
	    for(i=0;i<n1;++i)
		for(j=0;j<n2;++j) 
			for(k=0;k<n3;++k) r[i][j][k]=g.r[i][j][k];

	}
	nv=n4;
	val=create_4dgrid_contiguous(g.n1, g.n2, g.n3,n4);
}


//
//C++ destructors
//
GCLgrid::~GCLgrid()
{
	if(x1!=NULL) free_2dgrid_contiguous(x1,n1);
	if(x2!=NULL) free_2dgrid_contiguous(x2,n1);
	if(x3!=NULL) free_2dgrid_contiguous(x3,n1);
	if(lat!=NULL) free_2dgrid_contiguous(lat,n1);
	if(lon!=NULL) free_2dgrid_contiguous(lon,n1);
	if(r!=NULL) free_2dgrid_contiguous(r,n1);
}
GCLgrid3d::~GCLgrid3d()
{
	if(x1!=NULL) free_3dgrid_contiguous(x1,n1,n2);
	if(x2!=NULL) free_3dgrid_contiguous(x2,n1,n2);
	if(x3!=NULL) free_3dgrid_contiguous(x3,n1,n2);
	if(lat!=NULL) free_3dgrid_contiguous(lat,n1,n2);
	if(lon!=NULL) free_3dgrid_contiguous(lon,n1,n2);
	if(r!=NULL) free_3dgrid_contiguous(r,n1,n2);
}
//
//Note standard rule that a derived class utilizes the base class destructor
//after calling these
//
GCLscalarfield::~GCLscalarfield()
{
	if(val!=NULL) free_2dgrid_contiguous(val,n1);
}
GCLvectorfield::~GCLvectorfield()
{
	if(val!=NULL) free_3dgrid_contiguous(val,n1,n2);
}
GCLscalarfield3d::~GCLscalarfield3d()
{
	if(val!=NULL) free_3dgrid_contiguous(val,n1,n2);
}
GCLvectorfield3d::~GCLvectorfield3d()
{
	if(val!=NULL) free_4dgrid_contiguous(val,n1,n2,n3);
}
