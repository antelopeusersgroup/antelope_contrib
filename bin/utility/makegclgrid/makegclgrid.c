#include <string.h>
#include "stock.h"
#include "coords.h"
#include "elog.h"
#include "pf.h"
#include "db.h"
#include "gclgrid.h"
/* These are functions in dr3.c that should have been in
coords.h */
void dr3cros(double *,double *, double *);
void dr3mxv(double *, double *, double *);
void dr3sub(double *, double *, double *);
void dr3sxv(double ,double *,double *);
void dr3tran(double *,double *);
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
		
void usage(char *prog)
{
	elog_die(0,"Usage:  %s db [-pf pffile]\n",prog);
}
void main(int argc, char **argv)
{
	char *version="1.0 (August 2000)";
	GCL2Dgrid grid2d;
	GCL3Dgrid grid3d;
	char *pfin=NULL;
	Pf *pf;
	Dbptr db;
	char *dbname;
	/* All of the following are duplicates of GCLgrid parameters,
	but we use the simpler variables in their place in many place
	to avoid all the strange pointer constructs */
	double lat0, lon0,r0;
	double azimuth_x, rotation_angle, azimuth_i;
	double dx1, dx2, dx3, dx1_rad, dx2_rad;
	int n1, n2, n3;
	int i0,j0,k0;
	double xlow,xhigh,ylow,yhigh,zlow,zhigh;
	/* These are the vector of lat and lons used to define the
	baseline the effectively forms the equator of of the rotated
	coordinates passing through the origin*/
	double *baseline_lat, *baseline_lon;
	/* set nonzero if a 3d grid is to be created */
	int create_3d=0;
	/* pole to baseline */
	double pole_lat, pole_lon;
	/* name assigned to grid */
	char *gridname;
	int i,j,k;
	double deltax, deltay,delta;  
	double z0,z;
	double x[3],x0[3];
	char *dir;
	double rmatrix[9],rmtrans[9];
	double xwork[3],xwork2[3];

	/* Initialize the error log and write a version notice */
        elog_init (argc, argv);
        elog_log (0, "%s version %s\n", argv[0], version) ;

        /* usual cracking of command line */
        if(argc < 2) usage(argv[0]);
        dbname = argv[1];

        for(i=2;i<argc;++i)
        {
              if(!strcmp(argv[i],"-pf"))
                {
                        ++i;
                        if(i>=argc) usage(argv[0]);
                        pfin = argv[i];
                }
                else
                        usage(argv[0]);
        }
        /* this sets defaults */
        if(pfin == NULL) pfin = strdup("makegclgrid");

        i = pfread(pfin,&pf);
        if(i != 0) die(1,"Pfread error\n");

	if(dbopen(dbname,"r+",&db))
		die(0,"dbopen failed on database %s\n",dbname);
	db = dblookup(db,0,"gclgdisk",0,0);
	if(db.record == dbINVALID) die(0,"lookup fails for gclgdisk table\nSchema is probably defined incorrectly\n");

	/* Fetch the main grid parameters from the parameter file */
	gridname = pfget_string(pf,"gridname");
	if(strlen(gridname)<=0) 
		die(0,"Null gridname parameter in input parameter file\nYou must assign a name to this grid\n");
	lat0 = pfget_double(pf,"origin_latitude");
	lon0 = pfget_double(pf,"origin_longitude");
	lat0 = rad(lat0);
	lon0 = rad(lon0);
	r0 = r0_ellipse(lat0);
	azimuth_x = pfget_double(pf,"x_axis_azimuth");
	rotation_angle = rad(90.0 - azimuth_x);
	dx1 = pfget_double(pf,"delta_x1");
	dx2 = pfget_double(pf,"delta_x2");
	dx1_rad = dx1/r0;
	dx2_rad = dx2/r0;
	n1 = pfget_int(pf,"n1");
	n2 = pfget_int(pf,"n2");
	i0 = pfget_int(pf,"origin_offset_x1");
	j0 = pfget_int(pf,"origin_offset_x2");
	create_3d = pfget_boolean(pf,"build_3d_grid");
	dir = pfget_string(pf,"gridfile_directory");
	if(makedir(dir)) elog_die(0,"Cannot create output directory %s\n",
				dir);
	if(create_3d)
	{
		dx3 = pfget_double(pf,"delta_x3");
		n3 = pfget_int(pf,"n3");
		k0 = 0;  /* we force the r coordinate to always start 
			at the bottom of the grid.  The format allows
			something more general, but this is the simplest
			way to define this */
		z0 = ((double)(n3-1))*dx3;
	}
	/* We can now build the baseline vectors of length n1. They 
	effectively define a rotated equator of the new coordinate system
	passing through the origin.  */
	allot(double *,baseline_lat,n1);
	allot(double *,baseline_lon,n1);
	build_baseline(lat0,lon0,
		rotation_angle,dx1_rad,n1,i0,baseline_lat,baseline_lon);
	/* We need to compute the pole to our base line to use as a 
	target for grid lines that are locally perpendicular along
	the grid lines -- like longitude lines at the equator */
	latlon(lat0,lon0,M_PI_2,-rotation_angle,&pole_lat,&pole_lon);

	/* We now allocate memory for the large coordinate arrays
	themselves and set the elements of the structure */
	grid2d.lat = create_2dgrid_contiguous(n1,n2);
	grid2d.lon = create_2dgrid_contiguous(n1,n2);
	grid2d.r = create_2dgrid_contiguous(n1,n2);
	grid2d.x1 = create_2dgrid_contiguous(n1,n2);
	grid2d.x2 = create_2dgrid_contiguous(n1,n2);
	grid2d.x3 = create_2dgrid_contiguous(n1,n2);
	strcpy(grid2d.name,gridname);
	grid2d.lat0 = lat0;
	grid2d.lon0 = lon0;
	grid2d.r0 = r0;
	grid2d.azimuth_y = -rotation_angle;
	grid2d.dx1_nom = dx1;
	grid2d.dx2_nom = dx2;
	grid2d.n1 = n1;
	grid2d.n2 = n2;
	grid2d.i0 = i0;
	grid2d.j0 = j0;
	grid2d.cartesian_defined = 1;
	grid2d.geographic_defined = 1;
	if(create_3d)
	{
		grid3d.lat = create_3dgrid_contiguous(n1,n2,n3);
		grid3d.lon = create_3dgrid_contiguous(n1,n2,n3);
		grid3d.r = create_3dgrid_contiguous(n1,n2,n3);
		grid3d.x1 = create_3dgrid_contiguous(n1,n2,n3);
		grid3d.x2 = create_3dgrid_contiguous(n1,n2,n3);
		grid3d.x3 = create_3dgrid_contiguous(n1,n2,n3);
		strcpy(grid3d.name,gridname);
		grid3d.lat0 = lat0;
		grid3d.lon0 = lon0;
		grid3d.r0 = r0-z0;
		grid3d.azimuth_y = -rotation_angle;
		grid3d.dx1_nom = dx1;
		grid3d.dx2_nom = dx2;
		grid3d.dx3_nom = dx3;
		grid3d.n1 = n1;
		grid3d.n2 = n2;
		grid3d.n3 = n3;
		grid3d.i0 = i0;
		grid3d.j0 = j0;
		grid3d.k0 = k0;
		grid3d.cartesian_defined = 1;
		grid3d.geographic_defined = 1;
	}
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
					grid2d.lat[i]+j,grid2d.lon[i]+j);
			else if(j==j0)
			{
				grid2d.lat[i][j] = baseline_lat[i];
				grid2d.lon[i][j] = baseline_lon[i];
			}
			else
				latlon(baseline_lat[i],baseline_lon[i],
					fabs(delta),azimuth_i,
					grid2d.lat[i]+j,grid2d.lon[i]+j);
			grid2d.r[i][j] = r0_ellipse(grid2d.lat[i][j]);
		}
	}
	if(create_3d)
	{
		for(k=0,z=z0;k<n3;++k,z-=dx3)
		{
			/* At each level we copy the lat lon grid 
			exactly because we are generating a spherical
			shell based grid */
			for(i=0;i<n1;++i)
				for(j=0;j<n2;++j)
				{
					grid3d.lat[i][j][k]=grid2d.lat[i][j];
					grid3d.lon[i][j][k]=grid2d.lon[i][j];
					grid3d.r[i][j][k]
						=r0_ellipse(grid2d.lat[i][j]);
					grid3d.r[i][j][k]-=z;

				}
		}
	}
	/* We now compute the Cartesian coordinates of all these points
	with an origin at the earth's center.  After we do compute all 
	the Cartesian coordinates we then do a translation of the 
	coordinates to the grid origin to make the numbers more 
	manageable*/
	for(i=0;i<n1;++i) 
		for(j=0;j<n2;++j)
		{
			dsphcar(grid2d.lon[i][j],grid2d.lat[i][j],x);
			grid2d.x1[i][j] = x[0]*grid2d.r[i][j];
			grid2d.x2[i][j] = x[1]*grid2d.r[i][j];
			grid2d.x3[i][j] = x[2]*grid2d.r[i][j];
		}
	/* This computes the radial direction from the latitude and longitude.
	We translate the origin in this direction below */
	dsphcar(lon0,lat0,x0);
	/* We construct the transformation matrix for rotation of coordinates
	from the origin using unit vectors computed as follows:
	column 3= local vertical = copy of x0
	column 2 = local y = constructed from pole to baseline
	column 1 = y cross z 
	This yields a rotation matrix stored in fortran order in rmatrix 
	Note use of ugly pointer arithmetic done to store the matrix this way */
	for(i=0;i<3;++i)rmatrix[i+6]=x0[i];
	dsphcar(pole_lon,pole_lat,rmatrix+3);
	dr3cros(rmatrix+3,rmatrix+6,rmatrix);
	/* We actually need the transpose of this matrix */
	dr3tran(rmatrix,rmtrans);

	/* We now apply the combined translation and rotation 
	change of coordinates */
	r0=r0_ellipse(lat0);
	for(i=0;i<3;++i) x0[i]*=r0;
	for(i=0;i<n1;++i) 
		for(j=0;j<n2;++j)
		{
			xwork[0] = grid2d.x1[i][j];
			xwork[1] = grid2d.x2[i][j];
			xwork[2] = grid2d.x3[i][j];

			dr3sub(xwork,x0,xwork);
			dr3mxv(rmtrans,xwork,xwork2);

			grid2d.x1[i][j] = xwork2[0];
			grid2d.x2[i][j] = xwork2[1];
			grid2d.x3[i][j] = xwork2[2];
		}
	/* Now apply the same transformations to the 3d grid */
	if(create_3d)
	{
	  for(i=0;i<n1;++i) 
	    for(j=0;j<n2;++j)
	    {
		for(k=0;k<n3;++k)
		{
			dsphcar(grid3d.lon[i][j][k],grid3d.lat[i][j][k],x);
			dr3sxv(grid3d.r[i][j][k],x,xwork);

			dr3sub(xwork,x0,xwork2);
			dr3mxv(rmtrans,xwork2,xwork);

			grid3d.x1[i][j][k] = xwork[0];
			grid3d.x2[i][j][k] = xwork[1];
			grid3d.x3[i][j][k] = xwork[2];
		}
	    }
	}
	/* We have to compute the extents parameters as the minimum 
	and maximum in each cartesian direction */
	xlow=grid2d.x1[0][0];
	xhigh=grid2d.x1[0][0];
	ylow=grid2d.x2[0][0];
	yhigh=grid2d.x2[0][0];
	zlow=grid2d.x3[0][0];
	zhigh=grid2d.x3[0][0];
	for(i=0;i<n1;++i)
		for(j=0;j<n2;++j)
		{
			xlow = MIN(grid2d.x1[i][j],xlow);
			xhigh = MAX(grid2d.x1[i][j],xhigh);
			ylow = MIN(grid2d.x2[i][j],ylow);
			yhigh = MAX(grid2d.x2[i][j],yhigh);
			zlow = MIN(grid2d.x3[i][j],zlow);
			zhigh = MAX(grid2d.x3[i][j],zhigh);
		}
	grid2d.xlow = xlow;
	grid2d.ylow = ylow;
	grid2d.zlow = zlow;
	grid2d.xhigh = xhigh;
	grid2d.yhigh = yhigh;
	grid2d.zhigh = zhigh;
	if(create_3d)
	{
		xlow=grid2d.x1[0][0];
		xhigh=grid2d.x1[0][0];
		ylow=grid2d.x2[0][0];
		yhigh=grid2d.x2[0][0];
		zlow=grid2d.x3[0][0];
		zhigh=grid2d.x3[0][0];

		for(i=0;i<n1;++i)
		    for(j=0;j<n2;++j)
			for(k=0;k<n3;++k)
			{
				xlow = MIN(grid3d.x1[i][j][k],xlow);
				xhigh = MAX(grid3d.x1[i][j][k],xhigh);
				ylow = MIN(grid3d.x2[i][j][k],ylow);
				yhigh = MAX(grid3d.x2[i][j][k],yhigh);
				zlow = MIN(grid3d.x3[i][j][k],zlow);
				zhigh = MAX(grid3d.x3[i][j][k],zhigh);
			}

		grid3d.xlow = xlow;
		grid3d.ylow = ylow;
		grid3d.zlow = zlow;
		grid3d.xhigh = xhigh;
		grid3d.yhigh = yhigh;
		grid3d.zhigh = zhigh;
	}


	/* Now we save the results to the database */
	if(save_2dgclgrid(&grid2d,db,dir))
		elog_complain(0,"makegclgrid: problems saving 2d grid file\n");
	if(create_3d)
		if(save_3dgclgrid(&grid3d,db,dir))
			elog_complain(0,"makegclgrid: problems saving 2d grid file\n");
	exit(0);
}
