#include <string.h>
#include <iostream>
#include "stock.h"
#include "coords.h"
#include "elog.h"
#include "pf.h"
#include "seispp.h"
#include "dbpp.h"
#include "gclgrid.h"
using namespace std;
using namespace SEISPP;
void usage(char *prog)
{
	elog_die(0,"Usage:  %s db [-v -pf pffile]\n",prog);
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
	char *version="2.1 (Feb. 2005)";
	char *pfin=NULL;
	Pf *pf;
	char *dbname;
	int i;
	/* All of the following are duplicates of GCLgrid parameters,
	but we use the simpler variables in their place in many place
	to avoid all the strange pointer constructs */
	double lat0, lon0,r0;
	double azimuth_x, rotation_angle, azimuth_y;
	double dx1, dx2, dx3, dx1_rad, dx2_rad;
	int n1, n2, n3;
	int i0,j0,k0;
	double xlow,xhigh,ylow,yhigh,zlow,zhigh;
	/* set nonzero if a 3d grid is to be created */
	int create_3d=0;
	/* pole to baseline */
	double pole_lat, pole_lon;
	double deltax, deltay,delta;  
	double z0,z;
	double x[3],x0[3];
	char *dir;

	// needed because we mix stdio and streams
	ios::sync_with_stdio();

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
                        if(i>argc) usage(argv[0]);
                        pfin = argv[i];
                }
		else if(!strcmp(argv[i],"-v"))
			SEISPP_verbose=true;
                else
                        usage(argv[0]);
        }
        /* this sets defaults */
        if(pfin == NULL) pfin = strdup("makegclgrid");

        i = pfread(pfin,&pf);
        if(i != 0) die(1,"Pfread error\n");
        DatascopeHandle *dbh;
        try {
            dbh = new DatascopeHandle(dbname,false);
            dbh->lookup("gclgdisk");
        } catch(SeisppError& serr)
        {
            serr.log_error();
            exit(-1);
        }

	/* Fetch the main grid parameters from the parameter file */
	string gridname(pfget_string(pf,"gridname"));
	if(gridname.empty())
		die(0,"Null gridname parameter in input parameter file\nYou must assign a name to this grid\n");
	lat0 = pfget_double(pf,"origin_latitude");
	lon0 = pfget_double(pf,"origin_longitude");
	lat0 = rad(lat0);
	lon0 = rad(lon0);
	r0 = r0_ellipse(lat0);
	azimuth_x = pfget_double(pf,"x_axis_azimuth");
	rotation_angle = rad(90.0 - azimuth_x);
	azimuth_y = -rotation_angle;
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
	//
	//Use the full constructor to construct a 2D and 3D arrays and
	//write them out using the dbsave routines.  The 2d and 3d
	//codes are exact parallels due to interface design
	//
	GCLgrid g = GCLgrid(n1, n2, gridname,
			lat0, lon0, r0, azimuth_y, 
			dx1, dx2, i0, j0);
	try {
		g.save(*dbh,dir);
	}
	catch (...)
	{
		elog_die(0,"dbsave failed for 2d grid\n");
	};
	if(create_3d) {
	    GCLgrid3d g3d = GCLgrid3d(n1, n2, n3, gridname,
			lat0, lon0,r0, azimuth_y, 
			dx1, dx2, dx3, i0, j0);
	    try {
		g3d.save(*dbh,dir);
	    }
	    catch (...)
	    {
		elog_die(0,"dbsave failed for 3d grid\n");
	    }
	}
	exit(0);
}
