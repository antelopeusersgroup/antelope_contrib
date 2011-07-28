/* This set of functions are a general purpose set of routines to calculate
travel times and slowness vectors from a set of tables specified on a 
uniform grid in distances and source depth.  

Author:  Gary L. Pavlis
Written:  Sept-Oct. 1996
*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "stock.h"
#include "coords.h"
#include "arrays.h"
#include "location.h"
#define RADIUS_EARTH 6370.8
/* Special free function for travel time and slowness tables.  It handles the
dependencies correctly. 

ttable is the travel time table and utable the slowness table as should be
obvious */

int GenlocVerbose = 0 ;
#undef register_error
#define register_error  if ( GenlocVerbose ) elog_notify

void free_uniform_table(XZ_table_uniform *ttable, XZ_table_uniform *utable)
{
	free_matrix((char **)(ttable->values),0,(ttable->nx)-1,0);
	free_matrix((char **)(ttable->slopes),0,(ttable->nx)-1,0);
	free_matrix(ttable->branch,0,(ttable->nx)-1,0);
	free_matrix(utable->branch,0,(utable->nx)-1,0);
	free_matrix((char **)(utable->slopes),0,(utable->nx)-1,0);
	free(ttable->velocity);
	free(ttable);
	free(utable);
}

/* There may be a more clever way to do this, but this will work.  The problem 
is that we need to have a static area defined to store travel time tables for
a set of N possible seismic phases that might call this function under different
contexts.  To do this we define a static Arr that stores dynamically allocated
tables.  Each element of the Arr is keyed by the phase name and the Arr contains
pointers to appropriate table objects.  We have seperate tables for time and
slowness, although we fill them simultaneously.  
*/

static Arr *time_tables_uniform=NULL;
static Arr *slow_tables_uniform=NULL;

/* Init function for uniform table.  
 phase = phase name to tag this table with
 pf = input parameter file object to be parsed.


The following keys are required to be found in pf:
	int:
	nx, nz
	scalar double:
	dx, dz
	&Tbl:
	uniform_grid_time_slowness_table

The later contains the actual tables.  They are ascii tables make up
of nx*nz lines (x varies most rapidly) of the following format:
	time, slowness, slowness derivative wrt distance, branch

The "branch" variable is a character key defined in location.h

Optional parameters with defaults:
	scalar double:
	x0, y0 coordinates of first point in table  (default = (0,0))
	strings:

Notice that this routine requires mixed units.  dx, dz, x0, and y0 
must all be specified in degrees.  Everything else has units derived
from km and s.  That is, time is is in seconds, slowness (p) is 
assumed to be in s/km, and dpdx (slowness derivative) is (s/km)/km.  
This was done because the input tables are ascii, and these numbers
are scaled to units that make sense to most of us.  This format is
connected to a related program called taup_convert that writes 
ttables in this format using the tau-p library.  

Returns 0 if no problems are encountered.  REturns 1 if a serious
error occurred that rendered setup impossible for this phase.  
In the later case, register_error is always called and should be
handled by calling program.  

There are some fatal errors that lead to die being called here from
things like malloc failures.
*/
int uniform_table_interpolate_init(char *phase, Pf *pf)
{
	XZ_table_uniform *ttable, *utable;

	Tbl *t;  /* pfget_tbl return to hold strings of prototables stored
		in the pf structure. */
	int i,j,k;

	GenlocVerbose = verbose_exists() ; 

	if(time_tables_uniform==NULL) time_tables_uniform = newarr(0);
	if(slow_tables_uniform==NULL) slow_tables_uniform = newarr(0);

	ttable = (XZ_table_uniform *)malloc(sizeof(XZ_table_uniform));
	utable = (XZ_table_uniform *)malloc(sizeof(XZ_table_uniform));


	if( (ttable == NULL) || (utable == NULL) )
		elog_die(1,"Can't alloc memory in uniform_table_interpolate_init\n");
	
	/* This version requires t and u tables to be parallel.  This
	restriction would not be necessary, but it simplifies things
	greatly and we only have to store times in the values matrix
	and the slowness values in the slopes matrix. */
	
	ttable->nx = pfget_int(pf, "nx");
	ttable->nz = pfget_int(pf, "nz");
	utable->nx = ttable->nx;
	utable->nz = ttable->nz;
	ttable->dx = pfget_double(pf, "dx");
	ttable->dz = pfget_double(pf, "dz");
	utable->dx = ttable->dx;
	utable->dz = ttable->dz;
	/* These parameters default to 0 */
	if(pfget_string(pf,"x0")==NULL)
	{
		ttable->x0 = 0.0;
		utable->x0 = 0.0;
	}
	else
	{
		ttable->x0 = pfget_double(pf,"x0");
		utable->x0 = ttable->x0;
	}
	if(pfget_string(pf,"z0")==NULL)
	{
		ttable->z0 = 0.0;
		utable->z0 = 0.0;
	}
	else
	{
		ttable->z0 = pfget_double(pf,"z0");
		utable->z0 = ttable->z0;
	}

	/* IMPORTANT WARNING:  notice I only alloc one space for the
	slowness values array, although it gets placed in two different
	places -> values section of utable and slopes section of ttable 
	This leaves a nasty dependency if this space is to be freed, but
	saves a lot of memory.  p.s  I did the same thing with velocity,
	but not with the branch array (see below) */

	ttable->values = dmatrix(0,(ttable->nx)-1,0,(ttable->nz)-1);
	if(ttable->values == NULL) 
		elog_die(1,"Cannot alloc memory for travel time table of size %d by %d for phase %s\n",
			ttable->nx, ttable->nz, phase);
	ttable->slopes = dmatrix(0,(ttable->nx)-1,0,(ttable->nz)-1);
	if(ttable->slopes == NULL) 
		elog_die(1,"Cannot alloc memory for slowness table of size %d by %d for phase %s\n",
			ttable->nx, ttable->nz, phase);	
	ttable->branch = cmatrix(0,(ttable->nx)-1,0,(ttable->nz)-1);
	if(ttable->branch == NULL) 
		elog_die(1,"Cannot alloc memory for time branch table for phase %s\n",
				phase);
	utable->branch = cmatrix(0,(utable->nx)-1,0,(utable->nz)-1);
	if(utable->branch == NULL) 
		elog_die(1,"Cannot alloc memory for slowness branch table for phase %s\n",
				phase);

	ttable->velocity = (double *) calloc(ttable->nz,sizeof(double));
	if(ttable->velocity == NULL) 
		elog_die(1,"Cannot alloc memory for velocity model for phase %s\n",
				phase);

	utable->slopes = dmatrix(0,(utable->nx)-1,0,(utable->nz)-1);
	if(utable->slopes == NULL) 
		elog_die(1,"Cannot alloc memory for dudr table of size %d by %d for phase %s\n",
			utable->nx, utable->nz, phase);

	/* here is where we set the redundant pointers */
	utable->values = ttable->slopes;
	utable->velocity = ttable->velocity;		


	/* Now it is time to actually parse the tables.  We assume the
	table is entered as a pf &Tbl, and table is scanned with x
	varying most rapidly.  (i.e. you get the tables for x=x0 first, 
	then x=x0+dx, etc. Note we read three entries for each grid 
	point:  time, slowness, branch_code */
 	t = pfget_tbl(pf,"uniform_grid_time_slowness_table");
	if(t == NULL)
	{
		elog_log(1,"Can't find travel time-slowness table for phase %s\n",
			phase);
		free_uniform_table(ttable, utable);
		return(1);
	}

	if( maxtbl(t) != ( (ttable->nx)*(ttable->nz) ) )
	{
		elog_log(1,"Table size mismatch for phase %s\nTable should have %d rows\nFound %ld\n",
			phase, (ttable->nx)*(ttable->nz), maxtbl(t));
		free_uniform_table(ttable, utable);
		return(1);
	}

	for(j=0,k=0;j<ttable->nz;++j)
	{
		for(i=0;i<ttable->nx;++i)
		{
			char *s;
			int nitems;
			double tt,u,dudx;
			char b;
			s = gettbl(t,k);
			nitems = sscanf(s,"%lf%lf%lf%1s",
				&tt, &u, &dudx,&b);
			if(nitems !=4)
			{
				elog_log(1,"Syntax error reading table for phase %s, Problem read value for i=%d, j=%d\n",
					phase,i,j);
				free_uniform_table(ttable, utable);
				return(1);
			}
			ttable->values[i][j] = tt;
			ttable->slopes[i][j] = u;
			utable->slopes[i][j] = dudx;
			ttable->branch[i][j] = b;
			++k;
		}
	}

	/* In order to utilize a common set of interpolation routines, 
	scan the time->branch matrix.  Mark the crossover points for
	time as jump discontinuities for slowness (which they are) */
	for(j=0;j<ttable->nz;++j)
		for(i=0;i<ttable->nx;++i)
			if(ttable->branch[i][j] == CROSSOVER)
				utable->branch[i][j] = JUMP;
			else
				utable->branch[i][j] = ttable->branch[i][j];
	/* An error check is needed here so we don't have to worry about it
	later.  Other than a blunder, this can happen if x0 is anything
	other than 0, so we need to watch for this.  We could try to 
	repair this automatically, but because it mostly likely indicates
	a serious blunder we abort */

 	for(j=0;j<ttable->nz;++j)
		if( (utable->branch[0][j] == CROSSOVER)
			|| (ttable->branch[0][j] == CROSSOVER)
			|| (utable->branch[0][j] == JUMP)
			|| (ttable->branch[0][j] == JUMP) )
		{

			elog_log(1,
			  "Error in travel time table for phase %s\nFirst point cannot be marked as a crossover or jump discontinuity\n",phase);
			free_uniform_table(ttable, utable);
			return(1);
		}
	/* Now we read the velocity model parameters */
	t = pfget_tbl(pf,"velocities");
	if((ttable->nz) != maxtbl(t))
	{
		elog_log(1,"Error in phase parameter file.  \
Mismatch between velocity entries and table entries\n\
Tables have %d depth entries, but velocity vector is of length %ld\n",
			ttable->nz, maxtbl(t));
		free_uniform_table(ttable,utable);
		return(1);
	}
	for(i=0;i<maxtbl(t);++i)
	{
		char *s;
		s = gettbl(t,i);
		sscanf(s,"%lf", &(ttable->velocity[i]));
	}
	setarr(time_tables_uniform,phase,ttable);
	setarr(slow_tables_uniform,phase,utable);
	return(0);
}


/* This is a small companion function for uniform_time_table_interpolate to set return value and call register_error */
Travel_Time_Function_Output set_time_table_error(char *error)
{
	Travel_Time_Function_Output o;
	elog_log(0,"uniform_time_table_interpolate: %s\n",error);
	o.time = TIME_INVALID; 
	o.dtdx = 0.0;
	o.dtdy = 0.0;
	o.dtdz = 0.0;
	return(o);
} 
/* Special function companions to exec function below */

/* This function takes the branch table and index positions ix and iz
into the table, and checks for the kinds of discontinuities it needs to
deal with.  The algorithm assumes an important thing and that is that
all problem points are marked by a branch code at the point immediately 
after (in x distance that is) the discontinuity.  That means we can 
base all decisions by focusing on the points called b_hl and b_hh 
below.  NOWAY is returned if nothing can be computed.  TWO_LOW
is returned if we can compute a point by extrapolation from lh and ll.
ONE_LL and ONE_LH mean we can only extrapolate from the ll and lh positions.
Notice that there is not TWO_HIGH, ONE_HH, or ONE_HL because we always
define a break as the point between low and high.  Coarse grids might
profit from a back extrapolation from the nearest grid point, but this
would get pretty messy so I've avoided it. 

Notice that crossovers are not flagged, but marked as ok for 
interpolation using NO_PROBLEM_VALUE.  NO_PROBLEM is only returned if
there none of the four points are crossovers. 
*/
#define NO_PROBLEM 0
#define NO_PROBLEM_VALUE 1
#define NOWAY 2
#define TWO_LOW 3
#define ONE_LL 4
#define ONE_LH 5
int check_discontinuity(char **branch,int ix, int iz)
{
	char b_ll,b_lh,b_hl,b_hh;
	b_ll = branch[ix][iz];
	b_lh = branch[ix][iz+1];
	b_hl = branch[ix+1][iz];
	b_hh = branch[ix+1][iz+1];
	if( (b_ll == NOT_OBSERVABLE) && (b_hl == NOT_OBSERVABLE)
	  && (b_lh == NOT_OBSERVABLE) && (b_hh == NOT_OBSERVABLE) ) 
			return(NOWAY);
	/* Treat this case like a jump.  It assumes diffractions
	always make this kind of thing fuzzy anyway. */
	if( (b_hl == NOT_OBSERVABLE) || (b_hh == NOT_OBSERVABLE)
		   && ((b_ll != NOT_OBSERVABLE) || (b_lh != NOT_OBSERVABLE) ))
		if(b_ll == b_lh)
			return(TWO_LOW);
		else
			if(b_ll == NOT_OBSERVABLE)
				return(ONE_LH);
			else			
				return(ONE_LL);
		if( (b_hl == JUMP) || (b_hh == JUMP) )
		    if(b_ll == b_lh)
			return(TWO_LOW);
		    else
			if(b_ll == JUMP) 
				return(ONE_LH);
			else			
				return(ONE_LL);
	if((b_ll == CROSSOVER) || (b_lh == CROSSOVER) || (b_hl == CROSSOVER) 
			|| (b_hh == CROSSOVER)) return(NO_PROBLEM_VALUE);
	return(NO_PROBLEM);
}
	
	
/* Exec functions for uniform tables for time*/


/* Interpolates a function of two variables specified on a rectangular grid using
linear serendipity shape functions (hence the name) ala finite elements.  Reference
Zienkiewicz, "The Finite Element Method in Engineering Science', p. 107 of the 
second edition.

The function must be specified at the four points of a rectangle defined by two
ordered pairs, (xlow, ylow) and (xhigh, yhigh).  The values of the function
are passes as:
	f_ll = f(xlow, ylow)
	f_hl = f(xhigh, ylow)
	f_lh = f(xlow, yhigh)
	f_hh = f(xhigh,yhigh)
where from the definition we imply xlow < xhigh and ylow < yhigh.

The function returns the interpolated value of f(x,y).

Author: Gary L Pavlis
	Translated from old FORTRAN code by author September 1996
*/
double serendipity ( double xlow, double ylow, double xhigh, double yhigh,
		double f_ll, double f_hl, double f_lh, double f_hh, 
		double x, double y)
{
	double x0, y0, dx, dy, zeta, eta;
	double result=0.0;

	x0 = (xlow+xhigh)/2.0;
	y0 = (ylow+yhigh)/2.0;
	dx=xhigh-x0;
	dy=yhigh-y0;
	zeta=(x-x0)/dx;
	eta=(y-y0)/dy;
	result += f_ll*(1.0-zeta)*(1.0-eta)/4.0;
	result += f_hl*(1.0+zeta)*(1.0-eta)/4.0;
	result += f_lh*(1.0-zeta)*(1.0+eta)/4.0;
	result += f_hh*(1.0+zeta)*(1.0+eta)/4.0;
	return(result);
}
/* Companion function to handle case when the point x,y is near
a discontinuity in the curve being interpolated.  In this case
we interpolate from the points f_ll and f_lh using slopes defined
at those points and passed as the arguments slope_ll and slope_lh.
We interpolate the value of f between f_ll and f_lh based on y,
then project the value from x based on the interpolated slope.

Author:  Gary L Pavlis
*/

double interpolate_discontinuity_twopoint 
	(double xlow, double ylow, double xhigh, double yhigh,
	double f_ll, double f_lh, double slope_ll, double slope_lh,
	double x, double y)
{
	double f_y, fprime_y;
	double dfdz;

	/* use a linear formula a to interpolate f_y and fprime_y */
	dfdz = (f_lh - f_ll)/(yhigh - ylow);
	f_y = f_ll + dfdz*(y-ylow);
	dfdz = (slope_lh - slope_ll)/(yhigh - ylow);
	fprime_y = slope_ll + dfdz*(y-ylow);
	f_y += fprime_y*(x-xlow);
	return (f_y);
}
/* Call this to extrapolate from one point.  Crude approximation,
but occasionally useful */

double interpolate_discontinuity_onepoint
	(double x0,double f0, double fprime0, double x)
{
	
	return(f0 + fprime0*(x-x0));
}
Travel_Time_Function_Output uniform_time_table_interpolate(Ray_Endpoints x, char *phase, int mode)
{
	double delta;  /* epicentral distance in radians */
	double azimuth; /* source to receiver azimuth angle (radians) */
	Travel_Time_Function_Output o;
	XZ_table_uniform *ttable, *utable;
	double f_ll,f_hl,f_lh,f_hh;
	int ix_low, iz_low, ix_high, iz_high;
	double x_low, z_low, x_high, z_high;

	double u;  /* Slowness in s/km interpolated from tables */
	double vsource;  /* Source depth velocity interpolated from
		velocity depth vector */
	double vl, vh;  /* temporaries */
	double theta;  /* emergence angle used for elevation correction
			calculation */
	int discontinuity;


        /* First we compute the epicentral distance and azimuth */
        dist(rad(x.slat), rad(x.slon), rad(x.rlat), rad(x.rlon),
                &delta, &azimuth);

	/* Look up the correct table for this phase */
	ttable = (XZ_table_uniform *) getarr(time_tables_uniform,phase);
	utable = (XZ_table_uniform *) getarr(slow_tables_uniform,phase);
	if( (ttable == NULL) || (utable == NULL) )
	{
		char e[80];
		sprintf(e,"No travel time tables for phase %s",phase);
		o = set_time_table_error(e);
		return(o);
	}

	/* compute the indices assuming table is tabulated in degrees */
	ix_low = (int) ((deg(delta)-ttable->x0)/ttable->dx);
	iz_low = (int) ((x.sz-ttable->z0)/ttable->dz);
	ix_high = ix_low + 1;
	iz_high = iz_low + 1;

	if( (ix_high >= ttable->nx) || (iz_high >= ttable->nz) 
		|| (ix_low <0) || (iz_low<0) )
	{
		o = set_time_table_error("Requested point is outside table");
		return(o);
	}
	x_low = ((double)ix_low)*((ttable->dx))+ttable->x0;
	x_high = ((double)ix_high)*((ttable->dx))+ttable->x0;
	z_low = ((double)iz_low)*((ttable->dz))+ttable->z0;
	z_high = ((double)iz_high)*((ttable->dz))+ttable->z0;

	/* Now we have to handle the general problem of of how to handle
	discontinuities in the table.  These occur in earth models in three 
	forms:  (1) crossovers -- marked as points with continuous travel
	times, but discontinuous slopes, (2) termination of named branch,
	and (3) discontinuities in a generic branch like P (e.g. Pdiff
	passing to PkiKP in the core shadow.)  The handling of these
	is described above in the code for check_discontinuity */

	f_ll = ttable->values[ix_low][iz_low];
	f_hl = ttable->values[ix_high][iz_low];
	f_lh = ttable->values[ix_low][iz_high];
	f_hh = ttable->values[ix_high][iz_high];
	discontinuity = check_discontinuity(ttable->branch,ix_low, iz_low);

	switch (discontinuity)
	{
	case NOWAY:
		o.time = TIME_INVALID;
		o.dtdx = 0.0;
		o.dtdy = 0.0;
		o.dtdz = 0.0;
		return(o);
	case NO_PROBLEM:
	case NO_PROBLEM_VALUE:
		o.time = serendipity(x_low, z_low, x_high, z_high,
				f_ll, f_hl, f_lh, f_hh, deg(delta), x.sz);
		break;
	case TWO_LOW:
		o.time = interpolate_discontinuity_twopoint(x_low, z_low, 
				x_high, z_high, f_ll, f_lh, 
				ttable->slopes[ix_low][iz_low],
				ttable->slopes[ix_low][iz_high],
				deg(delta), x.sz);
		break;
	case ONE_LL:
		o.time = interpolate_discontinuity_onepoint(x_low,f_ll,
				ttable->slopes[ix_low][iz_low],deg(delta));
		break;
	case ONE_LH:
		o.time = interpolate_discontinuity_onepoint(x_low,f_lh,
				ttable->slopes[ix_low][iz_high],deg(delta));
	}


	if(mode == RESIDUALS_ONLY) 
	{
		o.dtdx = 0.0;
		o.dtdy = 0.0;
		o.dtdz = 0.0;
		return(o);
	}
	/* Now we turn to the travel time drivatives which we calculate
	from the slopes table */
	f_ll = utable->values[ix_low][iz_low];
	f_hl = utable->values[ix_high][iz_low];
	f_lh = utable->values[ix_low][iz_high];
	f_hh = utable->values[ix_high][iz_high];

	discontinuity = check_discontinuity(utable->branch,ix_low, iz_low);

	switch (discontinuity)
	{
	case NOWAY:
		u = SLOWNESS_INVALID;
		break ;
	case NO_PROBLEM:
	case NO_PROBLEM_VALUE:
		u = serendipity(x_low, z_low, x_high, z_high,
				f_ll, f_hl, f_lh, f_hh, deg(delta), x.sz);
		break;
	case TWO_LOW:
		u = interpolate_discontinuity_twopoint(x_low, z_low, 
				x_high, z_high, f_ll, f_lh, 
				utable->slopes[ix_low][iz_low],
				utable->slopes[ix_low][iz_high],
				deg(delta), x.sz);
		break;
	case ONE_LL:
		u = interpolate_discontinuity_onepoint(x_low,f_ll,
				utable->slopes[ix_low][iz_low],deg(delta));
		break;
	case ONE_LH:
		u = interpolate_discontinuity_onepoint(x_low,f_lh,
				utable->slopes[ix_low][iz_high],deg(delta));
	}
	/* We store the u grid in units of s/km so we do not have to 
	convert to travel time derivatives.  Init procedure 
	must handle this consistently.  First get the velocity at the
	source depth using a simple linear interpolation. */
	vl = ttable->velocity[iz_low];
	vh = ttable->velocity[iz_high];
	vsource = vl + (x.sz - ((double)iz_low*((ttable->dz))))
		*(vh - vl)/(ttable->dz);

	/* The x and y derivatives are now simple */
	o.dtdx = - u*sin(azimuth);
	o.dtdy = - u*cos(azimuth);
	o.dtdz = cos(asin(u*vsource))/vsource;
	/* We have to flip the sign of dtdz for upward traveling rays */
	if(ttable->branch[ix_high][iz_high] == UPWARD) o.dtdz = - o.dtdz;

	/* The following is seriously restrictive, but the alternative
	is to add near surface velocity as a seperate parameter for 
	each station.  This is rarely known, and probably better handled
	with a better travel time calculator function when it makes a
	difference.  That is, we will assume two things:
	(1)  ttable->velocity[0] is an appropriate velocity to use for
		elevation corrections.
	(2)  We are far enough away that we can use a simple correction
		based on dt/ddelta and some simple trigonometry.  This
		approximation will be incorrect when we are very close
		to the source and p varies rapidly with distance.  This
		is not the situations where tables like this should be
		used anyway.
	The first term is a correction along the horizontal plane for
	the projection of the elevation correction ray path to datum.
	The second term is adding back the time along the slant path
	elevation correction.  Note the -x.rz is used because rz is
	a "depth" not an elevation.
	*/

	theta = asin(u*ttable->velocity[0]);
	o.time -= u*(-x.rz)*tan(theta);
	o.time += (-x.rz)/((ttable->velocity[0])*cos(theta));

	return(o);
}
/* parallel functions for slowness */

Slowness_Function_Output set_slowness_table_error(char *error)
{
	Slowness_Function_Output o;
	elog_log(0,"uniform_slowness_table_interpolate: %s\n",error);
	o.ux = SLOWNESS_INVALID; 
	o.uy = SLOWNESS_INVALID; 
	o.duxdx = 0.0;
	o.duxdy = 0.0;
	o.duxdz = 0.0;
	o.duydx = 0.0;
	o.duydy = 0.0;
	o.duydz = 0.0;
	return o ; 
}
/* test function for handling discontinuous slope values. */
int slopes_valid(char c)
{
	if(c == JUMP) return(0);
	if(c == CROSSOVER) return(0);
	if(c==NOT_OBSERVABLE) return(0);
	return(1);
} 

Slowness_Function_Output uniform_slowness_table_interpolate(Ray_Endpoints x, char *phase, int mode)
{
	double delta, d_km;  /* epicentral distance in radians and km respectively*/
	double s2raz; /* source to receiver s2raz angle (radians) */
	double uaz;  /* slowness vector s2raz */
	Slowness_Function_Output o;
	XZ_table_uniform *utable;
	double f_ll,f_hl,f_lh,f_hh;
	int ix_low, iz_low, ix_high, iz_high;
	int ix, iz;
	double x_low, z_low, x_high, z_high;

	double u;  /* Slowness in s/km interpolated from tables */
	int discontinuity;
	/* temporaries used to calculate slowness derivatives */
	double sin_a, sin_a0, cos_a, cos_a0, dudz, dudr, du1, du2;

	


        /* First we compute the epicentral distance and source to receiver azimuth */
        dist(rad(x.slat), rad(x.slon), rad(x.rlat), rad(x.rlon),
                &delta, &s2raz);
	d_km = delta*RADIUS_EARTH;

        /* Now compute the slowness vector azimuth */
        dist(rad(x.rlat),rad(x.rlon),rad(x.slat),rad(x.slon),&delta,&uaz);
        uaz += M_PI;
        if(uaz >= 2.0*M_PI) uaz -= (2.0*M_PI);


	/* Look up the correct table for this phase */
	utable = (XZ_table_uniform *) getarr(slow_tables_uniform,phase);
	if( utable == NULL )
	{
		char e[80];
		sprintf(e,"No travel time tables for phase %s",phase);
		o = set_slowness_table_error(e);
		return(o);
	}

	/* compute the indices assuming table is tabulated in degrees */
	ix_low = (int) ((deg(delta)-utable->x0)/utable->dx);
	iz_low = (int) ((x.sz-utable->z0)/utable->dz);
	ix_high = ix_low + 1;
	iz_high = iz_low + 1;

	if( (ix_high >= utable->nx) || (iz_high >= utable->nz) 
		|| (ix_low <0) || (iz_low<0) )
	{
		o = set_slowness_table_error("Requested point is outside table");
		return(o);
	}
	x_low = ((double)ix_low)*((utable->dx))+utable->x0;
	x_high = ((double)ix_high)*((utable->dx))+utable->x0;
	z_low = ((double)iz_low)*((utable->dz))+utable->z0;
	z_high = ((double)iz_high)*((utable->dz))+utable->z0;

	f_ll = utable->values[ix_low][iz_low];
	f_hl = utable->values[ix_high][iz_low];
	f_lh = utable->values[ix_low][iz_high];
	f_hh = utable->values[ix_high][iz_high];
	discontinuity = check_discontinuity(utable->branch,ix_low, iz_low);

	switch (discontinuity)
	{
	case (NOWAY):
		o.ux = SLOWNESS_INVALID; 
		o.uy = SLOWNESS_INVALID; 
		o.duxdx = 0.0;
		o.duxdy = 0.0;
		o.duxdz = 0.0;
		o.duydx = 0.0;
		o.duydy = 0.0;
		o.duydz = 0.0;
		return(o);
	case NO_PROBLEM:
	case NO_PROBLEM_VALUE:
		u = serendipity(x_low, z_low, x_high, z_high,
				f_ll, f_hl, f_lh, f_hh, deg(delta), x.sz);
		break;
	case TWO_LOW:
		u = interpolate_discontinuity_twopoint(x_low, z_low, 
				x_high, z_high, f_ll, f_lh, 
				utable->slopes[ix_low][iz_low],
				utable->slopes[ix_low][iz_high],
				deg(delta), x.sz);
		break;
	case ONE_LL:
		u = interpolate_discontinuity_onepoint(x_low,f_ll,
				utable->slopes[ix_low][iz_low],deg(delta));
		break;
	case ONE_LH:
		u = interpolate_discontinuity_onepoint(x_low,f_lh,
				utable->slopes[ix_low][iz_high],deg(delta));
	}
	o.ux = u*sin(uaz);
	o.uy = u*cos(uaz);
	if(mode == RESIDUALS_ONLY) 
	{
		o.duxdx = 0.0;
		o.duxdy = 0.0;
		o.duxdz = 0.0;
		o.duydx = 0.0;
		o.duydy = 0.0;
		o.duydz = 0.0;
		return(o);
	}
	/* Now we turn to the slowness derivatives.  These require knowledge
	of both du/dr and angle terms.  First, we need to determine dudr using
	the slopes field of the utable */
	f_ll = utable->slopes[ix_low][iz_low];
	f_hl = utable->slopes[ix_high][iz_low];
	f_lh = utable->slopes[ix_low][iz_high];
	f_hh = utable->slopes[ix_high][iz_high];

	switch (discontinuity)
	{
	case (NOWAY):
		o.ux = SLOWNESS_INVALID; 
		o.uy = SLOWNESS_INVALID; 
		o.duxdx = 0.0;
		o.duxdy = 0.0;
		o.duxdz = 0.0;
		o.duydx = 0.0;
		o.duydy = 0.0;
		o.duydz = 0.0;
		return(o);
	case NO_PROBLEM:
		dudr = serendipity(x_low, z_low, x_high, z_high,
				f_ll, f_hl, f_lh, f_hh, deg(delta), x.sz);
		break;
	default:
		/* When one of the points contains a discontinuity, we
		simply hunt through the four nearest neighbor points 
		in a fixed order and return the nearest valid point. 
		This is fine for dudr since this quantity is usually
		rather small anyway. */
		ix = rint((deg(delta)-utable->x0)/utable->dx);
		iz = rint((x.sz-utable->z0)/utable->dz);
		if(ix==ix_low)
		{
			if(slopes_valid(utable->branch[ix_low][iz]))
				dudr = utable->slopes[ix_low][iz];
			else
			{
				if(slopes_valid(utable->branch[ix_high][iz]))
					dudr = utable->slopes[ix_high][iz];
				else 
				{
				    if(iz==iz_low)
					if(slopes_valid(utable->branch[ix_low][iz_high]))
					    dudr = utable->slopes[ix_low][iz_high];
					else
					    dudr = utable->slopes[ix_high][iz_high];
				    else
					if(slopes_valid(utable->branch[ix_low][iz_low]))
					    dudr = utable->slopes[ix_low][iz_low];
					else
					    dudr = utable->slopes[ix_high][iz_low];
				}
			}
		}
		else
		{
			if(slopes_valid(utable->branch[ix_high][iz]))
				dudr = utable->slopes[ix_high][iz];
			else
			{
				if(slopes_valid(utable->branch[ix_low][iz]))
					dudr = utable->slopes[ix_low][iz];
				else 
				{
				    if(iz==iz_low)
					if(slopes_valid(utable->branch[ix_high][iz_high]) )
					    dudr = utable->slopes[ix_high][iz_high];
					else
					    dudr = utable->slopes[ix_low][iz_high];
				    else
					if(slopes_valid(utable->branch[ix_high][iz_low]) )
					    dudr = utable->slopes[ix_high][iz_low];
					else
					    dudr = utable->slopes[ix_low][iz_low];
				}
			}
		}
		
	}
	sin_a0 = sin(s2raz);
	cos_a0 = cos(s2raz);
	sin_a = sin(uaz);
	cos_a = cos(uaz);
	o.duxdx = -sin_a*sin_a0*dudr - u*cos_a*cos_a0/d_km;
	o.duxdy = -sin_a*cos_a0*dudr + u*cos_a*sin_a0/d_km;
	o.duydx = -cos_a*sin_a0*dudr + u*sin_a*cos_a0/d_km;
	o.duydy = -cos_a*cos_a0*dudr - u*sin_a*sin_a0/d_km;

	/*All that is left is the calculation of du/dz.  du/dz is important only when
	the epicentral distance is close, but nonzero.  (oddly du/dz is 0 at zero offset). 
	We compute du/dz here by a simple finite difference from three depth points in the
	grid centered on the closest grid point in epicentral distance.  If these points are not
	on the same travel time branch we just set dudz to zero.  This may not always 
	be appropriate, but the philosphy is why throw out the baby with the bathwater 
	especially if the bathwater isn't that dirty.  */
	ix = rint((deg(delta)-utable->x0)/utable->dx);
	iz = rint((x.sz-utable->z0)/utable->dz);
	if(iz == 0)
	{
		/* We can only compute a forward difference in this case */
		if( slopes_valid(utable->branch[ix][iz]) && slopes_valid(utable->branch[ix][iz+1]) )
			dudz = (utable->values[ix][iz+1] - utable->values[ix][iz])/utable->dz;
		else
			dudz = 0.0;
	}
	else if(iz >= ((utable->nz)-1) )
		/* Can only compute a backward difference in this case */
		if( slopes_valid(utable->branch[ix][iz]) && slopes_valid(utable->branch[ix][iz-1]) )
			dudz = (utable->values[ix][iz] - utable->values[ix][iz-1])/utable->dz;
		else
			dudz = 0.0;
	else
	{
		du1 = (utable->values[ix][iz] - utable->values[ix][iz-1]);
		du2 = (utable->values[ix][iz+1] - utable->values[ix][iz]);

		if( slopes_valid(utable->branch[ix][iz]) && slopes_valid(utable->branch[ix][iz-1]) 
			&&  slopes_valid(utable->branch[ix][iz+1]) )
			dudz = (du1/utable->dz + du2/utable->dz)/2.0;
		else if( ! slopes_valid(utable->branch[ix][iz]))
			dudz = 0.0;
		else if( slopes_valid(utable->branch[ix][iz+1]) )
			dudz = du2/utable->dz;
		else
			dudz = du1/utable->dz;
	}
	o.duxdz = dudz*sin_a;
	o.duydz = dudz*cos_a;

	return(o);
}

/* $Id$ */
