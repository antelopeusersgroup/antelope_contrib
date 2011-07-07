#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "stock.h"
#include "arrays.h"
#include "coords.h"
#include "location.h"
/* We need to include the function prototype for the fortran function ttlvz*/
void ttlvz_(double *,double *,int *,double *,double *,double *, double *,
double *,double *,int *);


/* These are the interface routines to the simplest travel time calculator
based on a fortran function for layered velocity models.  It illustrates
a principle approach for interfacing any travel time calculator into 
this program.  Following an idea I stole from AVS, three interface routines
are required for any travel time calculator:
a.  An init procedure that loads earth structure information.  This can
be an actual velocity model, or a set of travel time tables.  It doesn't
matter provided it loads the information the exec procedure is going to
need to execute.  
b.  An exec procedure that takes generic arguments.  Any error conditions
should be set with elog_notify and an error condition flagged by
setting the time variable in the output structure to a negative number.
(This is an indisputable error in travel times since this is noncausal)
c.  A destroy procedure that frees up dynamic work areas set up in the 
init procedure.  This is necessary when velocity model are changed, for
example, on the fly.

The ttlvz interface that follows utilizes the confusing (at least to me)
scope rules of C.  I define the pointers for the layered velocity model
parameters, but alloc and free them in the init and destroy procedures
respectively.    

Author:  Gary L Pavlis
Written:  August 1996

Revisions:
September 13, 1996
Work on the design of the overall program showed we should rewrite 
the travel time interface to strive to make travel time functions 
independent of file systems.  The old code read velocity model files
directly from input.  I changed this to read the velocity parameters from
a Pf object.  This affects only the initialization code.  (GLP)
April 1997
Removed restriction that previously only allowed this calculator to 
be used for the phase called P and S.  I quickly learned that phases
like Lg, Pg, etc. are pretty easily calculated with this function.
This was done by using static Arr's to store velocity models rather
than fixed named variables (the old code).
*/
typedef struct Vmodel{
	double *velocity,*ztop;
	int nlayers;
} Vmodel;
static Arr *ttlvz_models=NULL;  /* contains pointers to Vmodel structures for
			each phase */

/* This function is called in ttlvz_init to parse the contents of the Pf
object containing the velocity model.  Both models are specified the same
way, so we call this same function for both model tables.  
Arguments:  
	t = Tbl set by pfget_tbl containing the velocity model table.
		It is assumed this tbl contains a set of strings that
		can be read with sscanf as velocity, depth pairs.  
	v = vector of layer velocities (space is alloced for *v and then
		values are set).  This pointer is set to alloced memory
		area here and on return it contains a vector of layer 
		velocities.  
	z = parallel vector to v for depths to layer tops.  
	n = length of z and n vectors (set by this routine)


*/
void ttlvz_parse_vmodel(Tbl *t,double **v, double **z, int *n)
{	
	int nlayers, i;

	nlayers = maxtbl(t);
	if(nlayers <= 0) 
		elog_die(0,"ttlvz_parse_vmodel:  No velocity model parameters for calculating travel times\n");

	/* the 100 here is an arbitrary constant, and this is only a warning */
	if(nlayers > 100) 
	{
		elog_complain(0,"Warning (ttlvz_parse_vmodel):  read data for %d layer velocity model\nConsider using a different travel time calculator for better performance\n", nlayers);
	}
	*v = (double *) calloc(nlayers, sizeof(double));
	*z = (double *) calloc(nlayers,sizeof(double));

	if( (*v == NULL) || (*z == NULL) )
		elog_die(1,"Cannot alloc memory in ttlvz_parse_vmodel function for %d layer model\n", nlayers);

	for(i=0;i<nlayers;++i)
	{
		char *line;
		line = gettbl(t,i);
		sscanf(line,"%lf %lf",(*v)+i,(*z)+i);
	}

	*n = nlayers;
}
/* Init routine for ttlvz function.

The first argument defines the phase to be initialized.  For this 
function this means either P or S.  Anything else and the function
returns immediately posting an error.  The second argument is a 
pointer to the input Pf object that defines travel time calculations.
This routine uses the key "velocity_model" for both the P and 
S tables stored as Tbl types in a pf as velocity, depth pairs.

This procedure returns 0 normally, if any problems occur it will
die since the current design specifies this.

Note the arguments here are pretty generic.  We probably could write
a generic init function interface like this, but it probably is not
necessary.
*/

int ttlvz_init(char *phase, Pf *pf)
{
	Tbl *t;

	Vmodel *mod;

	if(ttlvz_models == NULL) ttlvz_models = newarr(0);
	mod = (Vmodel *)malloc(sizeof(Vmodel));
	if(mod == NULL) elog_die(1,"ttlvz_init:  cannot malloc Vmodel structure for phase %s\n",phase);
	t = pfget_tbl(pf,"velocity_model");
	if(t == NULL) 
	{
		elog_complain(0,
		 "ttlvz_init:  no velocity model data found for phase %s\n",
			phase);
		return(1);
	}
	ttlvz_parse_vmodel(t,&(mod->velocity),&(mod->ztop),&(mod->nlayers));
	setarr(ttlvz_models,phase,mod);
	return(0);

}
#define RADIUS_EARTH 6370.8

/* this is the exec function for ttlvz.  The arguments for the init functions
can vary, but ALL exec functions must:
1.  Return the Travel_Time_Function_Output stucture
2.  Use the exact same argument list defined below.

The structure typedefs used here are defined in location.h 

phase is the name of the phase, which for ttlvz must be either P or S.
mode - how much to calculate flag.  If "ALL" (defined in location.h), we
calculate partial derivatives as well as the travel time.  Otherwise, only
the travel time is calculated */

Travel_Time_Function_Output ttlvz_time_exec(Ray_Endpoints x, 
		char *phase, int mode)
{
	double delta, d_km;  /* epicentral distance in radians and km resp.*/
	double azimuth;  /* source to receiver azimuth angle (radians) */
	double z0;  /* hold first layer depth (see below)  */
	Travel_Time_Function_Output o;
	Vmodel *mod;

	double *v, *z;
	int nz;
	double *work1,*work2;
	double p;
	int up;  /* direct ray flag from ttlvz */

	/* First we compute the epicentral distance and azimuth */
	dist(rad(x.slat), rad(x.slon), rad(x.rlat), rad(x.rlon), 
		&delta, &azimuth);
	d_km = delta*RADIUS_EARTH;

	/* We fetch the correct model structure for this phase */
	mod = (Vmodel *)getarr(ttlvz_models,phase);
	if (mod == NULL)
	{
		elog_complain(1,"ttlvz_time_exec: Don't know how to compute travel times for phase %s\n",phase);
		o.time = TIME_INVALID;
		return(o);
	}
 
	/* We cheat and distort the model to handle elevation corrections.
	We do this by storing the original velocity and depth of the 
	first layer in (v0,z0), and then set the first point to the 
	receiver depth.  I also do something devious here that could
	be confusing.  I set the pointers v and z to the P or S 
	model vectors depending on which phase I want to compute.  This
	avoids repetitious code, but adds a slight overhead at the end
	where we need to reset the first layer values again.*/
	z0 = mod->ztop[0];
	if(x.rz < mod->ztop[1])
		mod->ztop[0] = x.rz;
	else
	{
		elog_notify(0,"Warning (ttlvz_time_exec):  elevation correction error\nStation elevation %f lies below first layer depth %f\nElevation ignored\n",
			x.rz, mod->ztop[1]);
	}
	/* This could be avoided, but it is a relic of the earlier code */
	v = mod->velocity;
	z = mod->ztop;
	nz = mod->nlayers;

	work1 = (double *) calloc(2*nz,sizeof(double));
	work2 = (double *) calloc(2*nz,sizeof(double));
	if( (work1 == NULL) || (work2 == NULL) )
		elog_die(1,"ttlvz_time_exec:  Cannot alloc work arrays for phase %s\n",
			phase);


	ttlvz_(&d_km, &x.sz, &nz, v, z, work1, work2, &o.time, &p, &up); 
        if (o.time < 0.0)
        {
                elog_complain(1,"ttlvz_time_exec: ttlvz could not compute direct wave travel time for phase %s\n",phase);
                o.time = TIME_INVALID;
                return(o);
        }

	if(mode == ALL)
	{
	/* For this routine I calculate time derivatives from the
	ray parameter that is returned by ttlvz.*/
		int i,iz;
		double vsource;

		if(d_km <= 0.0)
		{
			o.dtdx = 0.0;
			o.dtdy = 0.0;
			p = 0.0;
		}
		else
		{
			o.dtdx = - fabs(p)*sin(azimuth);
			o.dtdy = - fabs(p)*cos(azimuth);
		}
		/* Find the velocity in the layer in which the
		source lies.  We need to for the dtdz calculation */
		for(i=1,iz=nz-1;i<nz;++i)
		{
			if(x.sz <= z[i])
			{
				iz = i-1;
				break;
			}
		}
		vsource = v[iz];
		o.dtdz = cos(asin(fabs(p)*vsource))/vsource;
		/* The strange logic for p<0.0 corrects the 
		sign of dtdz for the special case when the source
		is above the first layer top.  In this case, ttlvz
		returns a negative p, and the proper sign of dtdz 
		is negative. */
		if( (!up) || (p < 0.0) )
			o.dtdz = -o.dtdz;
				
	}
	mod->ztop[0] = z0;
	free(work1);
	free(work2);
	return(o);
}
/* This is the companion routine to ttlvz_time_exec that calculates 
slowness vector components and associated partial derivatives based 
on a layered model, but using spherical geometry to allow for 
angular differences that need to be accounted for with great
circle paths over large distances.  

ttlvz_ uses the "up" flag to signal direct, upward traveling 
rays.  The form of the equations for the slowness vector derivatives,
which by the way I've been unable to find published anywhere and
come here from a derivation I did myself, involve one term that
depends on the variation of slowness wrt epicentral distance.  
For refracted waves, this term is exactly zero so it is dropped.
It is included in this routine only for upward directed rays from
deeper sources.  Otherwise we use only the ray angle dependent terms.

Author:  Gary L Pavlis
Written:  August 1996
Modification:  April 1998, bug fix. Original code failed for z=0
when computing derivatives of u wrt z.  Always produced a infinitity.
*/

/* These constants are used in numerical calculation of du/dr below */

#define DX_STEP_SIZE 5.0 /* starting distance step size in km */
#define MINIMUM_DX 0.5 /* minimum step size in km */
#define D_KM_CUTOFF 1.0 /* Used to avoid singularity of formula at 0 offset*/
#define DX_SCALING_FACTOR 0.5  /* step size is multiplied by this 
			factor when necessary to avoid edges until it works
			or drops below MINIMUM_DX.  This constant must be
			less than 1 */
/* constant for dz numerical derivative*/
#define DZ_STEP_SIZE 1.0  /* depth step in km */

Slowness_Function_Output ttlvz_slow_exec (Ray_Endpoints x, 
		char *phase, int mode)
{
	double delta, d_km;  /* epicentral distance in radians and km resp.*/
	double s2raz;  /* source to receiver azimuth angle (radians) */
	double uaz;  /* gcp expected azimuth of propagation of arrival 
				(uaz != s2raz on a spherical earth) */
	double z0;  /* hold first layer depth (see below)  */
	Slowness_Function_Output o;

	Vmodel *mod;

	double *v, *z;
	double *work1,*work2;
	int nz;
	double time, p;
	int up;  /* direct ray flag from ttlvz */
	double ztmp;

	/* First we compute the epicentral distance and azimuth */
	dist(rad(x.slat), rad(x.slon), rad(x.rlat), rad(x.rlon), 
		&delta, &s2raz);
	d_km = delta*RADIUS_EARTH;
	/* Now compute the great circle path propagation azimuth */
	dist(rad(x.rlat),rad(x.rlon),rad(x.slat),rad(x.slon),&delta,&uaz);
	uaz += M_PI;
	if(uaz >= 2.0*M_PI) uaz -= (2.0*M_PI);

	/* We fetch the correct model structure for this phase */
	mod = (Vmodel *)getarr(ttlvz_models,phase);
	if (mod == NULL)
	{
		elog_complain(1,"ttlvz_slow_exec: Don't know how to compute slowness vectors for phase %s\n",phase);
		o.ux = SLOWNESS_INVALID;
		o.uy = SLOWNESS_INVALID;
		return(o);
	}
 
	/* We cheat and distort the model to handle elevation corrections.
	We do this by storing the original velocity and depth of the 
	first layer in (v0,z0), and then set the first point to the 
	receiver depth.  I also do something devious here that could
	be confusing.  I set the pointers v and z to the P or S 
	model vectors depending on which phase I want to compute.  This
	avoids repetitious code, but adds a slight overhead at the end
	where we need to reset the first layer values again.*/
	z0 = mod->ztop[0];
	if(x.rz < mod->ztop[1])
		mod->ztop[0] = x.rz;
	else
	{
		elog_notify(0,"Warning (ttlvz_slowness_exec):  elevation correction error\nStation elevation %f lies below first layer depth %f\nElevation ignored\n",
			x.rz, mod->ztop[1]);
	}
	/* This could be avoided, but it is a relic of the earlier code */
	v = mod->velocity;
	z = mod->ztop;
	nz = mod->nlayers;

	work1 = (double *) calloc(2*nz,sizeof(double));
	work2 = (double *) calloc(2*nz,sizeof(double));
	if( (work1 == NULL) || (work2 == NULL) )
		elog_die(1,"ttlvz_slow_exec:  Cannot alloc work arrays for phase %s\n",
			phase);

	ttlvz_(&d_km, &x.sz, &nz, v, z, work1, work2, &time, &p, &up);
        if (time < 0.0)
        {
                elog_complain(1,"ttlvz_time_exec: ttlvz could not compute direct wave slowness vector for phase %s\n",phase);
                o.ux = SLOWNESS_INVALID;
                o.uy = SLOWNESS_INVALID;
                return(o);
        }

	/* This would be executed only for close, shallow sources.  
	It could probably be deleted, but we'll do it for completeness*/
	if(p<0.0) p = -p;

	o.ux = p*sin(uaz);
	o.uy = p*cos(uaz);

 	if(mode == ALL)
	{
		double dx, dudr, dudz;
		double sin_a, sin_a0, cos_a, cos_a0;
		double p0,p1,p3,p4;
		double dis;

		sin_a0 = sin(s2raz);
		cos_a0 = cos(s2raz);
		sin_a = sin(uaz);
		cos_a = cos(uaz);
		if(up)
		{
			/* we calculate dudr by a 5 point central 
			difference formula.  I don't mess with
			truncation error calculations here, but I 
			do have to assure distance >=0 and < direct
			to refracted crossover.  Both points are
			singular.  When we get to close to the edges
			we revert to simple forward or backward difference 
			formula.*/

			dx = DX_STEP_SIZE;
			if( d_km < 2.0*MINIMUM_DX)
			{
				dis = d_km+dx;
				ttlvz_(&dis,&x.sz, &nz, v, z, 
					work1, work2, &time, &p1, &up);
        			if (time < 0.0)
        			{
                		  elog_complain(1,"ttlvz_time_exec: ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
                		  o.ux = SLOWNESS_INVALID;
                		  o.uy = SLOWNESS_INVALID;
                		  return(o);
        			}
				dudr = (p1-p)/dx;
			}
			else if(d_km-2.0*dx < 0.0)
			{
				while(d_km-2.0*dx < 0.0)
					dx *= DX_SCALING_FACTOR;
			}
			/* Now check that the value at x+2*dx is
			still an upward branch of the travel time curve.
			If it isn't, reset the step size until it is.
			When this isn't possible, revert to a backward
			difference at the minumum step size. */
			dis = d_km+2.0*dx;
			ttlvz_(&dis,&x.sz, &nz, v, z, 
					work1, work2, &time, &p4, &up);
        		if (time < 0.0)
        		{
                	  elog_complain(1,"ttlvz_time_exec: ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
                	  o.ux = SLOWNESS_INVALID;
                	  o.uy = SLOWNESS_INVALID;
                	  return(o);
        		}
			while(!up && (dx>=MINIMUM_DX) )
			{
				dx *= DX_SCALING_FACTOR;
				dis = d_km+2.0*dx;
				ttlvz_(&dis,&x.sz, &nz, v, z, 
					work1, work2, &time, &p4, &up);
        			if (time < 0.0)
        			{
                		  elog_complain(1,"ttlvz_time_exec: ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
                	   	  o.ux = SLOWNESS_INVALID;
                	  	  o.uy = SLOWNESS_INVALID;
                		  return(o);
        			}
			}
			if(dx<MINIMUM_DX)
			{
				/* In this situation, we revert to 
				a two point backward difference */
				dx = MINIMUM_DX;
				/* special for shallow sources close */
				if(d_km-dx < 0.0) dx = d_km;
				dis = d_km +dx;
				ttlvz_(&dis,&x.sz, &nz, v, z, 
					work1, work2, &time, &p0, &up);
				dudr = (p-p0)/dx;
        			if (time < 0.0)
        			{
                		  elog_complain(1,"ttlvz_time_exec: ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
                	   	  o.ux = SLOWNESS_INVALID;
                	  	  o.uy = SLOWNESS_INVALID;
                		  return(o);
        			}
			}
			else
			{
				/* if we land here we can safely use the
				full 5 point formula.  Five point is 
				a misnomer since the central point 
				cancels out, but so what*/
				dis = d_km-2.0*dx;
				ttlvz_(&dis,&x.sz, &nz, v, z, 
					work1, work2, &time, &p0, &up);
        			if (time < 0.0)
        			{
                		  elog_complain(1,"ttlvz_time_exec: ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
                	   	  o.ux = SLOWNESS_INVALID;
                	  	  o.uy = SLOWNESS_INVALID;
                		  return(o);
        			}
				dis = d_km-dx;
				ttlvz_(&dis,&x.sz, &nz, v, z, 
					work1, work2, &time, &p1, &up);
        			if (time < 0.0)
        			{
                		  elog_complain(1,"ttlvz_time_exec: ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
                	   	  o.ux = SLOWNESS_INVALID;
                	  	  o.uy = SLOWNESS_INVALID;
                		  return(o);
        			}
				dis = d_km+dx;
				ttlvz_(&dis,&x.sz, &nz, v, z, 
					work1, work2, &time, &p3, &up);
        			if (time < 0.0)
        			{
                		  elog_complain(1,"ttlvz_time_exec: ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
                	   	  o.ux = SLOWNESS_INVALID;
                	  	  o.uy = SLOWNESS_INVALID;
                		  return(o);
        			}
				dis = d_km+2.0*dx;
				ttlvz_(&dis,&x.sz, &nz, v, z, 
					work1, work2, &time, &p4, &up);
        			if (time < 0.0)
        			{
                		  elog_complain(1,"ttlvz_time_exec: ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
                	   	  o.ux = SLOWNESS_INVALID;
                	  	  o.uy = SLOWNESS_INVALID;
                		  return(o);
        			}
				dudr = (p0-8.0*p1+8.0*p3-p4)/(12.0*dx);
			}
			/* bug fix Feb 1998.  Else conditional is necessary
			to avoid an indeterminate form that arises when 
			offset is 0.0 */
			if(d_km > D_KM_CUTOFF) 
			{
			  o.duxdx = -sin_a*sin_a0*dudr - p*cos_a*cos_a0/d_km;
			  o.duxdy = -sin_a*cos_a0*dudr + p*cos_a*sin_a0/d_km;
			  o.duydx = -cos_a*sin_a0*dudr + p*sin_a*cos_a0/d_km;
			  o.duydy = -cos_a*cos_a0*dudr - p*sin_a*sin_a0/d_km;
			}
			else
			{
			  o.duxdx = -dudr;
			  o.duxdy = 0.0;
			  o.duydx = 0.0;
			  o.duydy = -dudr;
			}

			/* We do the z derivatives more crudely because
			they are of significant size only when a source is
			close and below the array.  In that situation,
			accuracy should not be a serious problem with 
			fairly crude scaling like this.  We do a simple
			backward difference to avoid crossing over into
			refracted branches. However, when source is shallow
			we always use a forward difference to avoid 
			negative depths problems.  */
			if(x.sz <= DZ_STEP_SIZE)
			{
				ztmp = x.sz+DZ_STEP_SIZE;
				ttlvz_(&d_km,&ztmp, &nz, v, z, 
					work1, work2, &time, &p0, &up);
        			if (time < 0.0)
        			{
                		  elog_complain(1,"ttlvz_time_exec: ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
                	   	  o.ux = SLOWNESS_INVALID;
                	  	  o.uy = SLOWNESS_INVALID;
                		  return(o);
        			}
				dudz = (p0 - p)/DZ_STEP_SIZE;
			}
			else
			{
				ztmp = x.sz-DZ_STEP_SIZE;
				ttlvz_(&d_km,&ztmp, &nz, v, z, 
					work1, work2, &time, &p0, &up);
        			if (time < 0.0)
        			{
                		  elog_complain(1,"ttlvz_time_exec: ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
                	   	  o.ux = SLOWNESS_INVALID;
                	  	  o.uy = SLOWNESS_INVALID;
                		  return(o);
        			}
				dudz = (p - p0)/DZ_STEP_SIZE;
			}
			o.duxdz = dudz*sin_a;
			o.duydz = dudz*cos_a;		
		}
		else
		{
			o.duxdx = -p*cos_a*cos_a0/d_km;
			o.duxdy = p*cos_a*sin_a0/d_km;
			o.duydx = p*sin_a*cos_a0/d_km;
			o.duydy = -p*sin_a*sin_a0/d_km;
			/* With constant velocity layer models, refracted
			rays have exactly zero slowness derviatives wrt z*/

			o.duxdz = 0.0;
			o.duydz = 0.0;  
		}
	}
	mod->ztop[0] = z0;
	free(work1);
	free(work2);
	return(o);
}
/* This is the cleanup procedure that needs to be called if the
model or travel time calculation method is changed */
void free_Vmodel(void *p)
{
	Vmodel *m;
	m = (Vmodel *) p;
	free(m->velocity);
	free(m->ztop);
	free(m);
}
void ttlvz_destroy()
{
	freearr(ttlvz_models,free_Vmodel);
}
	
	 
		

	
	

/* $Id$ */
