#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "stock.h"
#include "arrays.h"
#include "coords.h"
#include "db.h"
#include "elog.h"
#include "location.h"
#include "tt.h"

/* This set of routines compute travel times for one-dimensional, 
constant velocity layer models via the fortran function ttlvz.  
This is a stardard simple ray based calulator for this type of model.
It assumes a flat earth, but uses great circle path distances in
computations to compute distance.  

Author:  Gary L. Pavlis
Written:  July 1999 stealing generously from ttlvz_inteface.c file
in libgenloc.  
*/


/* We need to include the function prototype for the fortran function ttlvz*/
void ttlvz_(double *,double *,int *,double *,double *,double *, double *,
double *,double *,int *);

typedef struct Vmodel{
	char *name,*property;  /* name of model and property (P or S vel) */
	double *velocity,*ztop;
	int nlayers;
} Vmodel;

/* The hook for this function is just an Arr indexed by model:name 
that is used to lookup a particular model that has already been loaded */
typedef struct tt1dcvl_hook {
	Vmodel *current_model;
	Arr *ttlvz_models;
} tt1dcvl_hook;

/* Shared libraries in Solaris will call an initialization routine 
by this name when the library is first accessed.  This is the
initialization routine for this calculator.  It reads a database
name from the environment using a default if the variable is 
not set.  
*/
static Dbptr modeldb;
#define VMODEL_DBNAME_CUSTOM "VELOCITY_MODEL_DATABASE"
#define VMODEL_DBNAME_DEFAULT "vmodel"

int _tt1dcvl_has_run = 0;

void tt1dcvl_init();

void __attribute__ ((constructor))
_tt1dcvl_init()
{
    tt1dcvl_init() ;

    _tt1dcvl_has_run++;
}

void tt1dcvl_init()
{
	char *dbpath;
	char *dbname;

	dbname=getenv(VMODEL_DBNAME_CUSTOM);
	if(dbname==NULL)
		dbpath = datapath (NULL,"tables/genloc/db",VMODEL_DBNAME_DEFAULT,NULL);
	else
		dbpath = datapath (NULL,"tables/genloc/db",dbname,NULL);
	
	if (dbpath == NULL) { 
	    elog_die( 0, "tt1dcvl database open failed\n" ) ; 
	}
	if(dbopen(dbpath,"r",&modeldb) == dbINVALID) {
	    elog_die(0,"Could not open velocity model database '%s' during libtt1dcvl initialization\n"
	    	  "Exiting because all calls to this calculator will fail\n",
		  dbpath);
	}
}

static void free_Vmodel(void *modp)
{
	Vmodel *mod = (Vmodel *) modp;

	free(mod->name);
	free(mod->property);
	free(mod->velocity);
	free(mod->ztop);
	free(mod);
}
static void tt1dcvl_free_hook(void *oldp)
{
	tt1dcvl_hook *old = (tt1dcvl_hook *) oldp;

	freearr(old->ttlvz_models,free_Vmodel);
}
char *make_vmodel_key(char *model,char *property)
{
        int size;
        char *key;
        size = strlen(model) + strlen(property) + 2;
	allot(char*,key,size);
        strcpy(key,model);
        strcat(key,":");
        strcat(key,property);
        return(key);
}
/* Reads constant velocity, 1d, layered model from database keyed
by the name "mod" and for the physical quantity "property".  This
assumes then that "property" is defined for a particular phase
by a diffenent function.  Since this library only works for local
event P and S first arrival phases, the calculator should reject
anything else and map property onto appropriate keys for the database.
See below.

Algorithm is based on a css3.0 extension table called mod1d.
The function normally returns a pointer to an alloced Vmodel 
structure.  Any errors lead to a returning a NULL pointer. 
Messages are left in elog explaining reason when pointer is NULL.

Note use of db pointer initialized through _init above.

Author:  G Pavlis
Written:  July 1999
*/
Vmodel  *read_model_from_db(char *mod, char *property)
{
	Dbptr db,dbs,dbs2;
	char sstring[80];  /* used to define subset condition */
	long nrecs;
	Tbl *sortkeys;
	Vmodel *model;
	int i;

	db = dblookup(modeldb,0,"mod1d",0,0);
	if(db.table == dbINVALID)
	{
		elog_complain(0,"dblookup for mod1d table failed\nRequired schema extensions are probably not defined\n");
		return(NULL);
	}
	sprintf(sstring,"(modname =~ /%s/) && (paramname =~ /%s/)",
			mod, property);
	dbs = dbsubset(db,sstring,0);
	/* We sort by the depth in ascending order or we have real problems */
	sortkeys = newtbl(1);
	pushtbl(sortkeys,"depth");
	dbs2 = dbsort(dbs,sortkeys,0,0);
	
	dbquery(dbs2,dbRECORD_COUNT,&nrecs);
	if(nrecs <= 0)
	{
		elog_log(0,"No match in database for model named %s for property %s\n",
			mod,property);
		freetbl(sortkeys,0);
		dbfree(dbs);
		dbfree(dbs2);
		return(NULL);
	}
	allot(Vmodel *,model,1);
	allot(double *,model->velocity,nrecs);
	allot(double *,model->ztop,nrecs);
	model->name = strdup(mod);
	model->property = strdup(property);
	model->nlayers = nrecs;

	for(dbs2.record=0,i=0;dbs2.record<nrecs;++dbs2.record,++i)
	{
		if(dbgetv(dbs2,0,"paramval",&(model->velocity[i]),
			"depth",&(model->ztop[i]),NULL ) == dbINVALID)
		{
			elog_log(0,"dbgetv error loading record %d of model %s and property %s\nModel cannot be accessed -- expect additional errors\n",
				i,mod,property);
			free_Vmodel(model);
			return(NULL);
		}		
	}
	/* cleanup to avoid memory leaks */
	freetbl(sortkeys,0);
	dbfree(dbs);
	dbfree(dbs2);
	return(model);
}
/* As name implies this function manages the hook used by this
calculator.  This does "lazy initialization" meaning a model is
not loaded into memory until it is requested.  Function uses 
an arr to index loaded models using a key created by the 
make_vmodel_key function above.  The algorithm is basically
when a model is not found in the arr, we attempt to read it.
Normal return is 0 on sucesss, -5 if the read failed.  This 
can be passed directly back through ttcalc interface as a 
"no model" error.
*/


int manage_hook_1dcvl (
	char *model,
	char *property,
	Hook **hookp)
{
	tt1dcvl_hook *old;
	static char *key;
	Vmodel *test;

	if(*hookp == NULL) {
		*hookp = new_hook ( tt1dcvl_free_hook );
		allot(tt1dcvl_hook *, old, 1);
		(*hookp)->p = old;
		old->ttlvz_models = newarr(0);
		old->current_model = NULL;
	}
	else
	{
		old = (*hookp)->p;
	}
	key = make_vmodel_key(model,property);
	test = (Vmodel *)getarr(old->ttlvz_models,key);
	if(test == NULL)
	{
		test = read_model_from_db(model,property);
		if(test == NULL) return(-5);
		setarr(old->ttlvz_models,key,test);
	}
	free(key);
	old->current_model = test;
	return(0);
}
/* This dumb little function looks at the input string phase
and maps the name onto a "property" field used to define a 1d
model in the database.  It returns a NULL for phases it 
refuses to consider.  The list here is fairly generous by
including things like Pn and Pg.  It is called by tt and u
calculators below.
*/
#define SMODEL "Svelocity"
#define PMODEL "Pvelocity"
char *map_phase_to_property(char *p)
{
	/* Changed Nov. 2007 to add Pb,  Sb, and S* */
	char *validP[5]={"P","Pg","Pn","P*","Pb"};
	char *validS[6]={"S","Sg","Sn","Lg","S*","Sb"};
	int nvP=5,nvS=6;  /* size of the above lists.  I'm sure there is
			a more clever way to do this, but this will work.
			Just change it if phases are added to the list */
	int i;

	for(i=0;i<nvP;++i)
		if(!strcmp(p,validP[i])) return(strdup(PMODEL));
	for(i=0;i<nvS;++i)
		if(!strcmp(p,validS[i])) return(strdup(SMODEL));
	return(NULL);
}
/*
This is the main compute function for travel times for this calculator.
It uses the manage_hook function to handle lazy initialization of
velocity models from the database.  The core of the function is 
the function ttlvz_time_exec function found in libgenloc.  I cut
out the main analytic code, changed some symbols as needed, and
changed the initialization method via a hook instead of a direct
Arr access used in ttlvz_time_exec.  Soo, the calculator should
yield exactly the same results at ttlvz_time_exec.

I have NOT implemented all the functionality specified in the 
tt(3) (ttcalc) generic travel time interface.  In particular,
"mode" definitions beyond asking for travel times and derivatives
will be silently ignored.  A travel time will always be returned
in the value field of the TTTime structure.  Derivatives are only
computed if requested.  

Arguments:

x - TTGeoemtry passed directly
d_km - offset in kilometers 
azimuth - azimuth returned from dist function (see below)
phase - phase name
model - model name
property - database property used to tag appropriate entries in model database
	(see map_phase_to_property immediately above)
mode - passed down from call to ttcalc defining what to calculate
	(see comment above about limitations)
hookp - hook (daahhh ) used by this calculator (see manage_hook function)

*/
TTTime *tt1dcvl_compute_atime(TTGeometry *x,double d_km,double azimuth, 
		char *phase, char *model, char *property, 
		int mode, Hook **hookp)
{
	double z0;  /* hold first layer depth (see below)  */
	TTTime *t;
	Vmodel *mod;

	double *v, *z;
	int nz;
	double *work1,*work2;
	double p;
	int up;  /* direct ray flag from ttlvz */
	int result;
	tt1dcvl_hook *h;  

	result = manage_hook_1dcvl(model,property,hookp);
	if(result)
	{
		elog_complain(0,"Failure in computing travel time for %s phase with model %s\n",
			phase, model);
		return(NULL);
	}
	allot(TTTime *,t,1);
	strcpy(t->phase,phase);

	/* We fetch the correct model structure for this phase.
	The extra step seems needed because the hook p pointer
	has no intrinsic type and need to be explicitly
	cast to be resolved. */
	h = (tt1dcvl_hook *)((*hookp)->p);
	mod = (Vmodel *)(h->current_model);
 
	/* We cheat and distort the model to handle elevation corrections.
	We do this by storing the original velocity and depth of the 
	first layer in (v0,z0), and then set the first point to the 
	receiver depth.  */
	z0 = mod->ztop[0];
	if(x->receiver.z < mod->ztop[1])
		mod->ztop[0] = x->receiver.z;
	else
	{
		elog_log(0,"Warning (ttlvz_time_exec):  elevation correction error\nStation elevation %lf lies below first layer depth %lf\nElevation ignored\n",
			x->receiver.z, mod->ztop[1]);
	}
	/* This could be avoided, but it is a relic of the earlier code,
	It also reduces the symbolic complexity a lot. */
	v = mod->velocity;
	z = mod->ztop;
	nz = mod->nlayers;

	allot(double *,work1,2*nz);
	allot(double *,work2,2*nz);


	ttlvz_(&d_km, &(x->source.z), &nz, v, z, work1, work2, &(t->value), &p, &up); 
        if (t->value < 0.0)
        {
		elog_log(0,"ttlvz could not compute travel time for phase %s\n",phase);
		free(t);
		free(work1);
		free(work2);
		mod->ztop[0] = z0;
		return(NULL);
        }
	if (mode & TT_DERIVATIVES) 
	{
	/* For this routine I calculate time derivatives from the
	ray parameter that is returned by ttlvz.*/
		int i,iz;
		double vsource;

		if(d_km <= 0.0)
		{
			t->deriv[0] = 0.0;
			t->deriv[1] = 0.0;
			p = 0.0;
		}
		else
		{
			t->deriv[0] = - fabs(p)*sin(azimuth);
			t->deriv[1] = - fabs(p)*cos(azimuth);
		}
		/* Find the velocity in the layer in which the
		source lies.  We need to for the dtdz calculation */
		for(i=1,iz=nz-1;i<nz;++i)
		{
			if(x->source.z <= z[i])
			{
				iz = i-1;
				break;
			}
		}
		vsource = v[iz];
		t->deriv[2] = cos(asin(fabs(p)*vsource))/vsource;
		/* The strange logic for p<0.0 corrects the 
		sign of dtdz for the special case when the source
		is above the first layer top.  In this case, ttlvz
		returns a negative p, and the proper sign of dtdz 
		is negative. */
		if( (!up) || (p < 0.0) )
			t->deriv[2] = -(t->deriv[2]);
				
	} else { 
	    t->deriv[0] = 0.0 ;
	    t->deriv[1] = 0.0 ;
	    t->deriv[2] = 0.0 ;
	}

	mod->ztop[0] = z0;
	free(work1);
	free(work2);
	return(t);
}
/* This is the companion routine to tt1dcvl_compute_atime that calculates 
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

Rewrite:  This routine is a direct translation of the ttlvz_slowness_exec
function in genloc.  The algorithm is identical.  Only the symbols have
been changed to protect the innocent.
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
TTSlow *tt1dcvl_compute_slowness(TTGeometry *x,double d_km,
		double uaz, double s2raz, 
		char *phase, char *model, char *property, 
		int mode, Hook **hookp)
{
	double z0;  /* hold first layer depth (see below)  */
	Vmodel *mod;

	double *v, *z;
	int nz;
	double *work1,*work2;
	double time, p;
	int up;  /* direct ray flag from ttlvz */
	int result;
	tt1dcvl_hook *h;  

	TTSlow *slow;
	double ztmp;

	result = manage_hook_1dcvl(model,property,hookp);
	if(result)
	{
		elog_complain(0,"Failure in computing slowness vector for %s phase with model %s\n",
			phase, model);
		return(NULL);
	}
	allot(TTSlow *,slow,1);
	strcpy(slow->phase,phase);

	/* We fetch the correct model structure for this phase.
	The extra step seems needed because the hook p pointer
	has no intrinsic type and need to be explicitly
	cast to be resolved. */
	h = (tt1dcvl_hook *)((*hookp)->p);
	mod = (Vmodel *)(h->current_model);
 
	/* We cheat and distort the model to handle elevation corrections.
	We do this by storing the original velocity and depth of the 
	first layer in (v0,z0), and then set the first point to the 
	receiver depth.  I also do something devious here that could
	be confusing.  I set the pointers v and z to the P or S 
	model vectors depending on which phase I want to compute.  This
	avoids repetitious code, but adds a slight overhead at the end
	where we need to reset the first layer values again.*/
	z0 = mod->ztop[0];
	if(x->receiver.z < mod->ztop[1])
		mod->ztop[0] = x->receiver.z;
	else
	{
		elog_log(0,"Warning (ttlvz_slowness_exec):  elevation correction error\nStation elevation %lf lies below first layer depth %lf\nElevation ignored\n",
			x->receiver.z, mod->ztop[1]);
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

	ttlvz_(&d_km, &(x->source.z), &nz, v, z, work1, work2, &time, &p, &up);
        if (time < 0.0)
        {
                elog_log(1,"ttlvz could not compute slowness vector for phase %s\n",phase);
		free(slow);
		free(work1);
		free(work2);
		mod->ztop[0] = z0;
		return(NULL);
        }

	/* This would be executed only for close, shallow sources.  
	It could probably be deleted, but we'll do it for completeness*/
	if(p<0.0) p = -p;

	slow->ux = p*sin(uaz);
	slow->uy = p*cos(uaz);

 	if (mode & TT_DERIVATIVES) 
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
				ttlvz_(&dis,&(x->source.z), &nz, v, z, 
					work1, work2, &time, &p1, &up);
        			if (time < 0.0)
        			{
                		  elog_log(1,"ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
       				  free(slow);
				  free(work1);
			 	  free(work2);
				  mod->ztop[0] = z0;
				  return(NULL);
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
			ttlvz_(&dis,&(x->source.z), &nz, v, z, 
					work1, work2, &time, &p4, &up);
        		if (time < 0.0)
        		{
                		elog_log(1,"ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
   				free(slow);
				free(work1);
				free(work2);
				mod->ztop[0] = z0;
				return(NULL);
        		}
			while(!up && (dx>=MINIMUM_DX) )
			{
				dx *= DX_SCALING_FACTOR;
				dis = d_km+2.0*dx;
				ttlvz_(&dis,&(x->source.z), &nz, v, z, 
					work1, work2, &time, &p4, &up);
         			if (time < 0.0)
        			{
                		  elog_log(1,"ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
       				  free(slow);
				  free(work1);
			 	  free(work2);
				  mod->ztop[0] = z0;
				  return(NULL);
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
				ttlvz_(&dis,&(x->source.z), &nz, v, z, 
					work1, work2, &time, &p0, &up);
				dudr = (p-p0)/dx;
        			if (time < 0.0)
        			{
                		  elog_log(1,"ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
       				  free(slow);
				  free(work1);
			 	  free(work2);
				  mod->ztop[0] = z0;
				  return(NULL);
				}

 
 			}
			else
			{
				/* if we land here we can safely use the
				full 5 point formula.  Five point is 
				a misnomer since the central point 
				cancels out, but so what*/
				dis = d_km-2.0*dx;
				ttlvz_(&dis,&(x->source.z), &nz, v, z, 
					work1, work2, &time, &p0, &up);
        			if (time < 0.0)
        			{
                		  elog_log(1,"ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
       				  free(slow);
				  free(work1);
			 	  free(work2);
				  mod->ztop[0] = z0;
				  return(NULL);
				}
				dis = d_km-dx;
				ttlvz_(&dis,&(x->source.z), &nz, v, z, 
					work1, work2, &time, &p1, &up);
        			if (time < 0.0)
        			{
                		  elog_log(1,"ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
       				  free(slow);
				  free(work1);
			 	  free(work2);
				  mod->ztop[0] = z0;
				  return(NULL);
				}

				dis = d_km+dx;
				ttlvz_(&dis,&(x->source.z), &nz, v, z, 
					work1, work2, &time, &p3, &up);
        			if (time < 0.0)
        			{
                		  elog_log(1,"ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
       				  free(slow);
				  free(work1);
			 	  free(work2);
				  mod->ztop[0] = z0;
				  return(NULL);
				}

				dis = d_km+2.0*dx;
				ttlvz_(&dis,&(x->source.z), &nz, v, z, 
					work1, work2, &time, &p4, &up);
        			if (time < 0.0)
        			{
                		  elog_log(1,"ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
       				  free(slow);
				  free(work1);
			 	  free(work2);
				  mod->ztop[0] = z0;
				  return(NULL);
				}

				dudr = (p0-8.0*p1+8.0*p3-p4)/(12.0*dx);
			}
			/* bug fix Feb 1998.  Else conditional is necessary
			to avoid an indeterminate form that arises when 
			offset is 0.0 */
			if(d_km > D_KM_CUTOFF) 
			{
			  slow->uxderiv[0] = -sin_a*sin_a0*dudr - p*cos_a*cos_a0/d_km;
			  slow->uxderiv[1] = -sin_a*cos_a0*dudr + p*cos_a*sin_a0/d_km;
			  slow->uyderiv[0] = -cos_a*sin_a0*dudr + p*sin_a*cos_a0/d_km;
			  slow->uyderiv[1] = -cos_a*cos_a0*dudr - p*sin_a*sin_a0/d_km;
			}
			else
			{
			  slow->uxderiv[0] = -dudr;
			  slow->uxderiv[1] = 0.0;
			  slow->uyderiv[0] = 0.0;
			  slow->uyderiv[1] = -dudr;
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
			if((x->source.z) <= DZ_STEP_SIZE)
			{
				ztmp = (x->source.z)+DZ_STEP_SIZE;
				ttlvz_(&d_km,&ztmp, &nz, v, z, 
					work1, work2, &time, &p0, &up);
        			if (time < 0.0)
        			{
                		  elog_log(1,"ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
       				  free(slow);
				  free(work1);
			 	  free(work2);
				  mod->ztop[0] = z0;
				  return(NULL);
				}
				dudz = (p0 - p)/DZ_STEP_SIZE;
			}
			else
			{
				ztmp = (x->source.z)-DZ_STEP_SIZE;
				ttlvz_(&d_km,&ztmp, &nz, v, z, 
					work1, work2, &time, &p0, &up);
        			if (time < 0.0)
        			{
                		  elog_log(1,"ttlvz could not compute direct wave slowness vector for phase %s while computing derivatives\n",phase);
       				  free(slow);
				  free(work1);
			 	  free(work2);
				  mod->ztop[0] = z0;
				  return(NULL);
				}
				dudz = (p - p0)/DZ_STEP_SIZE;
			}
			slow->uxderiv[2] = dudz*sin_a;
			slow->uyderiv[2] = dudz*cos_a;		
		}
		else
		{
			slow->uxderiv[0] = -p*cos_a*cos_a0/d_km;
			slow->uxderiv[1] = p*cos_a*sin_a0/d_km;
			slow->uyderiv[0] = p*sin_a*cos_a0/d_km;
			slow->uyderiv[1] = -p*sin_a*sin_a0/d_km;
			/* With constant velocity layer models, refracted
			rays have exactly zero slowness derviatives wrt z*/

			slow->uxderiv[2] = 0.0;
			slow->uyderiv[2] = 0.0;  
		}
	} else { 
	    slow->uxderiv[0] = 0.0 ; 
	    slow->uxderiv[1] = 0.0 ;
	    slow->uxderiv[2] = 0.0 ;
	    slow->uyderiv[0] = 0.0 ; 
	    slow->uyderiv[1] = 0.0 ;
	    slow->uyderiv[2] = 0.0 ;
	}
	mod->ztop[0] = z0;
	free(work1);
	free(work2);
	return(slow);
}
	
#define RADIUS_EARTH 6370.8
/* this is the ttcalc entry point function for travel times. Arguments
are defined as standardized in tt(3) */

int tt1dcvl (
   char *model,
    char *phase_code,
    int mode,
    TTGeometry *geometry,
    Tbl **timesp,
    Hook **hookp
)
{
	double azimuth,delta;
	double d_km;
	char *property;  
	
	char *plist;
	char *phase;
	TTTime *atime;

	dist(rad(geometry->source.lat), rad(geometry->source.lon),
		rad(geometry->receiver.lat), rad(geometry->receiver.lon),
		&delta, &azimuth);
	d_km = delta*RADIUS_EARTH;

	/* I think it is necessary to strdup phase_code because strtok
	modifies what it parses */
	plist = strdup(phase_code);

	/* this loop is required to handle the fact that the ttcalc
	interface allows multiple phases to be returned using a
	comma separated listed in phase_code.  We use strtok to
	handle this. */

        phase = strtok(plist,",");
        while(phase != NULL)
	{
		property = map_phase_to_property(phase);
		/* Because of the way the ttcalc interface is specified
		we simply drop requested phases that are not computable */
		if(property != NULL)
		{
			atime = tt1dcvl_compute_atime(geometry,d_km,azimuth,
				phase, model, property, mode, hookp);
			if(atime != NULL) 
			{
				pushtbl(*timesp,atime);
				free(atime);
			}
			free(property);
		}	
		phase = strtok(NULL,",");
	}
	free(plist);
	/* The above list will come up empty if errors occur an now
	travel times are computed.  This is the reason for the maxtbl
	test */
	if(maxtbl(*timesp)) return(0);
	return(-1);
}

/* this is the parallel entry point for ucalc defined in tt(3).
This and tt1dcvl use the identical algorithm, but handle different
data structures */
int tt1dcvl_ucalc (
   char *model,
    char *phase_code,
    int mode,
    TTGeometry *geometry,
    Tbl **timesp,
    Hook **hookp
)
{
	double delta, d_km;  /* epicentral distance in radians and km resp.*/
	double s2raz;  /* source to receiver azimuth angle (radians) */
	double uaz;  /* gcp expected azimuth of propagation of arrival 
				(uaz != s2raz on a spherical earth) */
	char *property;  
	
	char *plist;
	char *phase;
	TTSlow *slowness;

	/* First we compute the epicentral distance and azimuth.  We need
	azimuth and both ends of the ray path for slowness partial 
	derivative estimates, which is why we have two calls to dist */

	dist(rad(geometry->source.lat), rad(geometry->source.lon), 
		rad(geometry->receiver.lat), rad(geometry->receiver.lon), 
		&delta, &s2raz);
	d_km = delta*RADIUS_EARTH;
	/* Now compute the great circle path propagation azimuth */
	dist(rad(geometry->receiver.lat),rad(geometry->receiver.lon),
		rad(geometry->source.lat),rad(geometry->source.lon),
		&delta,&uaz);
	uaz += M_PI;
	if(uaz >= 2.0*M_PI) uaz -= (2.0*M_PI);

	/* I think it is necessary to strdup phase_code because strtok
	modifies what it parses */
	plist = strdup(phase_code);

	/* this loop is required to handle the fact that the ttcalc
	interface allows multiple phases to be returned using a
	comma separated listed in phase_code.  We use strtok to
	handle this. */

        phase = strtok(plist,",");
        while(phase != NULL)
	{
		property = map_phase_to_property(phase);
		/* Because of the way the ttcalc interface is specified
		we simply drop requested phases that are not computable */
		if(property != NULL)
		{
			slowness = tt1dcvl_compute_slowness(geometry,
				d_km,uaz,s2raz,phase, model, 
				property, mode, hookp);
			if(slowness != NULL) 
			{
				pushtbl(*timesp,slowness);
				free(slowness);
			}
			free(property);
		}	
		phase = strtok(NULL,",");
	}
	free(plist);
	/* The above list will come up empty if errors occur an now
	travel times are computed.  This is the reason for the maxtbl
	test */
	if(maxtbl(*timesp)) return(0);
	return(-1);
}
