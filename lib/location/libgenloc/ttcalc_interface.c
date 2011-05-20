/* These are interface functions to general purpose travel time interface
described in man tt(3).  Initial version of this was written March 4, 1997.
This set of routines are interfaces between the "Phase_handle" structures
used internally in libgenloc with this general purpose interface.  When
that interface is stabilized, it will be more efficient to remove these
interface routines and call the tt functions directly.  For now I build
these interface function to allow greater flexibility in the transition.

Author:  Gary Pavlis
Written:  March 21, 1997
Modified:  July 1999
Previous version would not work correctly when multiple methods
were defined because it did not handle the potential variations in
type of the "hook" defined by different calculators.  This hopefully
fixes this.  
*/
#include <strings.h>
#include "stock.h"
#include "arrays.h"
#include "tt.h"
#include "location.h"
/* the generic tt interface defines a travel time calculator in 
terms of two abstract quantities:  (1) a "method" string used to
resolve a dynamically loaded function and (2) a "model" string that
defines the earth structure model that is being used for this 
time calculation.  The present phase_handle structure does not 
include a place for these quantities.  For now I will store these
quantities in an init procedure in an static Arr and look them 
up before each call to ttcalc.  This is not very efficient, but
it increases flexibility.  This way one can, in principle, use
different model or method with different phases. 

This is the init procedure that does this.  Pf is the input 
Pf object defining these relationships, and phase is the phase name
we use to key these Arr. 
*/
static Arr *TTmethod,*TTmodel,*TThooks;
int ttcalc_interface_init(char *phase, Pf *pf)
{
	char *model;
	char *method;

	/* This assumes the TTmethod, TTmodel, and TThooks pointers are
	initialized to NULL.  Earlier code had it explicitly set above
	but this led to seg faults for reasons I could not figure glp,6/2001*/
	if(TTmethod == NULL) TTmethod = newarr(0);
	if(TTmodel == NULL) TTmodel = newarr(0);
	if(TThooks == NULL) TThooks = newarr(0);

	model = strdup(pfget_string(pf,"TTmodel"));
	method = strdup(pfget_string(pf,"TTmethod"));

	if(model == NULL) 
	{
		elog_complain(1,"Unspecified model for phase %s\nData from this phase will be ignored\n",phase);
		return(1);
	}
	if(method == NULL) 
	{
		elog_complain(1,"Unspecified method for phase %s\nData from this phase will be ignored\n",phase);
		return(1);
	}
	setarr(TTmodel,phase,model);
	setarr(TTmethod,phase,method);
	setarr(TThooks,method,NULL);  /* this initializes the hook NULL for
					this method */

	return(0);
}
Travel_Time_Function_Output  ttcalc_interface_exec(Ray_Endpoints x, char *phase, int mode)
{
	TTGeometry geometry;
	Tbl *t=NULL;
	char *model,*method;
	int result,TTmode;
	TTTime *atime;
	Travel_Time_Function_Output o;
	Hook *hook;
	int hook_is_null=0;

	/*Always this value for "regular" phases.  Set to 0 for
	things like S-P */
	o.dtdtau = 1.0;

	strcpy(geometry.receiver.name,x.sta);
	geometry.receiver.lat = x.rlat;
	geometry.receiver.lon = x.rlon;
	geometry.receiver.z = x.rz;
	geometry.receiver.time = 0.0;
	geometry.source.lat = x.slat;
	geometry.source.lon = x.slon;
	geometry.source.z = x.sz;
	strcpy(geometry.source.name,"source");
	geometry.source.time = 0.0;
	

	model = (char *) getarr(TTmodel,phase);
	method = (char *) getarr(TTmethod,phase);
	hook = getarr(TThooks,method);
	if(hook == NULL) hook_is_null = 1;
	TTmode = 0;
	if(mode == ALL) TTmode |= TT_DERIVATIVES;

	/* Assume phase names containing - are to be treated like
	a difference of two arrival times of a pair of phases.*/
	if(strchr(phase,'-'))
	{
		/* minus phases like S-P are handled here.  
		First step is to split the phase name into
		components */
		char *phase1,*phase2;
		phase1 = strdup(phase);
		phase2 = strchr(phase1,'-');
		*phase2 = '\0';
		++phase2; 
		/* Theoretically we could call ttcalc once with phase1
		and phase2 in the list, but not all calculators can 
		be assured of working this way (I know as I got lazy
		myself on this point) so we call the calculator twice
		and form the results from the pieces. */
		result = ttcalc(method,model,phase1,TTmode,&geometry,&t,&hook);
		if(hook_is_null)setarr(TThooks,method,hook);
		if((result < 0) || ( (result == 1) && (mode == ALL) ))
		{
			elog_complain(1,"Station %s:  ttcalc returned error %d for phase %s of composite phase %s\nDatum skipped\n",
				x.sta, result, phase1,phase);
			o.time = TIME_INVALID;
			o.dtdx = 0.0;
			o.dtdy = 0.0;
			o.dtdz = 0.0;
			return(o);
		}
		atime = (TTTime *) gettbl(t,0);
		o.time = atime->value;
		if(mode == ALL)
		{
			o.dtdx = atime->deriv[0];
			o.dtdy = atime->deriv[1];
			o.dtdz = atime->deriv[2];
		}
		else
		{
			o.dtdx = 0.0;
			o.dtdy = 0.0;
			o.dtdz = 0.0;
		}
		result = ttcalc(method,model,phase2,TTmode,&geometry,&t,&hook);
		if((result < 0) || ( (result == 1) && (mode == ALL) ))
		{
			elog_complain(1,"Station %s:  ttcalc returned error %d for phase %s of composite phase %s\nDatum skipped\n",
				x.sta, result, phase2,phase);
			o.time = TIME_INVALID;
			o.dtdx = 0.0;
			o.dtdy = 0.0;
			o.dtdz = 0.0;
			return(o);
		}
		atime = (TTTime *) gettbl(t,0);
		o.time -= atime->value;
		if(mode == ALL)
		{
			o.dtdx -= atime->deriv[0];
			o.dtdy -= atime->deriv[1];
			o.dtdz -= atime->deriv[2];
			o.dtdtau = 0.0;
		}
		/* If we didn't ask for partials they are already set 0*/


		/* We only have one free here because of how this was created*/
		free(phase1);
	}
	else
	{
		/* normal phases are handled in this block */
		result = ttcalc(method,model,phase,TTmode,&geometry,&t,&hook);
		if(hook_is_null)setarr(TThooks,method,hook);
	
		if((result < 0) || ( (result == 1) && (mode == ALL) ))
		{
			elog_complain(1,"Station %s:  ttcalc returns error %d for phase %s\nDatum skipped\n",
				x.sta, result, phase);
			o.time = TIME_INVALID;
			o.dtdx = 0.0;
			o.dtdy = 0.0;
			o.dtdz = 0.0;
			return(o);
		}
		/* In this context, we can assume the returned Tbl has only one
		entry, and it corresponds to the phase requested.  This may
		not be guaranteed.  This is necessary to handle generic phase
		names like "P" */
		atime = (TTTime *) gettbl(t,0);
		o.time = atime->value;
		if(mode == ALL)
		{
			o.dtdx = atime->deriv[0];
			o.dtdy = atime->deriv[1];
			o.dtdz = atime->deriv[2];
		}
		else
		{
			o.dtdx = 0.0;
			o.dtdy = 0.0;
			o.dtdz = 0.0;
		}
	}
	if(t != NULL) freetbl(t,0);
	return(o);
}
Slowness_Function_Output ttcalc_interface_slow_exec(Ray_Endpoints x, char *phase, int mode)
{
	TTGeometry geometry;
	Tbl *u=NULL;
	char *model,*method;
	int result,TTmode = 0 ;

	Slowness_Function_Output o;
	TTSlow *ttu;
	Hook *hook;
	int hook_is_null=0;

	strcpy(geometry.receiver.name,x.sta);
	geometry.receiver.lat = x.rlat;
	geometry.receiver.lon = x.rlon;
	geometry.receiver.z = x.rz;
	geometry.receiver.time = 0.0;
	geometry.source.lat = x.slat;
	geometry.source.lon = x.slon;
	geometry.source.z = x.sz;
	strcpy(geometry.source.name,"source");
	geometry.source.time = 0.0;
	

	model = (char *) getarr(TTmodel,phase);
	method = (char *) getarr(TTmethod,phase);
	hook = getarr(TThooks,method);
	if(hook == NULL) hook_is_null=1;
	if(mode == ALL) 
	    TTmode |= TT_DERIVATIVES;

	result = ucalc(method,model,phase,TTmode,&geometry,&u,&hook);
	if(hook_is_null)setarr(TThooks,method,hook);
	if((result < 0) || ( (result == 1) && (mode == ALL) ))
	{
		elog_complain(1,"Station %s:  ucalc returns error %d for phase %s\nDatum skipped\n",
			x.sta, result, phase);
		o.ux = SLOWNESS_INVALID;
		o.uy = SLOWNESS_INVALID;
		o.duxdx = 0.0;
		o.duxdy = 0.0;
		o.duxdz = 0.0;
		o.duydx = 0.0;
		o.duydy = 0.0;
		o.duydz = 0.0;
		return(o);
	}
	/* In this context, we can assume the returned Tbl has only one
	entry, and it corresponds to the phase requested */
	ttu = (TTSlow *) gettbl(u,0);
	o.ux = ttu->ux;
	o.uy = ttu->uy;
	if(mode == ALL)
	{
		o.duxdx = ttu->uxderiv[0];
		o.duxdy = ttu->uxderiv[1];
		o.duxdz = ttu->uxderiv[2];
		o.duydx = ttu->uyderiv[0];
		o.duydy = ttu->uyderiv[1];
		o.duydz = ttu->uyderiv[2];
	}
	else
	{
		o.duxdx = 0.0;
		o.duxdy = 0.0;
		o.duxdz = 0.0;
		o.duydx = 0.0;
		o.duydy = 0.0;
		o.duydz = 0.0;
	}
	if(u != NULL) freetbl(u,0);
	return(o);
}
	


/* $Id$ */
