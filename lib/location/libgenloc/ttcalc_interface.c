/* These are interface functions to general purpose travel time interface
described in man tt(3).  Initial version of this was written March 4, 1997.
This set of routines are interfaces between the "Phase_handle" structures
used internally in libgenloc with this general purpose interface.  When
that interface is stabilized, it will be more efficient to remove these
interface routines and call the tt functions directly.  For now I build
these interface function to allow greater flexibility in the transition.

Author:  Gary Pavlis
Written:  March 21, 1997
*/
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
static Arr *TTmethod=NULL;
static Arr *TTmodel=NULL; 
static Hook *hook=0;  /* this is necessary, I think, to initialize
			the ttcalc function properly */
int ttcalc_interface_init(char *phase, Pf *pf)
{
	char *model;
	char *method;
	TTGeometry geometry;
	int mode=0;
	int result;
	Tbl *times=NULL;

	if(TTmethod == NULL) TTmethod = newarr(0);
	if(TTmodel == NULL) TTmodel = newarr(0);

	model = strdup(pfget_string(pf,"TTmodel"));
	method = strdup(pfget_string(pf,"TTmethod"));

	if(model == NULL) 
	{
		register_error(1,"Unspecified model for phase %s\nData from this phase will be ignored\n",phase);
		return(1);
	}
	if(method == NULL) 
	{
		register_error(1,"Unspecified method for phase %s\nData from this phase will be ignored\n",phase);
		return(1);
	}
	setarr(TTmodel,phase,model);
	setarr(TTmethod,phase,method);
/*
	geometry.source.lat=0.0;
        geometry.source.lon = 0.0 ;
        geometry.source.z = 0.0 ;
        geometry.source.time = 0.0 ;
        strcpy(geometry.source.name, "SOURCE" ) ;
        geometry.receiver.lat = 1.0 ;
        geometry.receiver.lon = 1.0 ;
        geometry.receiver.z = 0.0 ;
        geometry.receiver.time = 0.0 ;
        strcpy(geometry.receiver.name, "RECEIVER" ) ;
	result = ttcalc(method,model,phase,mode,&geometry,&times,&hook);
	if(result < 0) 
	{
		register_error(1,"ttcalc initialization failed for phase %s\nData from this phase will be ignored",phase);
		return(2);
	}
*/

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
	TTmode = 0;
	if(mode == ALL) TTmode |= TT_DERIVATIVES;

	result = ttcalc(method,model,phase,TTmode,&geometry,&t,&hook);

	if((result < 0) || ( (result == 1) && (mode == ALL) ))
	{
		complain(1,"Station %s:  ttcalc returns error %d for phase %s\nDatum skipped\n",
			x.sta, result, phase);
		o.time = TIME_INVALID;
		o.dtdx = 0.0;
		o.dtdy = 0.0;
		o.dtdz = 0.0;
		return(o);
	}
	/* In this context, we can assume the returned Tbl has only one
	entry, and it corresponds to the phase requested */
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
	freetbl(t,free);
	return(o);
}
Slowness_Function_Output ttcalc_interface_slow_exec(Ray_Endpoints x, char *phase, int mode)
{
	Slowness_Function_Output o;

	complain(0,"Cannot calculate slowness vector for station %s, phase %s\nNot yet implemented for general tt interface\n",
		x.sta,phase);
	o.ux = SLOWNESS_INVALID;
	o.uy = SLOWNESS_INVALID;
	return(o);
}
	


/* $Id$ */
