#include "stock.h"
#include "arrays.h"
#include "coords.h"
#include "pf.h"
#include "elog.h"
#include "location.h"


/* Simple little function to define the 3D reference model 
used to compute bias components of solution.  It simply 
uses the same mechanism used in dbgenloc to access a set of
standard models.  
*/
Arr *parse_3D_phase(Pf *pf)
{
	Arr *a;
	Pf *pf3d;
	char *vmodel;

	vmodel = pfget_string(pf,"3Dreference_model");
	if(pfload("GENLOC_MODELS","tables/genloc",vmodel,&pf3d) != 0)
		elog_die(0,"pfload failed on 3Dreference model %s\n",vmodel);
	a = parse_phase_parameter_file(pf3d);
	pffree(pf3d);
	return(a);
}

/* Edits the array of phase handles to keep only phases
named in the keeplist Tbl of phase names strings.  This 
is complicated by the fact that keeplist is a simple
list.  The algorithm used converts the keeplist to a
temporary associative array then passes through the 
array of phase handles calling the free routine on 
phases not found in the keeplist. 

Author:  G Pavlis
Written:  August 2001
*/
void edit_phase_handle(Arr *a,Tbl *keeplist)
{
	Tbl *akeys;
	Arr *akeeper;
	int dummy;  /* used purely as a placeholder in akeeper*/
	char *phase;
	int i,n;
	Phase_handle *ph;

	n = maxtbl(keeplist);
	if(n<=0) 
		elog_die(0,"List of phases to keep is empty.\n\
Check phases_to_keep parameter setting\n");
	akeeper = newarr(0);
	for(i=0;i<maxtbl(keeplist);++i)
	{
		phase = (char *)gettbl(keeplist,i);
		setarr(akeeper,phase,&dummy);
		ph = (Phase_handle *)getarr(a,phase);
		if(ph==NULL)elog_die(0,
			"Don't know how to handle required phase %s\n",
				phase);
	}
	akeys = keysarr(a);
	for(i=0;i<maxtbl(akeys);++i)
	{
		phase = gettbl(akeys,i);
		if(getarr(akeeper,phase) == NULL)
		{
			ph = (Phase_handle *)getarr(a,phase);
			free_phase_handle(ph);
			delarr(a,phase);
		}
	}

	freearr(akeeper,0);
	freetbl(akeys,0);
}

