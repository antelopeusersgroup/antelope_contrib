#include <math.h>
#include <float.h>
#include "stock.h"
#include "arrays.h"
#include "pf.h"
/* This function is used to check a list of pf names to verify they
are in the parameter space.  It is useful for any program that uses
parameter files that cracks the pf space anywhere except up front.
That is, the pf space is a convenient place to store all the run time
parameters of any program and is a convenient way to pass a large
control structure.  However, because flow can be complex it is possible
to have parameters that are only accessed deep within a program and 
in these situations it is desirable to run a check at program initialization
to verify the required parameters will be there when required.  

Normally a string variable will not cause pfget to die, but it will
here if it is listed as required

Parameters to be checked are grouped by type.  Each numerical fields
can contain an optional range check.  This is not allowed for strings.
Code below only checks int, double, boolean, and string variables.  Because of
the complexity of Tbl and Arrs I thought this not worth messing with.
Furthermore, it is not unusual to have an empty Tbl list or Arr that
the program should handle correctly.  

Function will die on the first occurence of a problem parameter.
It is void because it only returns if everything checks out.

Author:  Gary L. Pavlis
*/
void check_required_pf(Pf *pf)
{
	Tbl *t,*testtbl;
	Pf *pf_required;
	char *key;
	int i,j,nitems;
	int itest, ilowcheck, ihighcheck;
	double dtest, dlowcheck, dhighcheck;
	int bool;
	char *line;
	char *ctest;
	char name[50];

	/* This cracks the "required" &Arr dies if it isn't present 
	at all.  This assumes you wouldn't call this routine if you
	weren't serious about checking */
	if(pfget(pf,"require",(void **)&pf_required) != PFARR)
		elog_die(0,"Arr of required parameters (require &Arr) missing from parameter space\nMust be present to execute this program\n");
	t=pfkeys(pf_required);
	for(i=0;i<maxtbl(t);++i)
	{
		key = gettbl(t,i);
		if(!strcmp(key,"int"))
		{
			testtbl = pfget_tbl(pf_required,"int");
			for(j=0;j<maxtbl(testtbl);++j)
			{
				line = gettbl(testtbl,j);
				nitems = sscanf(line,"%s%d%d",name,&ilowcheck,
							&ihighcheck);
				itest = pfget_int(pf,name);
				if(nitems == 3)
				{
					if( (itest < ilowcheck)
					  || (itest > ihighcheck) ) elog_die(0,
						"Parameter %s has value %d which is outside required range of %d to %d\n",
						   name,itest,ilowcheck,ihighcheck);
				}
			}
		}
		else if(!strcmp(key,"double"))
		{
			testtbl = pfget_tbl(pf_required,"double");
			for(j=0;j<maxtbl(testtbl);++j)
			{
				line = gettbl(testtbl,j);
				nitems = sscanf(line,"%s%lg%lg",name,&dlowcheck,
							&dhighcheck);
				dtest = pfget_double(pf,name);
				if(nitems == 3)
				{
					if( (dtest < dlowcheck)
					  || (dtest > dhighcheck) ) elog_die(0,
						"Parameter %s has value %lg which is outside required range of %lg to %lg\n",
						   name,dtest,dlowcheck,dhighcheck);
				}
			}
		}
		else if(!strcmp(key,"boolean"))
		{
			testtbl = pfget_tbl(pf_required,"boolean");
			for(j=0;j<maxtbl(testtbl);++j)
			{
				line = gettbl(testtbl,j);
				itest = pfget_boolean(pf,line);
			}
		}
		else if(!strcmp(key,"string"))
		{
			testtbl=pfget_tbl(pf_required,"string");
			for(j=0;j<maxtbl(testtbl);++j)
			{
				line = gettbl(testtbl,j);
				ctest = pfget_string(pf,line);
				if(ctest == NULL)
					elog_die(0,"Missing required string variable = %s\n",
						line);
			}
		}
		else
		{
			elog_notify(0,"Unknown required type name = %s\nRequired parameters under this heading will not be checked\n",
				key);
		}
	}
	freetbl(t,0);
}
