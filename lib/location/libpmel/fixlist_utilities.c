#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "stock.h"
#include "arrays.h"
#include "pf.h"
/* The list of calibration event info is stored in an associative
array keyed by evid.  However, evid is an int while an Arr 
is only keyed by character strings.  To standardize the process
we use this function to generate the key from the integer evid.
This probably should be generalized, but it is so trivial it
probably isn't worth the trouble.

Author G pavlis
Written:  July 2001
*/
char *make_evid_key(int evid)
{
	char *s;
	/* The evid field is 8 bytes wide in css3.0 of datascope
	so we make an 9 byte string */
	s = (char *)malloc(9);
	/* We just left justify the string and not worry about 
	leading zeros.  This will not produce correct sort order
	necessarily, but for the application here this doesn't matter.*/
	sprintf(s,"%-8d",evid);
	return(s);
}

/* This function creates an associative array keyed by an integer
event id from a parameter file based descriptor &Tbl like this example:

pmel_calibration_events &Tbl{
	10 xyz
	11 xyzt
}
where 10 and 11 are event ids and the string defines coordinates to fix.

Author:  Gary Pavlis
*/
Arr *load_calibration_events(Pf *pf)
{
	Tbl *t;
	int evid;
	char *evidstr;
	char fix[5],*line;
	char *fptr;
	Arr *a;
	int i;

	a = newarr(0);

	t = pfget_tbl(pf,"pmel_calibration_events");
	if(t==NULL) 
	{
		elog_complain(0,
		  "pmel_calibration_events Tbl not in parameter file\nAssuming no calibration events exist\n");
	}
	else
	{
		for(i=0;i<maxtbl(t);++i)
		{
			line = (char *)gettbl(t,i);
			sscanf(line,"%d%s",&evid,fix);
			evidstr = make_evid_key(evid);
			fptr=strdup(fix);
			setarr(a,evidstr,fptr);
			free(evidstr);
		}
			
	}
	return(a);
}
char *get_fixlist(Arr *a,int evid)
{
	char *o;
	char *key;
	key = make_evid_key(evid);
	o=(char *)getarr(a,key);
	free(key);
	return(o);
}
