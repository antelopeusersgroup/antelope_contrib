#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "stock.h"
#include "arrays.h"
#include "pf.h"
#include "location.h"
#include "pmel.h"
/* The list of calibration event info is stored in an associative
array keyed by evid.  However, evid is an int while an Arr 
is only keyed by character strings.  To standardize the process
we use this function to generate the key from the integer evid.
This probably should be generalized, but it is so trivial it
probably isn't worth the trouble.

Author G pavlis
Written:  July 2001
Additions:  August 2008
Added procedures to implement a scan for setting fixed depth
for one member of a cluster.  All these are at the end of this file.
*/
char *make_evid_key(long evid)
{
	char *s;
	/* The evid field is 8 bytes wide in css3.0 of datascope
	so we make an 9 byte string */
	s = (char *)malloc(9);
	/* We just left justify the string and not worry about 
	leading zeros.  This will not produce correct sort order
	necessarily, but for the application here this doesn't matter.*/
	sprintf(s,"%-8ld",evid);
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
	long evid;
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
			sscanf(line,"%ld%s",&evid,fix);
			evidstr = make_evid_key(evid);
			fptr=strdup(fix);
			setarr(a,evidstr,fptr);
			free(evidstr);
		}
			
	}
	return(a);
}
char *get_fixlist(Arr *a,long evid)
{
	char *o;
	char *key;
	key = make_evid_key(evid);
	o=(char *)getarr(a,key);
	free(key);
	return(o);
}
/* New procedurs for freeze depth of a cluster features */
/* This simple procedure returns if a list of evids is found as a fixed 
depth entry already in the existing list */
int in_fixdepthlist(Arr *a, long *evid, int nevents)
{
	char *o;
	char *key;
	int i;
	for(i=0;i<nevents;++i)
	{
		key=make_evid_key(evid[i]);
		o=(char *)getarr(a,key);
		if(o!=NULL)
		{
		/* Accept any fixed coordinate as true.  Could
		spell trouble, but this whole features is a bit of 
		a bell and whistle. */
			free(key);
			return(1);
		}
		free(o);
	}
	return(0);
}
/* returns a list with one element using a recipe driven by the enum
FREEZE_METHOD defined in pmel.h.  The names and what they do should be
obvious.
Arguments:
	fm - FREEZE_METHOD enum defining method to use to build the freeze list definition.
	evid and h0 are parallel lists of event id and hypocenter objects respectively.
		The methods here assume the data count and rms figures have beens et.
	ta - vector of Tbls containing Arrival structs.  These are used
		as an unambiuous way to test for number of arrivals.
		ta is assumed to be same size as evid and h0 arrays
		(nevent) and totally parallel.  i.e. ta[i] is data
		for evid[i]
	nevents - number of events = size of evid and h0 vectors.
*/

Arr *get_freezearr(enum FREEZE_METHOD fm, Hypocenter *h0, long *evid, 
	Tbl **ta, int nevents)
{
	int i;  /* loop counter */
	char *fixstr;
	long minrmsevid, maxdataevid;
	double rmsmin,thisrms;
	int maxarrivals;
	minrmsevid=evid[0];
	maxdataevid=evid[0];
	maxarrivals=maxtbl(ta[0]);
	rmsmin=h0[0].rms_weighted;
	if(rmsmin<=0.0) rmsmin=-1.0;
	/* slighly inefficient to compute both of these measures all the
	time, but the effort is so trivial it should not matter much.
	More importantly, it allows a fallback in case the rms fields are not
	set.  When that is true the maxarrival method are used and a warning is 
	posted. */
	for(i=1;i<nevents;++i)
	{
		int thismaxarr;
		thismaxarr=maxtbl(ta[i]);
		if(thismaxarr>maxarrivals)
		{
			maxdataevid=evid[i];
			maxarrivals=thismaxarr;
		}
		thisrms=h0[i].rms_weighted;
		if(thisrms>0.0)
		{
			if(rmsmin>0.0) 
			{
				if(thisrms<rmsmin)
					minrmsevid=evid[i];
			}
			else
			{
				rmsmin=thisrms;
				minrmsevid=evid[i];
			}
		}
	}
		
	long evid_to_freeze;
	switch(fm)
	{
	case DEPTH_MINRMS:
	case ALLSPACE_MINRMS:
	case ALL_MINRMS:
		/* Tricky logic.  if minrms is null (0 or negative), 
		this implicitly defaults to arrival count metric */
		if(minrmsevid>0.0)
		{
			evid_to_freeze=minrmsevid;
			break;
		}
		else
			elog_log(0,"pmel warning:  freeze mode could not use rms measure to select event to fix\nrms field was null in all hypocenters in this group\n");
	case DEPTH_MAXARRIVALS:
	case ALLSPACE_MAXARRIVALS:
	case ALL_MAXARRIVALS:
	case NOTSET:
	default:
		evid_to_freeze=maxdataevid;
	}
	switch(fm)
	{
	case DEPTH_MAXARRIVALS:
	case DEPTH_MINRMS:
		fixstr=strdup("z");
		break;
		break;
	case ALL_MAXARRIVALS:
	case ALL_MINRMS:
		fixstr=strdup("xyzt");
	case ALLSPACE_MAXARRIVALS:
	case ALLSPACE_MINRMS:
	case NOTSET:
	default:
		fixstr=strdup("xyz");
	}
	char *key=make_evid_key(evid_to_freeze);
	Arr *result=newarr(0);
	setarr(result,key,fixstr);
	return(result);
}
