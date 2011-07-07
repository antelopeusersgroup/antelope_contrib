/* This group of function implement what amounts to a patch of genloc
for S-P time or any other formed as phase X - phase Y (e.g. Pg-Pn). 
It is a patch because I didn't design genloc originally to work
with this type of measurement due to an incorrect prejudice that it
was an archaic measurment.  It isn't because data loggers have clock
errors and some would argue s-p is always a superior observable to 
straight times.  I don't really accept the later, but in the interest
of generality I'll add it.  

The functions here implement minus phases one of two ways.  One function will
try to check a database extension table for time intervals in which 
the clock on a given station is known to be bad.  The other method is
to read a list of stations from a parameter file and assume only s-p
type times are to be used ALWAY for that station.  This allows, for 
example, a recipe where only S-P times are ever used.  Any station
marked by this mechanism will only allow S-P type measurements.
i.e. absolute arrivals for such stations will be discarded except where
they match a pair of minus phases.  This is all controlled by an 
associative array, keyed by station name, that holds a set of
pointers to a special structure called Bad_Clock.  This structure
has a switch that says the clock at station x is always bad.  If
that boolean is not set true, then a list of time intervals is
defined in the structure.  Database entries set the time intervals
and the parameter file can only be used to set the boolean part.
This is not the most flexible approach, but one that is workable
with any data set I can imagine.

It is noteworthy that the method used here is a big memory model.
That is, it assumes there is sufficient memory to hold the 
table of bad clock times for all stations.  For a large database
this could be problematic, but the way computers are headed this
should not really be an issue.  

Written:  September 2000
Author:  Gary Pavlis
*/
#include <string.h>
#include "stock.h" 
#include "arrays.h"
#include "pf.h"
#include "location.h"
/* This function cautiously attempts to open a special database table 
called "timing" and placing the contents in the associative array
"a" passed to the function.  a is keyed by station name.  The 
basic algorithm is that we loop through the timing table.  Whenever
a new station is found a new Bad_Clock structure is created.  If a 
previously set station is found the new time interval is pushed onto
a Tbl stack.  

Note that because we are accessing the associative array blindly
the Arr structure it points to must have been at least initialized
with newarr.  

Arguments:
	db - input database pointer
	a - associative array keyed by station name that holds 
		pointers to Bad_Clock stucture for each station 
		defined as having one or more time intervals with
		a bad clock.  
Returns:
	0 - normal all ok
	-1 - nothing done because the timing table was not defined
		in the schema
	> 0 list truncated at this record number that was less than 
		total table size.
Function will die only on a memory alloc error.

Written:  September 2000
Author:  Gary Pavlis
*/
int db_badclock_definition(Dbptr db, Pf *pf, Arr *a)
{
	Time_Interval *dtime;
	Bad_Clock *bc;
	long nrec;
	char sta[10];
	double tstart,tend;
	double clkerror_cutoff,clkerr;

	if(dbtable_invalid(db,"timing"))
	{
		elog_notify(0,"The \"timing\" extension table is not defined in the schema definition\nThis table is useful if you are using S-P times\n");
		return(-1);
	}
	clkerror_cutoff = pfget_double(pf,"clock_error_cutoff");

	db = dblookup(db,0,"timing",0,0);
	dbquery(db,dbRECORD_COUNT,&nrec);

	for(db.record=0;db.record<nrec;++db.record)
	{
		if(dbgetv(db,0,"sta",sta,
			"time",&tstart,
			"endtime",&tend,
			"clkerr",&clkerr,NULL ) == dbINVALID)
		{
			elog_complain(0,"dbgetv error reading timing table\nReading record %ld of %ld records in this table\nList of bad time intervals trucated at this point\n",
				db.record,nrec);
			return(db.record);
		}
		/* Don't use entries when the clkerr quoted is below
		the cutoff */
		if(clkerr<clkerror_cutoff) continue;
		allot(Time_Interval *,dtime,1);
		dtime->tstart = tstart;
		dtime->tend = tend;
		bc = (Bad_Clock *)getarr(a,sta);
		if(bc == NULL)
		{
			allot(Bad_Clock *,bc,1);
			strcpy(bc->sta,sta);
			bc->alltime = 0;
			bc->badtimes = newtbl(0);
			pushtbl(bc->badtimes,dtime);
			setarr(a,sta,bc);
		}
		else
		{
			pushtbl(bc->badtimes,dtime);
		}
	}
	return(0);
}
/* This function checks a parameter file object pf for stations
to flag clocks as bad.   It is a companion to the above and could
be called either before or after it.  The key thing here is that
stations marked bad in the parameter file cause the "alltime" 
boolean in the Bad_Clock structure to be set true which is used
to mean the clock is never to be trusted.  If the station was 
already set and has entries in the badtimes Tbl they will
be effectively ignored because the alltime boolean overrides
them.  To be stateless this function calls newtbl if it 
creates a Bad_Clock structure that is left empty.  This allows
a destroy routine to blindly call freetbl on each entry and 
any function that edits the list to assume it is already set.  

Arguments:
	pf - parameter space pointer
	a - associative array with pointers to Bad_Clock structures
		The routine will probably seg fault if this has not
		been at least initialized with newarr.

Function has no error conditions and will silently do nothing
if the bad_clocks &Tbl is empty, as it should.  It will die only
on memory alloc errors.  
Author:  Gary Pavlis
Written:  September 2000
*/
void pfget_badclocks(Pf *pf, Arr *a)
{
	Tbl *t;
	int i;
	char *sta;
	Bad_Clock *bc;
	Tbl *keys;

	/* We need to initialize all entries in array a 
	to have the boolean alltime set to initially false*/
	keys = keysarr(a);
	for(i=0;i<maxtbl(keys);++i)
	{
		sta = gettbl(keys,i);
		bc = (Bad_Clock *)getarr(a,sta);
		bc->alltime = 0;
	}
	freetbl(keys,0);

	t = pfget_tbl(pf,"bad_clocks");
	if(t==NULL) return;
	for(i=0;i<maxtbl(t);++i)
	{
		sta = gettbl(t,i);
		bc = (Bad_Clock *)getarr(a,sta);
		if(bc == NULL)
		{
			allot(Bad_Clock *,bc,1);
			strcpy(bc->sta,sta);
			bc->alltime = 1;
			bc->badtimes = newtbl(0);
			setarr(a,sta,bc);
		}
		else
			bc->alltime = 1;
	}
}
/* This is the free routine for a Bad_Clock stucture.
It should be called with freearr as 
freearr(a,Bad_Clock_free);
*/
void Bad_Clock_free(Bad_Clock *bc)
{
	freetbl(bc->badtimes,free);
	free(bc);
}
/* This routine tests the list of bad clock time intervals to see
if testtime is within one of the marked intervals.  It returns 
1 if the give time is inside one of these intervals and 0 if it
is not.
*/
int clock_is_bad(Tbl *intervals,double testtime)
{
	Time_Interval *tip;
	int i;
	for(i=0;i<maxtbl(intervals);++i)
	{
		tip = (Time_Interval *)gettbl(intervals,i);
		if( (testtime >= (tip->tstart)) 
			&& (testtime<=(tip->tend)))return(1);
	}
	return(0);
}
/* This small function makes a key string for associative
arrays in the function immediately below of the form
sta:phase where sta is a station name and phase is the 
name of given seismic phase defined for some Arrival.  */	
char *make_staphase_key(char *sta,char *phase)
{
	int size;
	char *key;
	size = strlen(sta) + strlen(phase) + 2;
	key = malloc(size);
        if(key == NULL) elog_die(0,
                "makez_staphase_key: malloc failure for string of length %d\n",size);
        strcpy(key,sta);
        strcat(key,":");
        strcat(key,phase);
        return(key);
}
/* This short function splits string s1 at the first occurrence
of character c.  It does this by setting the c character to a
null trucating s1 to the first word.  The function returns
a pointer to the next character after c.   e.g. if s1="AAB:P"
and c=':', on exit s1="AAB" and the returned pointer will be
the string="P" */
char *splitwords(char *s1,char c)
{
	char *s2;
	s2=strchr(s1,c);
	if(s2!=NULL)
	{
		*s2='\0';
		++s2;
	}
	return (s2);
}
	
	
/* This is the function that applies the Bad_Clock structures
to an arrival Tbl.  This is a fairly expensive operation 
because of an ancient decision to store Arrival structures in
a simple Tbl list instead of an associative array.  Because of
this the algorithm searches the entire table from beginning to 
end in a fairly complex algorithm that amounts to cascaded 
linear searches.  That is, we start at the top of the list
working our way to the the end.  Whenever a station in the
list matches one with an entry in the badclock list a 
secondary search for any other phases defined for this station
is started.  In this way we collect all the phases that belong 
to the station with a bad clock.  Pointers to these Arrival 
structures are removed from initial Tbl and pushed onto a 
new list.   These have to be compared with
a list of time intervals when alltimes is not set true to prepare
a list of arrivals to be differenced.  The phases are compared
against a list of phase handles to keep only phases for which 
a phase like "S-P" is defined.  All such entries are created
and pushed onto the arrival Tbl.  

The destruction process is complicated and subject to memory leaks
if altered.  The original list is edited by extracting pointers to
Arrival structures from the original list and placing them in
the edit list.  When the - processing is finished the entire 
contents of the edit list are destroyed.  This should not cause
a memory leak provided something isn't lost in the shuffle.  I
don't think that will happen in this algorithm, but be warned
if you mess with it.  

Arguments:
	arrival - tbl holding current arrival list
	phase - array of phase handles]
	bad - array of Bad_Clock structure.

This function does all it's work silently and always returns 0.
I chose to not make it void because when I debug it I'm suspicious
I may need to return an error code.  Calling routines should
contain a fragment like:
if(minus_phases_arrival_edit(blah)) elog_notify(0,"problems in blah");
even though the conditional will currently never be executed.  Call
it planning ahead.
Author:  Gary Pavlis
Written:  September 2000
*/
int minus_phases_arrival_edit(Tbl *arrivals,Arr *phases,Arr *bad)
{
	Arr *editlist;  /* We store the Arrival pointers that are
		marked bad here */
	Arr *badsta;  /* Array used purely as a logical to define
		stations with a bad clock.  Any sta with a key match 
		in this arr is used to flag bad */
	int bcflag=1;  /* contents of badsta arr all contain pointers 1*/
	char *key;  /* key for editlist created using function above */
	int narrivals;
	Arrival *a;
	int i,j;
	Bad_Clock *bcp;
	Tbl *kt,*ktbs; 

	/* Return immediately if there are no entries in the bad arr*/
	if(cntarr(bad)<=0)return(0);
	narrivals = maxtbl(arrivals);
	editlist = newarr(0);
	badsta=newarr(0);
	for(i=0;i<narrivals;++i)
	{
		a = (Arrival *)gettbl(arrivals,i);
		bcp = (Bad_Clock *)getarr(bad,a->sta->name);
		if(bcp != NULL)
		{
			/* hate this negative logic, but the purpose it
			to bypass the work of clock_is_bad if the alltime
			boolean is true */
			if(!(bcp->alltime))
			{
				if(!clock_is_bad(bcp->badtimes,a->time))
						continue;
			}
			key = make_staphase_key(a->sta->name,a->phase->name);
			setarr(editlist,key,a);
			/* the contents of badsta should not be accessed*/
			setarr(badsta,a->sta->name,&bcflag);
			free(key);
			/* we delete this entry form the list and because
			of this the length of the list changes.  This is
			the reason for the --'s */
			deltbl(arrivals,i);
			--i;
			--narrivals;
		}
	}
	/* We loop through the list of phase handles looking for any
	phase name that has a "-" in it.  When we find one of these
	we build an Arrival object from the pieces that define it.
	We look through the badsta list and grab the pieces for each
	part of the - phase.  If both aren't present we ignore them.*/
	kt = keysarr(phases);
	ktbs = keysarr(badsta);
	for(i=0;i<maxtbl(kt);++i)
	{
		char *phasename;
		Phase_handle *php;
		char *phase1,*phase2;
		Arrival *a1,*a2;
		char *work;
		char *sta;
		int nktbs;

		phasename = gettbl(kt,i);
		if(strchr(phasename,'-')!=NULL)
		{
			php = (Phase_handle *)getarr(phases,phasename);
			work = strdup(phasename);
			phase2=splitwords(work,'-');
			phase1=work;

			/* silently skip a couple of obvious problems */
			if(strlen(phase2)<=0) continue;
			if(!strcmp(phase1,phase2)) continue;
			nktbs = maxtbl(ktbs);

			for(j=0;j<nktbs;++j)
			{
				sta = gettbl(ktbs,j);
				key = make_staphase_key(sta,phase1);
				a1=(Arrival *)getarr(editlist,key);
				free(key);
				if(a1==NULL) 
					continue;
				key = make_staphase_key(sta,phase2);
				a2=(Arrival *)getarr(editlist,key);
				free(key);
				if(a2==NULL)
					continue;

				allot(Arrival *,a,1);
				/* Arbitrarily set arid
				to the one for a1 -- unclear
				what problems this will cause*/
				a->arid = a1->arid;
				a->arid2 = a2->arid;
				a->sta = a1->sta;
				a->phase = php;
				a->time = (a1->time)-(a2->time);
				/* This uses the standard formula
				for sum of uncorrelated errors*/
				a->deltat = hypot(a1->deltat,a2->deltat);
				/* initialize the residual*/
				a->res.raw_residual = 0.0;  
				a->res.weighted_residual=0.0;
				a->res.residual_weight=1.0;
				a->res.other_weights=1.0;
				pushtbl(arrivals,a);
			}
			free(work);
		}
	}
	/*Last, but not least we have to destory the contents of 
	editlist to avoid a memory leak.  There are stray pointers
	inside the Arrival structure, but these are not manageable
	from here without chaos so we just use free */
	freearr(editlist,free);
	/* we don't free the contents of badsta because is has none*/
	freearr(badsta,0);
	return(0);
}
