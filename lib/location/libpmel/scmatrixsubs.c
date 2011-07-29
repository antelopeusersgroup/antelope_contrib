#include <math.h>
#include "stock.h"
#include "arrays.h"
#include "db.h"
#include "pf.h"
#include "elog.h"
#include "location.h"
#include "pmel.h"
/* Creates an associative array of column index positions using
pattern from associative array of Station structures keyed by sta name.
The order will be controlled by what keysarr gives.  Note we use 
pointers to int to store column indexes because getarr from this 
array signals a error with a NULL.  If we used an int I can't see 
how we could tell this from 0, which is a valid column index and
will, in fact, always be in the resultant.  

Arguments:
	sa - associative array of Station * that are counted to
		produce output indices.

Returns an associative array keyed by station names that hold
indices to column positions.  These are basically count order
from sa input.

Author:  G Pavlis
Written:  October 2000
*/
Arr *create_sta_index(Arr *sa)
{
	int *cindex;
	int i;
	Arr *aout;
	Tbl *keys;
	char *sta;

	aout = newarr(0);
	keys = keysarr(sa);
	for(i=0;i<maxtbl(keys);++i)
	{
		sta = gettbl(keys,i);
		allot(int *,cindex,1);
		*cindex = i;
		setarr(aout,sta,cindex);
	}
	return(aout);
}
/* Creation routine for an SCMatrix structure used internally 
as the working internal object of pmel.  It defines static 
sizes and indices that define the columns of the working matrix.
It then allocs memory for the working vectors of station correction
(path anomalies) that are the major outputs of pmel.  The initial
working S matrix is created, but it's size is set to 1 row.  This
assumes that later on this space will be realloced to match variable
data size for different event groups that are processed.  That is
the number of data in a group is variable, but the station list and
phase list that define the column structure of S are static through
a single run of pmel. 
Arguments:
	stalist - Associative array of Station structures keyed by
		station name.  This is used to build the internal
		station column indexing array.
	arrp - Associate array of phase handles keyed by phase
		name.  

Always returns a pointer to a valid SCMatrix structure (object).  
If any problems happen this function will call die.  This would
normally be memory alloc problems, but it could also happen if
the stalist or arrp arrays are foobarred. 

Note the algorithm assumes the list of phase handles is all inclusive
and will create a matrix large enough to deal with every phase it
finds listed there.  

Author:  GAry Pavlis
Written: October 2000
*/
SCMatrix *create_SCMatrix(Arr *stalist, Arr *arrp)

{
	SCMatrix *s;
	Tbl *tkeys;
	char *key;
	int i;
	int *phase_col;

	allot(SCMatrix *,s,1);
	s->nsta = cntarr(stalist);
	s->nphases = cntarr(arrp);
	s->ncol = (s->nsta)*(s->nphases);
	s->sta_index = create_sta_index(stalist);
	if((s->ncol)<=0)
		elog_die(0,"create_SCMatrix:  illegal matrix request\nNumber stations = %d and number of phases = %ld yielding %d matrix columns\n",
			s->nsta,cntarr(arrp),s->ncol);
	/* We set the initial number of rows to 1 and depend on 
	a realloc late to make the S workspace larger. */
	s->nrow = 1;
	allot(double *,s->S,(s->nrow)*(s->ncol));
	allot(double *,s->scref,s->ncol);
	allot(double *,s->sc,s->ncol);
	allot(double *,s->scbias,s->ncol);
	allot(double *,s->scdata,s->ncol);

	tkeys = keysarr(arrp);
	s->phase_index = newarr(0);

	for(i=0;i<maxtbl(tkeys);++i)
	{
		key = gettbl(tkeys,i);
		allot(int *,phase_col,1);
		*phase_col = i*(s->nsta);
		setarr(s->phase_index,key,phase_col);
	}
	freetbl(tkeys,0);
	return(s);
}
/* Destroy routine for an SCMatrix object */
void destroy_SCMatrix(SCMatrix *s)
{
	free(s->S);
	free(s->scref);
	free(s->sc);
	free(s->scbias);
	free(s->scdata);
	freearr(s->phase_index,free);
	freearr(s->sta_index,free);
	free(s);
}
/* To avoid having station corrections on top of other station corrections
we need to make sure the station correction portion of each phase
handle is cleared.  This is done with a simple freearr on the station
correction associative array.

Arguments:  
	pha - associative array of phase handle keyed by phase name.

Author:  Gary Pavlis
Written:  October 2000
*/
void clear_station_corrections(Arr *pha)
{
	int i;
	Phase_handle *p;
	Tbl *keys;
	char *phase;

	keys = keysarr(pha);
	for(i=0;i<maxtbl(keys);++i)
	{

		phase = gettbl(keys,i);
		p = (Phase_handle *)getarr(pha,phase);
		if(cntarr(p->time_station_corrections)>0)
		{
			elog_notify(0,"Clearing time station correction for phase %s\nConsider editing your parameter file for this program\n",
				phase);
			freearr(p->time_station_corrections,free);
			p->time_station_corrections=newarr(0);

		}
	}
	freetbl(keys,0);
}
/*This is an important function that sets the set of base station corrections
used as the bias term in pmel.  These base station corrections are computed
as the difference between travel times computed by a 3D calculator 
(This program actually has no concept of this directly.  It just uses
the 3D model as a reference.) and a reference model (presumably generally
a 1D model like iasp91, but it could itself be 3D really).  Note
that if the 3D model has any station corrections set they will be applied.
For the reference model (1D) the station corrections are set by this 
function.  This allows testing or application with a global set of station
corrections used as the 3D equivalent, but allowing the results to be
space variable when used with dbpmel.  i.e. you can, if desired, use
the same travel time calculator for the 1D and 3D case, but if 
station corrections are defined for the 3D handle they will be used 
as a global bias term.  Similarly, if this same process is done with
no station corrections one can produce a "unbiased estimate" meaning
the bias term is always forced to 0 everywhere.  Experience has shown
that although this might be conceptually appealing it generally is a
bad idea unless the reference model is very good to start with.

Arguments:
	pha - associative array of phase handles for 1D reference model
		calculator (station correction of these handles are set here).
	pha3D - same as pha, but for the 3D (bias) model calculator
	sa - associative array of Station objects with station location
		information (keyed by sta name).
	hc - hypocentroid location.

Returns:
	0 - normal, aok
	> 0 - count of failures in computing travel times 
	< 0 - total failure
*/
int initialize_station_corrections(Arr *pha,Arr *pha3D, Arr *sa,Hypocenter *hc)
{
	int i,j;
	Phase_handle *p,*p3D;
	Tbl *keys;
	char *phase;
	Station *s;
	Tbl *stakeys;
	char *sta;
	int nsc_fail=0;
	Ray_Endpoints x;
	Travel_Time_Function_Output t,t3D;
	double *sc,*sc3D;

	keys = keysarr(pha);
	stakeys = keysarr(sa);
	x.slat = hc->lat;
	x.slon = hc->lon;
	x.sz = hc->z;

	for(i=0;i<maxtbl(keys);++i)
	{
		phase = gettbl(keys,i);
		p = (Phase_handle *)getarr(pha,phase);
		p3D = (Phase_handle *)getarr(pha3D,phase);
		if(pha3D == NULL)
		{
			elog_notify(0,"No handle for 3d (bias correction) for phase %s\nBias contribution set to 0 for this phase\n",
				phase);
			nsc_fail += cntarr(sa);
			/* Emptying the arr is a simple way to create
			the equivalent of all 0s*/
			if(cntarr(p->time_station_corrections)>0)
			{
				freearr(p->time_station_corrections,free);
				p->time_station_corrections=newarr(0);
			}
			continue;
		}
		for(j=0;j<maxtbl(stakeys);++j)
		{
			sta = gettbl(stakeys,j);
			s = (Station *)getarr(sa,sta);
			x.sta = s->name;
			x.rlat = s->lat;
			x.rlon = s->lon;
			x.rz = -(s->elev);
			/* Get a pointer to hold the new station correction.
			If it doesn't exist yet, create it and enter it into
			the associative array.  Note because these are direct
			pointers we don't have set the entry in the arr below*/
			sc = (double *)getarr(p->time_station_corrections,sta);
			if(sc == NULL)
			{
				allot(double *,sc,1);
				setarr(p->time_station_corrections,sta,sc);
			}
			t3D = p3D->ttcalc(x,phase,RESIDUALS_ONLY);
			t = p->ttcalc(x,phase,RESIDUALS_ONLY);
			if( (t.time == TIME_INVALID) 
				|| (t3D.time == TIME_INVALID) )
			{
				elog_notify(0,"Travel time failure for phase %s computing reference corrections for station %s\nCorrection set to 0.0\n",
					phase,sta);	
				*sc = 0.0;
				++nsc_fail;
			}
			else
			{
				*sc = t3D.time - t.time;
				sc3D = (double *)
				    getarr(p3D->time_station_corrections,sta);
				if(sc3D != NULL)
					*sc += *sc3D;
			}
		}
	}
	freetbl(stakeys,0);
	freetbl(keys,0);
	return(nsc_fail);
}
/* This function computes the "reference station corrections" 
for each station/phase defined in the structure s for the hypocenter
location h.  The reference is computed as the travel time difference
between the model computed with the phase handles pha and pha3D.  
In this program, however, these have already been computed and stored
in an associative array found in the phase handle *pha.  Thus the
primary task here is indexing this list and placing the values in
the right position of the vector in the SCMatrix s->scref.  

Arguments:
	s - Station correction structure used internally by dbpmel
	h - hypocentroid where corrections are to be computed
	stalist - associative array of Station structures (objects).
	pha - phase handles for model used for routine locations
	pha3d - bias generating model (usually 3d) to compute
		corrections from.  This is actually an associative
		array of phase handle similar to pha.  

Returns 0 for no problems.  Positive number is the count of the 
number of problems in computing travel times.  

Author:  G pavlis
Revision:  Original did redundant travel time calculation.  
Now it just grabs station corrections from pha.  This is
somewhat of a future software maintenance worry as it assumes
that the station correction array in pha has been set up to contain
the reference station corrections.  
*/
int compute_scref(SCMatrix *s, Hypocenter *h, Arr *stalist,
	Arr *pha, Arr *pha3D)
{
	Tbl *phskeys,*stakeys;
	int i,j;
	char *phase,*sta;
	int *iptr,iphacol,icol;
	Travel_Time_Function_Output t,t3d;
        Ray_Endpoints x;
	Phase_handle *phand,*p3dhand;
	Station *sobj;
	double *sc; 
	int ierr=0;

	/* Need to initialize this vector to zeros first since
	the loop below isn't guaranteed to hit each element especially
	when errors occur*/
	for(i=0;i<s->ncol;++i) s->scref[i]=0.0;

	phskeys = keysarr(pha);
	stakeys = keysarr(s->sta_index);

	for(j=0;j<maxtbl(phskeys);++j)
	{
		phase = (char *)gettbl(phskeys,j);
		iptr = (int *)getarr(s->phase_index,phase);
		iphacol = *iptr;
		phand = (Phase_handle *)getarr(pha,phase);
		p3dhand = (Phase_handle *)getarr(pha3D,phase);
		if((phand == NULL) || (p3dhand == NULL) )
		{
			elog_complain(0,"Cannot compute reference path anomaly corrections for phase %s\nIncomplete phase handles for this phase\nCheck that this phase is defined for both reference model and 3D model\nPath anomaly vector set to 0 for this phase.\n",
				phase);
			++ierr;
			continue;
		}
		for(i=0;i<maxtbl(stakeys);++i)
		{
			sta = gettbl(stakeys,i);
			iptr = (int *)getarr(s->sta_index,sta);
			icol = iphacol + (*iptr);
			sobj = (Station *)getarr(stalist,sta);
			if(sobj == NULL)
			{
				elog_complain(0,"Lookup failed for station %s\nReference path anomaly for phase %s set to 0 for this station\n",
					sta,phase);
				++ierr;
				continue;
			}
			sc = (double *)getarr(phand->time_station_corrections,
					sta);
			if(sc != NULL)
			{
				s->scref[icol]=*sc;
			}
			else
			{
				elog_complain(0,"WARNING: compute_scref\
lookup failed searching for station correction for %s\n\
Attempting recovery.  Inconsistency likely\n", sta);
 
				x.sta = sta;
				x.slat = h->lat;
				x.slon = h->lon;
				x.sz = h->z;
				x.rlat = sobj->lat;
				x.rlon = sobj->lon;
				x.rz = -(sobj->elev);
				t = phand->ttcalc(x,phase,RESIDUALS_ONLY);
				t3d = p3dhand->ttcalc(x,phase,RESIDUALS_ONLY);
				s->scref[icol]=(t.time)-(t3d.time);
				/* We actually apply any station corrections defined
				in the phase handle for either model */
				sc = (double *)getarr(phand->time_station_corrections,
					sta);
				if(sc != NULL) s->scref[icol] += (*sc);
				sc = (double *)getarr(p3dhand->time_station_corrections,
					sta);
				if(sc != NULL) s->scref[icol] -= (*sc);
			}
		}
	}
	freetbl(phskeys,0);
	freetbl(stakeys,0);
	return(ierr);
}
/* This is kind of the inverse of the above routines.  Once a new
set of station corrections are computed they need to be stored in
the time_station_correction array in the phase handles to be actually
used.  Because of how travel time calculators work in genloc the
station corrections cannot be applied any other way.  It is a bit
complex this way, but an example of the dark side of using objects
is programming.

arguments:
s - SCMatrix used by pmel
pha - phase handles for reference model (station corrections are
updated in these)

Returns 0 for all ok.  Positive number is a count of errors encountered
in update attempt .

Author:  G Pavlis
Written:  August 2001
First draft has a dohhh because I neglected to realize I had to have
this function.  Logic gap discovered in testing phase.
*/
int update_scarr(SCMatrix *s,Arr *pha)
{
	Tbl *phskeys,*stakeys;
	int i,j;
	char *phase,*sta;
	int *iptr,iphacol,icol;
	Phase_handle *phand;
	double *sc; 
	int ierr=0;

	phskeys = keysarr(pha);
	stakeys = keysarr(s->sta_index);

	for(j=0;j<maxtbl(phskeys);++j)
	{
		phase = (char *)gettbl(phskeys,j);
		iptr = (int *)getarr(s->phase_index,phase);
		iphacol = *iptr;
		phand = (Phase_handle *)getarr(pha,phase);
		if(phand == NULL)
		{
			elog_notify(0,"Cannot update station correction for phase %s\n",
				phase);
			++ierr;
			continue;
		}
		for(i=0;i<maxtbl(stakeys);++i)
		{
			sta = gettbl(stakeys,i);
			iptr = (int *)getarr(s->sta_index,sta);
			icol = iphacol + (*iptr);
			sc = (double *)getarr(phand->time_station_corrections,
					sta);
			if(sc != NULL)
			{
				*sc = s->sc[icol];
			}
			else
			{
				++ierr;
				elog_notify(0,"update_scarr:  lookup failed for station%s\nCannot update station correction for phase %s\n",
					sta,phase);
			}
		}
	}
	freetbl(stakeys,0);
	return(ierr);
}
