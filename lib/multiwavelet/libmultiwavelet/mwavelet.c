#include <stdlib.h>
#include <strings.h>

#undef  __USE_SVID     /* for Linux !! to get MAXFLOAT */
#define  __USE_XOPEN 1 /* for Linux !! */
#include <math.h>
#undef  __USE_XOPEN

#include <perf.h>
#include "multiwavelet.h"
#include "location.h"
#include "tr.h"


/* This collection of functions handle various tasks related to the 
multiwavelet transform.  This grouping is a little arbitrary, but 
useful in some contexts. */

/* phase measurements always have the standard 2*PI ambiguity. 
This little function computes the phase difference using
complex arithmetic.  This would be much easier if C had
complex numbers as a native data type.
This is much simpler than working with the phase directly 
as we don't need to worry about wrapping this way at all.
We still have cycle skip potential, but that is a different issue.
*/
double unwrap_delta_phase(complex z1, complex z0)
{
	double r1,r0;
	double rpart,ipart;
	complex nz1,nz0;  /* used to hold z1 and z0 normalized to unit length*/
	double dphi;

	r0 = hypot((double)z0.r,(double)z0.i);
	r1 = hypot((double)z1.r,(double)z1.i);
	nz0.r = z0.r/((float)r0);
	nz0.i = z0.i/((float)r0);
	nz1.r = z1.r/((float)r1);
	nz1.i = z1.i/((float)r1);
	/* This forms the product z1*z0(conjugate) that phase of
	which is phi1-phi0 */
	rpart = ((double)(nz0.r))*((double)(nz1.r))
		+ ((double)(nz0.i))*((double)(nz1.i));
	ipart = ((double)(nz0.r))*((double)(nz1.i))
		- ((double)(nz0.i))*((double)(nz1.r));
	dphi = atan2(ipart,rpart);
	return(dphi);
}


/* this small function makes the key string used to index the MWtransform
associative array output matrices.  It uses the sta and chan variables 
directly and assumes the strings are properly terminated with no white
space.  Separator is a :  
Note the string is alloced here and needs to be freed after use.
*/
char *make_mw_key(char *sta, char *chan)
{
	int size;
	char *key;
	size = strlen(sta) + strlen(chan) + 2;
	key = malloc(size);  
	if(key == NULL) elog_die(0,
		"make_mw_key: malloc failure for string of length %d\n",size);
	strcpy(key,sta);
	strcat(key,"/");
	strcat(key,chan);
	return(key);
}
	

/* This function is the main multiwavelet transform routine.  It computes
multiwavelet transforms in a group of band dependent time windows relative
to a set of arrival times.   The code is drastically complicated by
having to deal with possible data gaps in a window and handling
the gaps gracefully through multiple bands.  

Arguments:
	tr - trace database pointer.  Can be raw as the pure trace
		table is manipulated directly.  HOWEVER, it is assumed
		trsplit has been called previously to define data gaps.
		For efficiency tr should only contain entries for 
		data in standard coordinates, but the routine would
		blindly process anything setting sta/chan keys that
		would not be referenced in the mwap program.  
	arrivals - associative array of arrival reference times.  The
		windows are all aligned relative to this set of times,
		so results are only as good as these times.  
	win - vector of length nbands of analysis window definitions 
	decfac - vector of length nbands of decimation factors for each band
	decimators - decimation object definitions used by MWtransform routine.
		(see MWtransform.3)
	nbands - number of frequency bands in transform
	basis - multiwavelet basis functions
	nbasis - number of multiwavelets in each band


Returns an associate array keyed by sta/chan of MWtrace ** pointers
returned by MWtransform routine. 

Numerous complaint messages can come from this routine.  It will always
return something even if the arr is empty.  It dies only on malloc
errors.  It is also possible that it will return an arr with properly
set keys but for which all the contents are null.  

Author:  Gary Pavlis
Written:  June 1999
*/

Arr *tr_mwtransform(
	Dbptr tr, 
	Arr *arrivals,
	Time_Window *win, 
	int *decfac,
	Tbl **decimators, 
	int nbands,
	MWbasis *basis,
	int nbasis)
{
	Dbptr dbgrp;
	Tbl *sortkeys,*grpkeys;
	int nrows,nchan;
	Dbptr dbbundle;
	double rwinstart, rwinend;  /* relative start and end times in s*/
	double trace_start, trace_end;  /* trace start and end absolute times */
	double swtime, ewtime;  /* absolute start and end window times */

	Time_Window twinall;  /* Widest time window required */
	char sta[10],chan[10];
	double *atime;

	Trsample *trdata;
	int nsamp;
	double samprate,si;

	int is, ie, nrec;
	int istart; 
	int points_to_process;
	MWtrace ***mwt; 
	char *key;
	Arr *result=newarr(0);  /* output associative array keyed by key */

	/* This function returns the largest time window required
	in all bands.  */

	twinall = compute_time_window(win,decfac,nbands);
	rwinstart = ((double)(twinall.tstart))*(twinall.si);
	rwinend = ((double)(twinall.tend))*(twinall.si);

	/* The first step is to sort the trace table by sta/chan/time then
	group it by sta/chan.  This allows a simple test for gaps by
	count. i.e. single rows mean no gaps.  This assumes, of course,
	the trsplit has been called previously */

	sortkeys = newtbl(0);
	pushtbl(sortkeys,"sta");
	pushtbl(sortkeys,"chan");
	pushtbl(sortkeys,"time");
	tr = dblookup(tr,0,"trace",0,0);
	tr = dbsort(tr,sortkeys,0,0);

	grpkeys = newtbl(0);
	pushtbl(grpkeys,"sta");
	pushtbl(grpkeys,"chan");
	dbgrp = dbgroup(tr,grpkeys,0,2);
	/* The 2 in the above I think is right based on the Trace4.0 
	schema definition of bundle types */

	dbquery(dbgrp,dbRECORD_COUNT,&nrows);

	for(dbgrp.record=0;dbgrp.record<nrows;++dbgrp.record)
	{
		int is,ie,nrec,irec;
		double reclen;
		double s1, s2, tsegment;
		if(dbgetv(dbgrp,0,
			"sta",sta,
			"chan",chan,
			"bundle",&dbbundle,
			0) == dbINVALID)
		{
			  elog_complain(0,"dbgetv error while reading record %d of sta/chan grouping table\nData loss likely\n",
					dbgrp.record);
			  continue;
		}
		atime = (double *)getarr(arrivals,sta);
		if(atime == NULL)
		{
			elog_complain(0,"Station mismatch:  Cannot find arrival reference time for station %s\nmultiwavelet transform not computed for %s/%s\n",
				sta,sta,chan);
			continue;
		}
		swtime = (*atime) + rwinstart;
		ewtime = (*atime) + rwinend;
		/*This assumes trsplit has been called.  We trap 
		data with gaps and deal with the separately */
		dbget_range(dbbundle,&is,&ie);
		if(is == (ie-1) )
			nrec = is;
		else
		{
		/* enter here when data gaps are present in the time
		window.  We find the longest and use it */
fprintf(stderr,"DEBUG:  entered gap loop for %s:%s\n",sta,chan);

			for(reclen = 0.0,tr.record=is;
				tr.record<ie;++tr.record)
			{
				/*I intentionally don't trap errors here.
				It should never happen.*/
				dbgetv(tr,0,"time",&trace_start,
					"endtime",&trace_end,0);
				if(trace_end <= swtime) continue;
				if(trace_start >= ewtime) continue;
				if(trace_start < swtime)
					s1 = swtime;
				else
					s1 = trace_start;
				if(trace_end > ewtime)
					s2 = ewtime;
				else
					s2 = trace_end;
				tsegment = s2 - s1;
				if(tsegment > reclen)
				{
					reclen = tsegment;
					nrec = tr.record;
				}
			}
			if(reclen<=0.0)
			{
				elog_complain(0,"No data in window from %s to %s for sta/chan %s/%s -- probable data gap\n",
					strtime(swtime),strtime(ewtime),
					sta,chan);
				continue;
			}
		}
		tr.record = nrec;
		dbgetv(tr,0,"time",&trace_start,
				"endtime", &trace_end,
				"nsamp",&nsamp,
				"samprate",&samprate,
				"data",&(trdata),0);

		si = 1.0/samprate;
		/*We want to be careful to keep times accurate to 
		the subsample level so we are careful here to compute
		times to fractions of a sample.  The alignment here
		assumes time tag is coincident with a sample point. 
		The tag must be computed accurately since it cascades
		through all the stages of the mwtransform. */
		istart = nint((swtime - trace_start)*samprate);
		if(istart < 0 )
		{
			/* correction for gap if start time is offset.
			Careful -- swtime is reset, but is reset above
			within this loop. */
			istart = 0;
			swtime = trace_start;
		}
		else
		{
			swtime = trace_start + ((double)istart)/samprate;
		}
		ewtime = MIN(trace_end,ewtime);
		points_to_process = nint((ewtime - swtime)*samprate) + 1;
		if((points_to_process+istart)> nsamp)
		{
			points_to_process = nsamp - istart;
			elog_complain(0,"Data truncated for %s/%s at time %s\nProbable data gap led to truncation of processing window to %d points\n",
					sta,chan,strtime(swtime),
					points_to_process);
		}
		/* Need to allot this pointer so we can load it into the
		arr below.  setarr requires pointers stored in static 
		memory*/
		allot(MWtrace ***,mwt,1);
		*(mwt) = MWtransform(trdata+istart,
				si,swtime,points_to_process,
				basis, nbasis, decimators, nbands);
		key = make_mw_key(sta,chan);
		setarr(result,key,mwt);
		free(key);
	}
	return(result);
}
/*This pair of function create and destroy a MWgather structure
respectively.  */
MWgather *MWgather_alloc(int nsta)
{
	MWgather *gather;

	gather = (MWgather *)malloc(sizeof(MWgather));
	if(gather == NULL) elog_die(0,"MWgather_alloc cannot alloc MWgather struc\n");
	gather->sta = (MWstation **)calloc(nsta, sizeof(MWstation *));
	gather->x1 = (MWtrace **)calloc(nsta,sizeof(MWtrace *));
	gather->x2 = (MWtrace **)calloc(nsta,sizeof(MWtrace *));
	gather->x3 = (MWtrace **)calloc(nsta,sizeof(MWtrace *));
	return(gather);
}
/* This frees the trace portions of an MWgather object.  This is 
needed to clear memory created by a gather which uses transformed
coordinates or otherwise calls MWtrace_dup.  NOTE this routine NEVER
clear the sta pointer in the MWgather structure as it is assume these
are pointers to static station structure that are being manipulated 
here and should never be cleared.  Similarly, this routine should be
called only when you are sure the x1, x2, and x3 traces are copies of
data that aren't required later.  
*/
void free_MWgather(MWgather *gather)
{
	int i;

	for(i=0;i<gather->nsta;++i)
	{
		free(gather->x1[i]->z);
		free(gather->x2[i]->z);
		free(gather->x3[i]->z);
		free(gather->x1[i]);
		free(gather->x2[i]);
		free(gather->x3[i]);
	}
	free(gather->x1);
	free(gather->x2);
	free(gather->x3);
	free(gather);
}
/* mwap uses an associative array to store complicated pointers to 
MWtrace objects produced by the multiwavelet transform.  We can't just
call the normal freearr function to release this space because the
multiwavelet transform indexed in the array is a messy matrix of 
pointers to MWtrace objects.  Consequently we somewhat duplicate
the functionality of freearr here for this special case.
Note that the input arr is freed as well as it's contents.

Arguments:
	mwtarr - associative array of multiwavelet transform output pointers 
	nbands - number of frequency bands in transform
	nwavelets - number of wavelets in transform 
*/
void free_MWtransform_arr(Arr *mwtarr,int nbands, int nwavelets)
{
	MWtrace ***mwt;  /*entity extracted from arr */
	Tbl *t;   /* result of running keysarr */
	char *key;
	int i;
	
	t = keysarr(mwtarr);
	for(i=0;i<maxtbl(t);++i)
	{
		key = gettbl(t,i);
		mwt = (MWtrace ***)getarr(mwtarr,key);
		free_MWtrace_matrix(*(mwt),0,nbands-1,0,nwavelets-1);
	}
	/* We call freetbl in this form because we don't want to
	free the keys here */
	freetbl(t,0);
	/* The free here releases the MWtrace *** pointers */
	freearr(mwtarr,free);
}
/* Simple companion to below used to avoid cluttering main code.
A simple large negative number will suffice as an error code
since a signal to noise ratio less than 0 makes no physical sense. */
void set_snr_to_error(Signal_to_Noise *snr)
{
	snr->ratio_z = -99999.9;
	snr->ratio_n = -99999.9;
	snr->ratio_e = -99999.9;
	snr->ratio_3c = -99999.9;
	snr->noise_z = -99999.9;
	snr->noise_n = -99999.9;
	snr->noise_e = -99999.9;
	snr->noise_3c = -99999.9;
	snr->min_ratio_z = -99999.9;
	snr->min_ratio_n = -99999.9;
	snr->min_ratio_e = -99999.9;
	snr->min_ratio_3c = -99999.9;
	snr->max_ratio_z = -99999.9;
	snr->max_ratio_n = -99999.9;
	snr->max_ratio_e = -99999.9;
	snr->max_ratio_3c = -99999.9;
}
/* Tests snr structure for above error condition.  This is preferable
to something like a test against a particular element of the structure
embedded in the code because one might want to change the way an error
was defined.  This way they should stay together. 
Arguments:
	snr - snr structure to test
Returns a logical test.  1 if the snr is invalid and 0 if it is ok.
*/
int snr_is_invalid(Signal_to_Noise *snr)
{
	if((snr->ratio_z)<0.0) return(1);
	return(0);
}

/* This is a small companion function to the one immediately 
following.  It grabs items from pf to test if the computed
signal to noise passes.  It returns 1 when the snr is inadequate,
0 if snr is big by it's definition 

Arguments:
	snr - signal to noise ratio structure pointer t

Modification, June 2000:
Original version just tested a single snr cutoff and applied
it to all bands equally.  This was changed for two reasons.
First, it might be useful sometimes to make the snr cutoff
frequency dependent so it is useful to have the cutoff change
to a tbl with a different cutoff possible for each band.  
Second, segmented data or triggered data often have 
an insufficient preevent time window to allow for the 
long pad times induced by decimators.  This can make it 
hard to compute a reasonable noise window.  To avoid
an unrecoverable error in that situation, I added the
feature that if the cutoff value is <= 0.0 the test run
here will always return 0.  i.e. the snr values are not
tested and all data are used without checking in that
band.  A couple examples help here:

snr_cutoff &Tbl{
5.0
10.0
4.0
}
sets the cutoff to 5.0 for band 0, 10 for band 1, and 4 for band 3.

snr_cutoff &Tbl{
5.0
10.0
-1.0
}
Sets cutoff the same for band 0 and 1, but disables the
snr test for band 3.  
*/

int snr_is_too_low(Signal_to_Noise *snr,int band, Pf *pf)
{
	double snr_cutoff;
	char *snr_component;
	char *statistic;
	double test_value;
	char *line;
	Tbl *t;

	t = pfget_tbl(pf,"snr_cutoff");
	if(t == NULL) return(0);
	if(maxtbl(t)<band) return(0);
	line = gettbl(t,band);
	sscanf(line,"%lf",&snr_cutoff);

	statistic = pfget_string(pf,"snr_cutoff_statistic");
	snr_component = pfget_string(pf,"snr_component_to_test");
	if(!strcmp(snr_component,"z"))
	{
		if(!strcmp(statistic,"max"))
			test_value = snr->max_ratio_z;
		else if(!strcmp(statistic,"min"))
			test_value = snr->min_ratio_z;
		else 
			test_value = snr->ratio_z;
	}
	else
	{
		if(!strcmp(statistic,"max"))
			test_value = snr->max_ratio_3c;
		else if(!strcmp(statistic,"min"))
			test_value = snr->min_ratio_3c;
		else 
			test_value = snr->ratio_3c;
	}
	if(test_value > snr_cutoff) 
		return(0);
	else
		return(1);
}
/* This short function us used to check if a signal to 
noise ratio test is enabled for a particular band.  It 
parses the "snr_cutoff" Tbl in the pf space identical
to that used in snr_is_too_low, but here it simply 
looks for a value of the cutoff <= 0.0 which is a 
signal to not perform an snr test.  

Arguments:
	band - frequency band to check
	pf - parameter space

Returns 0 or 1.   0 means do not run an snr check
on this band while 1 says it is ok to try.
*/
int check_snr_this_band(int band, Pf *pf)
{
	char *line;
	Tbl *t;
	double snr_cutoff;

	t = pfget_tbl(pf,"snr_cutoff");
	if(t == NULL) return(0);
	if(maxtbl(t)<=band) return(0);
	line = gettbl(t,band);
	sscanf(line,"%lf",&snr_cutoff);
	if(snr_cutoff<=0.0) return(0);
	return(1);
}

/* This routine takes a collection of MWtransformed traces and builds
a MWgather object.  The algorithm is pretty straightforward except it
has to handle the fact that some bands may have no data due to gaps.

Arguments:
	band, wavelet define the frequency band and wavelet to 
		build this gather from respectively.
	mwdata is the data array keyed by sta/chan
	stations is the MWstations associative array key by sta
	snrarr is an associative array of Signal_to_Noise objects
		for this band.  
	pf - parameter space.  Signal to noise ratio cutoff criteria
		come from pf.  (see code for details)
*/
MWgather *build_MWgather(int band, int wavelet, 
	Arr *mwdata, Arr *stations, Arr *snrarr,
	Pf *pf)
{
	MWgather *gather;
	Tbl *t;  /* table of keys used to loop through data */
	int i,ii;  /* loop counter */
	int nsta;
	char *sta;
	char *key;
	MWtrace ***x1,***x2,***x3;
	Signal_to_Noise *snr;

	/* mwdata will have nsta*ncomponents traces.  We assume
	ncomponents is 3 and add one to compute nsta for safety.
	This is overly cautious because missing components will
	always lead to a station being tossed out within this
	routine, but better safe than sorry. */
	nsta = cntarr(mwdata);
	nsta = (nsta/3) + 1;
	gather = MWgather_alloc(nsta);
	gather->ncomponents = 3;   /* always this value here */

	/* We loop over the full list of stations rather than the mwdata
	array for simplicity.  It might be more efficient to do the opposite
	when a lot of data is lost, but I assume most data have reasonable
	recovery so this simpler algorithm is preferable */
	t = keysarr(stations);

	for(i=0,ii=0;i<maxtbl(t);++i)
	{
		sta = gettbl(t,i);
		key = make_mw_key(sta,EW);
		x1 = (MWtrace ***)getarr(mwdata,key);
		free(key);
		key = make_mw_key(sta,NS);
		x2 = (MWtrace ***)getarr(mwdata,key);
		free(key);
		key = make_mw_key(sta,VERTICAL);
		x3 = (MWtrace ***)getarr(mwdata,key);
		free(key);
		if( (x1 == NULL) || (x2 == NULL) || (x3 == NULL) ) continue;
		/* mwtransform sets the nz field of any MWtrace to 0 to
		signal a null trace.  When this happens the contents of
		the structure are junk, so we must delete them at this
		stage */
		if( (((*x1)[band][wavelet].nz) == 0)
		|| (((*x2)[band][wavelet].nz) == 0)
		|| (((*x3)[band][wavelet].nz) == 0) ) continue;
		/* test snr This test skips stations with low snr
		and stations for which snr could not be computed
		but do this only if the snr cutoff test is
		enabled.*/
		if(check_snr_this_band(band,pf))
		{
			snr = (Signal_to_Noise *)getarr(snrarr,sta);
			if(snr_is_too_low(snr,band,pf)) continue;
			if(snr_is_invalid(snr))continue;
		}

		gather->sta[ii] = (MWstation *)getarr(stations,sta);
		/* Hideous pointer/array syntax here, but the idea
		is that we are putting the band/wavelet component 
		for each channel at ith position in gather structure */
		gather->x1[ii] = &((*x1)[band][wavelet]);
		gather->x2[ii] = &((*x2)[band][wavelet]);
		gather->x3[ii] = &((*x3)[band][wavelet]);
		++ii;
	}
	gather->nsta = ii;
	return(gather);
}
/* this is a MWtrace equivalent to strdup.  It clones a copy of
the MWtrace object pointed to by t allocating memory as necessary.
*/
MWtrace *MWtrace_dup(MWtrace *t)
{
	MWtrace *new=NULL;

	allot(MWtrace *,new,1);
	new->dt0 = t->dt0;
	new->dt = t->dt;
	new->decimation_factor = t->decimation_factor;
	new->basis = t->basis;
	new->f0 = t->f0;
	new->fw = t->fw;
	new->nz = t->nz;
	new->starttime = t->starttime;
	new->endtime = t->endtime;
	allot(complex *,new->z,t->nz);
	ccopy(t->nz,t->z,1,new->z,1);
	return(new);
}
	
	
/* This function applies a given transformation matrix to a group of 
Multiwavelet transformed traces stored in a MWgather structure.  It
returns a new MWgather object with the transformation matrix applied
to the original groups of traces.  It is blindly assumed that
all the MWtrace objects contained in the original MWgather have the 
following properties:
1.  All three components are present in parallel x1, x2, and x3 vectors
of MWtrace objects.
2.  There are no null MWtrace objects (MWtransform returns these in
some situations, but the null pointers are used here and elsewhere to
mark problem data that had to be discarded.  Point is to watch when
this routine is called. )

The routine attempts to recover some types of data gap problems.  
It discards data if the start times are mismatched by more than 
a sample for a given station.  This could potentially be fixed, but
it is a messy algorithm I chose to not deal with (I believe it should
be a rare condition.)  Irregular end times are handled correctly by
resetting the trace length to the minimum of all three components.  
This is less ugly to deal with because a free called on the traces
will still work correctly (this is the ugliness of the irregular
start time I'm avoiding).    

The original data are not altered.  The allocs memory and returns a pointer
to a new MWgather structure.  This means, of course, some housecleaning
is required after finished with the transformed data.  

Arguments:
	ingath - MWgather object to be transformed.  size comes
		from a field in the MWgather structure
	u - 3X3 transformation matrix stored as a vector of doubles
		in FORTRAN order (i.e. columnwise 0=(1,1); 1=(2,1);
		3=(3,1); 4=(1,2);  etc.)  Note this can be an 
		arbitrary matrix as far as this routine is concerned,
		but it would normally be some rotational matrix.
		This matrix is applied directly to traces in x1,x2,x3 order.

Author:  Gary Pavlis
Written:  Sept. 1999
*/
MWgather *MWgather_transformation(MWgather *ingath,double *u)
{
	MWgather *gather;
	int i,j,ii,nz;
	complex *z1, *z2, *z3;  /*work vectors holds transformed seismograms */
	int nsta,nz_sta;
	complex ztmp;
	/* These are needed by cdotu because of fortran interface */
	int ndim=3,one=1;

	gather = MWgather_alloc(ingath->nsta);
	gather->nsta = ingath->nsta;
	nsta = gather->nsta;
	gather->ncomponents = 3;   /* always this value here */
	for(i=0;i<nsta;i++)
	{
		/* the station name should be set even on stations
		marked dead (with NULL pointers) and we always copy
		this pointer so errors can be tagged by station name */
		gather->sta[i] = ingath->sta[i];
		/* We require all three components to have a common
		start time.  If they don't, we mark them dead (NULL)
		and skip them (This could be recovered, but I'll not
		waste time on a rare condition now (Sept 1999).
		This should be a fairly robust test that avoids
		floating point equal tests.*/
		if((fabs((ingath->x1[i]->starttime)-(ingath->x2[i]->starttime))
			> (ingath->x1[i]->dt0 )) ||
		   (fabs((ingath->x1[i]->starttime)-(ingath->x3[i]->starttime))
			> (ingath->x1[i]->dt0 ) ) )
		{
			gather->x1[i] = NULL;
			gather->x2[i] = NULL;
			gather->x3[i] = NULL;
			elog_complain(0,"Irregular start time for three components at station %s\nData rotation fails -- skipping data for this station\n",
				ingath->sta[i]->sta);
		}
		else
		{
			gather->x1[i] = MWtrace_dup(ingath->x1[i]);
			gather->x2[i] = MWtrace_dup(ingath->x2[i]);
			gather->x3[i] = MWtrace_dup(ingath->x3[i]);
			/* reset the trace length so all three are 
			consistent if necessary */
			if( ( (gather->x1[i]->nz) != (gather->x2[i]->nz) )
			 || ( (gather->x1[i]->nz) != (gather->x3[i]->nz) ) )
			{
				int nznew;
				nznew = MIN(gather->x1[i]->nz,
						gather->x2[i]->nz);
				nznew = MIN(nznew,gather->x2[i]->nz);
				gather->x1[i]->nz = nznew;
				gather->x2[i]->nz = nznew;
				gather->x3[i]->nz = nznew;
			}
		}
	}
	/* find the longest complex vector in the gather (needed for size) 
	Note because of above tests we know all three components must
	be equal length so we only test x1.  */
	for(i=0,nz=0;i<nsta;i++)
	{
		if(gather->x1[i] != NULL) nz = MAX(nz,gather->x1[i]->nz);
	}

	allot(complex *,z1,nz);	
	allot(complex*,z2,nz);
	allot(complex *,z3,nz);

/*  Sun changed the argument list for caxpy in version
6.0 of their workshop compilers that use sunperf.  Rather than
have a bunch of ifdefs below, I enclose this entire loop in
an ifdef to make it cleaner */

#ifdef SUNPERF6
	for(i=0;i<nsta;i++)
	{
		if(gather->x1[i] == NULL) continue;
		nz_sta=gather->x1[i]->nz;
		/* This is a matrix multiply using csscal and caxpy */
		ccopy(nz_sta,gather->x1[i]->z,1,
					z1,1);
		csscal(nz_sta,(float)u[0],z1,1);
		ztmp.r = (float)u[1];
		ztmp.i = 0.0;
		caxpy(nz_sta,ztmp,gather->x2[i]->z,1,z1,1);
		ztmp.r = (float)u[2];
		ztmp.i = 0.0;
		caxpy(nz_sta,ztmp,gather->x3[i]->z,1,z1,1);

		/* repeat for x2 and x3 rotated components */

		ccopy(nz_sta,gather->x1[i]->z,1,
					z2,1);
		csscal(nz_sta,(float)u[3],z2,1);
		ztmp.r = (float)u[4];
		ztmp.i = 0.0;
		caxpy(nz_sta,ztmp,gather->x2[i]->z,1,z2,1);
		ztmp.r = (float)u[5];
		ztmp.i = 0.0;
		caxpy(nz_sta,ztmp,gather->x3[i]->z,1,z2,1);

		ccopy(nz_sta,gather->x1[i]->z,1,
					z3,1);
		csscal(nz_sta,(float)u[6],z3,1);
		ztmp.r = (float)u[7];
		ztmp.i = 0.0;
		caxpy(nz_sta,ztmp,gather->x2[i]->z,1,z3,1);
		ztmp.r = (float)u[8];
		ztmp.i = 0.0;
		caxpy(nz_sta,ztmp,gather->x3[i]->z,1,z3,1);

		/* finally copy work vectors back to gather */
		ccopy(nz_sta,z1,1,gather->x1[i]->z,1);
		ccopy(nz_sta,z2,1,gather->x2[i]->z,1);
		ccopy(nz_sta,z3,1,gather->x3[i]->z,1);

	}
#else
	for(i=0;i<nsta;i++)
	{
		if(gather->x1[i] == NULL) continue;
		nz_sta=gather->x1[i]->nz;
		/* This is a matrix multiply using csscal and caxpy */
		ccopy(nz_sta,gather->x1[i]->z,1,
					z1,1);
		csscal(nz_sta,(float)u[0],z1,1);
		ztmp.r = (float)u[1];
		ztmp.i = 0.0;
		caxpy(nz_sta,&ztmp,gather->x2[i]->z,1,z1,1);
		ztmp.r = (float)u[2];
		ztmp.i = 0.0;
		caxpy(nz_sta,&ztmp,gather->x3[i]->z,1,z1,1);

		/* repeat for x2 and x3 rotated components */

		ccopy(nz_sta,gather->x1[i]->z,1,
					z2,1);
		csscal(nz_sta,(float)u[3],z2,1);
		ztmp.r = (float)u[4];
		ztmp.i = 0.0;
		caxpy(nz_sta,&ztmp,gather->x2[i]->z,1,z2,1);
		ztmp.r = (float)u[5];
		ztmp.i = 0.0;
		caxpy(nz_sta,&ztmp,gather->x3[i]->z,1,z2,1);

		ccopy(nz_sta,gather->x1[i]->z,1,
					z3,1);
		csscal(nz_sta,(float)u[6],z3,1);
		ztmp.r = (float)u[7];
		ztmp.i = 0.0;
		caxpy(nz_sta,&ztmp,gather->x2[i]->z,1,z3,1);
		ztmp.r = (float)u[8];
		ztmp.i = 0.0;
		caxpy(nz_sta,&ztmp,gather->x3[i]->z,1,z3,1);

		/* finally copy work vectors back to gather */
		ccopy(nz_sta,z1,1,gather->x1[i]->z,1);
		ccopy(nz_sta,z2,1,gather->x2[i]->z,1);
		ccopy(nz_sta,z3,1,gather->x3[i]->z,1);

	}
#endif
	free(z1);  free(z2);   free(z3);
	return(gather);
}			

/* Copy function for an MWgather object.  In C++ this would
define the = operator. The ingath object is allocated and 
duplicated returning a pointer to the new copy.  
*/
MWgather *MWgather_copy(MWgather *ingath)
{
	MWgather *gather;
	int nsta;
	int i;

	gather = MWgather_alloc(ingath->nsta);
	gather->nsta = ingath->nsta;
	nsta = gather->nsta;
	gather->ncomponents = ingath->ncomponents;
	for(i=0;i<nsta;i++)
	{
		/* the station name should be set even on stations
		marked dead (with NULL pointers) and we always copy
		this pointer so errors can be tagged by station name */
		gather->sta[i] = ingath->sta[i];
		gather->x1[i] = NULL;
		gather->x2[i] = NULL;
		gather->x3[i] = NULL;
		if(ingath->ncomponents<=1)
		{
			gather->x3[i] = MWtrace_dup(ingath->x3[i]);
		}
		else
		{
			if(ingath->x1[i]!=NULL) 
				gather->x1[i] = MWtrace_dup(ingath->x1[i]);
			if(ingath->x2[i]!=NULL) 
				gather->x2[i] = MWtrace_dup(ingath->x2[i]);
			if(ingath->x3[i]!=NULL) 
				gather->x3[i] = MWtrace_dup(ingath->x3[i]);
		}
	}
	return(gather);
}
/*   SIGNAL TO NOISE computation related functions */

/* Destroy function for associative arrays of snr objects.
snr_vector is assumed to an array of Arr's of length nwavelets.
*/
void free_sn_ratios_arr(Arr **snr_vector,int nwavelets)
{
	int i;

	for(i=0;i<nwavelets;++i)
	{
		freearr(snr_vector[i],free);
	}
}
/* Another companion to signal to noise function below.  This one
takes a complex input vector,z, of length n and computes |x_i| 
and stores the result in a[i].  i.e. it returns a vector of 
complex moduli 
*/
void cmplx_vector_mod(int n, complex *z,float *a)
{
	int i;
	for(i=0;i<n;++i) 
	  a[i] = (float)hypot((double)z[i].r,(double)z[i].i);
}
/* This function computes estimates of signal to noise ratio from 
multiwavelet transformed traces passed and indexed through 
associative arrays "signal" and "noise".  Signal to noise in 
a band is defined as the ratio of the modulus of complex
valued samples contained in the signal window divided by the 
MEDIAN modulus of the noise window.  The median measure makes
the ratio more robust in the presence of noise bursts in a 
preevent window normally used to define the noise segment.  The
s/n ratios from each wavelet in the bank are averaged to define
the elements of the Signal_to_Noise structure.  

The function returns a vector of Arr pointers of length nbands.
Each element of this vector is an Arr indexed by station that
contains pointers to Signal_to_Noise structures for that station.
e.g. if a is the Arr ** returned by this function, a[1] would be
an Arr * from which one can extract Signal_to_Noise pointers for
a named station.  Complicated, I know, but a useful construct here.

Arguments:
	signal - associative array indexing multiwavelet transformed
		signal windows.  Indexed by the make_mw_key function
		that indexes by station channel.
	noise - comparable array to signal for noise window segments
	arrivals - associative array indexed by station name of pointers
		to doubles containing arrival time reference for that
		station.  (Time_Window structures components
		are relative to this absolute time).
	swin - vector of nbands time window structures used to define
		signal segments in each band
	nwin - same as swin for noise.
	nbands - number of frequency bands in multiwavelet transform
	nwavelets - number of wavelets. 

Author:  G Pavlis
Written:  Sept. 1999
Modified:
April 2000
Original code used full windows passed to it with no trimming.  
This sometimes caused incorrect values because the transformed
segments did not always mesh with the original time windows leading
to wrong results.  Modified at this time in two major ways:
1.  fixed the time window problem
2.  I changed the measure of signal/noise.  Original code used rms/rms.
I changed it to mod(max)/median (mod full vector of noise)
*/ 
Arr **compute_signal_to_noise(Arr *signal,Arr *noise,Arr *stations,
		Arr *arrivals, Time_Window *swin, Time_Window *nwin,
		int nbands,int nwavelets)
{
	Arr **snrarr_vector;
	int i,j,k,l;
	Signal_to_Noise *snr;   /* we build this */
	MW_scalar_statistics stats,noisestats;   /* target for statistics functions */
	MWtrace ***ts[3],***tn[3];	/* ugly constructs of MWtransform output.
					3 is for three components in x,y,z order */
	char *chans[3]={ EW, NS, VERTICAL };   /* set fixed channel code names 
						items are defined in multiwavelet.h*/
	int missing_channel;  /* flag*/
	char *sta,*key;   /*keys */
	float signal_rms,noise_rms;  
	float *snr_wavelet,*sig3c,*noise3c;
	float *noise1c;
	int nsamp;
	Tbl *tkeys;
	double *arrival_time;
	int is,ie;  /* start end in samples for a vector */
	double si, sttest, setest, dt;  /* temporaries */
	float *ampwork;   /* hold vector of amplitudes sorted
			to compute s/n statistics */

	snrarr_vector = (Arr **)calloc(nbands,sizeof(Arr *));
	if(snrarr_vector == NULL) elog_die(0,"compute_signal_to_noise:  cannot alloc memory for %d Arr pointers\n",nbands);
	for(i=0;i<nbands;++i) snrarr_vector[i] = newarr(0);
	allot(float *,snr_wavelet,nwavelets);
	allot(float *,sig3c,nwavelets);
	allot(float *,noise3c,nwavelets);
	allot(float *,noise1c,nwavelets);
	/* we loop through the list of station and channels to build
	signal to noise structures.  Stations use the Arr while the
	channel codes are locked down and come from the multiwavelet.h
	include file */
	tkeys = keysarr(stations);
	for(i=0;i<maxtbl(tkeys);++i)
	{
		sta = gettbl(tkeys,i);
		for(j=0,missing_channel=0;j<3;++j)
		{
			key = make_mw_key(sta,chans[j]);
			ts[j] = (MWtrace ***)getarr(signal,key);
			tn[j] = (MWtrace ***)getarr(noise,key);

			if( (ts[j] == NULL) || (tn[j] == NULL) ) 
			{
				missing_channel = 1;
				free(key);
				break;
			}
			free(key);
		}
		if(missing_channel) continue;

		/* At this point we have signal and noise matrices of
		MWtrace objects.  We also now know we have all three
		channels of signal and noise available to work with,
		but some of the bands may have null pointers.  We work
		through the band/wavelet matrix and compute s/n as 
		an average across all wavelet for each band.  The 
		result is a s/n structure for each band stored in
		an array keyed by sta */
		
		for(j=0;j<nbands;++j)
		{
			allot(Signal_to_Noise *,snr,1);
			strcpy(snr->sta,sta);
			/* Immediately flag any entity with a null signal or noise trace defined
			by mwtransform by setting the nz field to 0. */
			if((ts[0][0][j][0].nz <= 0)||(tn[0][0][j][0].nz <= 0))
			{
				set_snr_to_error(snr);
			}
			else
			{
			/* perhaps superflous, but paranoia can be good*/
			si = swin[j].si;
			if( (si != ts[0][0][j][0].dt) 
			  || (si != tn[0][0][j][0].dt) )
				elog_complain(0,
			 	 "Warning(compute_signal_to_noise): inconsistent sample intervals, window specifies %lf and traces have %lf\nUsing window value\n",
					si,ts[0][0][j][0].dt);
			/* We set time window positions for snr calculations.
			Normal algorithm is to use the boundaries defined
			by the input time window.  However, in the event
			the data have been truncated due to gaps or 
			who knows what, we use a more cautious algorithm
			that silently truncates the window in the event
			such a truncation occurs. It is assumed the user
			can catch this as the time periods will be stored
			in the mw programs in database tables */
			arrival_time = (double *)getarr(arrivals,sta);
			if(arrival_time == NULL)
			{
				/* This shouldn't happen and is probably
				unnecessarily paranoid */
				elog_notify(0,"Warning (compute_signal_to_noise):  no arrival time for station %s\nBand %d will use full trace window\n",
					sta,j);
				snr->nstime = tn[0][0][j][0].starttime;
				snr->netime = tn[0][0][j][0].endtime;
				snr->sstime = ts[0][0][j][0].starttime;
				snr->setime = ts[0][0][j][0].endtime;
			}
			else
			{
				sttest = (*arrival_time);
				setest = (*arrival_time);
				sttest += si*((double)swin[j].tstart);
				setest += si*((double)swin[j].tend);
				snr->sstime = MAX(sttest,
					ts[0][0][j][0].starttime);
				snr->setime = MIN(setest,
					ts[0][0][j][0].endtime);
				sttest = (*arrival_time);
				setest = (*arrival_time);
				sttest += si*((double)nwin[j].tstart);
				setest += si*((double)nwin[j].tend);
				snr->nstime = MAX(sttest,
					tn[0][0][j][0].starttime);
				snr->netime = MIN(setest,
					tn[0][0][j][0].endtime);
			}
			/* Careful of extreme truncation errors */
			if( ((snr->sstime)>(snr->setime))
			  || ((snr->nstime)>(snr->netime)) )
			{
			/* The logic here is perverted because I added
			this test.  The continue skips all the below
			stuff.  The part after the continue realy
			should be an else clause */
				set_snr_to_error(snr);
				elog_notify(0,"compute_signal_to_noise:  no data in requested time window for station %s in band %d\nData gap problem in signal or noise window\n",
					sta,j);
				setarr(snrarr_vector[j],sta,snr);
				continue;
			}

			for(l=0;l<nwavelets;++l){
				sig3c[l]=0.0;
				noise3c[l]=0.0;
				noise1c[l]=0.0;
			}
			for(l=0;l<3;++l)
			{
			    for(k=0;k<nwavelets;++k)
			    {
				dt = snr->sstime - ts[l][0][j][k].starttime;
				is = nint(dt/si);
				if(is<0)
				{
					snr->sstime = ts[l][0][j][k].starttime;
					is = 0;
				}
				dt = snr->setime - ts[l][0][j][k].starttime;
				ie = nint(dt/si);
				/* -1 is from the age old fact that C indexing
				starts at 0 */
				if(ie > ((ts[l][0][j][k].nz) - 1) )
				{
					ie = ts[l][0][j][k].nz;
					--ie;
					snr->setime = ts[l][0][j][k].endtime;
				}
				nsamp = ie - is + 1;
				allot(float *,ampwork,nsamp);
				cmplx_vector_mod(nsamp,
				   (ts[l][0][j][k].z)+is,ampwork);
				stats = MW_calc_statistics_float(ampwork,nsamp);
				signal_rms = (float)stats.high;
				/* This assumes nsamp is the same for 
				all wavelets in this band */
				sig3c[k] += (signal_rms*signal_rms);
				free(ampwork);

				/* Now we do almost the same thing with
				the noise segment, but use median amplitudes
				instead of the peak we used in the signal 
				window */
				dt = snr->nstime - tn[l][0][j][k].starttime;
				is = nint(dt/si);
				if(is<0)
				{
					snr->nstime = tn[l][0][j][k].starttime;
					is = 0;
				}
				dt = snr->netime - tn[l][0][j][k].starttime;
				ie = nint(dt/si);
				/* -1 is from the age old fact that C indexing
				starts at 0 */
				if(ie > ((tn[l][0][j][k].nz) - 1) )
				{
					ie = tn[l][0][j][k].nz;
					--ie;
					snr->netime = tn[l][0][j][k].endtime;
				}
				nsamp = ie - is + 1;
				allot(float *,ampwork,nsamp);
				cmplx_vector_mod(nsamp,
				   (tn[l][0][j][k].z)+is,ampwork);
				stats = MW_calc_statistics_float(ampwork,nsamp);
				noise_rms = (float)stats.median;
				/* This assumes nsamp is the same for 
				all wavelets in this band */
				noise3c[k] += (noise_rms*noise_rms);
				noise1c[k] = noise_rms;
				free(ampwork);
				snr_wavelet[k] = signal_rms/noise_rms;
			    }
			    stats=MW_calc_statistics_float(snr_wavelet,nwavelets);
			    noisestats=MW_calc_statistics_float(noise1c,nwavelets);
			    switch(l){
			    case(0):
				snr->ratio_e = stats.median;
				snr->noise_e = noisestats.median;
				snr->min_ratio_e = stats.low;
				snr->max_ratio_e = stats.high;
				break;
			    case(1):
				snr->ratio_n = stats.median;
				snr->noise_n = noisestats.median;
				snr->min_ratio_n = stats.low;
				snr->max_ratio_n = stats.high;
				break;
			    case(2):
				snr->ratio_z = stats.median;
				snr->noise_z = noisestats.median;
				snr->min_ratio_z = stats.low;
				snr->max_ratio_z = stats.high;
				break;
			    }
			}
			for(k=0;k<nwavelets;++k) 
				snr_wavelet[k] = sqrt((double)(sig3c[k]
							/noise3c[k]));
			stats=MW_calc_statistics_float(snr_wavelet,nwavelets);
			noisestats=MW_calc_statistics_float(noise3c,nwavelets);
			snr->ratio_3c = stats.median;
			snr->noise_3c = sqrt((double)(noisestats.median));
			snr->min_ratio_3c = stats.low;
			snr->max_ratio_3c = stats.high;
			}
			setarr(snrarr_vector[j],sta,snr);
		}
	}
	
	free(sig3c);
	free(noise3c);
	free(noise1c);
	return(snrarr_vector);
}
/* This function combines all the terms used to compute moveout 
times relative to a reference station.  This includes everything
including the current estimate of residual statics.

Arguments:
        g = pointer to MWgather object (for a common wavelet bank)
		Should have common values for whole band, so this could
		be the same for all wavelets in the band.
	s = associative array of pointers to MWstation objects 
	refsta = name of reference station 
	u = slowness vector to use to computer dynamic moveout
	refelev = reference elevation relative to datum (units 
		assumed consistent with u)
	phase = name of phase (calls elevation static routine that
		keys on this string to decide to us a P or S phase.
		See that routine for rules.)
	moveout = returned vector of moveout times.  This vector is
		ASSUMED to be preallocated and of length = number of stations
		in the gather.  

Routine takes one safety measure.  If an entry in g is NULL, the
moveout is silently set to 0.  The routine makes one sanity check.
If refsta is not defined in the array s, all moveouts are set to 
0.0, a complain message is issued, and the function returns -1.
Normal return is 0. 
*/
int compute_total_moveout(MWgather *g, 
		Arr *s,
		char *refsta, 
		MWSlowness_vector u,
		double refelev,
		char *phase,
		double *moveout)
{
	int nsta,i;
	MWstation *sref;

	nsta = g->nsta;
	sref = (MWstation *)getarr(s,refsta);
	if(sref == NULL)
	{
		elog_complain(0,"WARNING:  serious error. reference station %s not found in MWstation associative array\nSetting moveout to all 0s\n",
			refsta);
		for(i=0;i<nsta;++i) moveout[i] = 0.0;
		return(-1);
	}
	for(i=0;i<nsta;++i)
	{
		moveout[i] = compute_moveout(g->sta[i],sref,&u);
		moveout[i] += compute_elevation_static(g->sta[i],
					u,refelev,phase);
		moveout[i] += g->sta[i]->plane_wave_static;
		moveout[i] += g->sta[i]->residual_static;
	}
	
	return(0);
}
		
/* This short function returns a parallel vector to moveout giving
time lags from MWtrace start times in samples.  Note it assumes here
that all three components have a common start time and keys from 
contents of the x3 component (Used because tranformations used here
will make this the principal component direction).

Note also the returned vector is allocated here and needs to be
freed later.

Note also it will work correctly for data from something like a 
Quanterra with a nominal, but variable sample rate.  
*/
int *compute_lag_in_samples(MWgather *g,double *moveout, double timeref)
{
	int i,nsta;
	int *lags;

	nsta = g->nsta;

	allot(int *,lags,nsta);
	for(i=0;i<nsta;++i)
	{
		/*work with x3 and assume all the other three components
		have the same sample rate */
		if((g->x3[i]) == NULL)
			lags[i] = -9999999;  /* extra measure of security*/
		lags[i] = nint( (timeref + moveout[i] - (g->x3[i]->starttime) )
				/ (g->x3[i]->dt) );
	}
	return(lags);
}

/* returns coherence estimate for gather *g (using only x3 
component) based on current lag values in lags vector and
using window parameters passed via w.  Intimate companion
to function below.  

type is a switch defined with numeric value defined in 
multiwavelet.h.  Function will compute a coherence measure
analagous to semblance (type=USE_SEMBLANCE) given by
equation 13 of Bear and Pavlis.  
Alternatively, it will compute (type=USE_COHERENCE) a 
measure more similar, but not identical, to normal coherence
based on the singular value spectrum of the matrix.  Specifically
it computes s_max/(sum over min(m,n) s_i)  = fractional part of
signal described by largest principle component.  This is very
similar to coherence which is usually written in terms of 
residual power.  

Normal return is computed coherence measure at specified
lag.  A negative number indicates no data fell in the 
requested window and the value should not be used.
*/
float compute_coherence_measure(MWgather *g,Time_Window w,int *lags, int type)
{
	int i,ii;
	complex *A,*U,*Vt;  /* working matrix */
	float *svalue;
	float sumsv,measure;
	int total_window_length;
	int nsta_used;
	int info;
	complex num;
	double denom;
	int data_end;
	complex cweight={0.0,0.0};
/*
float seisr[1000];
float seisi[1000];
int idbug, jdbug;
*/

	/*This is one of those interval versus points things again */
	total_window_length = ((w.length)-1)*(w.increment);
	total_window_length += 1;

	allot(complex *,A,(g->nsta)*(w.length));
	allot(float *,svalue,MIN(g->nsta,w.length));
	for(i=0,ii=0;i<(g->nsta);++i)
	{
		if((g->x3[i]) == NULL) continue;
		if(lags[i]<0) continue;
		data_end = (g->x3[i]->nz);
		if((lags[i]+total_window_length)<=data_end)
		{
			ccopy(w.length,(g->x3[i]->z)+lags[i],(w.increment),
						A+ii,g->nsta);
			cweight.r = (float)(g->sta[i]->current_weight_base);
#ifdef SUNPERF6
			cscal(w.length,cweight,A+ii,g->nsta);
#else
			cscal(w.length,&cweight,A+ii,g->nsta);
#endif
/*
for(idbug=0,jdbug=0;jdbug<w.length;++jdbug,idbug+=(w.increment))
{
	seisr[idbug]=g->x3[i]->z[lags[i]+idbug].r;
	seisi[idbug]=g->x3[i]->z[lags[i]+idbug].i;
}
*/

			++ii;
		}
	}

	nsta_used = ii;
	if(nsta_used < 2)
	{
		elog_complain(0,"Critical data loss in semblance calculation:  %d stations of %d available were included\nTry increased tpad values\n",
			nsta_used,g->nsta);
		measure = -1.0;
	}
	else
	{
	/* call complex svd routine */
		cgesvd('o','n',nsta_used,w.length,A,g->nsta,
			svalue,U,nsta_used,Vt,nsta_used,&info);
		if(info)elog_complain(0,"cgesvd returned error code %d in computing semblance estimate\n",info);
		for(i=0,num.r=0.0,num.i=0.0,denom=0.0;i<nsta_used;++i)
		{
			num.r += A[i].r;
			num.i += A[i].i;
			denom += (double)( (A[i].r)*(A[i].r)+(A[i].i)*(A[i].i));
		}
		switch(type)
		{
		case (USE_COHERENCE):
			for(i=0,sumsv=0.0;i<MIN(nsta_used,w.length);++i)
							sumsv += svalue[i];
			measure = svalue[0]/sumsv;
			break;
		case (USE_SEMBLANCE):
		default:
		/* This in an intentional default condition and it is 
		not flagged if it is wrong because this routine is 
		low enough level that this would be a bad thing.
		i.e. semblance is the default condition */
			measure = (num.r)*(num.r)+(num.i)*(num.i);
			measure /= ((float)denom);
		}
		
	}
	free(A);
	free(svalue);
	return(measure);
}
/* This function returns the time lag to use for an analysis point.  
Here we choose the point from peak semblance as defined in 
equation (13) of Bear and Pavlis 1999a (p. 685 of BSSA).

The function always keys on the x3 component because the transformation
matrix created here is the largest principal component direction 
independent of the phase being analyzed.  

Arguments:
--inputs---
g - gather object used for computatoin
nwavelets - number of wavelets in transform
timeref - time reference (lag are computed from here based on
	this time at the reference station)
moveout - vector of time moveouts (s)
polarization - defines direction to transform coordinates to 
	Coordinates are rotated and only the x3 component is used
	to compute coherence measure
win - array of time window structures (value for win[band] is used )
coherence_type - coherence or semblance switch (see multiwavelet.h)
band - band index of mw transform
---output----
lagout - output optimal lag in samples (former function return).
peakcm - value of coherence measure at the lagout.

Author:  Gary Pavlis
History:  
This function went through some evolutionary changes that have
left it in less than desirable state.  The original version was a
int function that returned the optimal lag only.  It also had
some nasty bugs that were patched early on.  Now (Dec 2000) I
patched it again to return not only the lag but the actual 
coherence measure at the optimal lag value.  This made it into
a void subroutine like function that is somewhat bad form.  

Probably the most confusing issue here is the LAG_ERROR_RETURN.
This value means the computation failed due to a windowing error.
In that situation the peakcm value is not set and this return
must be trapped as the results are garbage.  
*/
#define LAG_ERROR_RETURN -9999999
void compute_optimal_lag(MWgather **g,int nwavelets,double timeref, 
	double *moveout,Spherical_Coordinate polarization,
	Time_Window *win,int coherence_type, int band,
	int *lagout, double *peakcm)
{
	int i,j;
	double U[9];  /* transformation matrix */
	MWgather **trans_gath;
	int *lags;  /* base lag computed from moveout for each 
			station.  Stored in a parallel array to 
			moveout and elements of MWgather */
	float **semb; /* a window size by nwavelets matrix */
	float *avgsemb;  /* This stores a trace of the average coherence*/
	int lenwin,lensemb; 
	MW_scalar_statistics stats; 
	float peak_semb;
	int lag_at_peak;
	char *array_name;
	int *window_error;


	/* First we define a rotation matrix to ray coordinates and
	then form new gathers rotated into ray coordinates */
	ray_coordinate_trans(polarization,U);
	
	allot(MWgather **,trans_gath,nwavelets);
	for(i=0;i<nwavelets;++i)
	{
		trans_gath[i] = MWgather_transformation(g[i],U);
	}
	/* Compute the base lag in samples for each station */
	lags = compute_lag_in_samples(*trans_gath,moveout,timeref);

	/* We shift the lags vector backward to the specified window
	start time then increment it forward below.  Called routines
	need to be cautious of negative or too long a lag. */
	for(j=0;j<trans_gath[0]->nsta;++j) lags[j] += win->tstart;

	/* I divided this up to use the temporary variable lenwin only
	for clarity.  lenwin = length in samples of the analysis window
	when a increment is other than 1.  The number of semblance 
	estimates computed, lensemb, is then computed by a odd formula
	involving the factor of 2, but it is right.  Note we must
	be careful here to avoid reading garbage at the end of the window */
	lenwin = ((win->length)-1)*(win->increment) + 1;
	lensemb = ((win->tend) - (win->tstart) - lenwin + 2)/(win->stepsize);
	if(lensemb<0) 
	{
		elog_complain(0,"Error in window parameter:  range is less than window length.  Set to 1\n");
		lensemb = 1;
	}
	/* this creates a matrix to hold the actual semblance
	 computed for each wavelet as a function of time.*/
	semb = matrix(0,lensemb,0,nwavelets-1);
	allot(float *,avgsemb,lensemb);
	allot(int *,window_error,lensemb);

	for(i=0;i<lensemb;++i)
	{
		window_error[i] = 0;
		for(j=0;j<nwavelets;++j)
		{
			/* This function returns a negative number 
			for no data in the window.  We mark this problem
			with this array of logical integers */
			semb[i][j] = compute_coherence_measure(trans_gath[j],
					*win,lags,coherence_type);
			if(semb[i][j]< 0.0) window_error[i] = 1;
		}
		for(j=0;j<trans_gath[0]->nsta;++j) lags[j] += win->stepsize;
	}
	/* Note LAG_ERROR_RETURN is the return when the the loop exits
	without ever setting the lag_at_peak value to anything else */
	for(i=0,j=0,peak_semb=0.0,lag_at_peak=LAG_ERROR_RETURN;
				i<lensemb;++i,j+=win->stepsize)
	{
	    if(!window_error[i])
	    {
		stats = MW_calc_statistics_float(semb[i],nwavelets);
		avgsemb[i] = stats.mean;
		if(avgsemb[i]>peak_semb)
		{
			peak_semb = avgsemb[i];
			lag_at_peak = j;
		}
	    }
	}
	free_matrix((char **)semb,0,lensemb,0);
	free(avgsemb);
	free(lags);
	free(window_error);
	for(i=0;i<nwavelets;++i) free_MWgather(trans_gath[i]);

	/* This is not the clearest way to do this, but this is repairing
	a bug in the original version of this code that failed to make
	this correction.  Basically, the lag_at_peak is not referenced
	correctly.  We have to take away the window start increment
	added above to get the timing right */
	if(lag_at_peak > LAG_ERROR_RETURN)
		lag_at_peak += win->tstart;

	*lagout = lag_at_peak;
	*peakcm = peak_semb;
	return;
}

/* This small function builds the complex matrix of trace data
used to compute static phase shifts in this code.  It contains a block
of code similar to that used above in computing semblance, but here
when data from a station can't be used (due to data gaps or any other
error that lead to trace pointers being null) the corresponding row
of the computed matrix is set to zeros.  A count of the actual 
number of valid stations available is returned and can be tested
against original count if desired.  This condition is handled 
silently under an assumption earlier routines that set this error
condition will flag an entry in elog. 

Arguments:
	g - gather with x3 containing component to be used to 
		build this matrix
	lags - vector of g->nsta lag values in samples used
		to define offsets in each trace based on
		current best estimates of arrival time 
	w - signal time window structure defines processing parameters
	A - work space (previously allocated) of length 
		(g->nsta)*(w->length) used to store the working array.
		Storage is in fortran form to allow calling LAPACK svd
		routine.
	weights - works space of length g->nsta.  On exit 
		this vector contains the row weights applied to A.
		It is assumed this vector has been preallocated.
Return:
1.  function returns the count of the number of valid stations 
set in A (!= full nsta if window does not overlap or data was null
to start with)
2.  lag  values of invalid data are set to a large negative number.
This allows a simple test for negative entry to flag null fields.
Note this depends upon stability of numerical algorithm used to handle
A later in the presence of zero rows.  
3.  weights vector entries are set.

Modification:  Major change in algorithm May 27, 2002.  New algorithm
is more forgiving.  Previous was when in doubt throw it out.  Now 
we zero pad missing data where before gaps led to discarding data.
*/
int build_static_matrix(MWgather *g, int *lags, Time_Window *w,
		float *weights, complex *A)
{
	int i,j;
	int nsta_used;
	int data_end,window_length;
	complex cweight={0.0,0.0};
	int i0,window_end,copy_length,ioffset;

	for(i=0,nsta_used=0;i<(g->nsta);++i)
	{
                /* weight and each row is set to 0 initially.  The
                error conditions lead to zero weight and zero row elements
                in the result.  This simplifies indexing downstream*/
		for(j=0;j<(w->length);++j) 
		{
			A[i+j*(g->nsta)].r = 0.0;
			A[i+j*(g->nsta)].i = 0.0;
		}
		weights[i]=0.0;
		if( (g->x3[i]) == NULL) continue;
		/* silently zero pad traces that are truncated
		on either side */
		if(lags[i]<0)
			i0= -lags[i];
		else
			i0=0;
		window_length = ((w->length)-1)*(w->increment)+1;
		data_end = g->x3[i]->nz;
		window_end = i0+window_length;
		if(window_end<=data_end)
                	copy_length=((w->length)-1)*(w->increment)+1;
		else
		{
			copy_length=window_length-(window_end-data_end)-i0;
			copy_length /= w->increment;
		}
		if(copy_length>0)
		{
			/* This is the pointer offset for this trace
			relative to start */
			ioffset=i+i0*g->nsta;
			ccopy(copy_length,(g->x3[i]->z)+lags[i]+i0,(w->increment),
						A+ioffset,g->nsta);
			/* This does row scaling */
			weights[i] = (float)(g->sta[i]->current_weight_base);
			cweight.r = weights[i];
#ifdef SUNPERF6
			cscal(copy_length,cweight,A+ioffset,g->nsta);
#else
			cscal(copy_length,&cweight,A+ioffset,g->nsta);
#endif
			
			++nsta_used;
		}
	}
	return(nsta_used);
}
/* This is a parallel routine to the above that builds a matrix from
the gather g that includes all three components.  The components are
assembled in station order in x,y,z triplets.  Other arguments are
exactly as defined in the preceding function.  The algorithm used
is almost identical.  One minor difference is that when any channel of
a station can't be handled, all data from that station is discarded an
a error is logged.  It also returns a parallel 
*/
int build_3c_matrix(MWgather *g,int *lags,Time_Window *w,
		float *weights, complex *A, char **sta)
{
	int i,ii,j;
	int nsta_used;
	int m;

	int window_length, data_end;
	complex cweight={0.0,0.0};

	m = (g->nsta)*3;
   	window_length = ((w->length)-1)*(w->increment)+1;

	for(i=0,ii=0,nsta_used=0;i<(g->nsta);++i,ii+=3)
	{
		/* We initialize each block to zero and errors will
		then just leave that block of the matrix 0.  This
		makes for inefficiencies if many stations are deleted
		but assures proper indexing and prevents copying garbage*/
		for(j=0;j<(w->length);++j) 
		{
			A[ii+j*m].r = 0.0;
			A[ii+j*m].i = 0.0;
			A[1+ii+j*m].r = 0.0;
			A[1+ii+j*m].i = 0.0;			
			A[2+ii+j*m].r = 0.0;
			A[2+ii+j*m].i = 0.0;	
		}
		weights[ii]=0.0;
		weights[ii+1]=0.0;
		weights[ii+2]=0.0;
		if(lags[i]<0) continue;

		if( ((g->x1[i]) != NULL) 
			&& ((g->x2[i]) != NULL) 
			&& ((g->x3[i]) != NULL))
		{
		    data_end = g->x3[i]->nz;
		    if(lags[i]+window_length<=data_end)
		    {
			ccopy(w->length,(g->x1[i]->z)+lags[i],(w->increment),
						A+ii,m);
			ccopy(w->length,(g->x2[i]->z)+lags[i],(w->increment),
						A+ii+1,m);
			ccopy(w->length,(g->x3[i]->z)+lags[i],(w->increment),
						A+ii+2,m);
			++nsta_used;
			/* sta must be initialized to null pointers or 
			this step will produce random garbage */
			if(sta[i] == NULL) sta[i] = strdup(g->sta[i]->sta);
			/* This does row scaling */
			weights[ii] = (float)(g->sta[i]->current_weight_base);
			weights[ii+1] = weights[ii];
			weights[ii+2] = weights[ii];
			cweight.r = weights[ii];
#ifdef SUNPERF6
			cscal(w->length,cweight,A+ii,m);
			cscal(w->length,cweight,A+ii+1,m);
			cscal(w->length,cweight,A+ii+2,m);
#else
			cscal(w->length,&cweight,A+ii,m);
			cscal(w->length,&cweight,A+ii+1,m);
			cscal(w->length,&cweight,A+ii+2,m);
#endif
		    }
		}
	}
	return(nsta_used);
}
/* This small function converts a phase angle measured in radians to 
an equivalent time.  It applies equation (15) of Bear and Pavlis(1999) 
p. 686.  The form follows after some trivial algebra because f0 is
assumed to be nondimensional with scaling by the Nyquist frequency (dt).
Arguments:
	phi - phase angle in radians
	dt - sample rate is s
	f0 - nondimensional center frequency of wavelet used here 
function returns a time in s derived from phi.

*/
double phase_to_time(double phi,double dt, double f0)
{
	double t;
	/* There is no 2 here because the 2 in omega=2*pi*f cancels with the
	2 in the normalization for f0 to Nyquist */
	t = phi*dt/(M_PI*f0);
	return(t);
}
/* This function takes a complete gather in which it is assumed x1,x2,
and x3 are components in standard coordinates.  It rotates these coordinates
into ray coordinates and computes time shifts relative to moveout times 
using the x3 coordinate only following Bear and Pavlis (1999a) 
BSSA, pp. 681-692.  Algorithm is iterative on the level that a base matrix
is reformed using lags computed from current moveout (including previously
computed time shifts) until the current adjust is less than one sample
at all stations.  Two cautions are taken in this regard.  First, it is
possible that the time lags computed lead to shifting of the analysis
window outside the bounds of the time window originally read from raw
data files.  In this situation a diagnostic is posted to elog and data 
for that station will not be posted as output.  Be warned this is only
implicit as the returned associative arrays contain data only for stations
this does not happen with.  Secondly, there is an iteration limit.  
elog_complain is called if the iteration does not converge, but an 
answer will still be returned.  It just may be bogus.  

This routine also computes amplitude static corrections but does not
apply them.  i.e. it always uses the amplitudes straight up, but 
computes amplitude corrections and amplitude uncertainty estimates
returned via the results_array (see below).  Note the returned amplitudes
are log10

Arguments:
	g - array of MWgather objects (g[i] is gather for ith wavelet)
		It is assumed the traces passed are in standard coordinates
		because internally they are immediately transformed to 
		ray coordinates.  
	timeref,moveout, opt_lag - these three are intimately related.  timeref
		is an epoch time giving absolute time reference.  moveout 
		is a vector of times relative to the timeref value.  It is 
		a vector parallel to the MWgather objects internal trace
		arrays.  The computed arrival time at trace i in gather g
		is timeref + moveout[i].  opt_lag is a dc shift relative to
		this computed time (in samples) derived earlier from 
		semblance analysis (see routine above).  Note this is
		trickier than one might think at first glance because the
		analysis can compute times to the subsample level.
	polarization - This structures defines the direction or particle
		motion used to compute statics.  This program always
		uses only the x3 component after the transformation
		defined by this structure.
	win - Time window structure defining trace window parameters
	arrival - associative array of output predicted arrival times
		(see returns below for more details)
	results_array - second associative array of results (more below).
	avgamp - average rms amplitude estimate for full gather
	amperr - error estimate in avgamp (both are log10 values)
	ampndgf - degrees of freedom in avgamp estimate

The major output of the program is returned in the two associative 
arrays arrival and results_array.  The excess baggage is necessary
because they are used in different ways.  arrival is passed into 
the routine that adjusts the slowness vector and projects the residual
static vector onto an unbiased basis.  It is an array of pointers to 
simple doubles that define these predicted arrival times.  results_array
has indexed pointers to more comprehensive MWstatic structures that
contain error information and amplitude factors.  

It is important to understand that arrival and results_array are 
created or destroyed as necessary by this routine.  That is, if 
the original pointer is null, a new array is created.  If it is not
null, the previous contents are freed (with freearr).  Consequently,
do not pass an arr whose contents need to be preserved because they 
will be destroyed.  

History:
Originally written fall 1999
Modified:  
March 2000
Three major changes:
1.  Added weighting.  Original version ignored weighting features
planned in original design (an error).
2.  The base algorithm for dealing with amplitudes was totally changed.
I learned on working with synthetic data what I has suspected earlier
when Bear was working on this.  That is, individual multiwavelet functions
have an amplitude bias wrt each other.  Much like we can't compare the
absolute phase of one wavelet relative to another we also can't compare
the amplitude.  So, I had to rearrange the algorithm to compute amplitude
statics much like the time statics.  That is, we compute relative amplitudes
only relative to the array median and do so wavelet by wavelet.  
3.  Argument list changed.  Added a return of an absolute amplitude 
estimate and corresponding errors.  
July 2000
Changed from a void function to int to allow an all is lost error return.
This was found necessary to catch a case when all the data in the static
matrix were null.  The calling function should discard the results when
this happens.  
*/
#define DT_MAX_ITERATION 20
/* We terminate the loop when the final adjustment is this fraction
of the sample rate for this band.  We use a number less than 1 because
lag computation uses a nint and any integer adjustment of the lag 
vector will change the result.  We want the iteration to not stop
until the lag vector does not change.  This is simpler than testing
the actual values of the lag vector.*/
#define DT_FRACTION_TERMINATE 0.5
/* This value yields a minimum scale factor for angles of approximately
+ to 1 20 degrees.   The value equals 0.5 radians */
#define MIN_PHI_SCALE 0.50
/* large phase angle jumps are dangerous because of the way this algorithm
works.  computed phase angles larger than this value will be 
truncated to this size.  Note because we try to unwrap the phase the 
largest possible angle is 180 degrees.  Setting this too small can force
no iterations for a wavelet short enough that phi of one sample is
less than this limit.
*/
#define PHASE_ANGLE_LIMIT 2.3561945
/*
int save_aligned_wavelets(MWgather *, int *, char *);
*/

int compute_mw_arrival_times(MWgather **g,int nwavelets,double timeref,
	double *moveout, int opt_lag, Spherical_Coordinate polarization,
	Time_Window *win, Arr **arrival, Arr **results_array,
	double *avgamp, double *amperr, int *ampndgf)
{
	int nsta,nsta_used,nsta_test;
	int i,j,ii;
	double U[9];  /* transformation matrix */
	MWgather **trans_gath;
	double *current_moveout;  /* initially copy of moveout, but changes
					with updates in time shifts */
	int *lags=NULL;  /* base lag computed from moveout for each 
			station.  Stored in a parallel array to 
			moveout and elements of MWgather */
	complex *A,*Usvd,*Vt;  /* Usvd and Vt are not actually accessed at present*/
	float *svalues,*sv1;  /* svalues is a work space, sv1 stores largest
				singular value for each wavelet */
	complex *eigenvectors;  /* Eigenvectors for each wavelet are 
				saved here in FORTRAN order */
	complex *centroid;
	double *ampwork;  /* work space for amplitude factors */
	int info;
	float *work,*r,*phi,*iqr,*iqphi, *dt;
	float *work2;
	float *weights;
	MW_scalar_statistics stats;
	double dtmax;
	int iteration=0;
	double sigma_t;

	/* This is probably not necessary, but better safe than sorry */
	if((*arrival)!=NULL)freearr(*arrival,free);
	if((*results_array)!=NULL) freearr(*results_array,free);
	*arrival = newarr(0);
	*results_array = newarr(0);

	/* First we define a rotation matrix to ray coordinates and
	then form new gathers rotated into ray coordinates */
	ray_coordinate_trans(polarization,U);
	
	allot(MWgather **,trans_gath,nwavelets);
	for(i=0;i<nwavelets;++i)
	{
		trans_gath[i] = MWgather_transformation(g[i],U);
	}
	nsta = trans_gath[0]->nsta;
	/* If the number of stations transformed is too small we
	don't want to proceed.*/
	if(nsta<3)
	{
		elog_complain(0,"compute_mw_arrival: number of stations in the input gather = %d\nAt least 3 stations are required to compute slowness and statics\n",
			nsta);
		for(i=0;i<nwavelets;++i) 
			free_MWgather(trans_gath[i]);
		return(-1);
	}
	allot(double *,current_moveout,nsta);
	allot(complex *,A,nsta*(win->length));
	allot(float *,weights,nsta);
	allot(float *,svalues,MIN(nsta,win->length));
	allot(complex *,eigenvectors,(nsta)*nwavelets);
	allot(double *,ampwork,nsta*nwavelets);
	allot(float *,sv1,nwavelets);
	allot(complex *,centroid,nwavelets);
	allot(float *,work,nwavelets);
	allot(float *,r,nsta);
	allot(float *,phi,nsta);
	allot(float *,iqr,nsta);
	allot(float *,iqphi,nsta);
	allot(float *,dt,nsta);
	allot(float *,work2,nsta);
	for(i=0;i<nsta;++i) current_moveout[i] = moveout[i];
	do {
	    /* We get lags from moveout and then add the optimal lag offset.
	    We free it here because the compute_lag function allocs this
	    vector every pass, and because it flags bad rows with negative
	    numbers we need that capability after the main loop exits. */
	    if(lags != NULL) free(lags);
	    lags = compute_lag_in_samples(*trans_gath,current_moveout,timeref);
	    for(i=0;i<nsta;++i) lags[i] += opt_lag;
/*
elog_log(0,"saving initial wavelets in w0_start.dat\n");
if(save_aligned_wavelets(trans_gath[0],lags,"w0_start.dat"))
  elog_complain(0,"save_aligned_wavelets failed\n");
*/
	    for(i=0;i<nwavelets;++i)
	    {
		nsta_used = build_static_matrix(trans_gath[i],lags,win,
					weights,A);
		if(nsta_used != nsta) 
		{
			elog_log(0,"compute_mw_arrival_times:  Computing statics for only %d of %d stations\n",
				nsta_used,nsta);
			if(nsta_used<3)
			{
				elog_complain(0,"Unrecoverable error in mw_arrival_compute:  only %d stations have nonzero entries in principle component matrix\n",
					nsta_used);
				free(current_moveout);
				free(lags);
				free(A);
				free(svalues);
				free(weights);
				free(eigenvectors);
				free(ampwork);
				free(sv1);
				free(centroid);
				free(work);
				free(work2);
				free(r);  
				free(phi);	
				free(iqr);	
				free(iqphi);
				free(dt);
				for(i=0;i<nwavelets;++i) 
					free_MWgather(trans_gath[i]);

				return(-1);
			}
		}
			
		/* Note that we use nsta instead of nsta_used for most of
		this because the corresponding rows of A are zero.  We only
		have to deal with deletions for statistical analysis and 
		to be sure we aren't scrambling things */
		cgesvd('o','n',nsta,win->length,A,nsta,
			svalues,Usvd,nsta,Vt,nsta,&info);
		if(info)elog_complain(0,"cgesvd returned error code %d in computing static estimate for wavelet %d\n",info,i);
		/* eigenvectors are stored in first column of A, so we just
		copy them to eigenvector work space.  This works because
		eigenvectors are sorted by singular values with largest 
		svalue being column 1 */
		ccopy(nsta,A,1,eigenvectors+i*nsta,1);
		sv1[i] = svalues[0];
	    }
	/* we compute the center of the cloud of points in the complex
	plane that define phase shifts and relative amplitudes for
	each wavelet using an m-estimator.  This has to be done 
	independently for each wavelet because we never could figure out
	how to normalize the relative phase between wavelets.  
	We use A as a work space here.  This isn't necessary for 
	current version (Sept. 1999) which does not alter A, but
	because alternative algorithms might and the cost is low, 
	I'll do it this way.*/
	    for(i=0;i<nwavelets;++i)
	    {
		ccopy(nsta,eigenvectors+i*nsta,1,A,1);
		/* We have to remove null values defined by 0 weights */
		if(nsta != nsta_used) 
		{
			nsta_test = remove_null_complex(nsta,weights,1,A,1);
			if(nsta_test != nsta_used)
			{
				elog_complain(0,"Static matrix weight inconsistency:  expected %d nonzero weight stations but found %d\nReset to %d\n",
					nsta_used, nsta_test,nsta_test);
				nsta_used = nsta_test;
			}
		}
		centroid[i] = M_estimator_complex(A,nsta_used);
	    }		

	    for(j=0;j<nsta;++j)
	    {
		/* Null answer for no data uses test against lags 
		array which sets lags value for problem rows negative 
		or a zero weight.  I use an unsafe test of weights
		against zero because I know it can't have been altered
		from 0.0 if it was set to zero  */
		if((lags[j]<0) || (weights[j] <= 0.0))
		{
			phi[j] = 0.0;
			r[j] = 0.0;
			iqphi[j] = 2.0*M_PI;
			iqr[j] = MAXFLOAT;
		}
		else
		{
			for(i=0;i<nwavelets;++i) 
				work[i] = unwrap_delta_phase(
					eigenvectors[j+i*nsta],centroid[i]);
			phi[j] = M_estimator_float(work,nwavelets,
					IQ_SCALE_ABSOLUTE,MIN_PHI_SCALE);
			/* Now here I do assume work was not altered by the
			previous routine.  This is not always the correct
			way to do this, as one should compute the iqs from
			weighted residuals, but it won't matter unless the
			data are seriously messed up.*/
			stats = MW_calc_statistics_float(work,nwavelets);
			iqphi[j] = (stats.q3_4) - (stats.q1_4);
			/* Now we deal with amplitudes */
			for(i=0;i<nwavelets;++i)
			{				
			 	work[i] = (float) hypot(
				    (double)(eigenvectors[j+i*nsta].r),
				    (double)(eigenvectors[j+i*nsta].i));
				/* This converts to a relative amplitude
				by removing the modulus of the centroid
				for this wavelet.  (Amplitudes like phase
				are biased between wavelets)*/
				work[i] /= (float)hypot(
						(double)centroid[i].r,
						(double)centroid[i].i);
				/* correct for weighting */
				work[i] /= weights[j];
				/* Scale by singular value to get back
				to original physical units instead of 
				orthogonal matrix scaled fraction */
				work[i] *= sv1[i];
				work[i] = (float)log10((double)work[i]);
			}
			/* IMPORTANT:  r and the computed interquartiles
			are left in log space here due to the log normal
			assumption.  The 0.1 sets the minimum relative
			scale at a somewhat arbitrary value that is
			fairly generous. */
			r[j] = M_estimator_float(work,nwavelets,
				IQ_SCALE_RELATIVE,0.01);
			stats = MW_calc_statistics_float(work,nwavelets);
			iqr[j] = (stats.q3_4) - (stats.q1_4);
		}
	    }
	    for(i=0;i<nsta;++i) 
	    {
		if(fabs((double)phi[i])>PHASE_ANGLE_LIMIT)
			phi[i] = (float)copysign(PHASE_ANGLE_LIMIT,
						(double)phi[i]);
		dt[i] = (float)phase_to_time((double)phi[i],
					trans_gath[0]->x3[0]->dt,trans_gath[0]->x3[0]->basis->f0);
	    }
	/* Because the dt is indeterminate, it is useful to 
	remove the median dt to keep things from jumping all over.
	Somewhat repetitious with how this is handled in the 
	slowness vector calculation, but better safe than sorry.
	The cost is low*/
	    scopy(nsta,dt,1,work2,1);
	    nsta_test = remove_null_float(nsta,weights,1,work2,1);
	    stats = MW_calc_statistics_float(work2,nsta_test);
	    /* hard coded constant here is ok because weights are
		always between 0 and 1.  This is a safe test for zero */
	    for(i=0;i<nsta;++i) 
		if(weights[i]>0.0001) 
		{
			dt[i] -= (stats.median);
			if(lags[i]>=0) current_moveout[i] += dt[i];
			
		}
	    /* We use the full dt here rather than work2 because we force
		zero weight stations to have zero dt */
	    dtmax = dt[isamax(nsta,dt,1)];
	    ++iteration;
	} while ((fabs(dtmax) > (DT_FRACTION_TERMINATE*(trans_gath[0]->x3[0]->dt))) 
			&& (iteration<DT_MAX_ITERATION));
	if(iteration >= DT_MAX_ITERATION) elog_complain(0,
	  "WARNING:  static shift computation did not converge\nLarge errors in time estimates are likely\n");
	else
		elog_notify(0,"Static calculation converged in %d iterations\n",
			iteration);
/*
elog_log(0,"saving corrected wavelets in w0_final.dat\n");
if(save_aligned_wavelets(trans_gath[0],lags,"w0_final.dat"))
  elog_complain(0,"save_aligned_wavelets failed\n");
*/

	/* We have to compute and remove the mean value from the amplitude
	values.  We have irq values that measure uncertainty of amplitude
	at each station, but r at this point has a large dc offset that
	has to be removed.  We use the multiwavelet averages 
	and assume a log normal distribution and the central limit theorem.
	Maybe we should have used an m estimator here, but generally the
	degrees of freedom should be high enough that the median will be
	sufficient*/

	scopy(nsta,r,1,work2,1);
	nsta_test = remove_null_float(nsta,weights,1,work2,1);
	stats = MW_calc_statistics_float(work2,nsta_test);
	*avgamp = stats.median;
	*amperr = ((stats.q3_4) - (stats.q1_4))*NORMAL_IQSCALE;
	*ampndgf = nsta_test - 1;
	
	/*here we load up the associative arrays that contain the results*/
	for(i=0;i<nsta;++i)
	{
		double *atime;
		MWstatic *mws;
		if((lags[i]>=0) && (weights[i]>=0.0))
		{
			allot(double *,atime,1);
			*atime = timeref + current_moveout[i];
			setarr(*arrival,trans_gath[0]->sta[i]->sta,atime);
			allot(MWstatic *,mws,1);
			mws->dt_final = dt[i];
			mws->t_raw = current_moveout[i] - moveout[i] 
				+ trans_gath[0]->sta[i]->residual_static;
			/* r is already in log space */
			mws->log10amp = r[i] - (*avgamp);
			/* We kept interquartiles as a phase angle so
			we need to convert it to a time */
			sigma_t = phase_to_time((double)iqphi[i],
				trans_gath[0]->x3[0]->dt,
				trans_gath[0]->x3[0]->basis->f0);
			mws->sigma_t =sigma_t*NORMAL_IQSCALE;
			mws->sigma_log10amp = iqr[i]*NORMAL_IQSCALE;
			mws->ndgf = nwavelets - 1;
			setarr(*results_array,trans_gath[0]->sta[i]->sta,mws);
		}
	}
	/* release all these work spacees */

	free(current_moveout);
	free(lags);
	free(A);
	free(svalues);
	free(weights);
	free(eigenvectors);
	free(ampwork);
	free(sv1);
	free(centroid);
	free(work);
	free(work2);
	free(r);  free(phi);	free(iqr);	free(iqphi);
	free(dt);
	for(i=0;i<nwavelets;++i) free_MWgather(trans_gath[i]);
	return(0);
}
/* small companion to below just sets structure to zeros.  The 
important one is that rectilinearity is set negative.  This can
be used to signal a null value.  
 */
void set_pm_null(Particle_Motion_Ellipse *e)
{
	int i;
	for(i=0;i<3;++i)
	{
		e->major[i] = 0.0;
		e->minor[i] = 0.0;
	}
	e->rectilinearity = -1.0;
}

/* This function does multiwavelet particle motion analysis on 
a gather of three-component seismograms in the complicated array 
of structures g.  The averaging used differs slightly from that
in Bear and Pavlis (1999).  The individual station particle 
motions are the same being computed from multiwavelet averages
of nwavelet estimates.  The array average is different.  In the 
1999 paper we used a dual averaging scheme to compute the
array average.  I saw little reasons for this extra complexity 
and instead compute the array average from all nwavelet*nsta
estimates.  

arguments:
	g - vector of gather structures, one for each wavelet.  
	nwavelets - number of wavelets = length of g
	timeref - absolute time reference
	moveout - vector of length = number of stations in the gather 
		of moveout times relative to the reference time.  
	int lag - lag (in samples) of first point in analysis window
		(analysis window for a station starts at time 
		timeref + moveout + lag (converted to time by dt) )
	Time_window *win - analysis time window definition.
	up - three vector (of doubles) used to defined positive 
		direction of particle motion ellipse.  For 
		P waves should be {0.0, 0.0, 1.0}   For S, 
		a more complex recipe is in order.
	avgpm - array average particle motion ellipse obtained by
		dual averaging scheme described in Bear and Pavlis 1999b.
	avgerr - estimated errors in avgpm.
	pmsta - associative array of pointers to Particle_Motion_Ellipse
		structures keyed by station names.  
	errsta - comparable associative array to pmsta holding pointers
		to error estimates -- keyed by sta like pmsta.

Note:  moveout is altered by this function 
*/
void compute_mw_particle_motion(MWgather **g,int nwavelets,double timeref,
	double *moveout, int lag, Time_Window *win, double *up,
	Particle_Motion_Ellipse *avgpm, Particle_Motion_Error *avgerr,
	Arr **pmsta, Arr **errsta) 
{
	int nsta,nsta_used,nrows,info;
	int i,j,ii,jj;
	MWgather **trans_gath;
	int *alllags;  /* base lag computed from moveout for each 
			station.  Stored in a parallel array to 
			moveout and elements of MWgather */
	complex *A,*U,*Vt;  /* U and Vt are not actually accessed at present*/
	float *svalues;  /* singular value work vector */
	float *weights;  /* Holds row of weights */
	Particle_Motion_Ellipse *ematrix;  /* this is set to be a 
				matrix of particle motion ellipse structures.
				These are indexed like the fortran type
				arrays A, eigenvectors, etc.  That is, 
				they really only have one subscript, but
				these index like fortran arrays to 
				yield pointers to particle motion structs */
	Particle_Motion_Ellipse *pmwork;  /* work space of pm pointers*/
	char **sta;  /* vector of character strings parallel to ematrix.  
			array.  NULL pointers define empty rows of ematrix */

	/* This is probably not necessary, but better safe than sorry */
	if((*pmsta)!= NULL) freearr(*pmsta,free);
	*pmsta = newarr(0);
	if((*errsta)!=NULL) freearr(*errsta,free);
	*errsta = newarr(0);

	nsta = g[0]->nsta;
	nrows = 3*nsta;

	allot(complex *,A,nrows*(win->length));
	allot(float *,svalues,MIN(nrows,win->length));
	allot(float *,weights,nrows);
	allot(Particle_Motion_Ellipse *,ematrix,nsta*nwavelets); 
	allot(Particle_Motion_Ellipse *,pmwork,nsta*nwavelets); 
	allot(char **,sta,nsta); 
	for(i=0;i<nsta;++i) sta[i] = NULL;
	/* compute_lag_in_samples allocates alllags,  This is bad form, but
	convenient for this code */
	alllags = compute_lag_in_samples(*g,moveout,timeref);
	for(i=0;i<nsta;++i) alllags[i] += lag;

	for(i=0;i<nwavelets;++i)
	{
		nsta_used = build_3c_matrix(g[i],alllags,win,weights,A,sta);
		if(nsta_used != nsta) 
			elog_log(0,"compute_mw_particle_motion:  Data problems, computing particle motions for only %d of %d stations\n",
				nsta_used,nsta);
		cgesvd('o','n',nrows,win->length,A,nrows,
			svalues,U,nrows,Vt,nrows,&info);
		if(info)elog_complain(0,"cgesvd returned error code %d in computing polarization estimate for wavelet %d\n",info,i);
		for(j=0,jj=0;j<nsta;++j,jj+=3)
		{
			ii = j + i*nsta;
			if((sta[j] == NULL) || (weights[jj]<0.001) )
			{
				set_pm_null(ematrix+ii);
			}
			else
			{
				ematrix[ii] = compute_particle_motion(A[jj],
						A[jj+1],A[jj+2],up);
			}
		}
	}
	/* Now we produce particle motion estimates for each station
	computed by averaging multiwavelet estimates of ellipses for
	each station */
	for(j=0;j<nsta;++j)
	{
		Particle_Motion_Ellipse *pm;
		Particle_Motion_Error *pme;
		if((sta[j]!=NULL) && (weights[3*j]>0.0001))
		{
			pmvector_copy(nwavelets,ematrix+j,nsta,pmwork,1);
			allot(Particle_Motion_Ellipse *,pm,1);
			allot(Particle_Motion_Error *,pme,1);
			pmvector_average(pmwork,nwavelets,pm,pme);
			setarr(*pmsta,sta[j],pm);
			setarr(*errsta,sta[j],pme);
		}
	}

	/* Now we compute array average.  First we copy ematrix to
	the pmwork work space removing all the null entries.  Then
	we call the averaging routine to compute a global average 
	of all particle motion estimates */
	for(i=0,ii=0;i<nwavelets*nsta;++i)
	{
		/* This can fail if any elements of ematrix are not
		initialized with rectilinearity = -1 used to signal
		a null */
		if(ematrix[i].rectilinearity > 0.0)
		{
			pmwork[ii] = ematrix[i];
			++ii;
		}
	}
	pmvector_average(pmwork,ii,avgpm,avgerr);

	for(j=0;j<nsta;++j) if(sta[j] != NULL) free(sta[j]);
	free(sta);
	free(A);
	free(svalues);
	free(alllags);
	free(ematrix);
	free(pmwork);
	free(weights);
}
