#include <stdio.h>
#include "multiwavelet.h"
#include "tr.h"



/* This is a group of functions for mwap that do various manipulations 
using the datascope/antelope trace library.  

Author:  Gary Pavlis
*/

/* This is a companion function to the higher level read function
mwap_loaddata below.  It reads data from one station defined
by the dbbundle pointer that is assumed to be a "bundle" 
pointer defining a range of database rows in a view formed
by a join event->origin->assoc->arrival->wfdisc->sitechan.  
(This is done in the main function in mwap).  It then does
a less than general load of parameters from the db and puts
them in the tr object database (tr).  It is less than
general because only entries I know are needed for mwap are
loaded.  I resorted to this because the trace library as
of this date failed with trload_cssgrp, which should be 
functionally similar, but more specialized.  

s and e are start and end epoch times (respectively) requested.
glp:  Dec 1999 */

int mwap_load_stagrp(Dbptr dbbundle, Dbptr tr, double s, double e)
{
	int nsamp;
	double time, endtime,samprate,calib;
	double hang,vang,edepth;
	char sta[8],chan[10];

	char net[4]="MW";   /* This is a required key on trace table so 
				we just set it to a value */

	int is,ie;
	float *data;
	int datasz;
	double t0,t1;
	int ierr;
	int error_count=0;

	dbget_range(dbbundle,&is,&ie);

	for(dbbundle.record=is;dbbundle.record<ie;++dbbundle.record)
	{
		/* This is necessary to force trgetwf to malloc space
		for each trace.  Without this it will recycle the 
		previous buffer*/
		data=NULL;
		datasz=0;
		ierr = trgetwf(dbbundle,0,&data,&datasz,s,e,&t0,&t1,&nsamp,0,0);
		if(ierr)
		{
			elog_notify(0,"trgetwf read error\n");
			++error_count;
			continue;
		}
		/* Note I intentionally ignore if s and e do not match 
		t0 and t1.  This is because I assume this routine is 
		called multiple times and trglue is used later to patch
		multiple pieces together */
		ierr = dbgetv(dbbundle,0,"sta",sta,
				"wfdisc.chan",chan,
				"samprate",&samprate,
				"calib", &calib,
				"hang",&hang,
				"vang",&vang,
				"edepth",&edepth,
				0);
		if(ierr)  
		{
		   elog_notify(0,
		  "dbgetv error reading %s:%s at %s\nContinuing, but additional problems likely\n",
			sta,chan,strtime(t0));
		    ++error_count;
		}

		ierr = dbaddv(tr,"trace",
				"net",net,
				"sta",sta,
				"chan",chan,
				"time",t0,
				"endtime",ENDTIME(t0,samprate,nsamp),
				"nsamp",nsamp,
				"samprate",samprate,
				"calib",calib,
				"data",data,
				"hang",hang,
				"vang",vang,
				0);
		if(ierr<0)
		{
			elog_notify(0,"Error appending to trace table for %s:%s at %s\nProbably data loss and associated memory leak\n",
				sta,chan,strtime(t0));
			++error_count;
		}
	}
	return(error_count);
}

/*this is the basic routine that reads an event gather defined by
the dbbundle input database pointer.  It creates a new trace database
and fills it with traces it reads.  Reading is done by running a higher
level grouping by station.  We then look up the arrival that corresponds
to that station and read a time window relative to the arrival time.
Because of this it is IMPORTANT to recognize this algorith ONLY works
if we are sure there is an arrival recorded for each station.  In mwap
this is assured because we do a series of dbjoins that guarantee this.
If this routine is recycle, beware of this assumption.  When this 
happens the routine simply calls elog_complain and skips the data for
that station.

Arguments:
	dbbundle - dbbundle pointer defining the full gather of traces
		for event to be processed.  (dbget_range is called 
		near the top against this bundle pointer)
	arrivals - associative array of arrival times extracted 
		previously from dbbundle and indexed by station name.
	swin, nwin - Signal and noise window defined by times relative
		to arrival time.  

Normal return is an intact db pointer to the trace database.
If the dbgroup operation fails, db.record is set to dbINVALID,
no data are read, and the bad tr pointer is returned.  This condition
needs to be trapped by calling program.  

Author:  Gary Pavlis
Written:  May 1999
*/

Dbptr mwap_readdata(Dbptr db, Arr *arrivals,
	Time_Window swin, Time_Window nwin)
{

	int i;
	double wstart, wend;  /* relative window positions */

	Dbptr tr;  /* trace data base pointer */
	Dbptr dbwf; 
	char ss[30];
	int nsta;  /* count of number of stations from subset group */
	char *sta;  /* holds station name */
	Tbl *t;  /* keysarr tbl of arrivals */

	/* this generality is probably overkill.  What it does, though,
	is cause the program to read the full trace from the start of
	earliest to latest start time. Normally that would be the
	noise window start time to signal end time, but this allows
	it to be anything.  */
	wstart = MIN((swin.si)*((double)(swin.tstart)),
			(nwin.si)*((double)(nwin.tstart)));
	wend = MAX((swin.si)*((double)(swin.tend)),
			(nwin.si)*((double)(nwin.tend)));

	tr = trnew(NULL,NULL);
	if(tr.record == dbINVALID)
	{
		elog_complain(0,"trnew failure\n");
		return(tr);
	}

	t = keysarr(arrivals);
	nsta = maxtbl(t);
	for(i=0;i<nsta;++i)
	{

		double *atime,stime,etime;
		Dbptr dbbundle;
		int ierr;

		/* Because the view dbwf has been grouped by 
		sta, we can get the bundle pointer by 
		this simple lookup trick */
		sta = gettbl(t,i);
		dbwf = dblookup(db,0,STABDLNAME,"sta",sta);
		if(dbwf.record == dbINVALID)
		{
			elog_complain(0,"No data wfdisc relations for tabulated arrival at station %s\n",sta);
			continue;
		}
		dbgetv(dbwf,0,"bundle",&dbbundle,0);

		atime = (double *)getarr(arrivals, sta);
		if(atime == NULL) 
		{
			elog_complain(0,"Cannot find arrival time for station %s  but trace data exists for this station\nData will be skipped\n",
				sta);
			continue;
		}

		stime = (*atime)+wstart;
		etime = (*atime)+wend;
		ierr = mwap_load_stagrp(dbbundle,tr,stime,etime);
		if(ierr)
		{
			elog_complain(0,"mwap_load_stagrp had %d errors reading data for station %s\n",
				ierr,sta);
			continue;
		}
	}
	return(tr);
}
/* This routine scans a trace database calling trfree on any record
that has a channel code that is not that used in mwap to define
traces mapped to standard, cardinal directions (i.e. the newchan
codes used in rotate_to_standard).  The only argument is the db 
pointer to the tr database.  The algorithm simply scans the 
trace table freeing any row that doesn't match the cardinal
direction pattern.  dbcrunch is called at the end before
returning.  The routine always returns 0.

Author:  Gary Pavlis
*/

int free_noncardinal_traces(Dbptr tr)
{
	Dbptr db;
	int nrec;
	char chan[20];
	char *cardinal[3]={ EW, NS, VERTICAL };
	int i,found;

	db = dblookup(tr,0,"trace",0,0);
	dbquery(db,dbRECORD_COUNT,&nrec);

	for(db.record=0;db.record<nrec;++db.record)
	{
		dbgetv(db,0,"chan",chan,0);
		for(found=0,i=0;i<3;++i)
		{
			if(!strcmp(chan,cardinal[i])) 
			{
				found=1;
				break;
			}
		}
		if(!found) trfree(db);
	}
	dbcrunch(db);
	return(0);
}
/* Companion to function immediately below.  See it's description
where symbols are defined */

int MWtrace_to_float(MWtrace *x,int lag,char part, 
	float **d, double *stime, int *nsamp)
{
	int i;

	if(lag>= (x->nz) )return(-1);
	*stime = (x->starttime)+((double)lag)*(x->dt);
	*nsamp = (x->nz) - lag;
	allot(float *,*d,*nsamp);
	if(part == 'i')
		for(i=0;i<(*nsamp);++i) 
		{
			(*d)[i]=x->z[i].i;
		}
	else
		for(i=0;i<(*nsamp);++i) 
		{
			(*d)[i]=x->z[i].r;
		}

	return(0);
}				
	
	
/* This routine takes a MWgather object and writes trace
data into the tr space.  

Arguments:
	g - MWgather object to be placed in tr
	tr - trace database. It is ASSUMED this has already 
		been created and is valid.  We just do a trace
		table lookup.
	wavelet - index number of this wavelet
	band - index number of band.
	lags - vector of length = number of stations giving lags from beginning
		of trace to use as start time.  (see below for details).

The data are placed in the trace table with the station name
extraced from the gather object and the channel produced by
a character string that should remain unique for each trace
produced.  The string has the form:

x[123]wwbb[ri]

where 
   1,2,3 = coordinate number (generated internally)
   ww = wavelet number
   bb = band number
   r = real, i = imaginary part

Example:  x20102r is x2 component seismogram multiwavelet transform
		for wavelet 1, band 2, and showing the real part.

lags is a vector of integer offsets used to align traces produced
as output.  These should be those computed in mwap using the 
compute_lag_in_samples function.  If the lags pointer is NULL
(i.e. called with a 0) start times are used directly.  

History:  Written for debugging mwap, but may be retained in final
version as an interactive option. 
Written:  January 2000 

*/
void MWgather_to_trace(MWgather *g,Dbptr tr, int wavelet, int band,
	int *lags)
{
	int i;
	char *sta;
	char chan[10];
	float *dr, *di;   /* created spaces for real and imag respective*/ 
	double samprate,stime,etime,calib=1.0;
	int nsamp;
	char net[4]="MW";  /* probably should be consistent with above, but
				for initial application should not matter*/
	char datatype[4]="t4";
	int lag_to_use;

	if((g->ncomponents) != 3) 
	{
		elog_complain(0,"MWgather_to_trace: number components in gather = %s -- Must be 3\n",
				g->ncomponents);
		return;
	}
	tr = dblookup(tr,0,"trace",0,0);
	for(i=0;i<(g->nsta);++i)
	{
		if( ((g->x1[i])!= NULL) && ((g->x2[i])!= NULL)
			&& ((g->x3[i])!= NULL) )
		{
			sta = strdup(g->sta[i]->sta);
			/* this next line assumes all data in this
			gather have a common sample rate */
			samprate = 1.0/(g->x1[i]->dt);
			if(lags == NULL)
				lag_to_use = 0;
			else
				lag_to_use = lags[i];

			/* the block of code that follows is repetitious,
			but I chose to not mess with a function and
			just copy/edit the block of code.
			Note we skip all results for this station
			if the lag is long enough to flow off
			the end of the data.  Later equivalent 
			branches are redundant, but better safe
			than sorry.*/

			if(MWtrace_to_float(g->x1[i],lag_to_use,'r',
					&dr,&stime,&nsamp)) continue;
			sprintf(chan,"x1%1.1d%1.1dr",wavelet,band);
			if( dbaddv(tr,"trace",
				"net",net,
				"sta",sta,
				"chan",chan,
				"time",stime,
				"endtime",ENDTIME(stime,samprate,nsamp),
				"nsamp",nsamp,
				"samprate",samprate,
				"calib",calib,
				"datatype",datatype,
				"data",dr,
				0) < 0) 
			{
				elog_complain(0,"dbaddv error in MWgather_to_trace %s:%s\n",
					sta,chan);
			}

			if(MWtrace_to_float(g->x1[i],lag_to_use,'i',
					&di,&stime,&nsamp)) continue;
			sprintf(chan,"x1%1.1d%1.1di",wavelet,band);
			if( dbaddv(tr,"trace",
				"net",net,
				"sta",sta,
				"chan",chan,
				"time",stime,
				"endtime",ENDTIME(stime,samprate,nsamp),
				"nsamp",nsamp,
				"samprate",samprate,
				"calib",calib,
				"datatype",datatype,
				"data",di,
				0) < 0) 
			{
				elog_complain(0,"dbaddv error in MWgather_to_trace %s:%s\n",
					sta,chan);
			}
			/* repeat for x2 */
			if(MWtrace_to_float(g->x2[i],lag_to_use,'r',
					&dr,&stime,&nsamp)) continue;
			sprintf(chan,"x2%1.1d%1.1dr",wavelet,band);
			if( dbaddv(tr,"trace",
				"net",net,
				"sta",sta,
				"chan",chan,
				"time",stime,
				"endtime",ENDTIME(stime,samprate,nsamp),
				"nsamp",nsamp,
				"samprate",samprate,
				"calib",calib,
				"datatype",datatype,
				"data",dr,
				0) < 0) 
			{
				elog_complain(0,"dbaddv error in MWgather_to_trace %s:%s\n",
					sta,chan);
			}

			if(MWtrace_to_float(g->x2[i],lag_to_use,'i',
					&di,&stime,&nsamp))continue;
			sprintf(chan,"x2%1.1d%1.1di",wavelet,band);
			if( dbaddv(tr,"trace",
				"net",net,
				"sta",sta,
				"chan",chan,
				"time",stime,
				"endtime",ENDTIME(stime,samprate,nsamp),
				"nsamp",nsamp,
				"samprate",samprate,
				"calib",calib,
				"datatype",datatype,
				"data",di,
				0) < 0) 
			{
				elog_complain(0,"dbaddv error in MWgather_to_trace %s:%s\n",
					sta,chan);
			}
			
			/* repeat for x3 */
			if(MWtrace_to_float(g->x1[i],lag_to_use,'r',
					&dr,&stime,&nsamp)) continue;
			sprintf(chan,"x3%1.1d%1.1dr",wavelet,band);
			if( dbaddv(tr,"trace",
				"net",net,
				"sta",sta,
				"chan",chan,
				"time",stime,
				"endtime",ENDTIME(stime,samprate,nsamp),
				"nsamp",nsamp,
				"samprate",samprate,
				"calib",calib,
				"datatype",datatype,
				"data",dr,
				0) < 0) 
			{
				elog_complain(0,"dbaddv error in MWgather_to_trace %s:%s\n",
					sta,chan);
			}

			if(MWtrace_to_float(g->x3[i],lag_to_use,'i',
					&di,&stime,&nsamp)) continue;
			sprintf(chan,"x3%1.1d%1.1di",wavelet,band);
			if( dbaddv(tr,"trace",
				"net",net,
				"sta",sta,
				"chan",chan,
				"time",stime,
				"endtime",ENDTIME(stime,samprate,nsamp),
				"nsamp",nsamp,
				"samprate",samprate,
				"calib",calib,
				"datatype",datatype,
				"data",di,
				0) < 0) 
			{
				elog_complain(0,"dbaddv error in MWgather_to_trace %s:%s\n",
					sta,chan);
			}
		}
	}
	free(sta);
}
/* Resets start times of traces produced by the MWgather_to_trace
function immediately above.  As such it is intimately linked to it
and changes to the above can easily cause this function to go
haywire.  The idea of this function is to effectively produce a 
group of traces that are corrected for the current moveout.  Because
MWgather_to_trace actually uses a vector of lags to determine a 
start time this involves only a resetting of the start times for 
each trace to a common value.  The only trick is making the time 
of this common start time something sensible and that followup 
routines can lay down quantities with a time stamp that aren't 
totally irrational.  

Arguments:
	g - MWgather object used to build trdb 
	trdb - trace object database pointer
	timeref - global reference epoch time.  This is used only
		if the reference station is not found in which 
		case this value is used as the start time.  
Author:  Gary Pavlis
Written:  May 2000, found graphics were a useful option to judge data
quality and progress of program. 
*/
void MWtrace_gather_reset_stime(MWgather *g,Dbptr trdb,double timeref)
{
	char *refsta;
	int i;
	double stime_at_refsta;
	int nrec;
	int nsamp;
	double samprate;

	/* Scan the gather object to determine the reference station.
	This avoids a need to pass it down a chain of function calls */
	for(i=0;i<g->nsta;++i)
		if( !strcmp(g->sta[i]->sta,g->sta[i]->refsta))break;
	refsta = g->sta[i]->refsta;

	/* We blindly assume all traces listed under refsta have a 
	common start time.  This normally a very bad assumption but
	it is appropriate as long as this function is only used in
	combination with the MWgather_to_trace function above. 
	The dblookup here only works right when that is the case */
	trdb = dblookup(trdb,0,"trace","sta",refsta);
	if(dbgetv(trdb,0,"time",&stime_at_refsta,0) == dbINVALID)
	{
		elog_complain(0,"MWtrace_gather_reset_stime:  cannot find reference station %s in trace database\nUsing global time reference\n",
			refsta);
		stime_at_refsta = timeref;
	}
	
	trdb = dblookup(trdb,0,"trace",0,0);
	dbquery(trdb,dbRECORD_COUNT,&nrec);

	for(trdb.record=0;trdb.record<nrec;++trdb.record)
	{
		dbgetv(trdb,0,"nsamp",&nsamp,"samprate",&samprate,0);
		dbputv(trdb,0,"time",stime_at_refsta,
			"endtime",ENDTIME(stime_at_refsta,samprate,nsamp),0);
	}
}
/* This short function is also intimately linked with the two previous
ones.  It adds arrival flags with predefined names to each station:chan in
a trace table.  It does so in a stupid way that works because of the way
the previous two function work.  As such, the point is this function should
ONLY be called after the two previous (i.e. this is the third of a sequence
of functions).  Because the two previous functions build fix length 
traces with moveout corrected times, the "arrival" we define here are 
all relative times in samples. 

Arguments:
	g - parent gather from which the trdb was created
	trdb - trace database produced by previous call to MWgather_to_trace
		subsequently modified by MWtrace_gather_reset_stime
	win - time window structure defining the analysis window for this
		wavelet band
	lag_at_peak - lag in samples of optimal coherence stack point.

Normal return is a count of the number of entries added to the arrival
table.

Author:  Gary Pavlis
Written:  May 2000, found graphics were a useful option to judge data
quality and progress of program. 
*/ 

/* These define fake phase names used to mark traces */
#define ANAL_STIME "AST"
#define ANAL_ETIME "AET"
#define WIN_STIME "WST"
#define WIN_ETIME "WET"
#define TWIN_STIME "TWST"
#define TWIN_ETIME "TWET"

int MWtrace_mark_window(MWgather *g, Dbptr trdb,Time_Window *win,
	int lag_at_peak)
{
	int nsamp_basis;  
	char sta[8],laststa[8];
	double trace_stime;
	double wstime,wetime,astime,aetime,twstime,twetime;
	int arrival_count;
	double si;
	int nrec;
	double samprate;

	Dbptr tra;

	tra = dblookup(trdb,0,"arrival",0,0);
	trdb = dblookup(trdb,0,"trace",0,0);
	dbquery(trdb,dbRECORD_COUNT,&nrec);
	/* return silently if the trace table is empty*/
	if(nrec <= 0) return(0);

	/* We need this to compute the grey area of 1/2 of a wavelet */
	nsamp_basis = g->x3[0]->basis->n;
	/* We set laststa to random sequence of characters to
	be sure the check against changes in sta is triggered in
	the loop below.  Probably a more elegant solution to this*/
	strcpy(laststa,"kIB%+@");
	for(trdb.record=0,arrival_count=0;trdb.record<nrec;++trdb.record)
	{
		if(dbgetv(trdb,0,"sta",sta,
			"time",&trace_stime,
			"samprate",&samprate,0) == dbINVALID)
		{
			elog_complain(0,"dbgetv error reading trace table at record %d\nMWtrace_mark_window returning after marking %d arrivals\n",
				trdb.record,arrival_count);
			return(arrival_count);
		}
		if(strcmp(sta,laststa))
		{
			/* this wastes a little space, but it is clearer
			to explicitly define all these times and then
			write the arrival records.*/
			si = 1.0/samprate; 
			wstime = trace_stime;
			wetime = trace_stime 
			   + ((double)((win->tend)-(win->tstart)))*si;
			astime = trace_stime + ((double)lag_at_peak)*si;
			aetime = astime +  ((double)(win->length)
				*(win->increment))*si;
			twstime = astime - ((double)nsamp_basis)*si/2.0;
			twetime = aetime + ((double)nsamp_basis)*si/2.0;
			dbaddv(tra,0,"sta",sta,
				"time",wstime,
				"phase",WIN_STIME,0);
			dbaddv(tra,0,"sta",sta,
				"time",wetime,
				"phase",WIN_ETIME,0);
			dbaddv(tra,0,"sta",sta,
				"time",astime,
				"phase",ANAL_STIME,0);
			dbaddv(tra,0,"sta",sta,
				"time",aetime,
				"phase",ANAL_ETIME,0);
			dbaddv(tra,0,"sta",sta,
				"time",twstime,
				"phase",TWIN_STIME,0);
			dbaddv(tra,0,"sta",sta,
				"time",twetime,
				"phase",TWIN_ETIME,0);

			strcpy(laststa,sta);
			arrival_count += 7;
		}
	}
	return(arrival_count);
}
#define SEM_CHAN_NAME "semb"
#define COH_CHAN_NAME "cohnce"

void MWtrace_put_semblance(Dbptr trdb,
	float *avgsemb,
	int lensemb,
	double timeref,
	int lag_at_peak,
	double dt,
	int coherence_type,
	char *staname)
{
	float *semtrace;  /* We need to copy trace here */
	char net[4]="MW"; 
	char chan[10];
	double calib=1.0;
	char datatype[4]="t4";
	double stime;
	double samprate;

	allot(float *,semtrace,lensemb);
	scopy(lensemb,avgsemb,1,semtrace,1);
	samprate = 1.0/dt;

	switch(coherence_type)
	{
		case(USE_COHERENCE):
			strcpy(chan,COH_CHAN_NAME);
			break;
		case(USE_SEMBLANCE):
		default:
			strcpy(chan,SEM_CHAN_NAME);
	}
	/* note this is the start time of the left side of 
	the analysis window which is the way the lag is defined
	internally here.  There is an ambiguity were to place
	the sample for graphical clarity, but this needs to just
	be documented clearly. */
	stime = timeref + ((double)lag_at_peak)*dt;

	if( dbaddv(trdb,"trace",
		"net",net,
		"sta",staname,
		"chan",chan,
		"time",stime,
		"endtime",ENDTIME(stime,samprate,lensemb),
		"nsamp",lensemb,
		"samprate",samprate,
		"calib",calib,
		"datatype",datatype,
		"data",semtrace,0) < 0) 
	{
		elog_complain(0,"dbaddv error creating coherence trace %s:%s\n",
					staname,chan);
	}
}
