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
