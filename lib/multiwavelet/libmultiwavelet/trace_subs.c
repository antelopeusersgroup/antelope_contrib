#include <stdio.h>
#include "multiwavelet.h"
#include "tr.h"



/* This is a group of functions for mwap that do various manipulations 
using the datascope/antelope trace library.  

Author:  Gary Pavlis
*/

/* This is a companion function to the higher level read function
mwap_loaddata below. It is a descendent of an earlier function that
didn't work properly that worked on group pointers.  This one uses
a completely different algorithm based on dbmatches.  The 
basic algorithm is:
1.  grab the dbpointer for the view wfdisc->sitechan
2.  use dbmatches to find all waveforms whose time spans overlap the
input interval s:e for the specified station sta.
3.  Loop through the match list loading the parts of the data falling
in s:e and copying required waveform parameters to the tr db (e.g.
orientation information).  

The arguments are:
	db - input database 
	tr - output tr database (assumed already open and that this is
		a valid db pointer to a trace db )
	s:e - start:endtime span of requested data
	sta - station to load data for (all channels are loaded).

Returns:
	dbINVALID if the call to dbmatches fails.
	0 - no data matches criteria 
	positive number = number of traces loaded in tr

Author:  Gary L. Pavlis
Date:  original december 1999, This version completely rewritten 
November 2000
*/

int mwap_load_sta(Dbptr db, Dbptr tr, double s, double e, char *sta)
{
	long nsamp;
	double samprate,calib;
	double hang,vang,edepth;
	char chan[10];
	static Hook *hook=NULL;
	Tbl *matches;
	Tbl *pattern;
	Dbptr dbk, dbt;
	int nmatch;
	int i,ierr;
        float *data;
        long datasz;
        double t0,t1;
	char net[4]="MW";   /* This is a required key on trace table so 
				we just set it to a value */
	int ntrread;

	/* This attaches the view formed from the join of wfdisc and sitechan*/
	dbt = dblookup(db,0,WFVIEW,0,0);
	if(dbt.record == dbINVALID) return(dbINVALID);
	dbk = dblookup(db,0,"wfdisc",0,0);
	dbk.record = dbSCRATCH;
	dbputv(dbk,0,"sta",sta,"time",s,"endtime",e,NULL);

	pattern = strtbl("sta","time::endtime",0L);
	nmatch = dbmatches(dbk,dbt,&pattern,&pattern,&hook,&matches);
	if( (nmatch == dbINVALID) || (nmatch == 0) )return(nmatch);
	for(i=0,ntrread=0;i<maxtbl(matches);++i)
	{
		data = NULL;
		datasz = 0;
		dbt.record = (long)gettbl(matches,i);
		ierr = trgetwf(dbt,0,&data,&datasz,s,e,&t0,&t1,&nsamp,0,0);
		if(ierr)
		{
			trgetwf_error(dbt,ierr);
			continue;
		}
		/* Note I intentionally ignore if s and e do not match 
		t0 and t1.  This is because I assume this routine is 
		called multiple times and trglue is used later to patch
		multiple pieces together */
		ierr = dbgetv(dbt,0,"sta",sta,
				"wfdisc.chan",chan,
				"samprate",&samprate,
				"calib", &calib,
				"hang",&hang,
				"vang",&vang,
				"edepth",&edepth,
				NULL);
		if(ierr)  
		{
		   elog_notify(0,
		  "dbgetv error reading %s:%s at %s\nContinuing, but additional problems likely\n",
			sta,chan,strtime(t0));
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
				NULL);
		if(ierr<0)
		{
			elog_notify(0,"Error appending to trace table for %s:%s at %s\nProbably data loss and associated memory leak\n",
				sta,chan,strtime(t0));
		}
		else
		{
			/* If the addv failed we assume nothing got saved
			in the trace table */
			++ntrread;
		}
	}	
	freetbl(pattern,0);
	freetbl(matches,0);
	return(ntrread);
}

/*this is the basic routine that reads an event gather defined by
the input arrival times.  It creates a new trace database
and fills it with traces it reads.  Time intervals read are
driven by the contents of the arrivals array and the time
windows defined by swin and nwin.  Note the window is defined
by looking at the range defined by the time span of both the
signal and noise time windows.  

Arguments:
	db - db pointer of input database (can be anything as long
		as the db.database field is correct and is an open
		database.)
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
		int ntraces;

		/* Because the view dbwf has been grouped by 
		sta, we can get the bundle pointer by 
		this simple lookup trick */
		sta = gettbl(t,i);
		atime = (double *)getarr(arrivals, sta);
		stime = (*atime)+wstart;
		etime = (*atime)+wend;
		ntraces = mwap_load_sta(db,tr,stime,etime,sta);
		if(ntraces==dbINVALID)
		{
			elog_complain(0,"Serious database problems trying to load data for station %s in time interval %lf:%lf\nAttempting to continue\n",
				sta,stime,etime);
			continue;
		}
		if(ntraces==0)
		{
			delarr(arrivals,sta);
			elog_log(0,"No waveform data for arrival at\
station %s\nCannot process data from that station.",
				sta);
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
	long nrec;
	char chan[20];
	char *cardinal[3]={ EW, NS, VERTICAL };
	int i,found;

	db = dblookup(tr,0,"trace",0,0);
	dbquery(db,dbRECORD_COUNT,&nrec);

	for(db.record=0;db.record<nrec;++db.record)
	{
		dbgetv(db,0,"chan",chan,NULL);
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
