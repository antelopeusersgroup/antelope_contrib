#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sunmath.h>

#include "stock.h"
#include "coords.h"
#include "db.h"
#include "arrays.h"
#include "scv2.h"
#include "tr.h"
/* These prototypes seem to be required */
void *my_malloc(char *,int);
int SCV_trace_fixgaps(Trace *,char*);

/* Descendent of an earlier program to read trace data based
on Danny Harvey's scv2 library Trace object.  This version 
uses trgetwf to read data, which allows reading virtually 
any existing seismic data format.  The algorithm used, however,
assumes blindly that there are no waveform segment breaks within
the requested time period.  This will, for example, fail randomly
for continuously data.  

Arguments 
	db - Database pointer to ONE row of wfdisc from which
		data is to be read.
	tstart:tend - specify time period to read data for from this row.

Author:  Gary Pavlis, based on code by Danny Harvey
Latest rev May 2001
*/

Trace *read_trace (Dbptr db, double tstart, double tend)

{
	char dtype[8];
	char segtype[8];
	int nsamp;
	float *data;
	Trace *trace;
	double time, dt, samprate;
	double calib, calper;
	double t0,t1;
	int npts;

	/* If this lookup failed, it needs to be a fatal error
		as something is seriously wrong */
	if( dbgetv (db, 0, "time", &time, "samprate", &samprate,
				"nsamp", &nsamp, "datatype", dtype, 
				"segtype", segtype, 
				"calib", &calib, "calper", &calper, 0)
				== dbINVALID)
		die(1,"read_trace error reading from database wfdisc table\n");

	nsamp = nint( (tend-tstart)*samprate);
	allot(float *,data,nsamp);
	dt = 1.0/samprate;
	if(trgetwf(db,0,&data,&nsamp,tstart,tend,&t0,&t1,&npts,0,0))
	{
		elog_complain(0,"trgetwf error for row %d of wfdisc table\n",
			db.record);
		return(NULL);
	}
	trace = (Trace *) my_malloc ("read_trace: trace", sizeof(Trace));
	if (trace == NULL) {
		elog_notify(0, "read_trace: Malloc error on Trace structure.\n");
		free (data);
		return (NULL);
	}
	if((fabs(t0-tstart)>dt) || (fabs(t1-tend)>dt) )
	{
		char sta[10],chan[10];
		dbgetv(db,0,"sta",sta,"chan",chan,0);
		elog_notify(0,"Window truncated for %s:%s at start time %s\nExpected to read %d samples, got only %d\n",
			sta,chan,strtime(time),nsamp,npts);
	}
	trace->tstart = t0;
	trace->dt = 1.0/samprate;
	trace->nsamps = npts;
	trace->calib = calib;
	trace->calper = calper;
	strcpy (trace->rawdata_format, "t4");
	strcpy (trace->rawdata_type, segtype);
	trace->data = NULL;
	trace->data_free = NULL;
	trace->data_malloc = 0;
	trace->raw_data = data;
	trace->rawdata_free = NULL;
	if (data) trace->rawdata_malloc = nsamp*sizeof(float); else trace->rawdata_malloc = 0;
	trace->prev = NULL;
	trace->next = NULL;
	if (data) trace = (Trace *) SCV_trace_fixgaps (trace, "segment");
	return (trace);
}

