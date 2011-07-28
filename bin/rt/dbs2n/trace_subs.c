#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "db.h"
#include "arrays.h"
#include "scv2.h"
#include "tr.h"

Trace *read_trace();

Trace *
copy_trace (trace, copydata)

Trace *     trace;
int                copydata;

{
	Trace *trhead, *tr;

	trhead = NULL;
	for (tr=trace; tr!=NULL; tr=tr->next) {
		trhead = (Trace *) copytr (trhead, tr, copydata);
		if (trhead == NULL) {
			fprintf (stderr, "copy_trace: copytr() error.\n");
			return (NULL);
		}
	}
	for (;trhead->prev!=NULL;trhead=trhead->prev);
	return (trhead);
}

int
add_trace (db, tstart, tend, trace, traceo)

Dbptr db;
double          tstart;
double                  tend;
Trace *                       trace;
Trace **                            traceo;

{
	Trace *tr, *trn;

	trn = read_trace (db, tstart, tend);
	if (trn == NULL) {
		fprintf (stderr, "add_trace: read_trace() error.\n");
		return (0);
	}
	if (trn->raw_data == NULL) {
		SCV_free_trace (trn);
		*traceo = trace;
		return (1);
	}
	if (trace == NULL) {
		*traceo = trn;
		return (1);
	}
	for (tr=trace; tr->next!=NULL; tr=tr->next);
	tr->next = trn;
	trn->prev = tr;
	*traceo = trace;
	return (1);
}

Trace *
read_trace (db, tstart, tend)

Dbptr db;
double          tstart;
double                  tend;

{
	float *buf=0;
	char fname[1024];
	char dtype[8];
	char segtype[8];
	char sta[8], chan[12];
	int nsamp;
	Trace *trace;
	double endtime, time, dt, samprate;
	double calib, calper;
	double te, ts;
	int isamp, jsamp, size;

	if (dbextfile (db, "wfdisc", fname) < 1) {
		fprintf (stderr, "read_trace: Unable to find input file '%s'\n",
						fname);
		return (NULL);
	}
	dbgetv (db, 0, 
	        "time", &time, 
	        "endtime", &endtime, 
		"samprate", &samprate,
		"nsamp", &nsamp, 
		"datatype", dtype,
		"segtype", segtype, 
		"calib", &calib, 
		"calper", &calper, 
		"sta", sta,
		"chan", chan, 0);

	size = atoi(&dtype[strlen(dtype)-1]);
	if( time > tend ) {
	     elog_complain( 0, "record starts at %lf but requested etime %lf\n", time, tend);
	     free(buf); buf = 0;
	}
	if ( endtime < tstart )  { 
	     elog_complain( 0, "record ends at %lf but requested stime %lf\n", endtime, tstart);
	     free(buf); buf = 0;
	}
	
	if (nsamp > 0) {

	   if( buf != 0 && *buf != 0)  
	      free(buf); 
	   
	   buf = trgetwf(db, 0, 0, 0,tstart, tend, &ts, &te, &nsamp, 0, 0) ; 
           if( buf == 0 )  { 
                elog_complain( 0, "can't get %s_%s data from %lf to %lf\n", 
                             sta, chan, tstart, tend ); 
                return 0;
           }
           if( fabs( ts - tstart ) > 1.0/samprate ) 
               elog_complain( 0, "problem get data. Ask %lf-%lf - got %lf-%lf\n", 
	                 tstart, tend, ts, te );
					       
           if( fabs( tend - te ) > 1.0/samprate ) 
               elog_complain( 0, "problem get data. Ask %lf-%lf - got %lf-%lf\n", 
	                 tstart, tend, ts, te );
					       

	   trace = (Trace *) malloc (sizeof(Trace));
	   if (trace == NULL) {
		fprintf (stderr, "read_trace: Malloc error on Trace structure.\n");
		return (NULL);
	   }
	   trace->tstart = ts;
	   trace->dt = 1.0/samprate;
	   trace->nsamps = nsamp;
	   trace->calib = calib;
	   trace->calper = calper;
	   strcpy (trace->rawdata_format, "s4");
	   strcpy (trace->rawdata_type, segtype);
	   trace->data = NULL;
	   trace->data_free = NULL;
	   trace->data_malloc = 0;
	   trace->raw_data = buf;
	   trace->rawdata_free = (void *)buf;
	   if (buf) trace->rawdata_malloc = nsamp*4; 
	   else trace->rawdata_malloc = 0;
	   trace->prev = NULL;
	   trace->next = NULL;
	   if (buf) trace = (Trace *) SCV_trace_fixgaps (trace, "segment");
	   return (trace);
	}
	return 0;
}

