#include "tr.h"
#include "multiwavelet.h"
/* This is a set of routines to manipulate trace tables to select the
output of trdisp.  These are mostly for debugging mwap, but they 
might prove useful later if I choose to do an interactive option.


sift is passed to dbsubset direction 
*/

void trplot_by_sta(Dbptr tr,char *sift)
{
	Tbl *t;
	Dbptr db;
	int nsta;
	char *sta;
	Dbptr dbbdl;

	t = newtbl(0);
	pushtbl(t,"sta");
	pushtbl(t,"chan");
	pushtbl(t,"time");
	tr = dblookup(tr,0,"trace",0,0);
	if(sift != NULL)
	{
		tr = dbsubset(tr,sift,0);
	}
	tr = dbsort(tr,t,0,0);
	clrtbl(t,0);
	pushtbl(t,"sta");
	db = dbgroup(tr,t,0,0);
	freetbl(t,0);

	dbquery(db,dbRECORD_COUNT,&nsta);
	for(db.record=0;db.record<nsta;++db.record)
	{
		dbgetv(db,0,"sta",sta,"bundle",&dbbdl,0);
		trdisp(dbbdl,sta);
	}
}

void trplot_one_mwtrace(MWtrace *t, char *chan)
{
	Dbptr trdb;
	float *dr, *di;
	double stime;
	int nsamp;
	char *sta;
	char net[4]="MW";
	double samprate;
	double calib=1.0;
	char datatype[4]="t4";
	int bundletype=0;
	int ierr;

float debug_buffer[10000];

	trdb = trnew(0,0);

	samprate = 1.0/(t->dt);
	MWtrace_to_float(t,0,'r',&dr,&stime,&nsamp);
scopy(nsamp,dr,1,debug_buffer,1);
	sta = strdup("real");
	trdb=dblookup(trdb,0,"trace",0,0);
	ierr=dbaddv(trdb,"trace","net",net,
		"sta",sta,
		"chan",chan,
		"time",stime,
		"endtime",ENDTIME(stime,samprate,nsamp),
		"nsamp",nsamp,
		"samprate",samprate,
		"calib",calib,
		"datatype",datatype,
		"bundletype",bundletype,
		"data",dr,0);
	free(sta);
	trdb.record =0;
	trdb.field = 1;
	trdisp(trdb,chan);

	MWtrace_to_float(t,0,'i',&di,&stime,&nsamp);
scopy(nsamp,di,1,debug_buffer,1);
	sta = strdup("imag");
	ierr=dbaddv(trdb,"trace","net",net,
		"sta",sta,
		"chan",chan,
		"time",stime,
		"endtime",ENDTIME(stime,samprate,nsamp),
		"nsamp",nsamp,
		"samprate",samprate,
		"calib",calib,
		"datatype",datatype,
		"bundletype",bundletype,
		"data",di,0);
	trdb.record = 1;
	trdb.field = 2;
	trdisp(trdb,chan);
	trdestroy(&trdb);
}
