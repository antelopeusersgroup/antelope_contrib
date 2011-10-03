/*  for reading wf-data:  access is from FORTRAN  
	Input is dbwf pointer to wfdisc 
	Output is tr, pointer to trace-structure with data, AND
	t0, t1  -- actual time window obtained, first waveform
	npts -- actual # points out, first waveform
	data -- array with first waveform

GAA 3/97
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/param.h>
#include "db.h"

int
readmywf_(dbwf, t0_req, t1_req, tr, t0, t1, npts, data)
Dbptr	*dbwf, *tr;
double	*t0_req, *t1_req, *t0, *t1;
int 	*npts;
float	*data;
{


	/* Make trace database */
	char	time_str[24], endtime_str[24];
	double	samprate;
	int	rs, re, nwf, i;
	float	*d1;

	*tr = dbinvalid() ;
	sprintf(time_str,"%f", *t0_req);
	sprintf(endtime_str,"%f", *t1_req);
	if (trload_css ( *dbwf, time_str, endtime_str, tr, 0, 0) < 0) 
		elog_die( 0, "Problems loading traces\n") ;

	dbquery (*tr, dbRECORD_COUNT, &nwf);
	if (nwf <1) {
		fprintf(stderr,"getmywf:  No data\n");
		return 0;
	}

	/* Get the first trace info */
	dbget_range(*tr, &rs, &re);
	(*tr).record=rs;
	if (dbgetv( *tr, 0,
		"data", &d1,
		"nsamp", npts,
		"samprate", &samprate,
		"time", t0,
		0)!=0) elog_die(0,"getmywf: dbgetv problem\n");

	*t1 = *t0 + (*npts - 1)/samprate;
	for (i=0; i<*npts; i++) data[i] = d1[i];

}
