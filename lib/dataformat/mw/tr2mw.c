
#include <stdio.h>

#include "db.h"
#include "tr.h"
#include "stock.h"


int
tr2mw (tr, mw,
       time_str, endtime_str,
       tbw, ntapers)
Dbptr           tr,
                mw;
char	          *time_str, *endtime_str ;
double          tbw;
int             ntapers;
{
    double		w0, w1 ; 
    float          *data,
                   *dp;
    int             rs,
                    re;
    Dbptr           bundle;
    int             bundletype;
    int             retcode = 0;
    double          time,
                    t0,
                    t1;
    int             nwanted,
                    pt0,
                    npts;
    char            net[10],
                    sta[10],
                    chan[10],
                    segtype[10];
    double          samprate,
                    calib;
    char           *response;
    double          sum,
                    variance;
    int             i;
    int		    nsamp ;
    Expression 	*time_expr, *endtime_expr ;


    dbget_range (tr, &rs, &re);

    if (dbex_compile (tr, time_str, &time_expr, dbTIME) < 0) {
	elog_log(0, "Can't compile start time: '%s'\n", time_str);
	return dbINVALID;
    }
    if (dbex_compile (tr, endtime_str, &endtime_expr, dbTIME) < 0) {
	elog_log(0, "Can't compile end time: '%s'\n", endtime_str);
	return dbINVALID;
    }

    for (tr.record = rs; tr.record < re; tr.record++) {
	dbgetv (tr, 0,
		"bundletype", &bundletype,
		0);

	switch (bundletype) {
	case 0:
	    dbgetv (tr, 0,
		    "net", net,
		    "sta", sta,
		    "chan", chan,
		    "response", &response,
		    "calib", &calib,
		    "segtype", segtype,
		    "time", &time,
		    "nsamp", &nsamp, 
		    "samprate", &samprate,
		    "data", &data,
		    0);

	if (dbex_eval (tr, time_expr, 0, &w0) < 0
		|| dbex_eval (tr, endtime_expr, 0, &w1) < 0) {
	    retcode++;
	    continue;
	}

	    nwanted = TIME2SAMP ( w0, samprate, w1) ;
	    if (trclip (time, samprate, nsamp, w0, w1, &pt0, &npts, &t0, &t1 ) >= nwanted) {
		dp = data + pt0;
		sum = 0.;
		for (i = 0; i < npts; i++)
		    sum += sqr (dp[i]);
		variance = sum / npts;

		tr.field = tr.record+1 ;
		mw.record = dbcopy (tr, mw, 0);
		dbputv (mw, 0,
			"time", t0,
			"endtime", ENDTIME(t0,samprate,npts), 
			"data", dp,
			"nsamp", npts,
			"tbw", tbw,
			"ntapers", ntapers,
			"nfft", 2 * npts,
			"nyquist", npts + 1,
			"net", net,
			"sta", sta,
			"chan", chan,
			"response", response,
			"calib", calib,
			"segtype", segtype,
			"samprate", samprate,
			"variance", variance,
			0);
	    }
	    break;

	default:
	    dbgetv (tr, 0,
		    "bundle", &bundle,
		    0);
	    retcode += tr2mw (bundle, mw, time_str, endtime_str, tbw, ntapers);
	    break;
	}

    }
    dbex_free(time_expr) ; 
    dbex_free(endtime_expr) ; 
    return retcode;
}

/* $Id$ */
