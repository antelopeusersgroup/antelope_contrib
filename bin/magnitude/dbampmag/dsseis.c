/* Copyright (c) 2002 Boulder Real Time Technologies, Inc. */
 
/* Boulder Real Time Technologies, Inc. grants permission to copy, 
   modify and/or use the source code in this software module as 
   long as the following conditions are satisfied:
   1 - This copyright statement and software agreement appears 
       in all files that contain any of the source code in this 
       software module.
   2 - This software module and any others that are derived from
       this software module must be made freely available for
       copy, modification and use by all others.
   3 - The terms of this software agreement apply only to the
       source code in this software module. */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "db.h"
#include "tr.h"
#include "response.h"
#include "arrays.h"

int grtr_sc_group ( Dbptr trace, Dbptr *trscgr);
int traccumulate (Dbptr tro, Dbptr tri, double factor);

static Arr *resp_arr=NULL;

#define    INT(x,y)        ((x)<0.0?((x)/(y)-0.5):((x)/(y)+0.5))

/*
 * NAME
 *	grdb_sc_loadcss - create a station-channel db grouping
 *
 * SYNOPSIS
 *	int
 *	grdb_sc_loadcss (dbin, net_expr, sta_expr, chan_expr, tstart, tend,
 *			 coords, ir, orient, dbscgr, dbsc)
 *
 *	Dbptr            dbin;
 *	char *                 net_expr;
 *	char *                           sta_expr;
 *	char *                                     chan_expr;
 *	double                                                tstart, tend;
 *	int              coords, ir, orient;
 *	Dbptr *                              dbscgr;
 *	Dbptr *                                      dbsc;
 *
 * DESCRIPTION
 * ARGUMENTS
 *	Dbptr		dbin		= (i) Input database pointer.
 *	char *		net_expr	= (i) Network code expression. If NULL,
 *					      then use all networks.
 *	char *		sta_expr	= (i) Station code expression. If NULL,
 *					      then use all stations.
 *	char *		chan_expr	= (i) Channel code expression. If NULL,
 *					      then use all channels.
 *	double		tstart		= (i) Start epoch time.
 *	double		tend		= (i) End epoch time. If tstart = tend = 0.0,
 *					      then all times are used.
 *	int		coords		= (i) Station coordinates needed flag. If
 *					      this is > 1 and there are missing
 *					      station coordinates, 
 *					      then an error exit will occur.
 *	int		ir		= (i) Instrument responses needed flag. If
 *					      this is > 1 and there are missing
 *					      instrument responses, 
 *					      then an error exit will occur.
 *	int		orient		= (i) Channel orientation angles needed flag. If
 *					      this is > 1 and there are missing
 *					      channel orientation angles, 
 *					      then an error exit will occur.
 *	Dbptr *		dbscgr		= (o) Station-channel db grouping.
 *	Dbptr *		dbsc		= (o) Station-channel db view.
 *
 * RETURNS
 *	0 if OK or -1 if ERROR.
 */

int
grdb_sc_loadcss (Dbptr dbin, char *net_expr, char *sta_expr, char *chan_expr, double tstart, double tend,
		 int coords, int ir, int orient, Dbptr *dbscgr, Dbptr *dbsc)

/*
Dbptr            dbin;
char *                 net_expr;
char *                           sta_expr;
char *                                     chan_expr;
double                                                tstart, tend;
int              coords, ir, orient;
Dbptr *                              dbscgr;
Dbptr *                                      dbsc;
*/
{
	Dbptr dbout, db, dbout2;
	char string[1024];
	char string2[1024];
	char sta_wfdisc[32], chan_wfdisc[32];
	int i, j, n, sensor=0, ok;
	Tbl *pat1, *pat2;
	Tbl *sortfields, *groupfields;
	FILE *file;
	Response *resp;

	/* Subset the wfdisc by station-channel-time sifters. */

	dbout = dblookup (dbin, 0, "wfdisc", 0, 0);
	strcpy (string, "");
	if (sta_expr) {
		strcpy (string, "( ");
        	sprintf (string2, "sta =~ /%s/", sta_expr);
        	strcat (string, string2);
	}
	if (chan_expr) {
		if (string[0]) strcat (string, " && ");
		else strcpy (string, "( ");
        	sprintf (string2, "chan =~ /%s/", chan_expr);
        	strcat (string, string2);
	}
	if (tstart != 0.0 || tend != 0.0) {
		if (string[0]) strcat (string, " && ");
		else strcpy (string, "( ");
        	sprintf (string2, "(time < %.5f && endtime > %.5f)", tend, tstart);
        	strcat (string, string2);
	}
	if (string[0]) {
		strcat (string, " )");
		dbout = dbsubset (dbout, string, 0);
	}
        dbquery (dbout, dbRECORD_COUNT, &n);
        if (n < 1) {
		elog_log(0, "grdb_sc_loadcss: No wfdisc rows to process.\n");
		return (-1);
        }

        /* Make the necessary joins and check for completeness. */

        if (coords) {
        	db = dblookup (dbin, 0, "site", 0, 0);
        	dbout = dbjoin (dbout, db, 0, 0, 1, 0, 0);
        	dbquery (dbout, dbRECORD_COUNT, &n);
        	if (n < 1) {
			elog_log(0, "grdb_sc_loadcss: No data rows to process.\n");
			return (-1);
        	}
        	for (dbout.record=0; dbout.record<n; dbout.record++) {
        		if (dbgetv (dbout, 0, "wfdisc.sta", sta_wfdisc,
        				"wfdisc.chan", chan_wfdisc,
        				"site.sta", string, 0) == dbINVALID) {
			    elog_log(0, "grdb_sc_loadcss: dbgetv() error while checking site.\n");
			    return (-1);
			}
        		if (coords > 1 && strcmp(string, sta_wfdisc)) {
        			elog_log(0, "grdb_sc_loadcss: Cannot find site parameters for %s %s.\n", 
        									sta_wfdisc, chan_wfdisc);
        			return (-1);
        		}
        	}
        }
        if (ir) {
        	db = dblookup (dbin, 0, "sensor", 0, 0);
        	dbout = dbjoin (dbout, db, 0, 0, 1, 0, 0);
        	dbquery (dbout, dbRECORD_COUNT, &n);
        	if (n < 1) {
			elog_log(0, "grdb_sc_loadcss: No data rows to process.\n");
			return (-1);
        	}
        	for (dbout.record=0; dbout.record<n; dbout.record++) {
        		if (dbgetv (dbout, 0, "wfdisc.sta", sta_wfdisc,
        				"wfdisc.chan", chan_wfdisc,
        				"sensor.sta", string, 0) == dbINVALID) {
			    elog_log(0, "grdb_sc_loadcss: dbgetv() error while checking sensor.\n");
			    return (-1);
			}
        		if (ir > 1 && strcmp(string, sta_wfdisc)) {
        			elog_log(0, "grdb_sc_loadcss: Cannot find sensor parameters for %s %s.\n", 
        									sta_wfdisc, chan_wfdisc);
        			return (-1);
        		}
        	}
        	sensor = 1;
        	db = dblookup (dbin, 0, "instrument", 0, 0);
        	dbout = dbjoin (dbout, db, 0, 0, 1, 0, 0);
        	dbquery (dbout, dbRECORD_COUNT, &n);
        	if (n < 1) {
			elog_log(0, "grdb_sc_loadcss: No data rows to process.\n");
			return (-1);
        	}
        	for (dbout.record=0; dbout.record<n; dbout.record++) {
        		if (dbgetv (dbout, 0, "wfdisc.sta", sta_wfdisc,
        				"wfdisc.chan", chan_wfdisc,
        				"sensor.inid", &j,
        				"instrument.insname", string2,
        				"instrument.inid", &i, 0) == dbINVALID) {
			    elog_log(0, "grdb_sc_loadcss: dbgetv() error while checking instrument.\n");
			    return (-1);
			}
        		if (ir > 1 && (i != j)) {
        			elog_log(0, "grdb_sc_loadcss: Cannot find instrument parameters for %s %s.\n", 
        									sta_wfdisc, chan_wfdisc);
        			return (-1);
        		}
        		if (i >= 0) {
				if (resp_arr == NULL) {
					resp_arr = newarr (0);
					if (resp_arr == NULL) {
        					elog_log(0, "grdb_sc_loadcss: newarr() error.\n");
        					return (-1);
					}
				}
				dbextfile (dbout, "instrument", string);
				resp = (Response *) getarr (resp_arr, string);
				if (resp == NULL) {
					file = fopen (string, "r");
					if (file == NULL) {
						if (ir > 1) {
        						elog_log(1, "grdb_sc_loadcss: fopen('%s') error.\n", string);
        						return (-1);
						}
					} else {
						if (read_response (file, &resp)) {
        						elog_log(0, "grdb_sc_loadcss: read_response('%s') error.\n", string);
        						return (-1);
						}
						fclose (file);
						resp->insname = strdup(string2);
					}
					setarr (resp_arr, string, resp);
				}
			}
        	}
        }
        if (orient) {
        	ok = 1;
        	db = dblookup (dbin, 0, "sitechan", 0, 0);
        	dbout2 = dbjoin (dbout, db, 0, 0, 1, 0, 0);
        	dbquery (dbout2, dbRECORD_COUNT, &n);
        	if (n < 1) {
        		ok = 0;
        	} else {
        		for (dbout2.record=0; dbout2.record<n; dbout2.record++) {
        			dbgetv (dbout2, 0, "wfdisc.sta", sta_wfdisc,
        				"wfdisc.chan", chan_wfdisc,
        				"sitechan.sta", string, 0);
        			if (strcmp(string, sta_wfdisc)) {
        				ok = 0;
        				break;
        			}
        		}
		}
		if (ok) {
			dbout = dbout2;
		} else {
			if (!sensor) {
        			db = dblookup (dbin, 0, "sensor", 0, 0);
        			dbout = dbjoin (dbout, db, 0, 0, 1, 0, 0);
        			dbquery (dbout, dbRECORD_COUNT, &n);
        			if (n < 1) {
					elog_log(0, "grdb_sc_loadcss: No data rows to process.\n");
					return (-1);
        			}
        			for (dbout.record=0; dbout.record<n; dbout.record++) {
        				if (dbgetv (dbout, 0, "wfdisc.sta", sta_wfdisc,
        						"wfdisc.chan", chan_wfdisc,
        						"sensor.sta", string, 0) == dbINVALID) {
			    			elog_log(0, "grdb_sc_loadcss: dbgetv() error while checking sensor.\n");
			    			return (-1);
					}
        				if (orient > 1 && strcmp(string, sta_wfdisc)) {
        					elog_log(0, "grdb_sc_loadcss: Cannot find sensor parameters for %s %s.\n", 
        											sta_wfdisc, chan_wfdisc);
        					return (-1);
        				}
        			}
			}
        		db = dblookup (dbin, 0, "sitechan", 0, 0);
        		pat1 = newtbl(1);
        		if (pat1 == NULL) {
        			elog_log(0, "grdb_sc_loadcss: newtbl() error.\n");
        			return (-1);
        		}
        		pat2 = newtbl(1);
        		if (pat2 == NULL) {
        			elog_log(0, "grdb_sc_loadcss: newtbl() error.\n");
        			return (-1);
        		}
        		settbl (pat1, 0, strdup("sensor.chanid"));
        		settbl (pat2, 0, strdup("sitechan.chanid"));
        		dbout = dbjoin (dbout, db, &pat1, &pat2, 1, 0, 0);
        		freetbl (pat1, free);
        		freetbl (pat2, free);
        		dbquery (dbout, dbRECORD_COUNT, &n);
        		if (n < 1) {
				elog_log(0, "grdb_sc_loadcss: No data rows to process.\n");
				return (-1);
        		} else {
        			for (dbout.record=0; dbout.record<n; dbout.record++) {
        				if (dbgetv (dbout, 0, "wfdisc.sta", sta_wfdisc,
        					"wfdisc.chan", chan_wfdisc,
        					"sitechan.sta", string, 0) == dbINVALID) {
			    		   elog_log(0, "grdb_sc_loadcss: dbgetv() error while checking sitechan.\n");
			    		   return (-1);
					}
        				if (orient > 1 && strcmp(string, sta_wfdisc)) {
        					elog_log(0, "grdb_sc_loadcss: Cannot find sitechan parameters for %s %s.\n", 
        											sta_wfdisc, chan_wfdisc);
        					return (-1);
        				}
        			}
			}
		}
        }

        /* Sort and group the output view. */

	sortfields = newtbl (3);
	if (sortfields == NULL) {
		elog_log(0, "grdb_sc_loadcss: newtbl() error.\n");
		return (-1);
	}
	settbl (sortfields, 0, strdup("wfdisc.sta"));
	settbl (sortfields, 1, strdup("wfdisc.chan"));
	settbl (sortfields, 2, strdup("wfdisc.time"));
        *dbsc = dbsort (dbout, sortfields, 0, 0);
	groupfields = newtbl (2);
	if (groupfields == NULL) {
		elog_log(0, "grdb_sc_loadcss: newtbl() error.\n");
		return (-1);
	}
	settbl (groupfields, 0, strdup("sta"));
	settbl (groupfields, 1, strdup("chan"));
	*dbscgr = dbgroup (*dbsc, groupfields, 0, 1);
	freetbl (sortfields, free);
	freetbl (groupfields, free);

	/* Normal exit */

	return (0);
}

/*
 * NAME
 *	grdb_sc_getstachan - Get the station-channel value
 *
 * SYNOPSIS
 *	int
 *	grdb_sc_getstachan (dbscgr, record, sta, chan, nsegs, time, endtime)
 *
 *	Dbptr               dbscgr;
 *	int			    record;
 *	char *				    sta;
 *	char *				         chan;
 *	int * 					       nsegs;
 *	double *                                              time;
 *	double *                                                    endtime;
 *
 * DESCRIPTION
 * ARGUMENTS
 *	Dbptr		dbscgr		= (i) Input database station-channel group (from grdb_sc_loadcss()).
 *	int		record		= (i) Record index. If < 0, then use dbsc.record
 *	char *		star		= (o) Station code.
 *	char *		chan		= (o) Channel code.
 *	int *		nsegs		= (o) Number of waveform segments.
 *	double *	time		= (o) Overall start time;
 *	double *	endtime		= (o) Overall end time;
 *
 * RETURNS
 *	0 if OK or -1 if ERROR.
 */

int
grdb_sc_getstachan (dbscgr, record, sta, chan, nsegs, time, endtime)

Dbptr               dbscgr;
int			    record;
char *				    sta;
char *				         chan;
int *                                          nsegs;
double *                                              time;
double *                                                    endtime;

{
	Dbptr db;
	int is, ie;

	if (record >= 0) dbscgr.record = record;
	if (dbgetv (dbscgr, 0, "sta", sta, "chan", chan, "bundle", &db, 0) == dbINVALID) {
        	elog_log(0, "grdb_sc_getstachan: dbgetv() error.\n");
        	return (-1);
	}
        dbget_range (db, &is, &ie);
	*nsegs = ie - is;
        db.record = is;
	dbgetv (db, 0, "time", time, 0);
        db.record = ie-1;
	dbgetv (db, 0, "endtime", endtime, 0);

	/* Normal exit. */

	return (0);
}

/*
 * NAME
 *	grtr_sc_create - Create Station-Channel trace grouping
 *
 * SYNOPSIS
 *	int
 *	grtr_sc_create (dbsc, net_expr, sta_expr, chan_expr, tstart, tend, 
 *			gap, calib, group, trscgr)
 *
 *	Dbptr           dbsc;
 *	char *                net_expr;
 *	char *                          sta_expr;
 *	char *                                    chan_expr;
 *	double                                               tstart, tend;
 *	char *          gap;
 *	int                  calib;
 *	int                         group;
 *	Dbptr *                            trscgr;
 *
 * DESCRIPTION
 * ARGUMENTS
 *	Dbptr		dbsc		= (i) Input database station-channel view (from grdb_sc_loadcss()).
 *	char *		net_expr	= (i) Network code expression. If NULL,
 *					      then use all networks.
 *	char *		sta_expr	= (i) Station code expression. If NULL,
 *					      then use all stations.
 *	char *		chan_expr	= (i) Channel code expression. If NULL,
 *					      then use all channels.
 *	double		tstart		= (i) Start epoch time.
 *	double		tend		= (i) End epoch time. If tstart = tend = 0.0,
 *					      then all times are used.
 *	char *		gap		= (i) What to do with data gaps. 
 *					      NULL, same as "segment"
 *					      "seg"     - Remove internally flagged data gap
 *							  values and replace with segments.
 *					      "interp"  - Remove internally flagged data gap
 *							  values and linear interpolate across
 *							  all data gaps.
 *					      "zero"    - Remove internally flagged data gap
 *							  values and replace all data gaps
 *							  with zero value.
 *					      "drop"    - Remove internally flagged data gap
 *							  values and drop the channel is any
 *							  gaps exist.
 *					      "leave"   - Leave all internally flagged gap
 *							  values and true data gaps as is.
 *	int		calib		= (i) Apply calib flag. If set, then apply calib
 *					      to the data after reading.
 *	int		group		= (i) Station channel grouping flag. If set, then
 *					      form a station channel group.
 *	Dbptr *		trscgr		= (o) Output trace table or station-channel group
 *					      (depending on group).
 *
 * RETURNS
 *	0 if OK or -1 if ERROR.
 */

int grtr_sc_create (dbsc, net_expr, sta_expr, chan_expr, tstart, tend, 
		gap, calib, group, trscgr)

Dbptr           dbsc;
char *                net_expr;
char *                          sta_expr;
char *                                    chan_expr;
double                                               tstart, tend;
char *          gap;
int                  calib;
int                         group;
Dbptr *                            trscgr;

{
	char time_str[100];
	char endtime_str[100];
	int ret, n, n2, i;
	double time, time2, endtime, endtime2;
	char sta[32], chan[32];
	char sta2[32], chan2[32];
	char string[1024];
	char string2[64];
	int new_view = 0;
	int is, crunch;
	Response *resp;
	Dbptr db;

	/* Subset input view */

	strcpy (string, "");
	if (sta_expr) {
		strcpy (string, "( ");
        	sprintf (string2, "sta =~ /%s/", sta_expr);
        	strcat (string, string2);
	}
	if (chan_expr) {
		if (string[0]) strcat (string, " && ");
		else strcpy (string, "( ");
        	sprintf (string2, "chan =~ /%s/", chan_expr);
        	strcat (string, string2);
	}
	if (string[0]) {
		strcat (string, " )");
		dbsc = dbsubset (dbsc, string, 0);
		new_view = 1;
	}
        dbquery (dbsc, dbRECORD_COUNT, &n);
        if (n < 1) {
		elog_log(0, "grtr_sc_create: No data to process.\n");
		if (new_view) dbfree (dbsc);
		return (-1);
        }

	/* Read in data */

	if (tstart == 0.0 && tend == 0.0) {
		dbquery (dbsc, dbRECORD_COUNT, &n);
		for (dbsc.record = 0; dbsc.record < n; dbsc.record++) {
			if (dbgetv (dbsc, 0, "time", &time, "endtime", &endtime, 0) == dbINVALID) {
        			elog_log(0, "grtr_sc_create: dbgetv() error.\n");
				if (new_view) dbfree (dbsc);
        			return (-1);
			}
        		if (tstart == 0.0 && tend == 0.0) {
        			tstart = time;
        			tend = endtime;
			} else {
				if (time < tstart) tstart = time;
				if (endtime > tend) tend = endtime;
			}
		}
	}
        sprintf (time_str, "(%.5f)", tstart);
        sprintf (endtime_str, "(%.5f)", tend);
	dbsc.record = dbALL;
        ret = trload_cssgrp (dbsc, time_str, endtime_str, trscgr, 0, 0);
        if (ret != 0) {
        	elog_log(0, "grtr_sc_create: trload_cssgrp() error.\n");
		if (new_view) dbfree (dbsc);
        	return (-1);
	}

	/* Split, splice, apply calib */

	if (gap == NULL) strcpy (string, "seg");
	else strcpy (string, gap);
	if (!strcmp(string, "leave")) {
	} else if (!strcmp(string, "seg")) {
		trsplit (*trscgr, 0, 0);
		trsplice (*trscgr, 0.5, 0, 0);
	} else if (!strcmp(string, "interp")) {
		elog_log(0, "grtr_sc_create: gap value '%s' not implemented yet.\n", string);
		if (new_view) dbfree (dbsc);
		return (-1);
	} else if (!strcmp(string, "zero")) {
		elog_log(0, "grtr_sc_create: gap value '%s' not implemented yet.\n", string);
		if (new_view) dbfree (dbsc);
		return (-1);
	} else if (!strcmp(string, "drop")) {
		trsplit (*trscgr, 0, 0);
		trsplice (*trscgr, 0.5, 0, 0);
		crunch = 0;
		dbquery (*trscgr, dbRECORD_COUNT, &n);
		strcpy (sta2, "");
		strcpy (chan2, "");
		n2 = 0;
		db = *trscgr;
		for (trscgr->record=0; trscgr->record<n; (trscgr->record)++) {
			if (dbgetv (*trscgr, 0, "sta", sta, "chan", chan,
					0) == dbINVALID) {
        			elog_log(0, "grtr_sc_create: dbgetv() error.\n");
				if (new_view) dbfree (dbsc);
        			return (-1);
			}
			if (strcmp(sta, sta2) || strcmp(chan, chan2)) {
				if (n2 > 1) {
        				for (db.record = is; db.record < is+n2; db.record++) {
        					trfree (db);
        					crunch = 1;
        				}
				}
				n2 = 1;
				is = trscgr->record;
				strcpy (sta2, sta);
				strcpy (chan2, chan);
				continue;
        		}
        		n2++;
		}
		if (n2 > 1) {
       			for (db.record = is; db.record < is+n2; db.record++) {
       				trfree (db);
       				crunch = 1;
       			}
		}
		if (crunch) {
			dbcrunch (db);
		}
	} else {
		elog_log(0, "grtr_sc_create: Illegal gap value '%s'.\n", string);
		if (new_view) dbfree (dbsc);
		return (-1);
	}
	if (calib) trapply_calib (*trscgr);

	/* Read in instrument responses. */

	dbsc.record = 0;
	if (dbgetv (dbsc, 0, "instrument.inid", &i, 0) != dbINVALID && i >= 0) {
		dbquery (*trscgr, dbRECORD_COUNT, &n);
		dbquery (dbsc, dbRECORD_COUNT, &n2);
		for (trscgr->record=0; trscgr->record<n; (trscgr->record)++) {
			if (dbgetv (*trscgr, 0, "sta", sta, "chan", chan,
					"time", &time, 0) == dbINVALID) {
        			elog_log(0, "grtr_sc_create: dbgetv() error.\n");
				if (new_view) dbfree (dbsc);
        			return (-1);
			}
			for (dbsc.record=0; dbsc.record<n2; dbsc.record++) {
				if (dbgetv (dbsc, 0, "sta", sta2, "chan", chan2,
					"time", &time2, "endtime", &endtime2, 0) == dbINVALID) {
        				elog_log(0, "grtr_sc_create: dbgetv() error.\n");
					if (new_view) dbfree (dbsc);
        				return (-1);
				}
				if (strcmp(sta, sta2)) continue;
				if (strcmp(chan, chan2)) continue;
				if (time < time2) continue;
				if (time >= endtime2) continue;
				dbextfile (dbsc, "instrument", string);
				resp = (Response *) getarr (resp_arr, string);
				dbputv (*trscgr, 0, "response", resp, 0);
				break;
			}
		}
	} else {
		elog_clear_register(0);
	}
	if (new_view) dbfree (dbsc);

	/* Set up Station-Channel grouping */

	if (group) {
		if (grtr_sc_group (*trscgr, trscgr)) {
			elog_log(0, "grtr_sc_create: grtr_sc_group() error.\n");
			return (-1);
		}
	}

	/* Normal exit. */

	return (0);
}

/*
 * NAME
 *	grtr_sc_group - Group existing Station-Channel trace table
 *
 * SYNOPSIS
 *	int
 *	grtr_sc_group (trace, trscgr)
 *
 *	Dbptr          trace;
 *	Dbptr *               trscgr;
 *
 * DESCRIPTION
 * ARGUMENTS
 *	Dbptr		trace		= (i) Input Station-Channel trace table
 *	Dbptr *		trscgr		= (o) Output trace group
 *
 * RETURNS
 *	0 if OK or -1 if ERROR.
 */

int grtr_sc_group ( Dbptr trace, Dbptr *trscgr)

{
	Tbl *groupfields;

	/* Set up Station-Channel grouping */

	groupfields = newtbl (2);
	if (groupfields == NULL) {
		elog_log(0, "grtr_sc_group: newtbl() error.\n");
		return (-1);
	}
	settbl (groupfields, 0, strdup("sta"));
	settbl (groupfields, 1, strdup("chan"));
	*trscgr = dbgroup (trace, groupfields, 0, 1);
	freetbl (groupfields, free);
	if (trscgr->database == dbINVALID) {
		elog_log(0, "grtr_sc_group: dbgroup() error.\n");
		return (-1);
	}

	/* Normal exit. */

	return (0);
}

/*
 * NAME
 *	grtr_sc_getstachan - Get the station-channel value
 *
 * SYNOPSIS
 *	int
 *	grtr_sc_getstachan (trscgr, record, sta, chan, nsegs, time, endtime)
 *
 *	Dbptr               trscgr;
 *	int			    record;
 *	char *				    sta;
 *	char *				         chan;
 *	int * 					       nsegs;
 *	double *                                              time;
 *	double *                                                    endtime;
 *
 * DESCRIPTION
 * ARGUMENTS
 *	Dbptr		trscgr		= (i) Input database station-channel group (from grtr_sc_create()).
 *	int		record		= (i) Record index. If < 0, then use trsc.record
 *	char *		star		= (o) Station code.
 *	char *		chan		= (o) Channel code.
 *	int *		nsegs		= (o) Number of waveform segments.
 *	double *	time		= (o) Overall start time;
 *	double *	endtime		= (o) Overall end time;
 *
 * RETURNS
 *	0 if OK or -1 if ERROR.
 */

int
grtr_sc_getstachan (trscgr, record, sta, chan, nsegs, time, endtime)

Dbptr               trscgr;
int			    record;
char *				    sta;
char *				         chan;
int *                                          nsegs;
double *                                              time;
double *                                                    endtime;

{
	Dbptr db;
	int is, ie;

	if (record >= 0) trscgr.record = record;
	if (dbgetv (trscgr, 0, "sta", sta, "chan", chan, "bundle", &db, 0) == dbINVALID) {
        	elog_log(0, "grtr_sc_getstachan: dbgetv() error.\n");
        	return (-1);
	}
        dbget_range (db, &is, &ie);
	*nsegs = ie - is;
        db.record = is;
	dbgetv (db, 0, "time", time, 0);
        db.record = ie-1;
	dbgetv (db, 0, "endtime", endtime, 0);

	/* Normal exit. */

	return (0);
}

/*
 * NAME
 *	grtr_s_group - Create Station trace grouping
 *
 * SYNOPSIS
 *	int
 *	grtr_s_group (trscgr, trsgr)
 *
 *	Dbptr         trscgr;
 *	Dbptr *               trsgr;
 *
 * DESCRIPTION
 * ARGUMENTS
 *	Dbptr		trscgr		= (i) Input trace station-channel group (from grtr_sc_create()).
 *	Dbptr *		trsgr		= (o) Output trace station group
 *
 * RETURNS
 *	0 if OK or -1 if ERROR.
 */

int
grtr_s_group (trscgr, trsgr)

Dbptr         trscgr;
Dbptr *               trsgr;

{
	Tbl *groupfields;

	groupfields = newtbl (1);
	if (groupfields == NULL) {
		elog_log(0, "grtr_s_group: newtbl() error.\n");
		return (-1);
	}
	settbl (groupfields, 0, strdup("sta"));
	*trsgr = dbgroup (trscgr, groupfields, 0, 2);
	freetbl (groupfields, free);
	if (trsgr->database == dbINVALID) {
		elog_log(0, "grtr_s_group: dbgroup() error.\n");
		return (-1);
	}

	/* Normal exit. */

	return (0);
}

/*
 * NAME
 *	grtr_s_getsta - Get the station value
 *
 * SYNOPSIS
 *	int
 *	grtr_s_getsta (trsgr, record, sta, nchans, nsegs, time, endtime)
 *
 *	Dbptr          trsgr;
 *	int		      record;
 *	char *			      sta;
 *	int *				   nchans;
 *	int *                                      nsegs;
 *	double *                                          time;
 *	double *                                                endtime;
 *
 * DESCRIPTION
 * ARGUMENTS
 *	Dbptr		trsgr		= (i) Input trace station group (from grtr_s_group()).
 *	int		record		= (i) Record index. If < 0, then use dbsc.record
 *	char *		sta		= (o) Station code.
 *	int *		nchans		= (o) Number of channels.
 *	int *		nsegs		= (o) Total number of waveform segments.
 *	double *	time		= (o) Overall start time;
 *	double *	endtime		= (o) Overall end time;
 *
 * RETURNS
 *	0 if OK or -1 if ERROR.
 */

int
grtr_s_getsta (trsgr, record, sta, nchans, nsegs, time, endtime)

Dbptr          trsgr;
int		      record;
char *			      sta;
int *				   nchans;
int *                                      nsegs;
double *                                          time;
double *                                                endtime;

{
	Dbptr db;
	int is, ie, i;
	char chan[32];
	int n;
	double ts, te;

	if (record >= 0) trsgr.record = record;
	if (dbgetv (trsgr, 0, "sta", sta, "bundle", &db, 0) == dbINVALID) {
        	elog_log(0, "grtr_s_getsta: dbgetv() error.\n");
        	return (-1);
	}
        dbget_range (db, &is, &ie);
	*nchans = ie - is;
	*nsegs = 0;
	*time = 0.0;
	*endtime = 0.0;
	for (i=is; i<ie; i++) {
		if (grtr_sc_getstachan (db, i, sta, chan, &n, &ts, &te)) {
			elog_log(0, "grtr_s_getsta: grtr_sc_getstachan() error.\n");
			return (-1);
		}
		*nsegs += n;
		if (*time == 0.0 && *endtime == 0.0) {
			*time = ts;
			*endtime = te;
		} else {
			if (ts < *time) *time = ts;
			if (te > *endtime) *endtime = te;
		}
	}

	/* Normal exit. */

	return (0);
}

/*
 * NAME
 *	getchannel - Retrieve a single channel of waveform data
 *
 * SYNOPSIS
 *	int
 *	getchannel (dbsc, net, sta, chan, tstart, tend, 
 *		    gap, calib, rotate, strike, dip, filter,
 *		    nsegs, trace)
 *
 *	Dbptr       dbsc;
 *	char *            net;
 *	char *                 sta;
 *	char *                      chan;
 *	double                            tstart, tend;
 *	char *      gap;
 *	int              calib;
 *	char *                  rotate;
 *	double                          strike, dip;
 *	char *                                       filter;
 *	int *       nsegs;
 *	Dbptr *            trace;
 *
 * DESCRIPTION
 * ARGUMENTS
 *	Dbptr		dbsc		= (i) Input database station-channel view (from grdb_sc_loadcss()).
 *	char *		net		= (i) Network code. If NULL,
 *					      then don't do network sifting.
 *	char *		sta		= (i) Station code. If NULL,
 *					      then don't do station sifting.
 *	char *		chan		= (i) Channel code expression. If NULL,
 *					      then don't do channel sifting.
 *					      The channel expression should correspond to
 *					      a single channel when rotate == 0. When
 *					      rotate == 1, the channel expression should
 *					      correspond to the (usually) three orthogonal
 *					      channels that will be used in the rotation
 *					      transformation.
 *	double		tstart		= (i) Start epoch time.
 *	double		tend		= (i) End epoch time. If tstart = tend = 0.0,
 *					      then don't time subset.
 *	char *		gap		= (i) What to do with data gaps. 
 *					      NULL, same as "segment"
 *					      "seg"     - Remove internally flagged data gap
 *							  values and replace with segments.
 *					      "interp"  - Remove internally flagged data gap
 *							  values and linear interpolate across
 *							  all data gaps.
 *					      "zero"    - Remove internally flagged data gap
 *							  values and replace all data gaps
 *							  with zero value.
 *					      "drop"    - Remove internally flagged data gap
 *							  values and drop the channel is any
 *							  gaps exist.
 *					      "leave"   - Leave all internally flagged gap
 *							  values and true data gaps as is.
 *	int		calib		= (i) Apply calib flag. If set, then apply calib
 *					      to the data after reading.
 *	char *		rotate		= (i) Coordinate rotation flag. If this is NULL, then
 *					      no coordinate rotation is done. If non-NULL,
 *					      then this should point to a NULL terminated
 *					      character string that specifies the channel
 *					      code for the rotated channel.
 *	double		strike		= (i) The strike angle in degrees for the rotation.
 *					      This is measured like an azimuth positive
 *					      clockwise from north.
 *	double		dip		= (i) The dip angle in degrees for the rotation.
 *					      This is measured like a nromal dip angle
 *					      with 0.0 pointing horizontally along the strike,
 *					      90.0 pointing straight down and -90.0
 *					      pointing straight up.
 *	char *		filter		= (i) An on-the-fly filter specification. If NULL, then
 *					      no on-the-fly filtering is done.
 *	int *		nsegs		= (o) Number of waveform segments.
 *	Dbptr *		trace		= (i/o) This is the returned trace table corresponding
 *					      to this request. If trace->table is set to
 *					      dbINVALID on input, then a new trace database
 *					      is created, otherwise the new channel is appended
 *					      to the end of an existing trace table.
 *					      On return, trace->record is set to the first
 *					      record of the returned channel and trace->field
 *					      is set to the last record of the returned channel
 *					      plus one (ala a bundle in dbget_range(3)).
 *
 * RETURNS
 *	0 if OK or -1 if ERROR.
 */

int
getchannel (dbsc, net, sta, chan, tstart, tend, 
	    gap, calib, rotate, strike, dip, filter,
	    nsegs, trace)

Dbptr       dbsc;
char *            net;
char *                 sta;
char *                      chan;
double                            tstart, tend;
char *      gap;
int              calib;
char *                  rotate;
double                          strike, dip;
char *                                       filter;
int *       nsegs;
Dbptr *            trace;

{
	char nete[32], stae[32], chane[32];
	char neti[32], stai[32], chani[32];
	char net2[32], sta2[32], chan2[32];
	int i, n, nchans, n0;
	int is[6], ie[6];
	double hang, vang;

	/* Read in the data */

	if (net) strcpy (nete, net); else strcpy (nete, "NULL");
	if (sta) strcpy (stae, sta); else strcpy (stae, "NULL");
	if (chan) strcpy (chane, chan); else strcpy (chane, "NULL");
	n0 = 0;
	if (trace->table != dbINVALID) {
		dbquery (*trace, dbRECORD_COUNT, &n0);
	}
	if (grtr_sc_create (dbsc, net, sta, chan, tstart, tend, 
				gap, calib, 0, trace) < 0) {
		elog_log(0, "getchannel: grtr_sc_create(%s, %s, %s) error.\n", nete, stae, chane);
		return (-1);
	}

	/* Reduce to single netsta */

	dbquery (*trace, dbRECORD_COUNT, &n);
	strcpy (net2, "");
	strcpy (sta2, "");
	for (trace->record=n0; trace->record < n; (trace->record++)) {
		if (dbgetv (*trace, 0, "net", neti, "sta", stai, 
				0) == dbINVALID) {
        		elog_log(0, "getchannel: dbgetv() error.\n");
        		return (-1);
		}
		if (strcmp(stai, sta2) || strcmp(neti, net2)) {
			if (trace->record > n0) break;
			strcpy (net2, neti);
			strcpy (sta2, stai);
		}
	}
	if (trace->record < n) {
		for (; trace->record < n; (trace->record++)) {
			trfree (*trace);
		}
		dbcrunch (*trace);
	}

	/* Check out channels */

	if (rotate) {
		dbquery (*trace, dbRECORD_COUNT, &n);
		strcpy (chan2, "");
		nchans = 0;
		for (trace->record=n0; trace->record < n; (trace->record++)) {
			if (dbgetv (*trace, 0, "chan", chani, 0) == dbINVALID) {
        			elog_log(0, "getchannel: dbgetv() error.\n");
        			return (-1);
			}
			if (strcmp(chani, chan2)) {
				if (nchans > 0) ie[nchans-1] = trace->record;
				nchans++;
				if (nchans > 6) break;
				is[nchans-1] = trace->record;
				strcpy (chan2, chani);
			}
		}
		ie[nchans-1] = n;

		if (nchans < 2 || nchans > 3) {
			elog_log(0, "getchannel: Cannot rotate with %d input channels.\n", nchans);
			return (-1);
		}
	} else {
		dbquery (*trace, dbRECORD_COUNT, &n);
		strcpy (chan2, "");
		for (trace->record=n0; trace->record < n; (trace->record++)) {
			if (dbgetv (*trace, 0, "chan", chani, 0) == dbINVALID) {
        			elog_log(0, "getchannel: dbgetv() error.\n");
        			return (-1);
			}
			if (strcmp(chani, chan2)) {
				if (trace->record > 0) break;
				strcpy (chan2, chani);
			}
		}
		if (trace->record < n) {
			for (; trace->record < n; (trace->record++)) {
				trfree (*trace);
			}
			dbcrunch (*trace);
		}
	}

	/* Coordinate rotation */

	if (rotate) {
		double cdip, sdip, cstr, sstr;
		double chang, shang, cvang, svang;
		double re, rn, rz, ct, cmax;
		double cthet[3];
		double ue[3], un[3], uz[3];
		char chanr[3][32];
		int imax;
		int nsamp;
		float *data;
		int iso, ieo;
		Dbptr dbo, dbi;

		/* Grab the orientations */

		dip *= M_PI/180.0;
		strike *= M_PI/180.0;
		cdip = cos(dip);
		sdip = sin(dip);
		cstr = cos(strike);
		sstr = sin(strike);
		dip *= 180.0/M_PI;
		strike *= 180.0/M_PI;
		re = cdip*sstr;
		rn = cdip*cstr;
		rz = -sdip;
		cmax = 0.0;
		imax = 0;
		for (i=0; i<nchans; i++) {
			trace->record=is[i];
			if (dbgetv (*trace, 0, "hang", &hang, "vang", &vang, "chan", chanr[i], 0) == dbINVALID) {
				elog_log(0, "getchannel: dbgetv() error.\n");
				return (-1);
			}
			hang *= M_PI/180.0;
			vang *= M_PI/180.0;
			chang = cos(hang);
			shang = sin(hang);
			cvang = cos(vang);
			svang = sin(vang);
			ue[i] = svang*shang;
			un[i] = svang*chang;
			uz[i] = cvang;
			cthet[i] = re*ue[i] + rn*un[i] + rz*uz[i];
			if (ABS(cthet[i]) > cmax) {
				imax = i;
				cmax = ABS(cthet[i]);
			}
		}

		/* Check for orthogonality */

		ct = ue[0]*ue[1] + un[0]*un[1] + uz[0]*uz[1];
		ct = ABS(ct);
		if (ct > 1.e-3) {
			elog_log(0, "getchannel: Channels %s and %s are not orthogonal.\n",
							chanr[0], chanr[1]);
			return (-1);
		}
		if (nchans == 3) {
			ct = ue[2]*ue[1] + un[2]*un[1] + uz[2]*uz[1];
			ct = ABS(ct);
			if (ct > 1.e-3) {
				elog_log(0, "getchannel: Channels %s and %s are not orthogonal.\n",
								chanr[1], chanr[2]);
				return (-1);
			}
			ct = ue[2]*ue[0] + un[2]*un[0] + uz[2]*uz[0];
			ct = ABS(ct);
			if (ct > 1.e-3) {
				elog_log(0, "getchannel: Channels %s and %s are not orthogonal.\n",
								chanr[0], chanr[2]);
				return (-1);
			}
		}

		/* Set up the reference channel */

		iso = -1;
		trace->field = dbALL;
		for (trace->record=is[imax]; trace->record < ie[imax]; (trace->record++)) {
			float *datao;
			Dbptr db;

			if (dbget (*trace, 0) == dbINVALID) {
				elog_log(0, "getchannel: dbget() error.\n");
				return (-1);
			}
			db = *trace;
			db.record = dbSCRATCH;
			if (dbgetv (db, 0, "nsamp", &nsamp, "data", &data, 0) == dbINVALID) {
				elog_log(0, "getchannel: dbgetv() error.\n");
				return (-1);
			}
			datao = (float *) malloc (nsamp*sizeof(float));
			if (datao == NULL) {
				elog_log(1, "getchannel: malloc() error.\n");
				return (-1);
			}
			dbputv (db, 0, "chan", rotate, "datatype", "t4", 
					"hang", strike, "vang", dip+90.0, "data", datao, 0);
			for (i=0; i<nsamp; i++) datao[i] = data[i]*cthet[imax];
			i = dbadd (db, 0);
			if (i == dbINVALID) {
				elog_log(0, "getchannel: dbadd() error.\n");
				return (-1);
			}
			if (iso < 0) iso = i;
			ieo = i+1;
		}

		/* Accumulate from the other channels */

		dbo = *trace;
		dbo.record = iso;
		dbo.field = ieo;
		dbi = *trace;
		for (i=0; i<nchans; i++) {
			if (i != imax && ABS(cthet[i]) > 1.e-10) {
				dbi.record = is[i];
				dbi.field = ie[i];
				traccumulate (dbo, dbi, cthet[i]);
			}
			for (trace->record=is[i]; trace->record < ie[i]; (trace->record++)) {
				trfree (*trace);
			}
		}
		dbcrunch (*trace);
		trsplit (*trace, 0, 0);
	}

	/* Filter */

	/* Normal exit */

	dbquery (*trace, dbRECORD_COUNT, nsegs);
	trace->record = n0;
	trace->field = *nsegs;
	*nsegs -= n0;
	return (0);
}

/*
 * NAME
 *	getsegment - Retrieve a waveform segment from a single channel of waveform data
 *
 * SYNOPSIS
 *	int
 *	getsegment (trace, iseg, tstart, dt, nsamp, data)
 *
 *	Dbptr       trace;
 *	int                iseg;
 *	double *                 tstart;
 *	double *                         dt;
 *	int *                                nsamp;
 *	float **                                    data;
 *
 * DESCRIPTION
 * ARGUMENTS
 *	Dbptr 		trace		= (i) Input data channel trace (returned from getchannel()).
 *	int		iseg		= (i) Segment index. 
 *	double *	tstart		= (o) Epoch time of first sample.
 *	double *	dt		= (o) Data sample increment in seconds.
 *	int *		nsamp		= (o) Number of samples.
 *	float **	data		= (o) Pointer to data.
 *
 * RETURNS
 *	0 if OK, or -1 if ERROR.
 */

int
getsegment (trace, iseg, tstart, dt, nsamp, data)

Dbptr       trace;
int                iseg;
double *                 tstart;
double *                         dt;
int *                                nsamp;
float **                                    data;

{
	double samprate;

	if (iseg >= 0) trace.record += iseg;
	if (dbgetv (trace, 0, "time", tstart, "nsamp", nsamp,
				"samprate", &samprate, "data", data, 0) == dbINVALID) {
		elog_log(0, "getsegment: dbgetv() error.\n");
		return (-1);
	}
	*dt = 1.0 / samprate;

	/* Normal exit */

	return (0);
}    

int traccumulate (Dbptr tro, Dbptr tri, double factor)

{
	int iso, ieo, isi, iei;
	int i, j, nsamp, nsampi;
	double time, t, endtimei, timei, samprate, dt, dti;
	float *data, *datai;

        dbget_range (tro, &iso, &ieo);
        dbget_range (tri, &isi, &iei);
	tri.record = isi;
	dbgetv (tri, 0, "time", &timei, "samprate", &samprate,
			"nsamp", &nsampi, "data", &datai, 0);
	dti = 1.0 / samprate;
	endtimei = timei + (nsampi-1)*dti;
	for (tro.record=iso; tro.record<ieo; tro.record++) {
		dbgetv (tro, 0, "time", &time, "samprate", &samprate,
				"nsamp", &nsamp, "data", &data, 0);
		dt = 1.0 / samprate;
		for (i=0,t=time; i<nsamp; i++,t+=dt) {
			if (data[i] > 1.e30) continue;
			while (t > endtimei) {
				if (tri.record >= iei-1) break;
				(tri.record)++;
				dbgetv (tri, 0, "time", &timei, "samprate", &samprate,
						"nsamp", &nsampi, "data", &datai, 0);
				dti = 1.0 / samprate;
				endtimei = timei + (nsampi-1)*dti;
			}
			if (t > endtimei || t < timei-0.1*dti) {
				data[i] = 2.e30;
				continue;
			}
			j = INT ((t-timei),dti);
			data[i] += factor*datai[j];
		}
	}
}

/* $Id$ */
