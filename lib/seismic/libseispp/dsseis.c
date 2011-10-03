
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "db.h"
#include "tr.h"
#include "response.h"
#include "arrays.h"

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

int grdb_sc_loadcss (Dbptr dbin, char *net_expr, char *sta_expr,
        char *chan_expr, double tstart, double tend, 
        int coords, int ir, int orient, Dbptr *dbscgr, Dbptr *dbsc)
{
	Dbptr dbout, db, dbout2;
	char string[1024];
	char string2[1024];
	char sta_wfdisc[32], chan_wfdisc[32];
	long i, j, n, sensor=0, ok;
	Tbl *pat1, *pat2;
	Tbl *sortfields, *groupfields;
	FILE *file;
	Response *resp;
	int is_view=0;
	Dbptr db_to_clear;

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
		is_view=1;
	}
        dbquery (dbout, dbRECORD_COUNT, &n);
        if (n < 1) {
		elog_log(0, "grdb_sc_loadcss: No wfdisc rows to process.\n");
		return (-1);
        }

        /* Make the necessary joins and check for completeness. */

        if (coords) {
        	db = dblookup (dbin, 0, "site", 0, 0);
		if(is_view)db_to_clear=dbout;
        	dbout = dbjoin (dbout, db, 0, 0, 1, 0, 0);
		if(is_view) dbfree(db_to_clear);
		is_view=1;
		
        	dbquery (dbout, dbRECORD_COUNT, &n);
        	if (n < 1) {
			elog_log(0, "grdb_sc_loadcss: No data rows to process.\n");
			return (-1);
        	}
        	for (dbout.record=0; dbout.record<n; dbout.record++) {
        		if (dbgetv (dbout, 0, "wfdisc.sta", sta_wfdisc,
        				"wfdisc.chan", chan_wfdisc,
        				"site.sta", string, NULL ) == dbINVALID) {
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
		if(is_view)db_to_clear=dbout;
        	dbout = dbjoin (dbout, db, 0, 0, 1, 0, 0);
		if(is_view) dbfree(db_to_clear);
		is_view=1;
        	dbquery (dbout, dbRECORD_COUNT, &n);
        	if (n < 1) {
			elog_log(0, "grdb_sc_loadcss: No data rows to process.\n");
			return (-1);
        	}
        	for (dbout.record=0; dbout.record<n; dbout.record++) {
        		if (dbgetv (dbout, 0, "wfdisc.sta", sta_wfdisc,
        				"wfdisc.chan", chan_wfdisc,
        				"sensor.sta", string, NULL ) == dbINVALID) {
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
		if(is_view)db_to_clear=dbout;
        	db = dblookup (dbin, 0, "instrument", 0, 0);
        	dbout = dbjoin (dbout, db, 0, 0, 1, 0, 0);
		if(is_view) dbfree(db_to_clear);
		is_view=1;
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
        				"instrument.inid", &i, NULL ) == dbINVALID) {
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
		if(is_view)db_to_clear=dbout;
        	db = dblookup (dbin, 0, "sitechan", 0, 0);
        	dbout2 = dbjoin (dbout, db, 0, 0, 1, 0, 0);
		is_view=1;
        	dbquery (dbout2, dbRECORD_COUNT, &n);
        	if (n < 1) {
        		ok = 0;
        	} else {
        		for (dbout2.record=0; dbout2.record<n; dbout2.record++) {
        			dbgetv (dbout2, 0, "wfdisc.sta", sta_wfdisc,
        				"wfdisc.chan", chan_wfdisc,
        				"sitechan.sta", string, NULL );
        			if (strcmp(string, sta_wfdisc)) {
        				ok = 0;
        				break;
        			}
        		}
		}
		if (ok) {
			dbout = dbout2;
			if(is_view) dbfree(db_to_clear);
		} else {
			if (!sensor) {
        			db = dblookup (dbin, 0, "sensor", 0, 0);
				if(is_view)db_to_clear=dbout;
	       			dbout = dbjoin (dbout, db, 0, 0, 1, 0, 0);
				if(is_view) 
				{
					dbfree(dbout2);
					dbfree(db_to_clear);
				}
				is_view=1;
        			dbquery (dbout, dbRECORD_COUNT, &n);
        			if (n < 1) {
					elog_log(0, "grdb_sc_loadcss: No data rows to process.\n");
					return (-1);
        			}
        			for (dbout.record=0; dbout.record<n; dbout.record++) {
        				if (dbgetv (dbout, 0, "wfdisc.sta", sta_wfdisc,
        						"wfdisc.chan", chan_wfdisc,
        						"sensor.sta", string, NULL ) == dbINVALID) {
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
			if(is_view)db_to_clear=dbout;
        		settbl (pat1, 0, strdup("sensor.chanid"));
        		settbl (pat2, 0, strdup("sitechan.chanid"));
        		dbout = dbjoin (dbout, db, &pat1, &pat2, 1, 0, 0);
			if(is_view) dbfree(db_to_clear);
			is_view=1;
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
        					"sitechan.sta", string, NULL ) == dbINVALID) {
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

	if(is_view)db_to_clear=dbout;
	sortfields = newtbl (3);
	if (sortfields == NULL) {
		elog_log(0, "grdb_sc_loadcss: newtbl() error.\n");
		return (-1);
	}
	settbl (sortfields, 0, strdup("wfdisc.sta"));
	settbl (sortfields, 1, strdup("wfdisc.chan"));
	settbl (sortfields, 2, strdup("wfdisc.time"));
        *dbsc = dbsort (dbout, sortfields, 0, 0);
	if(is_view) dbfree(db_to_clear);
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

int grdb_sc_getstachan(Dbptr dbscgr, int record, char *sta, char *chan,
        int *nsegs, double *time, double *endtime)
{
	Dbptr db;
	long is, ie;

	if (record >= 0) dbscgr.record = record;
	if (dbgetv (dbscgr, 0, "sta", sta, "chan", chan, "bundle", &db, NULL ) == dbINVALID) {
        	elog_log(0, "grdb_sc_getstachan: dbgetv() error.\n");
        	return (-1);
	}
        dbget_range (db, &is, &ie);
	*nsegs = (int)(ie - is);
        db.record = is;
	dbgetv (db, 0, "time", time, NULL );
        db.record = ie-1;
	dbgetv (db, 0, "endtime", endtime, NULL );

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
 *      int 		ir		= (i) Load response if nonzero.
 *	Dbptr *		trscgr		= (o) Output trace table database
 *
 * RETURNS
 *	0 if OK or -1 if ERROR.
 */

int grtr_sc_create(Dbptr dbsc, char *net_expr, char *sta_expr, 
        char *chan_expr, double tstart, double tend, char *gap,
        int calib, int ir, Dbptr *trscgr)
{
	char time_str[100];
	char endtime_str[100];
	long ret, n, n2, i;
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
			if (dbgetv (dbsc, 0, "time", &time, "endtime", &endtime, NULL ) == dbINVALID) {
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
        ret = trload_css (dbsc, time_str, endtime_str, trscgr, "wfdisc", 0);

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
					NULL ) == dbINVALID) {
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

	if (ir )
	{
	/* Run this section if instrument response is to be loaded */
	    dbsc.record = 0;
	    if (dbgetv (dbsc, 0, "instrument.inid", &i, NULL ) != dbINVALID && i >= 0) {
		dbquery (*trscgr, dbRECORD_COUNT, &n);
		dbquery (dbsc, dbRECORD_COUNT, &n2);
		for (trscgr->record=0; trscgr->record<n; (trscgr->record)++) {
			if (dbgetv (*trscgr, 0, "sta", sta, "chan", chan,
					"time", &time, NULL ) == dbINVALID) {
        			elog_log(0, "grtr_sc_create: dbgetv() error.\n");
				if (new_view) dbfree (dbsc);
        			return (-1);
			}
			for (dbsc.record=0; dbsc.record<n2; dbsc.record++) {
				if (dbgetv (dbsc, 0, "sta", sta2, "chan", chan2,
					"time", &time2, "endtime", &endtime2, NULL ) == dbINVALID) {
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
				dbputv (*trscgr, 0, "response", resp, NULL );
				break;
			}
		}
	    }
	}
	elog_clear_register(0);
	if (new_view) dbfree (dbsc);

	/* Normal exit. */

	return (0);
}
