
#include <stdio.h>

#include "db.h"
#include "scv2.h"
#include "arrays.h"

#define	SC_WFDISC	0
#define	SC_STGRID	1

typedef struct stachan_ {
	Dbptr db;
	int type;
	int recstart;
	int recend;
} Stachan;

Trace *add_trace();

int
make_scs (db, stai, chani, stgrid, tstart, tend, scs)

Dbptr     db;
char *        stai;
char *              chani;
int                        stgrid;
double                             tstart;
double                                     tend;
Tbl **                                           scs;

{
	static char string[1024];
	Dbptr dbi;
	int n;
	Tbl *fields;
	char sta[32], chan[32];
	char staold[32], chanold[32];
	Stachan *sc;

	*scs = NULL;
	*scs = newtbl(0);
	if ((*scs) == NULL) {
		clear_register (1);
		fprintf (stderr, "make_scs: newtbl() error.\n");
		return (0);
	}

        /* Subset and sort input database */

	if (stgrid) {
        	db = dblookup (db, 0, "stgrid", 0, 0);
	} else {
        	db = dblookup (db, 0, "wfdisc", 0, 0);
	}
 	if (chani[0] == '/') {
        	sprintf (string, "( endtime >= %.5f && time <= %.5f && sta == \"%s\" && chan =~ %s )", tstart, tend, stai, chani);
 	} else {
        	sprintf (string, "( endtime >= %.5f && time <= %.5f && sta == \"%s\" && chan == \"%s\" )", tstart, tend, stai, chani);
	}
        dbi = dbsubset (db, string, 0);
        dbquery (dbi, dbRECORD_COUNT, &n);
        if (n < 1) {
                fprintf (stderr, "make_scs: No data to process.\n");
                return (0);
        }
        dbquery (dbi, dbPRIMARY_KEY, &fields);
        dbi = dbsort (dbi, fields, 0, 0);

        /* Go through sorted table and form station-channel structures */

	strcpy (staold, "");
	strcpy (chanold, "");
        for (dbi.record=0; dbi.record<n; dbi.record++) {
        	dbgetv (dbi, 0,	"sta", sta,
        			"chan", chan,
        			NULL);
		if (strcmp(sta, staold) || strcmp(chan, chanold)) {
			sc = (Stachan *) malloc (sizeof(Stachan));
			if (sc == NULL) {
				fprintf (stderr, "make_scs: Malloc error.\n");
				return (0);
			}
			sc->db = dbi;
			sc->recstart = dbi.record;
			sc->type = stgrid;
			if (settbl ((*scs), -1, sc) < 0) {
				clear_register (1);
				fprintf (stderr, "make_scs: settbl() error.\n");
				return (0);
			}
		}
		strcpy (staold, sta);
		strcpy (chanold, chan);
		sc->recend = dbi.record;
        }

	/* Normal exit */

	return (1);
}

int
make_scs2 (db, stai, chani, tstart, tend, scs)

Dbptr     db;
char *        stai;
char *              chani;
double                     tstart;
double                             tend;
Tbl **                                   scs;

{
	static char string[1024];
	Dbptr dbi;
	int n;
	Tbl *fields;
	char sta[32], chan[32];
	char staold[32], chanold[32];
	Stachan *sc;

	*scs = NULL;
	*scs = newtbl(0);
	if ((*scs) == NULL) {
		clear_register (1);
		fprintf (stderr, "make_scs: newtbl() error.\n");
		return (0);
	}

        /* Subset and sort input database */
 
        db = dblookup (db, 0, "wfdisc", 0, 0);
        if (chani[0] == '(') {
        	sprintf (string, "( endtime >= %.5f && time <= %.5f && sta == \"%s\" && %s )", tstart, tend, stai, chani);
	} else {
        	sprintf (string, "( endtime >= %.5f && time <= %.5f && sta == \"%s\" && chan == \"%s\" )", tstart, tend, stai, chani);
	}
        dbi = dbsubset (db, string, 0);
        dbquery (dbi, dbRECORD_COUNT, &n);
        if (n < 1) {
                return (0);
        }
        dbquery (dbi, dbPRIMARY_KEY, &fields);
        dbi = dbsort (dbi, fields, 0, 0);

        /* Go through sorted table and form station-channel structures */

	strcpy (staold, "");
	strcpy (chanold, "");
        for (dbi.record=0; dbi.record<n; dbi.record++) {
        	dbgetv (dbi, 0,	"sta", sta,
        			"chan", chan,
        			NULL);
		if (strcmp(sta, staold) || strcmp(chan, chanold)) {
			sc = (Stachan *) malloc (sizeof(Stachan));
			if (sc == NULL) {
				fprintf (stderr, "make_scs: Malloc error.\n");
				return (0);
			}
			sc->db = dbi;
			sc->type = SC_WFDISC;
			sc->recstart = dbi.record;
			if (settbl ((*scs), -1, sc) < 0) {
				clear_register (1);
				fprintf (stderr, "make_scs: settbl() error.\n");
				return (0);
			}
		}
		strcpy (staold, sta);
		strcpy (chanold, chan);
		sc->recend = dbi.record;
        }

	/* Normal exit */

	return (1);
}

int
get_sc_stachan (sc, sta, chan)

Stachan *       sc;
char *              sta;
char *                   chan;

{
	sc->db.record = sc->recstart;
	dbgetv (sc->db, 0, "sta", sta, "chan", chan, NULL);

	/* Normal exit */

	return (1);
}


int
read_sc (sc, tstart, tend, tr1, tr2)

Stachan *sc;
double       tstart;
double               tend;
Trace **                   tr1;
Trace **                        tr2;

{
	Trace *trace, *trace2;

	trace = NULL;
	trace2 = NULL;
	*tr1 = NULL;
	*tr2 = NULL;
	for (sc->db.record=sc->recstart; sc->db.record<=sc->recend; (sc->db.record)++) {
		if (!add_trace (sc->db, sc->type, tstart, tend, trace, &trace, trace2, &trace2)) {
			fprintf (stderr, "read_sc: add_trace() error.\n");
			return (0);
		}
	}
	if (!trace) return (NULL);
	trace = (Trace *) SCV_trace_glue (trace);
	trace = (Trace *) SCV_trace_tofloat (trace, 0);
	*tr1 = trace;
	if (trace2) {
		trace2 = (Trace *) SCV_trace_glue (trace2);
		trace2 = (Trace *) SCV_trace_tofloat (trace2, 0);
		*tr2 = trace2;
	}
	return (1);
}

/* $Id$ */
