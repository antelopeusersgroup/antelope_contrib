#include <stdio.h>

#include "db.h"
#include "scv2.h"
#include "arrays.h"

typedef struct seg_ {
	int record;
	double time;
	double endtime;
} Seg;

typedef struct stachan_ {
	Dbptr db;
	int recstart;
	int recend;
	Tbl *segs;
} Stachan;

Trace *add_trace();

int
make_scs (db, tstart, tend, scs)

Dbptr     db;
double        tstart;
double                tend;
Tbl **                      scs;

{
	char string[1024];
	Dbptr dbi;
	int n;
	Tbl *fields;
	char sta[32], chan[32];
	char staold[32], chanold[32];
	Stachan *sc;
	double time, endtime;
	Seg *seg;

	*scs = NULL;
	*scs = newtbl(0);
	if ((*scs) == NULL) {
		elog_clear_register(1);
		fprintf (stderr, "make_scs: newtbl() error.\n");
		return (0);
	}

        /* Subset and sort input database */
 
        db = dblookup (db, 0, "wfdisc", 0, 0);
        sprintf (string, "( endtime >= %.5f && time <= %.5f )", tstart, tend);
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
        			"time", &time,
        			"endtime", &endtime,
        			0);
		if (strcmp(sta, staold) || strcmp(chan, chanold)) {
			sc = (Stachan *) malloc (sizeof(Stachan));
			if (sc == NULL) {
				fprintf (stderr, "make_scs: Malloc error.\n");
				return (0);
			}
			sc->db = dbi;
			sc->recstart = dbi.record;
			if (settbl ((*scs), -1, sc) < 0) {
				elog_clear_register(1);
				fprintf (stderr, "make_scs: settbl() error.\n");
				return (0);
			}
			sc->segs = newtbl(10);
			if (sc->segs == NULL) {
				fprintf (stderr, "make_scs: newstbl() error.\n");
				return (0);
			}
		}
		strcpy (staold, sta);
		strcpy (chanold, chan);
		sc->recend = dbi.record;
		seg = (Seg *) malloc (sizeof(Seg));
		if (seg == NULL) {
			fprintf (stderr, "make_scs: Malloc error.\n");
			return (0);
		}
		seg->time = time;
		seg->endtime = endtime;
		seg->record = dbi.record;
		if (settbl(sc->segs, -1, seg) < 0) {
			fprintf (stderr, "make_scs: settbl() error.\n");
			return (0);
		}
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
	dbgetv (sc->db, 0, "sta", sta, "chan", chan, 0);

	/* Normal exit */

	return (1);
}


Trace *
read_sc (sc, tstart, tend)

Stachan *sc;
double       tstart;
double               tend;

{
	int n, i;
	Seg *seg;
	Trace *trace;

	trace = NULL;
	n = maxtbl(sc->segs);
	for (i=0; i<n; i++) {
		seg = (Seg *) gettbl (sc->segs, i);
		if (tstart > seg->endtime) continue;
		if (tend < seg->time) break;
		sc->db.record = seg->record;
		if (!add_trace (sc->db, tstart, tend, trace, &trace)) {
			fprintf (stderr, "read_sc: add_trace() error.\n");
			return (NULL);
		}
	}
	if (!trace) return (NULL);
	trace = (Trace *) SCV_trace_glue (trace);
	trace = (Trace *) SCV_trace_tofloat (trace, 0);
	return (trace);
}
