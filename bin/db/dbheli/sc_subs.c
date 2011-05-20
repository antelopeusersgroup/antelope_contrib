
#include "sc_subs.h"

int
make_scs (Dbptr db, char *stai, char *chani, double tstart, double tend, Tbl **scs)

{
	static char string[1024];
	Dbptr dbi;
	long n;
	Tbl *fields;
	char sta[32], chan[32];
	char staold[32], chanold[32];
	Stachan *sc;

	*scs = NULL;
	*scs = newtbl(0);
	if ((*scs) == NULL) {
		elog_clear_register(1);
		fprintf (stderr, "make_scs: newtbl() error.\n");
		return (0);
	}

        /* Subset and sort input database */
 
        db = dblookup (db, 0, "wfdisc", 0, 0);
        sprintf (string, "( endtime >= %.5f && time <= %.5f && sta == \"%s\" && chan == \"%s\" )", tstart, tend, stai, chani);
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
			if (settbl ((*scs), -1, sc) < 0) {
				elog_clear_register(1);
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
get_sc_stachan (Stachan *sc, char *sta, char *chan)

{
	dbgetv (sc->db, 0, "sta", sta, "chan", chan, NULL);

	/* Normal exit */

	return (1);
}


Trace *
read_sc (Stachan *sc, double tstart, double tend)

{
	Trace *trace;

	trace = NULL;
	for (sc->db.record=sc->recstart; sc->db.record<=sc->recend; (sc->db.record)++) {
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

/* $Id$ */
