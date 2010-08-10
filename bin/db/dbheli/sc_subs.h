
#include <stdio.h>

#include "db.h"
#include "scv2.h"
#include "arrays.h"
#include "trace_subs.h"

typedef struct stachan_ {
	Dbptr db;
	long recstart;
	long recend;
} Stachan;

extern int make_scs (Dbptr db, char *stai, char *chani, double tstart, double tend, Tbl **scs);
extern int get_sc_stachan (Stachan *sc, char *sta, char *chan);
extern Trace *read_sc (Stachan *sc, double tstart, double tend);
