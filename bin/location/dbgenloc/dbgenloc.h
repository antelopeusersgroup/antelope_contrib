
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <math.h>

#include "db.h"
#include "stock.h"
#include "location.h"
#include "coords.h"

#ifdef __STDC__
#define PL_(x) x
#else
#define PL_(x) ( )
#endif /* __STDC__ */

extern int load_observations PL_(( Pf *pf, Dbptr db, Arr *arr_phase, Arr **stationsp, Arr **arraysp, Tbl **tap, Tbl **tup ));
extern int run_location PL_(( Dbptr dbin, Dbptr dbout, char *pfname, int *orid, char **error ));
extern int save_results PL_(( Dbptr dbin, Dbptr dbout, Pf *pf, Tbl *ta, Tbl *tu, Location_options *o, char *vmodel, Hypocenter *hypo, Tbl *residual, int *oridp ));
extern int write_log PL_(( char *outfile_name, Hypocenter *h0, Tbl *ta, Tbl *tu, Location_options *o, Tbl *converge_history, Tbl *reason_converged, Tbl *residual ));

#undef PL_

/* $Id$ */
