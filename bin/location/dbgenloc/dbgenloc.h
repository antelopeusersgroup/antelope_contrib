
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <math.h>

#include "db.h"
#include "stock.h"
#include "location.h"
#include "coords.h"

extern int load_observations ( Pf *pf, Dbptr db, Arr *arr_phase, Arr **stationsp, 
	Arr **arraysp, Tbl **tap, Tbl **tup, Tbl **tapro, Tbl **tupro );

extern int run_location ( Dbptr dbin, Dbptr dbout, char *pfname, long *orid, char **error );

extern int compute_residual_only_results ( Hypocenter hypo, Tbl *attbl, Tbl *utbl );

extern int save_results ( Dbptr dbin, Dbptr dbout, Pf *pf, Tbl *ta, Tbl *tu, Tbl *taro, Tbl *turo, Location_options *o, char *vmodel, Hypocenter *hypo, Tbl *residual, long *oridp, double **C, float *emodel );

extern int write_log ( char *outfile_name, Hypocenter *h0, Tbl *ta, Tbl *tu, Location_options *o, Tbl *converge_history, Tbl *reason_converged, Tbl *residual,
				double **C, float *emodel);


/* $Id$ */
