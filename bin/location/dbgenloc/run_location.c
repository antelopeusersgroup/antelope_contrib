#include "dbgenloc.h"
/*  process routine for dbgenloc.  Handles the set of operations of
computing a location and associated error estimates.  

arguments:

dbin, dbout - input and output db handles respectively.
pfname - parameter file name
orid - origin id of event being processed
error - contains a string describing and error condition if 
processing fails. Left unaltered if there are not errors.

Normal return is a 0.  -1 if there is a error.

Original code by Dan Quinlan.

Altered May 2001 by G Pavlis.  

The original code used an associative array to store handles
to multiple models and travel time calculators.  This, unfortunately
causes a problem with some genloc calculators that are not
initialized correctly with that logic and probably leave memory
leaks when reinitialized with a different model name.  The fix
here slows the code execution, but avoids this initialization problem
by reinitializing the phase handles anytime the velocity model name
changes.  
*/

static char *lastmodel=NULL; /* needs to be here to initialize correctly */
static Arr *badclocks=NULL;

int
run_location (Dbptr dbin, Dbptr dbout, char *pfname, long *orid, char **error)
{
    int             retcode = -1;
    Arr            *stations=0;
    Arr            *arrays=0;
    static Arr *arr_phase;
    Pf		   *pf, *vpf ;
    Tbl		   *converge_history=0, *reason_converged=0, *residual=0 ;
    Location_options o;
    char	   *vmodel ;
    Tbl		   *ta, *tu, *taro, *turo ;
    Hypocenter h0;
    Hypocenter *hypo ;
    float *emodel;
    double **C;
    static char static_error[256];
    int loc_ret;
    int reload=0;
    int nbcs;

    *error = "" ; 

    if (pfread (pfname, &pf) < 0) {
	elog_complain(1, "Can't open parameter file %s\n", pfname);
	*error = "Can't read parameter file";
	return -1;
    }

    o = parse_options_pf (pf);
    o.max_hypo_adjustments = pfget_int ( pf, "maximum_iterations") ;
    if(badclocks==NULL)
    {
	badclocks = newarr(0);
	if(db_badclock_definition(dbin,pf,badclocks))
                elog_notify(0,"Problems in database definitions of bad clock time periods\n");
    }
    /* This is intentionally outside the previous block to allow turning
    S-P  on and off interactively through this mechanism.*/
    pfget_badclocks(pf,badclocks);
    nbcs = cntarr(badclocks);

    vmodel = pfget_string (pf, "travel_time_model");
    if ( vmodel == 0 ) {
	elog_complain( 0, "travel_time_model not specified in parameter file\n") ;
	*error = "travel_time_model not specified in parameter file" ; 
	return -1 ; 
    } 
    if(lastmodel==NULL)
    {
	lastmodel = strdup(vmodel);
	reload = 1;
    }
    else if(strcmp(lastmodel,vmodel))
    {
	if(arr_phase!=NULL)freearr(arr_phase,free_phase_handle);
	reload = 1;
    }
    if(reload)
    {
	    if(pfload("GENLOC_MODELS", "tables/genloc", vmodel, &vpf) != 0) {
		*error = "can't read travel time model" ;
		return -1 ; 
	    } else { 
		arr_phase = parse_phase_parameter_file (vpf);
		pffree ( vpf ) ; 
	    }
    }
    free(lastmodel);
    lastmodel=strdup(vmodel);

    if ( load_observations ( pf, dbin, arr_phase, 
	    &stations, &arrays, &ta, &tu, &taro, &turo ) < 1 ) {
	elog_complain(0, "No data to locate\n" ) ; 
	*error = "No arrival data for a location" ; 
	return -1 ;
    }
    if(nbcs)
    {
	/* Intentionally do not run this on residuals only list.  */
	if(minus_phases_arrival_edit(ta,arr_phase,badclocks))
		elog_notify(0,"problems in minus_phase_arrival_edit function\n");
    }
    /* When depth is fixed,  we have to reset center_depth to 
    the initial_depth field and set the number of depths for
    grid searches to 1.  This is the correct approach as it allows
    the dbloc2 script to set the initial_depth which makes sense. */
    if(o.fix[2])
    {
	h0.z = pfget_double(pf,"initial_depth");
	pfput_double(pf,"center_depth",h0.z);
	pfput_int(pf,"ndepths",1);
    }
    h0 = initial_locate (ta, tu, o, pf);
/*
    if (strcmp(pfget_string(pf, "initial_location_method"), "manual") == 0 )  {
        double time ;
        if ( maxtbl(ta) > 0 ) {
            Arrival *a ; 
            a = (Arrival *) gettbl(ta, 0) ;
            time = a->time ;
        } else {
            elog_complain( 0, "No time data for location\n" ) ; 
            *error = "No time data for location" ; 
            return -1 ;
        }
	h0.time = time ;
    }
*/

    loc_ret =  ggnloc (h0, ta, tu, o, &converge_history, &reason_converged, &residual);
    if(loc_ret >= 0)
    {
	C = dmatrix(0,3,0,3);
	emodel = (float *) calloc(4,sizeof(float));
	if((*C==NULL) || (emodel == NULL))
	                elog_die(0,"malloc failed for error arrays\n");
	hypo = (Hypocenter *) gettbl (converge_history, 
 			maxtbl(converge_history)-1);
	/*Bypass this function when all coordinates are fixed
	or this will seg fault */
	if( !(o.fix[0] && o.fix[1] && o.fix[2] && o.fix[3]) )
		predicted_errors(*hypo, ta, tu, o, C, emodel);
    	write_log (pfget_string(pf, "output_file"), &h0, ta, tu, &o,
		converge_history, reason_converged, residual,C,emodel);

	if (reason_converged != 0 
		&& maxtbl(reason_converged) > 0 ) {
	if ( strncmp(gettbl(reason_converged,0), "Location hit iteration count limit", 34) != 0) {
                if(compute_residual_only_results(*hypo,taro,turo))
                {
                    elog_notify(0,"Warning(dbgenloc): problems computing residual only data");
                }
		save_results (dbin, dbout, pf, ta, tu, taro, turo, &o, 
			vmodel, hypo, residual, orid , C, emodel) ;
		retcode = 0 ;
	  } else { 
		strcpy(static_error, gettbl ( reason_converged, 0) ); 
		*error = static_error ;
	  }
	} else {
		*error = "no reasons returned from location algorithm" ;
	}
	free_matrix((char **)C,0,3,0);
	free(emodel);
    }

    if (converge_history)
	freetbl (converge_history, free);
    if (reason_converged) 
	freetbl (reason_converged, free);
    if (residual)
	freetbl (residual, free);
    freetbl(tu,free);
    freetbl(ta,free);
    pffree(pf) ;

    return retcode ;
}



/* $Id$ */
