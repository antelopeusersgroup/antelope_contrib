#include "dbgenloc.h"

static Hypocenter 
initial_hypocenter ( double lat, double lon, double depth, double time ) 
{
    Hypocenter h;
    h.lat = lat ; 
    h.lon = lon ; 
    h.z = depth ; 
    h.time = time ;

    h.dz = 0.0;
    h.dx = 0.0;
    h.dy = 0.0;
    h.dt = 0.0;
    h.rms_raw = -1.0;
    h.rms_weighted = -1.0;
    h.interquartile = -1.0;
    h.number_data = 0;
    h.degrees_of_freedom = 0;
    h.lat0 = h.lat;
    h.lon0=h.lon;
    h.z0 = h.z;
    h.t0 = h.time;
    return(h);
}

int
run_location (Dbptr dbin, Dbptr dbout, char *pfname, int *orid, char **error)
{
    int             retcode = -1;
    Arr            *stations=0;
    Arr            *arrays=0;
    Arr *arr_phase;
    Pf		   *pf, *vpf ;
    Tbl		   *converge_history=0, *reason_converged=0, *residual=0 ;
    Location_options o;
    char	   *vmodel ;
    Tbl		   *ta, *tu ;
    Hypocenter h0;
    static Arr *models = 0 ; 
    static char static_error[256];
    int loc_ret;

    *error = "" ; 

    if (pfread (pfname, &pf) < 0) {
	complain (1, "Can't open parameter file %s\n", pfname);
	*error = "Can't read parameter file";
	return -1;
    }

    o = parse_options_pf (pf);
    o.max_hypo_adjustments = pfget_int ( pf, "maximum_iterations") ;

    vmodel = pfget_string (pf, "travel_time_model");
    if ( vmodel == 0 ) {
	complain ( 0, "travel_time_model not specified in parameter file\n") ;
	*error = "travel_time_model not specified in parameter file" ; 
	return -1 ; 
    } else {
	if ( models == 0 ) {
	    models = newarr (0) ; 
	}
	if ( (arr_phase = getarr ( models, vmodel ) ) == 0 ) {
	    if(pfload("GENLOC_MODELS", "tables/genloc", vmodel, &vpf) != 0) {
		*error = "can't read travel time model" ;
		return -1 ; 
	    } else { 
		arr_phase = parse_phase_parameter_file (vpf);
		setarr ( models, vmodel, arr_phase ) ;
		pffree ( vpf ) ; 
	    }
	}
    }

    if ( load_observations ( pf, dbin, arr_phase, 
	    &stations, &arrays, &ta, &tu ) < 1 ) {
	complain (0, "No data to locate\n" ) ; 
	*error = "No arrival data for a location" ; 
	return -1 ;
    }

    h0 = initial_locate (ta, tu, o, pf);
    if (strcmp(pfget_string(pf, "initial_location_method"), "manual") == 0 )  {
        double time ;
        if ( maxtbl(ta) > 0 ) {
            Arrival *a ; 
            a = (Arrival *) gettbl(ta, 0) ;
            time = a->time ;
        } else {
            complain ( 0, "No time data for location\n" ) ; 
            *error = "No time data for location" ; 
            return -1 ;
        }
	h0.time = time ;
    }

    loc_ret =  ggnloc (h0, ta, tu, o, &converge_history, &reason_converged, &residual);
    if(loc_ret >= 0)
    {
    	write_log (pfget_string(pf, "output_file"), &h0, ta, tu, &o,
		converge_history, reason_converged, residual);

	if (reason_converged != 0 
	&& maxtbl(reason_converged) > 0 ) {
	if ( strncmp(gettbl(reason_converged,0), "Location hit iteration count limit", 34) != 0) {
		Hypocenter * hypo ;
		hypo = (Hypocenter *) gettbl (converge_history, maxtbl(converge_history)-1);
		save_results (dbin, dbout, pf, ta, tu, &o, vmodel, hypo, residual, orid ) ;
		retcode = 0 ;
	} else { 
		strcpy(static_error, gettbl ( reason_converged, 0) ); 
		*error = static_error ;
	}
	} else {
	*error = "no reasons returned from location algorithm" ;
	}
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
