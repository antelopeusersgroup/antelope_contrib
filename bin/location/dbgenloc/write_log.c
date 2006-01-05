#include "dbgenloc.h"

#define BOOLEAN(X) ((X) ? "yes" : "no " )  
int
write_log (char *outfile_name, 
    Hypocenter *h0,
    Tbl *ta, 
    Tbl *tu,
    Location_options *o,
    Tbl *converge_history, 
    Tbl *reason_converged, 
    Tbl *residual,
    double **C,
    float *emodel)
{
    int i, j, n ;
    Hypocenter *hypo ; 
    char *s ;
    FILE *file ;
    Arrival *a ; 
    Slowness_vector *u ;

    if ( (file = fopen ( outfile_name, "w+" )) == 0 )
	return -1 ;

    fprintf(file, "%s Version $Revision$ $Date$\n", Program_Name); 

    n = maxtbl(ta) ;
    if ( n > 0 ) {
	fprintf(file, "\nInput Arrival data:\n" 
		      "\tArid Sta         Phase     Time                  uncertainty\n" ) ;
	for ( i=0 ; i<n ; i++ ) {
	    a = (Arrival *) gettbl(ta, i) ;
	    fprintf ( file, "\t%5d %-12s %-6s %s %10.3f\n", 
		a->arid,
		a->sta->name, 
		a->phase->name, 
		strtime(a->time),
		a->deltat ) ; 
	}
    }


    n = maxtbl(tu) ;
    if ( n > 0 ) {
	fprintf(file, "\nInput Slowness data:\n" 
		      "Array        Phase    ux            uy      uncertainty\n" ) ;
	for ( i=0 ; i<n ; i++ ) {
	    u = (Slowness_vector *) gettbl(tu, i) ;
	    fprintf ( file, "%5d %-12s %-6s %10.3f %10.3f %10.3f %10.3f\n", 
		u->arid,
		u->array->name, 
		u->phase->name, 
		u->ux, 
		u->uy,
		u->deltaux,
		u->deltauy
		) ;
	}
    }

    fprintf ( file, "\nParameters\n" ) ; 
    fprintf ( file, "\tatime_residual_weight = %s\n", 
    		BOOLEAN(o->atime_residual_weight)) ; 
    fprintf ( file, "\tatime_distance_weight = %s\n", 
    		BOOLEAN(o->atime_distance_weight)) ; 
    fprintf ( file, "\tslow_residual_weight = %s\n", 
    		BOOLEAN(o->slow_residual_weight)) ; 
    fprintf ( file, "\tslow_distance_weight = %s\n", 
    		BOOLEAN(o->slow_distance_weight)) ; 
    fprintf ( file, "\trelative weight slowness data/arrival data = %10.3f\n", 
    			o->slowness_weight_scale_factor) ;
    fprintf ( file, 
	"%10.3f < scaled rms residuals in residual weighting < %10.3f\n", 
	o->min_error_scale, o->max_error_scale ) ;
    fprintf ( file, "\tfix initial lat ? %s ; lon ? %s ; depth ? %s ; time ? %s\n", 
    		 BOOLEAN(o->fix[0]),
		 BOOLEAN(o->fix[1]),
		 BOOLEAN(o->fix[2]),
		 BOOLEAN(o->fix[3]) ) ;
    fprintf ( file, "\tgeneralized_inverse = %d\n", o->generalized_inverse ) ;
    fprintf ( file, "\t%10.3f < damping < %10.3f, damping adjustment = %10.3f\n",
    		o->min_relative_damp,
		o->max_relative_damp,
		o->damp_adjust_factor ) ;
    fprintf ( file, "\tpseudo-inverse relative truncation parameter = %10.3f\n",
		o->sv_relative_cutoff) ;
    fprintf ( file, "\t%10.3f < depth < %10.3f (kilometers)\n", 
    		o->depth_ceiling, o->depth_floor ) ; 
    fprintf ( file, "\tstep_length_scale_factor = %10.3f\n", 
    		o->step_length_scale_factor ) ; 
    fprintf ( file, "\tmin_step_length_scale = %10.3f\n", 
    		o->min_step_length_scale ) ;
    fprintf ( file, "\tmaximum iterations = %d\n", o->max_hypo_adjustments ) ;
    fprintf ( file, "\tconvergence when relative weighted residual change < %10.6f\n", 
    		o->relative_rms_convergence ) ; 
    fprintf ( file, "\tconvergence when position change < %10.6f km\n", 
    		o->dx_convergence ) ;

    n = maxtbl (converge_history) ;
    fprintf ( file, 
	"\nConvergence History\n"
    	"\t                                                                       residual        inter     degrees of\n"
    	"\titer    lat        lon        depth      time	                    raw       weighted  quartile  freedom\n" ) ; 
    for (i=0 ; i<n ; i++ ) {
	hypo = (Hypocenter *) gettbl (converge_history, i);
	fprintf (file, 
	    "\t%3d %10.5f %10.5f %10.5f %s %10.3g %10.3g %10.3g   %3d\n",
	     i,
	     hypo->lat, hypo->lon, hypo->z, s=strtime(hypo->time),
	     hypo->rms_raw, hypo->rms_weighted,
	     hypo->interquartile,
	     hypo->degrees_of_freedom
	     );
	free(s) ;
    }

    fprintf ( file, "\nConverged because:\n" ) ;
    n = maxtbl(reason_converged) ; 
    for ( i=0 ; i<n ; i++ ) {
	fprintf ( file, "\t%s\n", (char *) gettbl(reason_converged, i )) ; 
    }
    /* These need to be bypassed in thsi special case */
    if( !(o->fix[0] && o->fix[1] && o->fix[2] && o->fix[3] ) )
    {
	fprintf( file, "Unscaled Covariance matrix (x,y,z,t)\n");
	for(i=0;i<4;++i)
	{
	for(j=0;j<4;++j) fprintf(file,"%15.6lg ",C[i][j]);
	fprintf(file,"\n");
	}
	fprintf(file,"Model Error Bounds (x,y,z,t)\n");
	fprintf(file,"%15.6g km EW\n%15.6g km NS\n%15.6g km Z\n%15.6g s origin time\n",
			emodel[0],emodel[1],emodel[2],emodel[3]);
    }
#if 0
    fprintf ( file, "\nResiduals\n" ) ;
    fprintf ( file, "\t                               weighted   raw       residual      other\n"  ) ; 
    fprintf ( file, "\tsta         phase    obs       residual residual    weight        weight\n"  ) ; 
    if ( residual != 0 ) {
	char *s, sta[32], phase[32], meas[32] ; 
	double weighted_residual, raw_residual, weight, residual_weight ;
	
	n = maxtbl(residual) ; 
	for ( i=0 ; i<n ; i++ ) {
	    s = (char *) gettbl(residual, i ) ; 
	    sscanf ( s, "%s %s %s %lg %lg %lg %lg", sta, phase, meas, 
	    	&weighted_residual, &raw_residual, &weight, &residual_weight ) ; 
	    fprintf ( file, "\t%-12s %-6s %-3s %10.3g %10.3g %10.3g %10.3g\n", 
	    	sta, phase, meas, weighted_residual, raw_residual, residual_weight, weight ) ;
	}
    } else { 
	fprintf ( file, "\tNo residuals calculated!!" ) ; 
    }
#endif

    n = maxtbl(ta) ;
    if ( n > 0 ) {
	fprintf(file, "\nArrival Residuals:\n" 
			"\t                               weighted   raw       residual      other\n"  
			"\tarid sta         phase         residual residual    weight        weight\n"  ) ; 

	for ( i=0 ; i<n ; i++ ) {
	    a = (Arrival *) gettbl(ta, i) ;
	    fprintf ( file, "\t%5d %-12s %-6s %10.3g %10.3g %10.3g %10.3g\n",
		a->arid,
		a->sta->name, 
		a->phase->name, 
		a->res.weighted_residual,
		a->res.raw_residual,
		a->res.residual_weight,
		a->res.other_weights 
	    ) ;
	}
    }


    n = maxtbl(tu) ;
    if ( n > 0 ) {
	fprintf(file, "\nSlowness Residuals:\n" 
		      "\t                             weighted   raw       residual      other\n"  
		      "\tarid array        phase      residual residual    weight        weight\n"  ) ; 
	for ( i=0 ; i<n ; i++ ) {
	    u = (Slowness_vector *) gettbl(tu, i) ;
	    fprintf ( file, "\t%5d %-12s %-6s ux: %10.3f %10.3f %10.3f %10.3f\n" 
			    "                         uy: %10.3g %10.3g %10.3g %10.3g\n",
		u->arid,
		u->array->name, 
		u->phase->name, 
		u->xres.weighted_residual,
		u->xres.raw_residual,
		u->xres.residual_weight,
		u->xres.other_weights,
		u->yres.weighted_residual,
		u->yres.raw_residual,
		u->yres.residual_weight,
		u->yres.other_weights 
		) ;
	}
    }

    fclose(file) ;
    return 0 ;
}

/* $Id$ */
