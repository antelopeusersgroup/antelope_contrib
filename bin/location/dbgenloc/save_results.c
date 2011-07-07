#include <stdio.h>
#include <string.h>
#include "dbgenloc.h"

/* New procedure added Dec. 2006 to support residual only
data.  Done at the request of users in June 2006 AUG meeting
in Tucson.  

This procedure was produced as a simplification of the 
form_equations procedure in libgenloc.   Here we basically 
loop through all the arrivals in the input lists and post
residuals to the Arrival structure.  Weights are all set to 
0.0 to emphasize these are assumed to be unused data. 
If the travel time/slowness vector calculation fails the 
input tbl's are edited with the problem entries removed.
*/
int compute_residual_only_results(Hypocenter hypo,
	Tbl *attbl, 
		Tbl *utbl)
{
	Arrival *atimes;
	Slowness_vector *slow;
	int natimes, nslow;
	int i,ii;
	natimes=maxtbl(attbl);
	nslow=maxtbl(utbl);

	for(i=0,ii=0; i<natimes;++i,++ii)
	{
		Travel_Time_Function_Output tto;
		atimes = (Arrival *) gettbl (attbl, ii);
		tto = calculate_travel_time (*atimes, hypo, RESIDUALS_ONLY);
		if (tto.time == TIME_INVALID) {
			elog_log(1,"Station: %s, Phase: %s Travel time calculator failed\n",
				atimes->sta->name, atimes->phase->name);
			deltbl(attbl,ii);
			/* Tbl index is stale and needs to be decremented
			like this when we delete such an entry. */
			--ii;
			free(atimes);
		}
		else
		{
			atimes->res.raw_residual=atimes->time-tto.time
                                - hypo.time;;
			atimes->res.weighted_residual=atimes->res.raw_residual;
			atimes->res.residual_weight=0.0;
			atimes->res.other_weights=0.0;
		}
	}
	for(i=0,ii=0;i<nslow;++i,++ii)
	{
		Slowness_Function_Output u_calc;
		slow = (Slowness_vector *) gettbl (utbl, ii);
		u_calc = calculate_slowness_vector (*slow,hypo,RESIDUALS_ONLY);
		if (u_calc.ux == SLOWNESS_INVALID) {
			elog_log(1,"Array: %s, Phase: %s Slowness vector calculator failed\n",
				atimes->sta->name, atimes->phase->name);
			deltbl(utbl,ii);
			--ii;
			free(slow);
		}
		else
		{
			slow->xres.raw_residual=slow->ux - u_calc.ux;
			slow->xres.weighted_residual=slow->xres.raw_residual;
			slow->xres.residual_weight=0.0;
			slow->xres.other_weights=0.0;
			slow->yres.raw_residual=slow->uy - u_calc.uy;
			slow->yres.weighted_residual=slow->yres.raw_residual;
			slow->yres.residual_weight=0.0;
			slow->yres.other_weights=0.0;
		}
	}
    return 0 ;
}

int
save_results (Dbptr dbin, Dbptr dbout, 
	Pf *pf, 
	Tbl *ta, Tbl *tu,
	Tbl *taro, Tbl *turo,
	Location_options *o, 
	char *vmodel, 
	Hypocenter *hypo, 
	Tbl *residual, 
	long *oridp,
	double **C,
	float *emodel )
{
    long             i, n, orid, grn, srn, retcode = 0;
    char algorithm[32];
    char *str;
    double delta, esaz, seaz, slores, duphi, azres, azimuth ;
    double smajax, sminax, strike, sdepth, stime;
    Arrival *a ; 
    Slowness_vector *u ;
    Dbptr dborigin, dbassoc ;
    long old, new ;
    double conf;
    char *modtype;
    int model;
    int	rc;
    double wgt;
    double weight_scale;  /* Added June 2006 see below*/

    *oridp = orid = dbnextid(dbin, "orid" ) ;
    if ( orid < 1 ) {
        elog_complain( 0, "Can't write to lastid table\n" ) ;
	return -1 ;
    }

    dborigin = dblookup ( dbout, 0, "origin", 0, 0 ) ; 
    dborigin.record = dbaddnull ( dborigin ) ; 
    grn = grnumber(hypo->lat, hypo->lon) ;
    srn = srnumber ( grn ) ; 
    str = strstr(vmodel,"/");
   /* because vmodels passed to genloc contain a directory
   and model name string, we strip the directory name.  This
  is necessary because the algorithm field is only 15 long */
    if(str == NULL)
	str = vmodel;
    else
	++str;
    sprintf(algorithm,"dbgenloc:%-6.6s",str);
    if (dborigin.record < 0
       ||  dbputv(dborigin, 0,
	   "orid", orid, 
	    "lat", hypo->lat,
	    "lon", hypo->lon,
	    "depth", hypo->z,
	    "time", hypo->time,
	   "jdate", yearday(hypo->time),
	   "dtype", o->fix[2] ? "g" : "f", 
	   "grn", grn,
	   "srn", srn,
	   "algorithm", algorithm,
	   "auth", pfget_string ( pf, "author" ),
	   NULL )  ) {
	elog_complain(0, "Couldn't add origin record to database.\n");
	retcode = -1;
      }
     /* Bypass all error calculations when all coordinates are fixed.
     In that situation C and emodel are both undefined at this point */
     if( !(o->fix[0] && o->fix[1] && o->fix[2] && o->fix[3]) )
     {

         conf = pfget_double(pf,"confidence");
         modtype = pfget_string(pf,"ellipse_type");
    
         if(modtype == NULL)
         {
    	elog_complain(0,"parameter ellipse_type not defined--default to chi_square");
    	model = CHI_SQUARE;
         }
         else if( strcmp( modtype, "chi_square" ) == 0 ) 
         {
            model = CHI_SQUARE;
         }
         else if( strcmp( modtype, "F_dist" ) == 0 )
         {
            model = F_DIST;
         }
         else
         {
            elog_complain(0, "parameter ellipse_type %s incorrect (must be F_dist or chi_square)--default to chi_square", modtype );
            model = CHI_SQUARE;
         }
    
        rc = project_covariance( C, model, &conf, 
    			     hypo->rms_weighted, hypo->degrees_of_freedom, 
    			     &smajax, &sminax, &strike, &sdepth, &stime );
    
        if( rc != 0 ) 
        {
    	elog_complain(0, "project_covariance failed." );
            smajax = -1;
            sminax = -1;
    	strike = -1;
    	sdepth = -1;
    	stime = -1;
    	conf = 0.;
        }
    
        if(dbaddv(dbout, "origerr", 
    		"orid", orid,
    		"sxx",C[0][0], 
    		"syy",C[1][1], 
    		"szz",C[2][2], 
    		"stt",C[3][3], 
    		"sxy",C[0][1], 
    		"sxz",C[0][2], 
    		"syz",C[1][2], 
    		"stx",C[0][3], 
    		"sty",C[1][3], 
    		"stz",C[2][3], 
    		"sdobs", hypo->rms_raw,
    		"smajax", smajax,
    		"sminax", sminax,
    		"strike", strike, 
    		"sdepth", sdepth,
    		"stime", stime,
    		"conf", conf,
    		NULL ) < 0 ) {
    	elog_complain(1,"couldn't add origerr record to database\n" ) ;
    	retcode = -1 ;
        }
        if(save_emodel(orid, emodel, dbout))
    	elog_complain(0,"Problems saving emodel vector\n");
    }    	

    dbassoc = dblookup ( dbout, 0, "assoc", 0, 0 ) ; 
    dbquery (dbassoc, dbRECORD_COUNT, &old ) ;
    n = maxtbl(ta) ;
    /* Patch made June 2006 in response to request at Antelope User
	group meeting that weights need to be normalized.  */
    for ( i=0,weight_scale=0.0 ; i<n ; i++ ) {
        a = (Arrival *) gettbl(ta, i) ;
	wgt = (double)((a->res.weighted_residual)/(a->res.raw_residual));
	weight_scale=MAX(wgt,weight_scale);
    }
    /* weight_scale is now the largest weight.  wgt normalized by
    this constant below to make all weights < 1.0.  Does not change
    relative scaling of solution.*/


    for ( i=0 ; i<n ; i++ ) {
	a = (Arrival *) gettbl(ta, i) ;
	dist(rad(hypo->lat),rad(hypo->lon),rad(a->sta->lat),rad(a->sta->lon), &delta,&esaz);
	dist(rad(a->sta->lat),rad(a->sta->lon),rad(hypo->lat),rad(hypo->lon), &delta,&seaz);

	/* The previous version of this program had this computed 
	incorrectly */
	wgt = (double)((a->res.weighted_residual)/(a->res.raw_residual));
        wgt/=weight_scale;
	/* S-P type phases are inconsistent with the use in datascope
	of time as a key.  For this reason we have to keep a reference
	to the original arid of both components of a double phase
	like this.  This is done through the arid and arid2 elements
	of the Arrival structure.  This condition, of course, branches
	for normal phases and "minus" phases like S-P composites
	handling them differently. */
	if(strchr(a->phase->name,'-'))
	{
		char *phase1,*phase2;

		/* This strange use of char pointers splits a name 
		like S-P so phase1="S" and phase2="P" */
		phase1=strdup(a->phase->name);
		phase2 = strchr(phase1,'-');
		*phase2='\0';
		++phase2;
		/* The error is only a notify level when the addv
		fails because this can happen readily of one uses
		multiple composite phases.  e.g. S-P, Pg-P, etc. */
		if ( dbaddv(dbout, "assoc", 
			"orid", orid, 
			"arid", a->arid, 
			"sta", a->sta->name, 
			"phase", phase1, 
			"delta", deg(delta),
			"seaz", deg(seaz),
			"esaz", deg(esaz),
			"timeres", (double) a->res.raw_residual,
			"timedef", "d",
			"vmodel", vmodel,
			"wgt",wgt, 
				NULL ) < 0 ) 
	    			elog_notify ( 0, "Can't add assoc record for station %s, arid=%ld, orid=%ld for composite component %s of %s\n", 
	    				a->sta->name, a->arid, orid,
					 phase1,a->phase->name) ;
		/* Be a bit more cautious for the second component */
		if( ((a->arid2)>0) && ((a->arid)!=(a->arid2)) )
			if ( dbaddv(dbout, "assoc", 
				"orid", orid, 
				"arid", a->arid2, 
				"sta", a->sta->name, 
				"phase", phase2, 
				"delta", deg(delta),
				"seaz", deg(seaz),
				"esaz", deg(esaz),
				"timeres", (double) a->res.raw_residual,
				"timedef", "d",
				"vmodel", vmodel,
				"wgt",wgt, 
					NULL ) < 0 ) 
	    			    elog_notify ( 0, "Can't add assoc record for station %s, arid=%ld, orid=%ld for composite component %s of %s\n", 
	    				a->sta->name, a->arid2, orid,
					 phase2,a->phase->name) ;


		elog_log(0,"Station %s used composite phase %s for orid %ld\n",
			a->sta->name,a->phase->name,orid);
		free(phase1);
	}
	else
	{
	    /* Regular phases land here */
	    if ( dbaddv(dbout, "assoc", 
		"orid", orid, 
		"arid", a->arid, 
		"sta", a->sta->name, 
		"phase", a->phase->name, 
		"delta", deg(delta),
		"seaz", deg(seaz),
		"esaz", deg(esaz),
		"timeres", (double) a->res.raw_residual,
		"timedef", "d",
		"vmodel", vmodel,
		"wgt", wgt,
		NULL ) < 0 ) 
	    	    elog_complain( 0, "Can't add assoc record for station %s arid=%ld orid=%ld\n", 
	    			a->sta->name, a->arid, orid ) ;
	}
    }


    n = maxtbl(tu) ;
    dbout = dblookup ( dbout, 0, "assoc", 0, 0 ) ;
    for ( i=0 ; i<n ; i++ ) {
	u = (Slowness_vector *) gettbl(tu, i) ;
	if ( (dbout.record = dbaddv(dbout, "assoc", 
		"orid", orid, 
		"arid", u->arid, 
		"vmodel", vmodel,
		NULL )) < 0 ) {
	    elog_complain( 0, "Can't add assoc record for station %s arid=%ld orid=%ld\n", 
	    	a->sta->name, a->arid, orid ) ;
	} else {
	    slores = deg2km(sqrt(sqr(u->xres.raw_residual) + sqr(u->yres.raw_residual))) ;
	    azimuth = atan2 ( u->uy, u->ux ) ;
	    duphi = (u->ux*cos(azimuth) - u->uy*sin(azimuth)) / sqrt(sqr(u->ux)+sqr(u->uy)) ;
	    azres = deg(duphi);

	    if ( dbputv ( dbout, 0, 
		"slores", slores,
		"slodef", "d",
		"azres", azres,
		"azdef", "d", 
		NULL ) < 0 ) 
		elog_complain( 0, "Can't add slowness and azimuth residuals to assoc record #%ld\n",
			dbout.record ) ; 
	}
    }
    /* Disgustingly repetitious code to handle residual only solutions. Appropriate because
    for residuals only computation we don't allow S-P type phases anyway.*/
    n=maxtbl(taro);
    for ( i=0 ; i<n ; i++ ) {
	a = (Arrival *) gettbl(taro, i) ;
	dist(rad(hypo->lat),rad(hypo->lon),rad(a->sta->lat),rad(a->sta->lon), &delta,&esaz);
	dist(rad(a->sta->lat),rad(a->sta->lon),rad(hypo->lat),rad(hypo->lon), &delta,&seaz);
	wgt=0.0;
 	/* Regular phases land here */
	if ( dbaddv(dbout, "assoc", 
		"orid", orid, 
		"arid", a->arid, 
		"sta", a->sta->name, 
		"phase", a->phase->name, 
		"delta", deg(delta),
		"seaz", deg(seaz),
		"esaz", deg(esaz),
		"timeres", (double) a->res.raw_residual,
		"timedef", "n",
		"vmodel", vmodel,
		"wgt", wgt,
	    				NULL ) < 0 ) 
	    	    elog_complain( 0, "Can't add assoc record for station %s arid=%ld orid=%ld\n", 
	    			a->sta->name, a->arid, orid ) ;
    }
    n = maxtbl(turo) ;
    for ( i=0 ; i<n ; i++ ) {
	u = (Slowness_vector *) gettbl(turo, i) ;
	if ( (dbout.record = dbaddv(dbout, 0, 
		"orid", orid, 
		"arid", u->arid, 
		"vmodel", vmodel,
		NULL )) < 0 ) {
	    elog_complain( 0, "Can't add assoc record for station %s arid=%ld orid=%ld\n", 
	    	a->sta->name, a->arid, orid ) ;
	} else {
	    slores = deg2km(sqrt(sqr(u->xres.raw_residual) + sqr(u->yres.raw_residual))) ;
	    azimuth = atan2 ( u->uy, u->ux ) ;
	    duphi = (u->ux*cos(azimuth) - u->uy*sin(azimuth)) / sqrt(sqr(u->ux)+sqr(u->uy)) ;
	    azres = deg(duphi);

	    if ( dbputv ( dbout, 0, 
		"slores", slores,
		"slodef", "n",
		"azres", azres,
		"azdef", "n", 
		NULL ) < 0 ) 
		elog_complain( 0, "Can't add slowness and azimuth residuals to assoc record #%ld\n",
			dbout.record ) ; 
	}
    }   

    dbquery (dbassoc, dbRECORD_COUNT, &new ) ;

    dbputv(dborigin, 0,
	   "nass", new-old,
	   "ndef", hypo->number_data,
	   NULL ) ; 
   /* This routine saves the predicted arrival information for each data point 
    WARNING:  the str variable passed here is computed above as the base 
    velocity "name" removing the directory portion used by dbgenloc.  
    This should interact correctly with the stavel table to allow generic
    naming of a velocity model computed by multiple calculators */
    if(save_predarr(dbout,ta,tu,*hypo,orid,str))
    	elog_complain(0,"save_results:  problems saving predarr table\n");

    return retcode ;
}

/* $Id$ */
