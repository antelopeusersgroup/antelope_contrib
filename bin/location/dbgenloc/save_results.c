#include <stdio.h>
#include <string.h>
#include "dbgenloc.h"

int
save_results (Dbptr dbin, Dbptr dbout, 
	Pf *pf, 
	Tbl *ta, Tbl *tu,
	Location_options *o, 
	char *vmodel, 
	Hypocenter *hypo, 
	Tbl *residual, 
	int *oridp,
	double **C,
	float *emodel )
{
    int             i, n, orid, grn, srn, retcode = 0;
    char algorithm[15];
    char *str;
    double delta, esaz, seaz, slores, duphi, azres, azimuth ;
    double smajax, sminax, strike, sdepth, stime;
    Arrival *a ; 
    Slowness_vector *u ;
    Dbptr dborigin, dbassoc ;
    int old, new ;
    double conf;
    char *modtype;
    int model;
    int	rc;

    *oridp = orid = dbnextid(dbin, "orid" ) ;
    if ( orid < 1 ) {
        complain ( 0, "Can't write to lastid table\n" ) ;
	return -1 ;
    }

    dborigin = dblookup ( dbout, 0, "origin", 0, 0 ) ; 
    dbquery ( dborigin, dbRECORD_COUNT, &i ) ; 

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
	   0)  ) {
	complain(0, "Couldn't add origin record to database.\n");
	retcode = -1;
      }

     conf = pfget_double(pf,"confidence");
     modtype = pfget_string(pf,"ellipse_type");

     if(modtype == NULL)
     {
	complain(0,"parameter ellipse_type not defined--default to chi_square");
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
        complain(0, "parameter ellipse_type %s incorrect (must be F_dist or chi_square)--default to chi_square", modtype );
        model = CHI_SQUARE;
     }

    rc = project_covariance( C, model, &conf, 
			     hypo->rms_weighted, hypo->degrees_of_freedom, 
			     &smajax, &sminax, &strike, &sdepth, &stime );

    if( rc != 0 ) 
    {
	complain(0, "project_covariance failed." );
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
		0) < 0 ) {
	complain (1,"couldn't add origerr record to database\n" ) ;
	retcode = -1 ;
    }
    if(save_emodel(orid, emodel, dbout))
	complain(0,"Problems saving emodel vector\n");
	

    dbassoc = dblookup ( dbout, 0, "assoc", 0, 0 ) ; 
    dbquery (dbassoc, dbRECORD_COUNT, &old ) ;
    n = maxtbl(ta) ;
    for ( i=0 ; i<n ; i++ ) {
	a = (Arrival *) gettbl(ta, i) ;
	dist(rad(hypo->lat),rad(hypo->lon),rad(a->sta->lat),rad(a->sta->lon), &delta,&esaz);
	dist(rad(a->sta->lat),rad(a->sta->lon),rad(hypo->lat),rad(hypo->lon), &delta,&seaz);

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
		"wgt", (double) a->res.residual_weight, 
		0 ) < 0 ) 
	    complain ( 0, "Can't add assoc record for station %s arid=%d orid=%d\n", 
	    	a->sta->name, a->arid, orid ) ;
    }


    n = maxtbl(tu) ;
    dbout = dblookup ( dbout, 0, "assoc", 0, 0 ) ;
    for ( i=0 ; i<n ; i++ ) {
	u = (Slowness_vector *) gettbl(tu, i) ;
	if ( (dbout.record = dbaddv(dbout, "assoc", 
		"orid", orid, 
		"arid", u->arid, 
		"vmodel", vmodel,
		0 )) < 0 ) {
	    complain ( 0, "Can't add assoc record for station %s arid=%d orid=%d\n", 
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
		0 ) < 0 ) 
		complain ( 0, "Can't add slowness and azimuth residuals to assoc record #%d\n",
			dbout.record ) ; 
	}
    }
    dbquery (dbassoc, dbRECORD_COUNT, &new ) ;

    dbputv(dborigin, 0,
	   "nass", new-old,
	   "ndef", new-old,
	   0 ) ; 
   /* This routine saves the predicted arrival information for each data point 
    WARNING:  the str variable passed here is computed above as the base 
    velocity "name" removing the directory portion used by dbgenloc.  
    This should interact correctly with the stavel table to allow generic
    naming of a velocity model computed by multiple calculators */
    if(save_predarr(dbout,ta,tu,*hypo,orid,str))
    	complain(0,"save_results:  problems saving predarr table\n");

    return retcode ;
}

/* $Id$ */
