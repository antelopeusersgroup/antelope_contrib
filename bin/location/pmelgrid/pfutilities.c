#include "stock.h"
#include "coords.h"
#include "brttutil.h"
#include "pf.h"
#include "location.h"
#include "pmel.h"
#include "pfstream.h"
/* This function takes a vector Pf *'s from is to ie and 
 * builds an output list of Arrival objects.  This should be
 * thought of like a database get routine that works from row is
 * to ie extracting information from the arrival table associated
 * with a given event with an assuption that the table is sorted
 * so that is to ie is associated with a common event.  
 * 
 * phases and stations are associative arrays for phase handles
 * and station object (defined in libgenloc).  This routine
 * is assumed to be called at a high level and little error
 * checking is done.  It always returns a valid Tbl handle, but
 * that Tbl could be empty.  Any problems in cracking a single
 * Pf item will lead to skipping that entry with a diagnostic
 * issued to elog.
 *
 * Author:  Gary L. Pavlis
 * Written:  October 2002
 */
Tbl *pfextract_arrivals(Pf **pfall, int is, int ie, 
	Arr *phases,Arr *stations)
{
	Arrival *a;
	Tbl *tout;
	int i;
	Pf *pf;

	tout=newtbl(0);
	for(i=is;i<=ie;++i)
	{
		char *sta,*phase_name;
		/* a convenient shorthand */
		pf=pfall[i];
		allot(Arrival *,a,1);
		sta = pfget_string(pf,"sta");
		if(sta==NULL)
		{
			elog_log(0,"No sta parameter for ensemble member %d\nSkipped\n",
				i);
			free(a);
			continue;
		}
                a->sta = (Station *) getarr(stations,sta);
                if(a->sta == NULL)
                {
                        elog_complain(0,"Warning (read_arrivals):  Can't find coordinates for station %s\n%s phase arrival for this station skipped\n",
                                sta, phase_name);
                        free(a);
                        continue;
                }
		phase_name=pfget_string(pf,"phase");
                a->phase = (Phase_handle *) getarr(phases,phase_name);
                if(a->phase == NULL)
                {
                        elog_complain(0,"Warning (read_arrivals):  No phase handle for phase name %s\nArrival for station %s skipped\n",
                                phase_name,sta);
                        free(a);
                        continue;
                }
		a->arid = pfget_int(pf,"arid");
		a->time = pfget_time(pf,"arrival.time");
                /* Here set set deltat to default when this is required */
		a->deltat = pfget_double_wdef(pf,"deltim",
			(double)a->phase->deltat0);
                pushtbl(tout,a);
	}
	return(tout);
}
/*  This routine updates the Pf_ensemble with revised hypocenter 
 *  estimates, the origerr table, and assoc.  It does some things
 *  that are very dangerous if not warned by end user.  That is, 
 *  some important database keys are NOT changed but are simply
 *  left alone.  This is necessary because one cannot possibly coordinate
 *  database keys within a structure like this.  These must be viewed
 *  as junk parameters that are just passed through at best.  This 
 *  will work because the ensemble defines the grouping that the orid
 *  and evid keys are used for.  In fact, many important database 
 *  attributes required to construct a valid catalog downstream are
 *  simply assumed to be present in the input Pf_ensemble and are 
 *  simply passed downstream.  
 *
 *  Arguments:
 *    pfe - input Pf_ensemble to be altered and passed to output.
 *       That is, pfe is edited and pushed to the output stream streamout.
 *    controlpf - some global parameters are extracted from the 
 *       control parameter space for the calling program.  See the 
 *       code for the list.
 *    h - vector of hypocenter structures storing new hypocenter estimates
 *    ta - vector of Tbls containing pointers to Arrival objects 
 *       used by genloc.
 *
 *    VERY IMPORTANT to recognize that the length of the h and ta vectors 
 *    is assumed to be pfe->ngroups.  seg faults are guaranteed to happen
 *    if this is not true.
 *
 *    Author:  Gary Pavlis
 *    Written:  October 2002
   */
void update_ensemble(
		Pf_ensemble *pfe,
		Pf *controlpf,
		Hypocenter *h,
		Tbl **ta,
		Location_options o)
{
	int i,j,jj;
	int narrival;
	double conf;
	double smajax,sminax,strike,sdepth,stime;
        int rc;
	char *modtype;
	int model;
	char *auth;
	double **C;
	int gs, ge;  /* shorthand variables */
	float emodel[4];
	Tbl *utbl;

	/* assume all the pf's have the same for this parameter*/
	auth=pfget_string(controlpf,"author");
	C = dmatrix(0,3,0,3);
	
	/* In pmel we don't handle slowness vectors, so this will be an
	empty list.  It needs to be a valid tbl or the program will seg
	fault, but it will be empty */
	utbl=newtbl(0);

	conf = pfget_double(controlpf,"confidence");
	modtype = pfget_string(controlpf,"ellipse_type");
	if(modtype == NULL)
	{
		elog_complain(0,"parameter ellipse_type not defined--default to chi_square\n");
		model = CHI_SQUARE;
	}
	else if ( strcmp( modtype, "chi_square" ) == 0 )
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

	for(i=0;i<pfe->ngroups;++i)
	{
    	    double delta,seaz,esaz,wgt;
    	    Arrival *a;
    	    int narrivals;
	    if(h[i].used)
	    {
	    	predicted_errors(h[i],ta[i],utbl,o,C,emodel);
		rc = project_covariance( C, model, &conf,
                            h[i].rms_weighted, h[i].degrees_of_freedom,
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
		/* This loop sets rows of output for this grouping which here
		 * is defined by the arrivals connected to one evid.
		 * i.e. alll pfe->pf's in this group have copies of the
		 * same parameters -- like a joined db table */
		for(j=pfe->group_start[i];j<=pfe->group_end[i];++j)
		{
			/* These are in saved in origerr */
			pfput_double(pfe->pf[j],"sxx",C[0][0]);
			pfput_double(pfe->pf[j],"syy",C[1][1]);
			pfput_double(pfe->pf[j],"szz",C[2][2]);
			pfput_double(pfe->pf[j],"stt",C[3][3]);
			pfput_double(pfe->pf[j],"sxy",C[0][1]);
			pfput_double(pfe->pf[j],"sxz",C[0][2]);
			pfput_double(pfe->pf[j],"syz",C[1][2]);
			pfput_double(pfe->pf[j],"stx",C[0][3]);
			pfput_double(pfe->pf[j],"sty",C[1][3]);
			pfput_double(pfe->pf[j],"stz",C[2][3]);
			pfput_double(pfe->pf[j],"sdobs",h[i].rms_raw);
			pfput_double(pfe->pf[j],"smajax",smajax);
			pfput_double(pfe->pf[j],"sminax",sminax);
			pfput_double(pfe->pf[j],"strike",strike);
			pfput_double(pfe->pf[j],"sdepth",sdepth);
			pfput_double(pfe->pf[j],"stime",stime);
			pfput_double(pfe->pf[j],"conf",conf);
			/* These are saved in origin */
			pfput_double(pfe->pf[j],"origin.lat",h[i].lat);
			pfput_double(pfe->pf[j],"origin.lon",h[i].lon);
			pfput_double(pfe->pf[j],"origin.z",h[i].z);
			pfput_time(pfe->pf[j],"origin.time",h[i].time);
			pfput_string(pfe->pf[j],"algorithm","pmelgrid");
			pfput_string(pfe->pf[j],"auth",auth);
			/* for emodel table if desired */
			pfput_double(pfe->pf[j],"emodelx",(double)emodel[0]);
			pfput_double(pfe->pf[j],"emodely",(double)emodel[1]);
			pfput_double(pfe->pf[j],"emodelz",(double)emodel[2]);
			pfput_double(pfe->pf[j],"emodelt",(double)emodel[3]);
			/* special Boolean to mark these records ok*/
			pfput_boolean(pfe->pf[j],"data_valid",1);
		}
		/*This is a somewhat different loop to create assoc.
		* it is more complex because of searching described below */
	        narrivals=maxtbl(ta[i]);
		gs = pfe->group_start[i];
		ge = pfe->group_end[i];
		for(j=gs,jj=0;jj<narrivals;++j,++jj)
		{
			int arid_pfe=-1;

			a = (Arrival *)gettbl(ta[i],jj);
			/* ta gets edited when clock errors are present and
			all phases not requested for path anomaly corrections
			are dropped.  Because of this we have to match arid in
			the structure "a" with that in the Pf_ensemble we 
			are inserting information into.  We do a simple forward
			linear search because ta[i] will always be only shorter
			than the Pfe group and will (we assume)always be in the
			same order.*/
			do {
				arid_pfe=pfget_int(pfe->pf[j],"arid");
				++j;
			}
			while ((arid_pfe!=a->arid) 
				&& (j<=ge ));

			if((j>ge)&&(arid_pfe!=a->arid))
			{
				elog_notify(0,"pfstream_save_results:  arid mismatch\nDid not find arid %d in Pf_ensemble\n",
					a->arid);
				j=gs;
				continue;
			}
			else
			{
				--j;
			}
			/* A few simple quantities need to be computed */
			dist(rad(h[i].lat),rad(h[i].lon),
				rad(a->sta->lat),rad(a->sta->lon), 
				&delta,&esaz);
        		dist(rad(a->sta->lat),rad(a->sta->lon),
				rad(h[i].lat),rad(h[i].lon), 
				&delta,&seaz);

        		wgt = (double)((a->res.weighted_residual)
				/(a->res.raw_residual));

			/* now we can dump all these parameters to the Pf_ensemble*/
			pfput_double(pfe->pf[j],"delta",deg(delta));
			pfput_double(pfe->pf[j],"seaz",deg(seaz));
			pfput_double(pfe->pf[j],"esaz",deg(esaz));
			pfput_double(pfe->pf[j],"timeres",deg(delta));
			pfput_double(pfe->pf[j],"timeres",a->res.raw_residual);
			pfput_double(pfe->pf[j],"wgt",wgt);
		}
	    }
	    else
	    {
		/* This is a bit dangerous.  We set this boolean and 
		downstream better catch it since numerous parameters 
		will not be set that since the above is bypassed */
		for(j=pfe->group_start[i];j<=pfe->group_end[i];++j)
			pfput_boolean(pfe->pf[j],"data_valid",0);
	    }
	}
	free_matrix((char **)C,0,3,0);
	freetbl(utbl,0);
}
Pf_ensemble *build_sc_ensemble(int gridid, SCMatrix *s, Pf *pf)
{
	char *pmelrun,*gridname;
	Tbl *phskeys,*stakeys;
	int i,j,icol,ii;
	char *phase, *sta;
	int phacol;
	int *iptr;
	Pf_ensemble *pfesc;
	int nsc;
	char *str;

	/* this will cause the program to die if these aren't defined */
	pmelrun=pfget_string(pf,"pmel_run_name");
	gridname = pfget_string(pf,"gridname");

	/* We loop over phases and station names using the contents 
	 * of these tbl's as iterators */
	phskeys = keysarr(s->phase_index);
	stakeys = keysarr(s->sta_index);
	nsc = maxtbl(phskeys)*maxtbl(stakeys);
	pfesc = create_Pf_ensemble(nsc,0);
	/* The constructor just called does not initialize the pf members so
	we need to create empty pf struct for each member */
	for(i=0;i<nsc;++i) pfesc->pf[i]=pfnew(PFARR);
	/* Ugly ugly memory problem.  BIG MAINTENANCE HEADACHE.  
	The ensemble_keys list is placed in a dynamic structure that is later
	released.  This means we have to allot the memory for the strings that
	define the keys here to avoid seg faults caused by trying to 
	release this later.  That is, I once tried doing the following with
	strtbl, but it lead to a seg fault when the free_Pf_ensemble routine
	called freetbl.  Ouch.  */
	pfesc->ensemble_keys=newtbl(5);
	str=strdup("sta");
	pushtbl(pfesc->ensemble_keys,str);
	str=strdup("phase");
	pushtbl(pfesc->ensemble_keys,str);
	str=strdup("gridname");
	pushtbl(pfesc->ensemble_keys,str);
	str=strdup("gridid");
	pushtbl(pfesc->ensemble_keys,str);
	str=strdup("pmelrun");
	pushtbl(pfesc->ensemble_keys,str);

	for(j=0,ii=0;j<maxtbl(phskeys);++j)
	{
		phase = (char *)gettbl(phskeys,j);
                iptr = (int *)getarr(s->phase_index,phase);
                phacol = *iptr;
                for(i=0;i<maxtbl(stakeys);++i)
                {
			sta = (char *)gettbl(stakeys,i);
			iptr = (int *)getarr(s->sta_index,sta);
	                icol = phacol + (*iptr);
			if((icol>=0) && (icol<s->ncol) )
			{
				pfput_string(pfesc->pf[ii],"sta",sta);
				pfput_string(pfesc->pf[ii],"phase",phase);
				pfput_string(pfesc->pf[ii],"gridname",gridname);
				pfput_string(pfesc->pf[ii],"pmelrun",pmelrun);
				pfput_double(pfesc->pf[ii],"tsc",s->sc[icol]);
				pfput_double(pfesc->pf[ii],"tscref",s->scref[icol]);
				pfput_double(pfesc->pf[ii],"tscbias",s->scbias[icol]);
				pfput_boolean(pfesc->pf[ii],"data_valid",1);
			}
			else
			{
				elog_complain(0,
				 "build_sc_ensemble: computed index %d for accessing station correction vector is outside range of 0 to %d\nSkipped for output.  Expect more problems\n",
				  icol,s->ncol);
			}
			++ii;
		}
	}
	return(pfesc);
}

