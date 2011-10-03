#include <math.h>
#include <strings.h>
#include "stock.h"
#include "arrays.h"
#include "elog.h"
#include "location.h"
#include "glputil.h"
#include "perf.h"
#include "pmel.h"
#define SSWR_TEST_LEVEL 0.30
/* This routine takes advantage of the sparse form of the S matrix 
that forms what is used to form the matrix SN (see PMEL paper).  That
is, S is a scrambled diagonal matrix.  Because of this fact it is much
smarter to form the product SN = UTn*S using the form below that
uses a simple dscal and dcopy algorithm.  

Arguments:
	m - data space dimension (rows and columns in U)
	Un - null space collection of m vectors computed from original
		equations of condition for current location.  Assume
		these are stored in the columns of Un in fortran order.
	n1u - leading dimension of U (fortran thing)
	nnull - number of null vectors = number of columns in Un
	sn - matrix to hold the product U_N^T * S (FORTRAN storage).  These
		will be written to m-ncn rows below this index point.  This
		may be the first element of a matrix, but it doesn't have
		to be.  In this program it is dynamic and moves to 
		progressively higher row numbers as data is accumulated.
	n1sn - leading dimension of sn (Fortran form)
	column_index - This is a vector of integers of length m that gives
		the column index position for each input row of U.  The
		algorithm works by using this index to copy and scale rows
		of U (columns of U^T) into the correct column of sn.
	w - m vector of weights for each row of original equations.  These
		are the diagonal components of S that scale the vectors
		copied to sn.

Return codes:
	0 - normal return, no problems flagged
	-1 - failure in svd routine makes station correction estimation
		impossible, ignore results.    

Author:  GAry Pavlis
Written:  October 2000
*/
void accumulate_sn_matrix(int m, double *U, int n1u, int nnull,
		double *sn, int n1sn, int *column_index, double *w)
{
	int i, ic;
	double *snvector;

	for(i=0;i<m;++i)
	{
	    /* 0 weights and a negative column index are both
	    methods used by earlier routines to flag problem data.
	    Note this assumes sn was cleared to 0s before running this
	    or there may be garbage left in skipped data columns */
	    if( (w[i]>0.0) && (column_index[i]>=0) )
	    {
		ic = column_index[i];
		snvector = sn + ic*n1sn;
		dcopy(nnull,U+i,n1u,snvector,1);
		dscal(nnull,w[i],snvector,1);
	    }
	}
}
			
/* This small function actually forms the column index vector used
in the algorithm immediately above.  

Arguments:
	smatrix - SCMatrix structure pointer holding sc matrix work
		space and associative arrays required to produce
		the index.
	ta - list of Arrival structures used to form original 
		equations.  
	cindex - vector to return parallel vector of column indexes
		for each row of matrix formed from ta.

Author:  GAry Pavlis
Written:  October 2000
*/
void form_column_indices(SCMatrix *sc, Tbl *ta, int *cindex)
{
	Arrival *a;
	int i;
	int *iphase,*ista;
	/* This assumes cindex has been alloced to be of this size.
	We have to clear it to 0 initially */
	for(i=0;i<(sc->ncol);++i) cindex[i]=0;

	for(i=0;i<maxtbl(ta);++i)
	{
		a = (Arrival *)gettbl(ta,i);
		ista = (int *)getarr(sc->sta_index,a->sta->name);
		iphase = (int *)getarr(sc->phase_index,a->phase->name);
		if((ista==NULL) || (iphase==NULL))
		{
			elog_complain(0,"Indexing error searching for station %s and phase %s while trying to build station correction matrix\nDatum deleted\n",
				a->sta->name,a->phase->name);
			cindex[i] = -1;
		}
		else
			cindex[i] = (*iphase)+(*ista);
	}
}
/* Small error function companion to LAPACK/sunperf svd routine.
*/
void svd_error(int ecode)
{
	if(ecode > 0)
		elog_complain(0,"Convergence failure in (dsc)gesvd SVD routine\nResults may be unreliable\n");
	else if(ecode < 0)
		elog_complain(0,"Illegal argument number %d passed to (dsc)gesvd SVD routine\nResults are probably unreliable\n",-ecode);
}
/* Small error function for ggnloc that checks the reasons for convergence
Tbl (reasons argument) errors.  It returns if it defines the solution as
ok and 1 if it defines it as "bad".  This abstracts the process of 
defining a convergence making it easier to change the definition of 
bad as well as reducing clutter in the main program.

Author:  Gary Pavlis
Written: October 2000
*/

int pmel_hypo_solution_bad(Tbl *reasons)
{
	int i;
	char *s;
	char *test;

	for(i=0;i<maxtbl(reasons);++i)
	{
		s = (char *)gettbl(reasons,i);
		test = strstr(s,"Error");
		if(test!=NULL) return(1);
		test = strstr(s,"Location hit iteration");
		if(test!=NULL) return(1);
	}
	return(0);
}
/* Sets elements of the hypocenter structure to invalid values to 
flag bad solutions.  Several definitions of bad are used to 
allow multiple ways of testing for this condition.  Probably less than
ideal as it can be confusing, but better than setting one arbitrary 
element of the complicated structure.

*/
void set_hypo_as_bad(Hypocenter *h)
{
	h->rms_raw = -1.0;
	h->rms_weighted = -1.0;
	h->number_data = -1;
	h->degrees_of_freedom = -1;
	h->t0 = -1000.0;
	h->z0 = -10000.0;
	h->used = 0;
}
/* inverse routine that test if a hypo set as above is so marked */
int hypo_is_bad(Hypocenter *h)
{
	if(h->rms_raw<=0.0) return(0);
	if(h->rms_weighted<=0.0) return(0);
	if(h->number_data<=0)return(0);
	if(h->degrees_of_freedom<=0)return(0);
	return(1);
}
	
	


/* 

Arguments:
    nevents - number of events in this group to be processed.
    evid - vector of nevent ints that are event ids for associated
	parallel arrays ta and h0.  
    ta - vector of Tbl pointers with nevents elements.  Each
        element of ta is assumed to contain an input Tbl
        to be passed as data to ggnloc.
    h0 - vector nevents Hypocenter pointers with initial hypocenter
        estimates for data stored in ta arrival lists.   On
        exit it contains the revised hypocenter structure for
        the relocated events.  NEEDS A METHOD TO FLAG FAILED 
        AND/OR NOT USED
    fix - array keyed by evid of events that have one or more
	coordinates to be fixed.  These can be calibration events
	or they have fixed depth, but the result is the same.
	That is, that coordinate is viewed as not a variable.
	The actual contents of what is returned from a "fix"
	evid is a string of characters.  We look for 'x',
	'y', 'z', and 't'.  These are handled by genloc
	as fixed coordinates.
    hypocen - hypocentroid
    s - SCMatrix structure that holds work space for S matrix 
        and associated indexing and size parameters (see definition
        in location.h)  It also is the primary input and output
	workspace.  The sc vector within s is the primary output
	along with the rms parameters also stored there.
    phase_arr - associative array of phase handles.   The station
	correction field of these objects are altered by pmel.
    pf - parameter file pointer (used to parse options for 
        ggnloc.
    sc_converge_history - contains a list of stings describing reason
	pmel broke the convergence loop 
    pmelhistory - list of hypocentroids (pointers to Hypocenter
	objects) summarizing convergence history.  

Returns a pointer to a Tbl that contains a stack of character strings
that describe reasons for breaking the main iteration loop.
Calling program may wish to trap nonconvergence on count.  

Author:  Gary Pavlis
Written:  October 2000
*/

int pmel(int nevents,
    long *evid,
        Tbl **ta,
            Hypocenter *h0, 
		Arr *fixarr,
                    Hypocenter *hypocen,
                        SCMatrix *s,
			    Arr *phase_arr,
				Location_options *o,
                                	Pf *pf,
					    Tbl **sc_converge_reasons,
					        Tbl **pmelhistory)
{
    int i,j,k;
    Tbl *tu;
    char *fix;
    int locrcode;
    double *A;  /* FORTRAN order matrix of equation of condition */
    double *U,*svalue,*Vt;  /* Used to store full SVD */
    double *wts;  /* used here as product of w and reswt */
    double *residuals;  /* working vector of residuals */
    int *cindex;  /* working vector of column index positions.
	That is, these hold column index for sta:phase for each row of A*/

    int nr,nc;  /* Copies of s->nrow and s->ncol used for clarity*/
    /* These keep track of counts of data and events used in solution*/
    int nev_used, ndata_used, total_ndgf,total_ndgf2,sc_iterations=0;
    int sc_adjustments=0,sc_adjusted_last_pass;
    int hypo_iterations;
    int nrows_S, nnull;  /* nrows_S is an index to the current high water
			mark as S is filled */

    /* These are total residual figures */
    double total_rms_raw,total_ssq_raw, total_wssq,total_wssq2;
    double rhsnrm;
    double *bwork;
    double sswrodgf,sswrodgf2;
    /* ggnloc output lists */
    Tbl *history=NULL, *reasons=NULL, *restbl=NULL;
    double escale;
    /* These are pmel control parameters parsed from pf */
    double esmin,esinit;
    double sswrodgf_min;
    double sswr_test;
    int ndgf_test;
    int maxscit;
    int delete_bad;
    double F_critical_value;
    int cluster_mode; 
    double rsvc;  
    /* Related to form_equations */
    Robust_statistics stats;
    float **Amatrix, *b, *r, *w, *reswt;
    int nused,svdinfo;
    /* hold residuals for station correction inversion */
    double *scrhs;
    /* holds station correction perturbation solved for in each interation */
    double *sc_solved;
    double ds_over_s,ds_over_s_converge;
    Hypocenter *hypocen_history;
    double centroid_lat, centroid_lon, centroid_z;
    int adjust_sc_ok;   /*used in ftest set true when list of events kept is constant */

    /* initialize the output lists, WARNING:  memory leak here if
    caller doesn't clear these before calling. */
    *pmelhistory=newtbl(0);
    *sc_converge_reasons=newtbl(0);

    nr = s->nrow;
    nc = s->ncol;
    /* 
    The +4 is a fudge factor to fix an apparent bug in sunperf's
    C interface to dgesvd that generates rua errors in access checking
    */
    allot(double *,A,4*nc+4);
    allot(double *,U,nc*nc);
    /* Vt and svalue are used as work space in inversion of S matrix 
    as well as accumulation phase for individual events */
    allot(double *,Vt,nc*nc+2);
    allot(double *,svalue,nc);
    allot(double *,wts,nc);
    allot(double *,residuals,nc);
    allot(int *,cindex,nc);
    allot(double *,scrhs,nr);
    allot(double *,bwork,nr);
    allot(double *,sc_solved,nc);
    /* I hate this mixed double and float, but at this point I 
    don't want to create the havoc it would cause to make everything
    double */
    allot(float *,b,nc);
    allot(float *,r,nc);
    allot(float *,w,nc);
    allot(float *,reswt,nc);
    Amatrix = matrix(0,nc-1,0,3);

    /* We now extract parameters from pf specific to pmel */
    esmin = pfget_double(pf,"pmel_minimum_error_scale");
    esinit = pfget_double(pf,"pmel_initial_error_scale");
    sswrodgf_min = pfget_double(pf,"pmel_minimum_sswrodgf");
    maxscit = pfget_int(pf,"pmel_maximum_sc_iterations");
    delete_bad = pfget_boolean(pf,"pmel_autodelete_high_rms");
    F_critical_value = pfget_double(pf,"pmel_F_test_critical_value");
    cluster_mode = pfget_boolean(pf,"pmel_cluster_mode");
    rsvc = pfget_double(pf,"pmel_svd_relative_cutoff");
    ds_over_s_converge = pfget_double(pf,"pmel_sc_fraction_convergence_error");

    /* We initialize the Tbl for slowness vectors to a zero length
    list that will be passed to ggnloc.  This is by design under
    a view that slowness vector corrections should be determined
    independently from time corrections and can be done by simple
    averaging of measurments from well located events.  Because
    ggnloc looks at tu it must at least be initialized.*/
    tu = newtbl(0);
    /* This controls residual weight error scaling in a global sense */
    escale = esinit;
    o->min_error_scale = escale;
    /* The boolean "used" in the hypo structure, which was added for this
    program, needs to be cleanly initialized at the top of this loop.  It
    may have been already set earlier, but the algorithm depends on this
    initialization so we must be sure */
    for(i=0;i<nevents;++i) h0[i].used = 1;

    /* Top of processing loop for this group */
    fprintf(stdout,"Iteration Raw_rms Escale  sswrodgf sswrodgf2 ndgf  ndgf2\
Nevents Nevents_used\n");
    sc_iterations = 0;
    sc_adjusted_last_pass=0;
    do {
        Hypocenter *current_hypo;
        double current_wssq;
	int nrow_amatrix,ncol_amatrix;
	/* top of loop initializations */
	adjust_sc_ok = 1;
        nev_used = 0;
        ndata_used = 0;
        total_ndgf = 0;
	nrows_S = 0;
	for(k=0;k<((s->nrow)*(s->ncol));++k) s->S[k]=0.0;

	/* main loop over events */
        for(i=0,total_ssq_raw=0.0,total_wssq=0.0,centroid_lat=0.0,
		centroid_lon=0.0,centroid_z=0.0;i<nevents;++i)
        {
	    for(j=0;j<4;++j)o->fix[j]=0;
	    fix = get_fixlist(fixarr,evid[i]);
	    if(fix==NULL)
	    {
		ncol_amatrix = 4;
	    }
	    else
	    {
		ncol_amatrix = 4;
		if(strstr(fix,"x")!=NULL) 
		{
			o->fix[0]=1;
			--ncol_amatrix;
		}
 		if(strstr(fix,"y")!=NULL)
		{
			o->fix[1]=1;
			--ncol_amatrix;
		}
		if(strstr(fix,"z")!=NULL)
		{
			o->fix[2]=1;
			--ncol_amatrix;
		}
		if(strstr(fix,"t")!=NULL)
		{
			o->fix[3]=1;
			--ncol_amatrix;
		}
	    }
            locrcode=ggnloc(h0[i],ta[i],tu,*o,
                &history,&reasons,&restbl);
	    /* maintenance note in the segment below:  watch out for distinction
	    between h0[i] (starting hypo) and current_hypo (one returned by ggnloc).
	    Eventually h0 has to be replaced by current_hypo, but because of the 
	    fact an event can be marked with h0[i].used set false by several mechanism
	    changing the logic is error prone.  Be advised */
            if(locrcode < 0)
            {
                elog_notify(0,"ggnloc failed to produce a solution for event %d in current group for iteration %d\n",
                    i,sc_iterations);
		if(h0[i].used) adjust_sc_ok = 0;
		/* note this routine does not change the hypocenter, but only 
		alters rms figure statistics toif(h0[i].used) adjust_sc_ok = 0; flag a bad solution */
                set_hypo_as_bad(h0+i);
            }
              /* This function checks the reasons list
                for failures in ggnloc not flagged with
                the negative convergence code. Currently
                this means hitting the iteration limit, but
                I abstract it here to make this easier to
                maintain.*/
            else if(pmel_hypo_solution_bad(reasons))
	    {
		if(h0[i].used) adjust_sc_ok = 0;
		/* we don't mark the solution "bad" like before hoping we can recover
		in a later iteration */
		h0[i].used = 0;
		elog_clear_register(1);
	    }
            else
            {
                if(locrcode>0)
                {
                    elog_notify(0,"%d travel time errors locating event %ld of current group for iteration %d\n",
                        locrcode,evid[i],sc_iterations);
                }
		elog_clear_register(1);
                hypo_iterations = maxtbl(history);
                current_hypo = (Hypocenter *)gettbl(history,
                            hypo_iterations - 1);
   
                /* This tests for high rms relative to
                the rest of the group and discards events
                with high average weighted rms residuals */
                current_wssq = (current_hypo->rms_weighted)
                        *(current_hypo->rms_weighted);
		if((current_hypo->degrees_of_freedom)<=0)
		{
			if(h0[i].used) adjust_sc_ok = 0;
			current_hypo->used = 0;
		}
                else if(sc_iterations>0 && delete_bad
			&& (ncol_amatrix>0))
		{
			if(h0[i].used)
			{
				if(ftest_subset(current_wssq,
				    current_hypo->degrees_of_freedom,
				    sswr_test,ndgf_test,
					F_critical_value))
						current_hypo->used=0;
				else
					current_hypo->used=1;
			}
			else
			{
				if(ftest1(current_wssq,
				    current_hypo->degrees_of_freedom,
				    sswr_test,ndgf_test,F_critical_value))
					current_hypo->used = 0;
				else
					current_hypo->used = 1;

			}
			if(current_hypo->used != h0[i].used) adjust_sc_ok=0;
		}
		/* Note the above works because this function copies the boolean
		"used" member of the structure */
                copy_hypocenter(current_hypo,h0+i);
		/* skip all the steps below when an event is not used */
		if(current_hypo->used)
		{
		    /* accumulate hypocentroid and global statistics*/
 		    centroid_lat += current_hypo->lat;
		    centroid_lon += current_hypo->lon;
		    centroid_z += current_hypo->z;                     
                    total_ndgf += current_hypo->degrees_of_freedom;
                    ndata_used += current_hypo->number_data;
                    total_ssq_raw += (current_hypo->rms_raw)
					*(current_hypo->rms_raw);
                    total_wssq += current_wssq;
		    ++nev_used;

                    /* Now we turn to the station correction
                    accumulation*/
		    nrow_amatrix = maxtbl(ta[i]);
		    stats = form_equations(ALL,*current_hypo,ta[i],tu,*o,
				Amatrix,b,r,w,reswt,&nused);
		    for(j=0;j<nrow_amatrix;++j)
		    {
		        wts[j] = ((double)w[j])*((double)reswt[j]);
		        residuals[j] = (double)b[j];
		    }
		    if(ncol_amatrix>0)
		    {
		    if(cluster_mode)
		    {
		    /* The hypocentroid has no time field.  We set it
			to the current_hypo value to keep from having
			absurdly large residuals */
		    hypocen->time=current_hypo->time;
		        stats = form_equations(ALL,*hypocen,ta[i],tu,*o,
				    Amatrix,b,r,w,reswt,&nused);
		    /* We want these equations weighted by the ones
		    computed from the actual hypocenter, not the hypocentroid.
		    This sets the matrix weights back to be consistent with
		    those computed above.  */
		        for(j=0;j<nrow_amatrix;++j)
			    for(k=0;k<ncol_amatrix;++k)
			        if((w[j]>0.0)&&(reswt[j]>0.0)) 
				    Amatrix[j][k] 
				          *= ((float)wts[j])/(w[j]*reswt[j]);
		    }
		    /* C matrix form to FORTRAN matrix conversion required to
		    interface with LAPACK svd routine.  Note we use nc as
		    equivalent to the leading dimension of a FORTRAN array.
		    Symbol is confusing because this is actually rows of
		    a matrix in fortran form.*/
		    for(j=0;j<nrow_amatrix;++j)
		        for(k=0;k<ncol_amatrix;++k)
			        A[j+nc*k] = (double)(Amatrix[j][k]);

		    /* Compute the svd of this matrix.  All we need here 
		    is the left singular vectors spanning the data space */
		    dgesvd('a','n',nrow_amatrix,ncol_amatrix,A,
			    nc, svalue, U, nc, Vt, nc, &svdinfo);
  		    if(svdinfo) 
		    {
		        elog_notify(0,"pmel:  svd error processing event number %d\n",i);
		        svd_error(svdinfo);
		    }
		    /* Compute S_N as U_N^T*S.  WARNING:  this assumes
		    blindly that smatrix->S was allocated big enough
		    to hold all of S as we accumulate it here. */
		    nnull = nrow_amatrix-ncol_amatrix;
    		    form_column_indices(s,ta[i],cindex);
		    accumulate_sn_matrix(nrow_amatrix,U+nc*ncol_amatrix,
			    nc,nnull,(s->S)+nrows_S,s->nrow,cindex,wts);
		    /* This forms the annulled data */
		    for(j=0,k=nrows_S;j<nnull;++j,++k)
		    {
			    scrhs[k] = ddot(nrow_amatrix,U+nc*(ncol_amatrix+j),1,
						residuals,1);
		    }
		    nrows_S += nnull; 
		    }
		    else
		    {
			/* Special block for all fixed coordinates */
			form_column_indices(s,ta[i],cindex);
			for(j=0,k=nrows_S;j<nrow_amatrix;++j,++k)
			{
				scrhs[k]=residuals[j];
				s->S[k+cindex[j]*(s->nrow)]=wts[j];
			}
			nrows_S+=nrow_amatrix;
		    }
		}   
            }
            if(maxtbl(history))freetbl(history,free);
            if(maxtbl(reasons))freetbl(reasons,free);
            if(maxtbl(restbl))freetbl(restbl,free);
        }
	/* We always have to compute the residual stats even if 
	we are not going to adjust the station corrections.
	There are two variants on weighted rms: before and after
	the current linear correction.  We compute them both. 
	Before the current station correction vector applied is
	produced through accumulation above in total_wssq.  The
	linear corrected version is total_wssq2.  A critical thing
	is that sometimes that later can be zero of have limited
	degrees of freedom */
	if(total_ndgf<=0)
	{
		elog_notify(0,"Insufficient data:  degrees of freedom not\
 positive\n");
		pushtbl(*sc_converge_reasons,
			strdup("ABORT on insufficient data"));
	        free(A);
	        free(U);
	        free(Vt);
	        free(svalue);
	        free(wts);
	        free(residuals);
	        free(cindex);
	        free(scrhs);
	        free(bwork);
	        free(sc_solved);
	        free(b);
	        free(r);
	        free(w);
	        free(reswt);
	        free_matrix((char **)Amatrix,0,nc-1,0);
	        freetbl(tu,free);
		return(-1);
	}
	/* This test will incorrect terminate the loop if the station
	corrections were not adjusted in the previous iteration. It has
	to be outside the station correction adjustment section because
	we base rms only on misfit after relocation. */
	sswrodgf = total_wssq/((double)total_ndgf);
	if(sc_adjustments>0 && sc_adjusted_last_pass)
	    if(ftest2(sswrodgf, total_ndgf, s->sswrodgf, s->ndgf,
			SSWR_TEST_LEVEL)==0)
		    pushtbl(*sc_converge_reasons,
			strdup("No improvement in data fit"));

	escale = sqrt(sswrodgf);
	if(escale<esmin) escale = esmin;
	o->min_error_scale = escale;
	s->ndgf = total_ndgf;
	s->sswrodgf = sswrodgf;
	total_rms_raw = sqrt(total_ssq_raw/((double)total_ndgf));
	s->rmsraw = total_rms_raw;
	if(sswrodgf<sswrodgf_min) 
	{
		sswr_test = sswrodgf_min*((double)total_ndgf);
		ndgf_test = total_ndgf;
	}
	else
	{
		sswr_test = total_wssq;
		ndgf_test = total_ndgf;
	}
	/* We only adjust they station corections if this
	flag is true.  This only happens when the list of
	events kept stabilizes from one pass to the next */
	if(sc_adjusted_last_pass==0) adjust_sc_ok=1;  /* force this or we may not converge */
	if(adjust_sc_ok)
	{
	    if(nrows_S<=0)
	    {
		elog_notify(0,"pmel:  Insufficient data to compute\
station corrections\n");
		return(-1);
	    }
	    /* Now we solve for station correction adjustments that 
	    are determinate from the available data. U is not actually
	    hit because we use the 'o' flag, but Vt is set*/
	    dgesvd('o','a',nrows_S,nc,s->S,nr,svalue,U,nc,Vt,nc,&svdinfo);
	    if(svdinfo) 
	    {
	        elog_complain(0,"pmel:  svd error inverting station correction matrix\n");
	        svd_error(svdinfo);
	        return(-1);
	    }
	    /* This is a pseudoinverse solver. It returns the number of
	    singular values used for the solution which we used below to
	    do subspace projections */
	    nused = dpinv_solver(nrows_S,nc,s->S,nr,svalue,Vt,nc,
		scrhs,sc_solved,rsvc);

	    /* Now we apply the projectors.  We first add the current solution
	    as a perturbation factor to the current total station correction
	    vector and then project it onto range defined by svd of the S
	    matrix.  This may not be necessary for teleseismic events, but
	    can be significant if large changes happen in initial steps
	    that distort the matrices used to construct S. */
	    daxpy(nc,1.0,sc_solved,1,s->sc,1);
	    if(model_space_range_project(Vt,nc,nused,nc,s->sc,s->scdata))
		elog_complain(0,"sc matrix size error\n");
	    if(model_space_null_project(Vt,nc,nused,nc,s->scref,s->scbias))
		elog_complain(0,"sc matrix size error\n");
	    dcopy(nc,s->scdata,1,s->sc,1);
	    for(k=0;k<nc;++k)s->sc[k] += s->scbias[k];

	    /* This routine updates the active list of station corrections
	    in the phase handles.  */
	    if(update_scarr(s,phase_arr))
	    {
		elog_complain(0,"problems updating station \
corrections during iteration %d\n",
			sc_iterations);
	    }
	    /* Test for small correction vector relative to norm s.
	    It is intentional to divide by scdata as adjustments
	    only happen in the subspace covered by scdata. If we used the
	    full vector s->sc it can be artificially large */
	    ds_over_s = dnrm2(nc,sc_solved,1)/dnrm2(nc,s->scdata,1);
/*
ds_over_s=dnrm2_(&nc,sc_solved,&one)/dnrm2_(&nc,s->scdata,&one);
*/

	    if(ds_over_s<ds_over_s_converge)
		pushtbl(*sc_converge_reasons,
			strdup("Small adjustment to station corrections"));

	    /* a second measure of rms*/
	    if(data_space_null_project(s->S,nr,nused,nrows_S,scrhs,bwork))
		elog_complain(0,"Problems in data_space_null_project\n");
	    rhsnrm = dnrm2(nrows_S,bwork,1);
	    total_wssq2 = rhsnrm*rhsnrm;
	    total_ndgf2 = total_ndgf - nused;  /*use nused as svd truncation
						will be the norm with
						missing phases */
	    if(total_ndgf2<=0)
		sswrodgf2=0.0;
	    else
		sswrodgf2=total_wssq2/((double)total_ndgf2);

	    /* compute the new hypocentroid 
	    initialize the new hypocentroid this way */
	    allot(Hypocenter *,hypocen_history,1);
	    initialize_hypocenter(hypocen);
	    copy_hypocenter(hypocen,hypocen_history);
	    hypocen_history->lat0 = hypocen_history->lat;
	    hypocen_history->lon0 = hypocen_history->lon;
	    hypocen_history->z0 = hypocen_history->z;
	    hypocen_history->lat = centroid_lat/((double)nev_used);
	    hypocen_history->lon = centroid_lon/((double)nev_used);
	    hypocen_history->z = centroid_z/((double)nev_used);
	    /* This is a convenient place to store these, but they aren't
	    totally consistent with the concept of a hypocentroid */
	    hypocen_history->rms_raw = total_rms_raw;
	    hypocen_history->rms_weighted=sswrodgf;
	    hypocen_history->number_data = nr;
	    hypocen_history->degrees_of_freedom = total_ndgf;
	    pushtbl(*pmelhistory,hypocen_history);
	    copy_hypocenter(hypocen_history,hypocen);
	    fprintf(stdout,"%d  %lf  %lf  %lf %lf %d %d %d %d\n",
		sc_iterations,total_rms_raw,escale,
			sswrodgf,sswrodgf2,
			total_ndgf,total_ndgf2,nevents,nev_used);
	    ++sc_adjustments;
	    sc_adjusted_last_pass = 1;
	}
	else
	{
	    sc_adjusted_last_pass = 0;
	    fprintf(stdout,"%d  --No station correction change this pass--\n",
		sc_iterations);
	    /* This forces another iteration whenever irregularities 
	    in hypocenter use changes */
	    if(maxtbl(*sc_converge_reasons)>0)
	    {
		freetbl(*sc_converge_reasons,free);
		*sc_converge_reasons = newtbl(0);
	    }
	}

	++sc_iterations;
        if(sc_iterations>maxscit) pushtbl(*sc_converge_reasons,
			strdup("Hit station correction iteration limit"));
    }
    while (maxtbl(*sc_converge_reasons)<=0 );
    fprintf(stdout,"Convergence in %d total iterations with %d adjustments\n",
		sc_iterations,sc_adjustments);

    free(A);
    free(U);
    free(Vt);
    free(svalue);
    free(wts);
    free(residuals);
    free(cindex);
    free(scrhs);
    free(bwork);
    free(sc_solved);
    free(b);
    free(r);
    free(w);
    free(reswt);
    free_matrix((char **)Amatrix,0,nc-1,0);
    freetbl(tu,free);
    return(0);
}
