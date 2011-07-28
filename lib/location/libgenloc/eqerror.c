
#include "stock.h"
#include "coords.h"
#include "db.h"
#include "location.h" 

#include "perf.h"
#define SUNPERF 1

/* This routines cautiously saves the emodel vector.  It is cautious
because the emodel table is not part of the css3.0 schema.  If it
cannot find info on the emodel table, it issues an error and returns.
Otherwise, it will save the emodel vector to this special table.
Author:  Gary L. Pavlis
Written: June 29, 1998 */
int save_emodel(int orid, float *emodel, Dbptr db)
{
	Dbptr dbemod;  

	dbemod = dblookup(db, 0,"emodel",0,0);
	if(dbemod.table == dbINVALID)
	{
		elog_log(0,"emodel table not defined for output database\nAdd genloc mods\n");
		return(1);
	}
	else
	{
		if(dbaddv(dbemod,0,
			"orid",orid,
			"emodelx",emodel[0],
			"emodely",emodel[1],
			"emodelz",emodel[2],
			"emodelt",emodel[3],
				NULL ) == dbINVALID)
		{
			elog_log(0,"dbaddv error for emodel table\n");
			return(-1);
		}
	}
	return(0);
}

/* This routine computes the pure pseudoinverse from the svd returned
by svdcmp (U*S*V^T) so Agi = V*S^-1*U^T.  The algorithm used is a
little overly tricky using an internally allocated work vector to 
make the routine nondestructive to the input matrices.  This computes
the "pure" pseudoinverse by setting the svd cutoff value based on
float epsilon (from float.h).

Note input U is mxn, s is an n vector, V is nxn, and the output
Agi is nxm.  

Function returns the number of singular values actually used to 
compute Agi.  

Author:  Gary L. Pavlis
*/
int pseudoinverse(float **U, float *s, float **V, int m, int n, float **Agi)
{
	int i,j, k;  /* counters*/
	float *work;  /* work space */
	float smax;
	float sinv;
	double sv_cutoff;
	int nsv_used;
#ifndef SUNPERF
	int one=1;
#endif


        if((work=(float *)calloc(n,sizeof(float))) == NULL)
                elog_die(1,"Pseudoinverse computation: cannot alloc work array of length %d\n",
                        n);

	/* first find the larges singular value, then just zero
	all those smaller than the cutoff determined as the ratio
	wrt to largest singular value */
	smax = 0.0;
	for(i=0;i<n;++i) 
		if(s[i] > smax) smax = s[i];
	sv_cutoff = (double)smax*FLT_EPSILON;
	/* This is a copy operation */
	for(i=0;i<m;++i)
		for(j=0;j<n;++j) Agi[j][i] = U[i][j];
	/* this works because of C storage order, but is strange.
	It is the multiply by S^-1 */
	for(j=0,nsv_used=0;j<n;++j)
	{
		if( (double)s[j] > sv_cutoff)
		{
			sinv = 1.0/s[j];
			++nsv_used;
		}
		else
			sinv = 0.0;
#ifdef SUNPERF
		sscal(m,sinv,Agi[j],1);
#else
		sscal_(&m,&sinv,Agi[j],&one);
#endif
	}
	/* multiply by V using a column work vector*/
	for(j=0;j<m;++j)
	{
		for(k=0;k<n;++k) work[k] = Agi[k][j];
		for(i=0;i<n;++i)
#ifdef SUNPERF
			Agi[i][j] = sdot(n,work,1,V[i],1);
#else
			Agi[i][j] = sdot_(&n,work,&one,V[i],&one);
#endif
	}
	free(work);
	return(nsv_used);
}
			



/* This routine computes the covariance matrix from the generalized
inverse stored in Agi in numerical recipes matrix format (doubly
index C arrays).  Agi is assumed to be n x m.  What is returned 
is controlled by the fix vector.  fix flags fixed parameters in
the rows of Agi.  C must be of size ntotal by ntotal.  The algorithm 
is to assume some rows of Agi are missing whenever any elements of 
fix are nonzero.  The corresponding rows/columns to these "fixed"
parameter values are flagged. This makes for some messy indexing.

Author:  Gary L Pavlis
Written:  June 26, 1998
November 2000
Changed to mixed precision arithmetic with output C matrix now being
double.  Original was float.  This was done by using dsdot.
*/
void compute_covariance(float **Agi, int m, int n, int ntotal, double **C, int *fix)
{
	int i,j,ii,jj;
#ifndef SUNPERF
	int one=1;
#endif

	for(i=0,ii=0;i<ntotal;++i)
	{
		if(fix[i])
			for(j=0;j<ntotal;++j) C[i][j] = 0.0;
		else
		{
			for(j=0,jj=0;j<ntotal;++j)
				if(fix[j])
					C[i][j] = 0.0;
				else
				{
#ifdef SUNPERF
					C[i][j] = dsdot(m,Agi[jj],1,
							Agi[ii],1);
#else
					C[i][j] = dsdot_(&m,Agi[jj],&one,
							 Agi[ii],&one);
#endif
					++jj;
				}
			++ii;
		}
	}
	
}
/* Computes unscaled emodel from generalized inverse, Agi, strored in
numerical recipes matrix form (with 0 first index) using componentwise
bounding criteria.  input vector b (of length n) is assumed positive and 
contains a set of time bounding errors.  In Pavlis (1986) this term is
defined in equation (25).  Here, however, I use the time form of the 
vector b rather than use the distance term described in the 1986 paper.
That is, b has units of time derived here as a fixed scaling constant
times the computed travel time.  (see below)

Note that the emodel is alloced here and needs to be freed elsewhere.

Author:  Gary L. Pavlis
Written:  June 25, 1998
*/
void compute_emodel(float **Agi, int m, int n, float *b, float *emodel)
{
	int i,j;
	for(i=0;i<n;++i)
	{
		emodel[i]=0.0;
		for(j=0;j<m;++j) emodel[i] += (float)fabs((double)Agi[i][j])*b[j];
	}
}
/* This routine computes standard and nonstandard earthquake location 
error estimates.  standard error estimate returned is the covariance 
matrix of the predicted errors that can be used to derive standard
error ellipses.  The covariance that is returned is unscaled.  It is the
true covariance only when the weights are ideal 1/sigma weighting.
(Note residual (robust) weighting does not really alter this as the goal
of residual weighting is to downweight outliers and reshape the residual
distribution to be closer to a normal disltribution.  

The nonstandard error that is returned is the "model error" defined
in Pavlis (1986) BSSA, 76, 1699-1717.  

It would be preferable in some ways to split these into two modules, but
it is more efficient to compute them together.

Arguments:
-input-
h - hypocenter location of point to appraise errors from
attbl- Associate array of Arrival structures
utbl - associate array of slowness structures
o- Location options structure 
-output-
C - 4x4 covariance matrix.  Assumed allocated externally and 
passed here.  alloc with modified numerical recipes "matrix" routine.
emodel - 4 vector of x,y,z model error estimates 

Both error estimates have 0 in positions where fix is set.  

Returns 0 if no problems are encountered.  Nonzero return
indicates a problem.  

+1 - Inverse matrix was singular to machine precision, error results
	are unreliable as svd truncation was need to stabilize 
	calculations.


Author:  Gary L. Pavlis
Written:  June 25, 1998
*/
void predicted_errors(Hypocenter h, 
	Tbl *attbl, Tbl *utbl,
	Location_options o,
	double **C, float *emodel)
{
        Arrival *atimes;
	Slowness_vector *slow;
	int natimes;
	int nslow;
	int npar, nused, ndata_feq;

	float **U, **V, s[4];  /* SVD of A = USVT */
	float **Agi;  /* holds computed generalized inverse */
	float *b;  /* weighted residual vector (rhs of Ax=b)*/
	float *r,*w;  /* r=raw residual and w= vector of total weights */
	float *reswt;  /* we store residual weights seperately to calculate
			effective degrees of freedom */
	int m;  /* total number of data points = natimes+2*nslow */
	Robust_statistics statistics;
	int mode;
	int i,ii,j;

	/* because o is a dynamic variable, we force the
	plain pseudoinverse soltuion */
	o.generalized_inverse = PSEUDOINVERSE;

	natimes = maxtbl(attbl);
	nslow = maxtbl(utbl);

	m = 2*nslow + natimes;
	for(i=0,npar=0;i<4;++i)
		if(!o.fix[i]) ++npar; 

	Agi = matrix(0,3,0,m-1);
	U = matrix(0,m-1,0,3);
	V = matrix(0,3,0,3);


	b=(float *)calloc(m,sizeof(float));
	r=(float *)calloc(m,sizeof(float));
	w=(float *)calloc(m,sizeof(float));
	reswt=(float *)calloc(m,sizeof(float));

	if ( (b==NULL) || (r==NULL)
		|| (w==NULL) || (reswt==NULL) )
		elog_die(1,"Alloc errors in location error function\n");

	statistics = form_equations(ALL, h, attbl,utbl,
			o, U, b, r, w, reswt,&ndata_feq);
	svdcmp(U,m,npar,s,V);
	/* This computes the generalized inverse as the pseudoinverse.
	This version forces this to be the true pseudoinverse no
	matter what the user selected as a location option.  This is
	my (glp) prejudice that reporting anything else is a lie.*/

	nused = pseudoinverse(U,s,V,m,npar,Agi);
	if(nused != npar ) 
	{
		elog_log(0,"predicted_errors function found system of equations was singular, Errors estimates are unreliable\n");
	}
	compute_covariance(Agi,m,npar,4,C,o.fix);

	/* Now we compute the emodel vector defined in 
	 Pavlis (1986) BSSA, 76, 1699-1717. A complication is that
	we need to do a complex rescan of the arrival and slowness
	data tables.  This is necessary to allow different 
	bounding terms for different phases and to obtain most
	easily the computed travel time to each stations.  
	We just recompute the travel times and use an associated
	scale factor.  For slowness we just enter a fixed value
	under and assumption slowness errors do not increase 
	with distance as travel times tend to do. */
	mode = RESIDUALS_ONLY;
	for(i=0;i<natimes;++i)
        {
                Travel_Time_Function_Output tto;
                atimes = (Arrival *) gettbl(attbl,i);
                tto = calculate_travel_time(*atimes, h,mode);
                if(tto.time == TIME_INVALID)
			b[i] = 0.0;
		else
		{
			b[i] = tto.time;
			b[i] *= w[i]*reswt[i]*atimes->phase->deltat_bound;
		}
	}
	for(i=0,j=natimes;i<nslow;++i,j+=2)
        {
                slow = (Slowness_vector *) gettbl(utbl,i);
		b[j] = slow->phase->deltau_bound;
		b[j+1]=b[j];
	}
	/* we recycle w here blindly assuming m>n */
	compute_emodel(Agi, m, npar, b, w);
	for(i=0,ii=0;i<4;++i)
		if(o.fix[i])
			emodel[i] = 0.0;
		else
		{
			emodel[i] = b[ii];
			++ii;
		}
        free_matrix((char **)Agi,0,3,0);
        free_matrix((char **)U,0,m-1,0);
        free_matrix((char **)V,0,3,0);
        free(b);
        free(r);
        free(w);
        free(reswt);
}
/* Implements error ellipses from input covariance matrix assuming 
particular error model defined by "model".  
(inputs)
C - 4x4 covariance matrix in order x,y,z,t.  If depth is fixed it is assumd
	all the z terms have been zeroed.  Use of an eigenvector routine 
	assures it will still be stable and algorithms used should still
	all be stable.  
model - error model (CHI_SQUARE or F_DIST defined in location.h) 
conf - confidence level (this function will complain if not 0.683 or 0.9
	and round to closest)  Must be 0<conf<1.0  This value is altered
	to one of these values if necessary.
rms - residual norm used to scale error ellipse in F distribution model 
	(ignored with chisquare model)
dgf - number of degrees of freedom (enters only in F_DIST case, ignored
	for chi-square error model)
(outputs)
smajax, sminax, strike - horizontal error ellipse as defined in css3.0
sdepth, stime - depth and time error attributes in css3.0

Normal return is 0.  Returns -1 if computation totally fails.  Calling
function should trap this condition results are all set 0 in that 
situation.
Written:  November 2000
Authors:  G Pavlis and Kent Lindquist
*/
int project_covariance(double **C, int model, double *conf, double rms, int dgf,
		double *smajax, double *sminax, double *strike,
		double *sdepth, double *stime)
{
	int i ;
	double cwork[16];  /* covariance subset work space in FORTRAN order*/
	/* These are chisquare critical values.  First index is critical
	level and second is degrees of freedom.  e.g. 
	chisq_crit[1][2] is 90% level for 3 dgf*/
	double chisq_crit[2][3]={{1.001284,2.297707,3.529159},
               {2.705543, 4.60517, 6.251389}};
	double evals[4];
	double vwork[16];
	int info,ecount=0;
	int iconf;

	if((*conf<=0.0) || (*conf>=1.0))
	{
		elog_complain(0,"project_confidence passed illegal confidence level.  Default to 68.3 percent\n");
		iconf = 0;
	}
	if(fabs(*conf-0.683)<0.001)
		iconf = 0;
	else if(fabs(*conf-0.9)<0.001)
		iconf = 1;
	else
	{
		elog_complain(0,"Confidence level %f not implemented--default to 68%%\n",*conf);
		iconf = 0;
	}
	/* first compute the horizontal error ellipse, unscaled.
	This uses the packed storage mode of lapack */
	cwork[0] = C[0][0];
	cwork[1] = C[1][0];
	cwork[2] = C[1][1];
	/* this is CLAPACK/sunperf routine to compute eigenvalues and
	eigenvectors for a positive definite matrix */
	dspev('v','l',2,cwork,evals,vwork,2,&info);
	if(info<0)
	{
		elog_complain(0,"dspev illegal value\n");
		return(-1);
	}
	else if(info>0) 
	{
		elog_notify(0,"dspev convergence failure.  Error ellipse esults may be unreliable\n");
		++ecount;
	}
	/* dspev returns eigenvalues in ascending order of size */
	switch(model)
	{
	case(F_DIST):
		elog_complain(0,"F distribution not yet implemented--default to chi-square\n");
	case(CHI_SQUARE):
	default:
		*smajax = chisq_crit[iconf][1]*sqrt(evals[1]);  /*2 dgf */
		*sminax = chisq_crit[iconf][1]*sqrt(evals[0]);
		*strike= 90.0 - deg(atan2(vwork[3],vwork[2]));
		/* phase-wrap so the principal value of the arctangent 
		   maps to the valid range of the CSS origerr.strike field */
		if( *strike < 0 ) *strike += 360.;
	}
	/* Now we have to do the full covariance to properly compute
	the depth and time uncertainties*/
	cwork[0] = C[0][0];
	cwork[1] = C[1][0];
	cwork[2] = C[2][0];
	cwork[3] = C[3][0];
	cwork[4] = C[1][1];
	cwork[5] = C[2][1];
	cwork[6] = C[3][1];
	cwork[7] = C[2][2];
	cwork[8] = C[3][2];
	cwork[9] = C[3][3];
	dspev('v','l',4,cwork,evals,vwork,4,&info);	
	if(info<0)
	{
		elog_complain(0,"dspev illegal value\n");
		return(-1);
	}
	else if(info>0) 
	{
		elog_notify(0,"dspev convergence failure.  Error ellipse esults may be unreliable\n");
		++ecount;
	}

	/* We scale each eigenvector by the eigenvalue and find
	the largest z and t components to compute errors in z and t*/
	for(i=0;i<4;++i)dscal(4,sqrt(evals[i]),vwork+4*i,1);
	i = idamax(4,vwork+2,4);
	/* Note sunperf implementation of blas returns an index based
	from 0 ala C while the FORTRAN version uses 1.  Dangerous
	inconsistency to watch out for. */
        /* This is the sunperf version 
	*sdepth = fabs(vwork[2+i*4]); 
        This is the correct version for perf which returns 1-4 */
        *sdepth =fabs(vwork[2+(i-1)*4]);
	i = idamax(4,vwork+3,4);
        /* other old sunperf version 
	*stime = fabs(vwork[3+i*4]); */
        *stime = fabs(vwork[3+(i-1)*4]);
	switch(model)
	{
	case(F_DIST):
	case(CHI_SQUARE):
	default:
		*sdepth *= chisq_crit[iconf][0];
		*stime *= chisq_crit[iconf][0];
	}
	return(ecount);
}
