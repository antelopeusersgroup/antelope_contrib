
#include "stock.h"
#include "db.h"
#include "tr.h"

typedef struct complex
{
    float           r,
                    i;
}               complex;

int
adaptspectra(mw, jackknife_flag)
Dbptr           mw;
int             jackknife_flag;
{
    int             nsamp,
                    nfft,
                    ntapers;
    float          *data;
    double         *tapers;
    double         *eigen,
                   *eigen_delta;
    float	   *work;
    int             need,
                    nwork = 0;
    complex        *eigenspectra;
    double          time_bandwidth;
    double	    sum, scale ;
    double	    dvariance ;
    float           variance;
    int		    nyquist ; 
    int             rs,
                    re;
    float	   *weights, *spectra, *jackknife, *dof ;
    float	   *lspectra, *ldof ;
    int		    nlines ;
    int		    i ;
    double	    stability ;

    dbget_range(mw, &rs, &re);
    for (mw.record = rs; mw.record < re; mw.record++)
      {
	dbgetv(mw, 0,
	       "nsamp", &nsamp,
	       "nfft", &nfft,
	       "ntapers", &ntapers,
	       "tbw", &time_bandwidth,
	       "eigenspectra", &eigenspectra,
	       "data", &data,
	       "variance", &dvariance,
	       "nlines", &nlines, 
	       "lspectra", &lspectra, 
	       "ldof", &ldof,
	       0);
	variance = dvariance ;

	need = 8 * ntapers;
	if (need > nwork)
	  {
	    if (nwork > 0)
		free(work);
	    allot(float *, work, need);
	  }

	nyquist = nfft / 2 + 1;
	allot(float *, weights, nyquist*ntapers);
	allot(float *, spectra, nyquist);
	allot(float *, jackknife, nyquist);
	allot(float *, dof, nyquist);

	get_mwtapers(nsamp, ntapers, time_bandwidth,
		     &eigen, &eigen_delta, &tapers);

	/* 
	Because of stupidity in the fortran routine which is
	being called here, it is essential that the work array
	be allocated in one piece and passed to the subroutine as
	written here.  In particular, the bk and sk arrays are 
	addressed as one long array inside fspec, which is called
	by fzero inside adaptwt1.  While this is the only one I know
	about, there may be other such usages.
	*/
				/* Name in the FORTRAN routine */
	adaptwt1_(&nfft,		/* nfft */
		  &nyquist,		/* ne */
		  &ntapers,		/* kspec */
		  &variance,		/* dvar */
		  eigen_delta,		/* evalu1 */
		  &nfft,		/* mxfreq */
		  eigenspectra,		/* yk */
		  work,			/* evalu */
		  work + 2 * ntapers,	/* sqev */
		  work + 4 * ntapers,	/* bk */
		  work + 5 * ntapers,	/* sk */
		  work + 6 * ntapers,	/* eigsp */
		  work + 7 * ntapers,	/* delsp */
		  &nyquist,		/* mxwt */
		  spectra,		/* spec */
		  dof,			/* se */
		  weights,		/* wt */
		  jackknife,		/* err */
		  &jackknife_flag) ;	/* jack */

	sum = 0. ; 
	for ( i=1 ; i<nyquist-1 ; i++ ) 
	    spectra[i] += spectra[i] ;

	for ( i=0 ; i<nyquist ; i++ ) 
	    sum += spectra[i] ; 
	for ( i=0 ; i<nlines ; i++ ) 
	    sum += lspectra[i] ; 

	scale = nfft * variance / sum ; 

	for ( i=0 ; i<nyquist ; i++ ) 
	    spectra[i] *= scale ; 
	for ( i=0 ; i<nlines ; i++ ) 
	    lspectra[i] *= scale ; 


	sum = 0 ; 
	for ( i=0 ; i<nyquist ; i++ ) 
	    sum += dof[i] ; 
	for ( i=0 ; i<nlines ; i++ ) 
	    sum += ldof[i] ; 
	stability = sum / ( 2 * ntapers * (nyquist + nlines) ) ;

	dbputv(mw, 0,
		   "nyquist", nyquist,
		   "spectra", spectra,
		   "dof", dof,
		   "weights", weights,
		   "jackknife", jackknife,
		   "stability", stability,
		   0);
      }
    free(work);
}

/* $Id$ */
