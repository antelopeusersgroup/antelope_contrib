
#include "stock.h"
#include "db.h"
#include "tr.h"

typedef struct complex
{
    float           r,
                    i;
}               complex;

int 
compute_fstat(mw)
Dbptr           mw;
{
    int             nsamp,
                    nfft,
                    ntapers;
    double         *tapers;
    double         *eigen,
                   *eigen_delta;
    float          *work;
    int             need, nwork=0;
    complex        *eigenspectra;
    double          time_bandwidth;
    int		    nyquist ;
    float	   *fstat, *pstat ;
    int		    i, nfree1, nfree2, ier ; 
    int             rs,
                    re;

    dbget_range(mw, &rs, &re);
    for (mw.record = rs; mw.record < re; mw.record++)
      {
	dbgetv(mw, 0,
	       "nsamp", &nsamp,
	       "nfft", &nfft,
	       "ntapers", &ntapers,
	       "tbw", &time_bandwidth,
	       "eigenspectra", &eigenspectra,
	       0);
	nyquist = nfft / 2 + 1 ;

	need = 2 * ntapers ;
	if ( need > nwork ) { 
	    if ( nwork > 0 ) 
		free ( work ) ;
	    allot ( float *, work, need ) ; 
	    nwork = need ;
	}

	get_mwtapers(nsamp, ntapers, time_bandwidth,
		     &eigen, &eigen_delta, &tapers);

	allot ( float *, fstat, nyquist ) ; 
	allot ( float *, pstat, nyquist ) ; 

	ftest_(&nsamp, 		/* npts - length of data series */
		&nyquist, 	/* ne -- number of elements in yk to test */
		&ntapers,	/* kspec -- number of prolate windows */ 
		&nsamp,		/* mxdata - leading dimension of dpss */
		tapers, 	/* dpss -- double precision array of tapers */
		&nfft, 		/* mxfreq -- leading dimentsion of yk */
		eigenspectra, 	/* yk -- complex ffts */
		work, 		/* dpsw0 -- work area >= 2*kspec */
		fstat 		/* f - computed f-test values */
	) ; 

	nfree1 = 2 ; 
	nfree2 = 2 * (ntapers-1) ; 
	for ( i=0 ; i<nyquist ; i++ ) {
	    mdfd_ ( &(fstat[i]), 	/* f -- input constant to which integration is performed */
		    &nfree1, 	/* n1 - first degree of freedom */
		    &nfree2,	/* n2 - second degree of freedom */
		    &(pstat[i]), /* p - probability that a random variable following the
		    			f distribution with degrees of freedom
					n1 and n2 will be less than or equal to 
					input f.
				*/
		    &ier
	    ) ;
	    if ( ier == 129 || ier == 130 ) 
		elog_complain( 0, "mdfd returns error code = %d\n", ier ) ; 
	}

	dbputv(mw, 0,
	       "fstat", fstat,
	       "pstat", pstat,
	       0);
      }
    free(work);
}

/* $Id$ */
