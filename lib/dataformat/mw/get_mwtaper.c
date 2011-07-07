
#include "stock.h"
#include "math.h"

/*-
    subroutine rtpss(nndim,ndata,nefn,w,efn,theta,ierr,work)
c
c  computes discrete prolate spheroidal sequences using the symmetric
c  tridiagonal form given in d.slepian, bell syst. tech. j. 57 (1978),
c  p. 1376, equation 14. there is a further reduction into even and
c  odd symmetric functions to minimize the size of the eigenvalue
c  problem, see eispack sections 2.1.17 and 2.2.8.
c  adapted by a.d. chave, lanl, jan 1986 from code written by
c  d.j. thomson, at&t bell labs.
c  argument list:
c    nndim-leading dimension of efn
c    ndata-number of data points (e.g., time) to return in efn,
c          this corresponds to n in slepian
c    nefn-number of dpss to compute, lowest order ones are returned
c         first
c    w-the bandwidth (=time-bandwidth product/ndata)
c    efn-array to contain the dpss, first dimension corresponds to
c        time and second to the order of the sequence
c    theta-vector of length nefn containing the differential eigenvalues
c    ierr-error flag, 0 for normal return, 1 for error in tridib,
c         2 for error in tinvit
c    work-work space of dimension at least 9*ndata/2+3*nefn/2+
c         ndata*nefn/4+11
c
c  calls rtpss1
c
*/



typedef struct Taper
{
    double         *taper_coeffs;
    double         *eigen;
    double         *eigen_delta;
}               Taper;

int
get_mwtapers(nsamp, ntapers, time_bandwidth, eigen_p, eigen_delta_p, tapers_p)
int             nsamp,
                ntapers;
double          time_bandwidth;
double        **eigen_p;
double        **eigen_delta_p;
double        **tapers_p;
{
    static Arr     *Computed = 0;
    Taper          *taper;
    static double  *work,
                   *lefnh,
                   *levh,
                   *lfv1,
                   *lfv2,
                   *lfv3,
                   *lw;
    static int      nwork = 0;
    char	   name[50] ; 
    int		   nwork_needed ;
    double	  *theta, *taper_coeffs, *eigen, *eigen_delta ; 
    double	   relative_tolerance = 1.0e-8, 
		   absolute_tolerance = 1.0e-14; 
    int		   ierr=0, retcode = 0  ;
    int		   n_odd_tapers, n_even_tapers, half_nsamp, mdim ; 
    double	   cos_time_bandwidth ;
    int		   flag ; 
    double	   sym ;
    double	   tbw_per_samp;

    tbw_per_samp = time_bandwidth/nsamp ;

    sprintf(name, "%d#%d%.3f", nsamp, ntapers, time_bandwidth);
    if (Computed == 0)
	Computed = newarr(0);
    else if ((taper = (Taper *) getarr(Computed, name)))
      {
	*eigen_p = taper->eigen;
	*eigen_delta_p = taper->eigen_delta ; 
	*tapers_p = taper->taper_coeffs;
	return 0;
      }

    nwork_needed = 9 * nsamp / 2 + 3 * ntapers / 2 + nsamp * ntapers / 4 + 11;
    if (nwork < nwork_needed)
      {
	if (nwork)
	    free(work);
	nwork = nwork_needed;
	allot(double *, work, nwork);
      }

    allot(Taper *, taper, 1);
    allot(double *, theta, ntapers);
    allot(double *, eigen, ntapers);
    allot(double *, eigen_delta, ntapers);
    allot(double *, taper_coeffs, ntapers * nsamp);
    taper->taper_coeffs = taper_coeffs ; 
    taper->eigen = eigen ; 
    taper->eigen_delta = eigen_delta ; 

    cos_time_bandwidth = cos(M_PI * 2 * tbw_per_samp );
    n_odd_tapers = ntapers / 2;
    n_even_tapers = ntapers - n_odd_tapers;
    half_nsamp = nsamp / 2;
    mdim = nsamp - half_nsamp ; 


    /*  divide up the space in work */
    lefnh = work;
    levh = lefnh + mdim * n_even_tapers ;
    lfv1 = levh + n_even_tapers ;
    lfv2 = lfv1 + mdim ;
    lfv3 = lfv2 + mdim ;
    lw = lfv3 + mdim ;

    /*  compute even  eigenfunctions */
    sym = 1.;
    flag = 0 ;
    rtpss1_(taper_coeffs,
	    &nsamp,
	    &nsamp,
	    &mdim,
	    &mdim,
	    &n_even_tapers,
	    &cos_time_bandwidth,
	    theta,
	    &flag,
	    &sym,
	    lefnh,
	    levh,
	    lfv1,
	    lfv2,
	    lfv3,
	    &ierr,
	    lw);

    if ( ierr ) 
	elog_die( 0, "Failure computing discrete prolate spheroidal functions error code = %d\n", 
		ierr ) ; 

    /*  compute odd eigenfunctions */
    sym = -1. ;
    flag = (nsamp/2 != mdim) ;
    if (n_odd_tapers > 0)
	rtpss1_(taper_coeffs + nsamp,
		 &nsamp,
		 &nsamp,
		 &half_nsamp, 
		 &mdim,
		 &n_odd_tapers,
		 &cos_time_bandwidth,
		 theta + 1,
		 &flag,
		 &sym,
		 lefnh,
		 levh,
		 lfv1,
		 lfv2,
		 lfv3,
		 &ierr,
		 lw);
    if ( ierr ) 
	elog_die( 0, "Failure computing discrete prolate spheroidal functions error code = %d\n", 
		ierr ) ; 
    free ( theta ) ; 

    pssev_ ( &nsamp, 
    	&nsamp, 
	&ntapers, 
	&tbw_per_samp, 
	&relative_tolerance, 
	&absolute_tolerance, 
	taper_coeffs, 
	eigen, 
	eigen_delta
    )  ;

    setarr ( Computed, name, taper ) ; 
    *eigen_p = eigen ; 
    *eigen_delta_p = eigen_delta ; 
    *tapers_p = taper_coeffs ; 

    return retcode;
}

/* $Id$ */
