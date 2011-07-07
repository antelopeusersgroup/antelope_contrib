
#include "stock.h"
#include "db.h"
#include "tr.h"

typedef struct complex
{
    float           r,
                    i;
}               complex;

static int 
cmp_lines ( a, b, lines_tmp ) 
int *a, *b, *lines_tmp ; 
{
    return lines_tmp[*a] - lines_tmp[*b] ; 
}

int
line_remove(mw, nranges, lofreq, hifreq, critical)
Dbptr           mw;
int             nranges;
float          *lofreq,
               *hifreq;
double	        critical ;
{
    int             nsamp,
                    nfft,
                    ntapers;
    double         *tapers;
    double         *eigen,
                   *eigen_delta;
    float          *work;
    int     	   *work1;
    int             need,
                    nwork = 0, 
		    nline_power = 0 ;
    complex        *exponentials, *eigenspectra, *xmul, *xmul_tmp;
    double          time_bandwidth;
    int             nyquist;
    float          *pstat;
    int	           *lines, *lines_tmp ;
    int		    maxlines=100 ;
    int             rs,
                    re;
    int		    i, j;
    float	    fcritical = critical ;
    int		    nfound, nlines, ndiff ;
    int		    ndpsw = 0 ;
    float	   *dpsw, *line_power ;
    float           variance;
    double	    dvariance ;
    float          *lweights, *lspectra, *ljackknife, *ldof ;
    float          *lspectra_cmp, *ljackknife_cmp, *ldof_cmp ;
    int		   *sort, lastlin ;


    allot(complex *, xmul_tmp, 2*maxlines);
    allot(complex *, xmul, 2*maxlines);
    allot(int *, lines_tmp, 2*maxlines);
    allot ( int *, work1, 10 * maxlines ) ; 
    allot(float *, lspectra_cmp, 2*maxlines);
    allot(float *, ljackknife_cmp, 2*maxlines);
    allot(float *, ldof_cmp, 2*maxlines);
    allot(int *, sort, maxlines ) ; 

    dbget_range(mw, &rs, &re);
    for (mw.record = rs; mw.record < re; mw.record++)
      {
	dbgetv(mw, 0,
	       "nsamp", &nsamp,
	       "nfft", &nfft,
	       "ntapers", &ntapers,
	       "nyquist", &nyquist,
	       "tbw", &time_bandwidth,
	       "eigenspectra", &eigenspectra,
	       "exponentials", &exponentials,
	       "variance", &dvariance,
	       "pstat", &pstat,
	       0);

	variance = dvariance ;
	need = 2 * maxlines * ntapers ; 
	if ( need > nline_power ) { 
	    if ( nline_power != 0 ) 
		free ( line_power ) ; 
	    allot ( float *, line_power, need ) ; 
	    nline_power = need ;
	}

	need = 8 * ntapers;
	if (need > nwork)
	  {
	    if (nwork > 0)
		free(work);
	    allot(float *, work, need);
	    nwork = need;
	  }

	get_mwtapers(nsamp, ntapers, time_bandwidth,
		     &eigen, &eigen_delta, &tapers);

	linefind_(&nsamp,	       /* npts -- number of data points */
		 &nfft,		       /* nfft - length of the fft */
		 &ntapers,	       /* kspec -- number of prolate windows */
		 &maxlines,	       /* mlin --- maximum number of lines
				        * allowed */
		 &nranges,	       /* nf -- number of entries in flo and
				        * fup */
		 lofreq,	       /* flo -- (list of) low ends of
				        * frequency range to search */
		 hifreq,	       /* fup -- (list of) upper ends of
				        * frequency range to search */
		 &fcritical,	       /* pc -- critical probability for
				        * search */
		 pstat,		       /* p - vector of f-test probabilities */
		 &nsamp,	       /* mxdata - leading dimension of dpss */
		 tapers,	       /* dpss -- double precision array of
				        * tapers */
		 &nfft,		       /* mxfreq -- leading dimentsion of yk */
		 eigenspectra,	       /* yk -- complex ffts */
		 work1,		       /* work vector of length at least 10 *
				        * mlin */
		 work,		       /* dpsw0 -- work area >= 2*kspec */
		 &nfound,	       /* nl -- number of lines found */
		 xmul_tmp,	       /* xmul -- complex vector of mean
				        * values for spectra at the line
				        * frequencies */
		 lines_tmp	       /* ilin -- vector of indices of the
				        * lines */
	    ) ;

	/* sort the lines found, eliminating duplicates, and reordering
	    the xmul array at the same time */
	for ( i=0; i<nfound ; i++ ) 
	    sort[i] = i ; 
	isort ( sort, nfound, sizeof(int), cmp_lines, lines_tmp ) ; 

	allot ( int *, lines, 2*nfound ) ;
	/* copy the elements to ordered array, but drop duplicates */
	lastlin = -1 ; 
	j=0 ; 
	for ( i=0 ; i<nfound ; i++ ) { 
	    if ( sort[i] != lastlin ) { 
		lastlin = sort[i] ; 
		lines[j] = lines_tmp[lastlin] ; 
		xmul[j] = xmul_tmp[lastlin] ; 
		j++ ; 
	    }
	}

	nlines = nfound = j;
	for (i = 0; i < nfound; i++)
	  {
	    if (lines[i] != 1 && lines[i] != nyquist)
	      {
		nlines++;
		lines[nlines] = nfft - lines[i] + 2;
		xmul[nlines].r =  xmul[i].r;
		xmul[nlines].i = -xmul[i].i;
	      }
	  }

	if ( 4*nfft > ndpsw ) { 
	    if ( ndpsw != 0 ) 
		free ( dpsw ) ; 
	    ndpsw = 4 *nfft ; 
	    allot ( float *, dpsw, ndpsw ) ; 
	}
	reshape_(&nsamp,	       /* npts */
		 &nfft,		       /* nfft */
		 &ntapers,	       /* kspec -- number of prolate windows */
		 &nlines,	       /* kl --- number of spectral line components */
		 &nsamp,	       /* mxdata - leading dimension of dpss */
		 tapers,	       /* dpss -- double precision array of
				        * tapers */
		 lines,		       /* il -- vector of line indices */
		 xmul,		       /* xmul -- complex vector of mean
				        * values for spectra at the line
				        * frequencies */
		 &nfft,		       /* mxfreq -- leading dimentsion of yk */
		 eigenspectra,	       /* yk -- complex ffts */
		 exponentials,	       /* fft complex exponentials */
		 dpsw,		       /* dpsw -- work area of size 4*nfft */
		 &nlines, 	       /* mxlin - leading dimension of slin */
		 line_power	       /* slin - real array containing total power removed 
					  from kspec eigen-fts at kl frequencies */ 
	    ) ;

	allot(float *, lweights, nlines*ntapers);

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
	adaptwtl1_(&nfft,		/* nfft */
		  &ntapers,		/* kspec - number of prolate windows */
		  &nlines,		/* nlin -- number of lines */
		  &variance,		/* dvar -- data variance */
		  eigen_delta,		/* evalu1 -- vector of 1 - eigenvalue */
		  lines, 		/* ilin -- vector in indices for lines */
		  &nlines, 		/* mxlin - leading dimension of line_power and line weight */
		  line_power, 		/* slin */
		  work,			/* evalu */
		  work + 2 * ntapers,	/* sqev */
		  work + 4 * ntapers,	/* bk */
		  work + 5 * ntapers,	/* sk */
		  work + 6 * ntapers,	/* eigsp */
		  work + 7 * ntapers,	/* delsp */
		  lspectra_cmp,		/* spec - adaptively weighted line spectra */
		  ldof_cmp,		/* se -- degrees of freedom at each frequency */
		  lweights,		/* wt - nlin weights normalized so sum of squares is one */
		  ljackknife_cmp	/* err -- jackknife error estimates*/
		  ) ;	

	/* add back the negative frequencies -- I wonder if something similar should be
	   done with ldof and ljackknife? */
	if ( lines[0] == 1 ) 
	    ndiff = nfound-1 ; 
	else
	    ndiff = nfound ; 
	for ( i=0 ; i<nfound ; i++ ) { 
	    if ( lines[i] != 1 && lines[i] != nyquist ) 
		lspectra_cmp[i] += lspectra_cmp[i+ndiff] ;
	}

	allot ( float *, lspectra, nyquist ) ; 
	allot ( float *, ldof, nyquist ) ; 
	allot ( float *, ljackknife, nyquist ) ; 
	for ( i=0 ; i<nyquist ; i++ ) {
	    lspectra[i] = 0.0 ; 
	    ldof[i] = 0.0 ; 
	    ljackknife[i] = 0.0 ; 
	    }
	for ( i=0 ; i<nfound ; i++ ) {
	    lspectra[lines[i]-1] = lspectra_cmp[i] ; 
	    ldof[lines[i]-1] = ldof_cmp[i] ; 
	    ljackknife[lines[i]-1] = ljackknife_cmp[i] ; 
	}

	if ( dbputv(mw, 0,
		   "nlines", nfound,
		   "lines", lines, 
		   "lspectra", lspectra,
		   "ldof", ldof,
		   "lweights", lweights,
		   "ljackknife", ljackknife,
		   0)  < 0)  
	    elog_complain( 0, "Couldn't save to table.\n" ) ;

      }
    free(dpsw) ;
    free(work);
    free(xmul) ; 
    free(lines_tmp) ; 
    free(work1) ; 
    free(line_power) ;
    free(sort);
}

/* $Id$ */
