#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int dlarrv_(integer *n, doublereal *d__, doublereal *l, 
	integer *isplit, integer *m, doublereal *w, integer *iblock, 
	doublereal *gersch, doublereal *tol, doublereal *z__, integer *ldz, 
	integer *isuppz, doublereal *work, integer *iwork, integer *info)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    DLARRV computes the eigenvectors of the tridiagonal matrix   
    T = L D L^T given L, D and the eigenvalues of L D L^T.   
    The input eigenvalues should have high relative accuracy with   
    respect to the entries of L and D. The desired accuracy of the   
    output can be specified by the input parameter TOL.   

    Arguments   
    =========   

    N       (input) INTEGER   
            The order of the matrix.  N >= 0.   

    D       (input/output) DOUBLE PRECISION array, dimension (N)   
            On entry, the n diagonal elements of the diagonal matrix D.   
            On exit, D may be overwritten.   

    L       (input/output) DOUBLE PRECISION array, dimension (N-1)   
            On entry, the (n-1) subdiagonal elements of the unit   
            bidiagonal matrix L in elements 1 to N-1 of L. L(N) need   
            not be set. On exit, L is overwritten.   

    ISPLIT  (input) INTEGER array, dimension (N)   
            The splitting points, at which T breaks up into submatrices.   
            The first submatrix consists of rows/columns 1 to   
            ISPLIT( 1 ), the second of rows/columns ISPLIT( 1 )+1   
            through ISPLIT( 2 ), etc.   

    TOL     (input) DOUBLE PRECISION   
            The absolute error tolerance for the   
            eigenvalues/eigenvectors.   
            Errors in the input eigenvalues must be bounded by TOL.   
            The eigenvectors output have residual norms   
            bounded by TOL, and the dot products between different   
            eigenvectors are bounded by TOL. TOL must be at least   
            N*EPS*|T|, where EPS is the machine precision and |T| is   
            the 1-norm of the tridiagonal matrix.   

    M       (input) INTEGER   
            The total number of eigenvalues found.  0 <= M <= N.   
            If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.   

    W       (input) DOUBLE PRECISION array, dimension (N)   
            The first M elements of W contain the eigenvalues for   
            which eigenvectors are to be computed.  The eigenvalues   
            should be grouped by split-off block and ordered from   
            smallest to largest within the block ( The output array   
            W from DLARRE is expected here ).   
            Errors in W must be bounded by TOL (see above).   

    IBLOCK  (input) INTEGER array, dimension (N)   
            The submatrix indices associated with the corresponding   
            eigenvalues in W; IBLOCK(i)=1 if eigenvalue W(i) belongs to   
            the first submatrix from the top, =2 if W(i) belongs to   
            the second submatrix, etc.   

    Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M) )   
            If JOBZ = 'V', then if INFO = 0, the first M columns of Z   
            contain the orthonormal eigenvectors of the matrix T   
            corresponding to the selected eigenvalues, with the i-th   
            column of Z holding the eigenvector associated with W(i).   
            If JOBZ = 'N', then Z is not referenced.   
            Note: the user must ensure that at least max(1,M) columns are   
            supplied in the array Z; if RANGE = 'V', the exact value of M   
            is not known in advance and an upper bound must be used.   

    LDZ     (input) INTEGER   
            The leading dimension of the array Z.  LDZ >= 1, and if   
            JOBZ = 'V', LDZ >= max(1,N).   

    ISUPPZ  (output) INTEGER ARRAY, dimension ( 2*max(1,M) )   
            The support of the eigenvectors in Z, i.e., the indices   
            indicating the nonzero elements in Z. The i-th eigenvector   
            is nonzero only in elements ISUPPZ( 2*i-1 ) through   
            ISUPPZ( 2*i ).   

    WORK    (workspace) DOUBLE PRECISION array, dimension (13*N)   

    IWORK   (workspace) INTEGER array, dimension (6*N)   

    INFO    (output) INTEGER   
            = 0:  successful exit   
            < 0:  if INFO = -i, the i-th argument had an illegal value   
            > 0:  if INFO = 1, internal error in DLARRB   
                  if INFO = 2, internal error in DSTEIN   

    Further Details   
    ===============   

    Based on contributions by   
       Inderjit Dhillon, IBM Almaden, USA   
       Osni Marques, LBNL/NERSC, USA   

    =====================================================================   


       Test the input parameters.   

       Parameter adjustments */
    /* Table of constant values */
    static doublereal c_b6 = 0.;
    static integer c__1 = 1;
    
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2, i__3, i__4, i__5, i__6;
    doublereal d__1, d__2;
    /* Builtin functions */
    double sqrt(doublereal);
    /* Local variables */
    static integer iend, jblk;
    extern doublereal ddot_(integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    static integer iter, temp[1], ktot;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    static integer itmp1, itmp2, i__, j, k, p, q;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *);
    static integer indld;
    static doublereal sigma;
    static integer ndone, iinfo, iindr;
    static doublereal resid;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *);
    static integer nclus;
    extern /* Subroutine */ int daxpy_(integer *, doublereal *, doublereal *, 
	    integer *, doublereal *, integer *);
    static integer iindc1, iindc2;
    extern /* Subroutine */ int dlar1v_(integer *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *, integer *, doublereal *);
    static doublereal lambda;
    static integer im, in;
    extern doublereal dlamch_(char *);
    static integer ibegin, indgap, indlld;
    extern /* Subroutine */ int dlarrb_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *, integer *);
    static doublereal mingma;
    static integer oldien, oldncl;
    static doublereal relgap;
    extern /* Subroutine */ int dlarrf_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *), 
	    dlaset_(char *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *);
    static integer oldcls, ndepth, inderr, iindwk;
    extern /* Subroutine */ int dstein_(integer *, doublereal *, doublereal *,
	     integer *, doublereal *, integer *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, integer *, integer *);
    static logical mgscls;
    static integer lsbdpt, newcls, oldfst;
    static doublereal minrgp;
    static integer indwrk, oldlst;
    static doublereal reltol;
    static integer maxitr, newfrs, newftt;
    static doublereal mgstol;
    static integer nsplit;
    static doublereal nrminv, rqcorr;
    static integer newlst, newsiz;
    static doublereal gap, eps, ztz, tmp1;
#define z___ref(a_1,a_2) z__[(a_2)*z_dim1 + a_1]


    --d__;
    --l;
    --isplit;
    --w;
    --iblock;
    --gersch;
    z_dim1 = *ldz;
    z_offset = 1 + z_dim1 * 1;
    z__ -= z_offset;
    --isuppz;
    --work;
    --iwork;

    /* Function Body */
    inderr = *n + 1;
    indld = *n << 1;
    indlld = *n * 3;
    indgap = *n << 2;
    indwrk = *n * 5 + 1;

    iindr = *n;
    iindc1 = *n << 1;
    iindc2 = *n * 3;
    iindwk = (*n << 2) + 1;

    eps = dlamch_("Precision");

    i__1 = *n << 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	iwork[i__] = 0;
/* L10: */
    }
    i__1 = *m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	work[inderr + i__ - 1] = eps * (d__1 = w[i__], abs(d__1));
/* L20: */
    }
    dlaset_("Full", n, n, &c_b6, &c_b6, &z__[z_offset], ldz);
    mgstol = eps * 5.;

    nsplit = iblock[*m];
    ibegin = 1;
    i__1 = nsplit;
    for (jblk = 1; jblk <= i__1; ++jblk) {
	iend = isplit[jblk];

/*        Find the eigenvectors of the submatrix indexed IBEGIN   
          through IEND. */

	if (ibegin == iend) {
	    z___ref(ibegin, ibegin) = 1.;
	    isuppz[(ibegin << 1) - 1] = ibegin;
	    isuppz[ibegin * 2] = ibegin;
	    ibegin = iend + 1;
	    goto L170;
	}
	oldien = ibegin - 1;
	in = iend - oldien;
/* Computing MIN */
	d__1 = .01, d__2 = 1. / (doublereal) in;
	reltol = min(d__1,d__2);
	im = in;
	dcopy_(&im, &w[ibegin], &c__1, &work[1], &c__1);
	i__2 = in - 1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    work[indgap + i__] = work[i__ + 1] - work[i__];
/* L30: */
	}
/* Computing MAX */
	d__2 = (d__1 = work[in], abs(d__1));
	work[indgap + in] = max(d__2,eps);
	ndone = 0;

	ndepth = 0;
	lsbdpt = 1;
	nclus = 1;
	iwork[iindc1 + 1] = 1;
	iwork[iindc1 + 2] = in;

/*        While( NDONE.LT.IM ) do */

L40:
	if (ndone < im) {
	    oldncl = nclus;
	    nclus = 0;
	    lsbdpt = 1 - lsbdpt;
	    i__2 = oldncl;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		if (lsbdpt == 0) {
		    oldcls = iindc1;
		    newcls = iindc2;
		} else {
		    oldcls = iindc2;
		    newcls = iindc1;
		}

/*              If NDEPTH > 1, retrieve the relatively robust   
                representation (RRR) and perform limited bisection   
                (if necessary) to get approximate eigenvalues. */

		j = oldcls + (i__ << 1);
		oldfst = iwork[j - 1];
		oldlst = iwork[j];
		if (ndepth > 0) {
		    j = oldien + oldfst;
		    dcopy_(&in, &z___ref(ibegin, j), &c__1, &d__[ibegin], &
			    c__1);
		    dcopy_(&in, &z___ref(ibegin, j + 1), &c__1, &l[ibegin], &
			    c__1);
		    sigma = l[iend];
		}
		k = ibegin;
		i__3 = in - 1;
		for (j = 1; j <= i__3; ++j) {
		    work[indld + j] = d__[k] * l[k];
		    work[indlld + j] = work[indld + j] * l[k];
		    ++k;
/* L50: */
		}
		if (ndepth > 0) {
		    dlarrb_(&in, &d__[ibegin], &l[ibegin], &work[indld + 1], &
			    work[indlld + 1], &oldfst, &oldlst, &sigma, &
			    reltol, &work[1], &work[indgap + 1], &work[inderr]
			    , &work[indwrk], &iwork[iindwk], &iinfo);
		    if (iinfo != 0) {
			*info = 1;
			return 0;
		    }
		}

/*              Classify eigenvalues of the current representation (RRR)   
                as (i) isolated, (ii) loosely clustered or (iii) tightly   
                clustered */

		newfrs = oldfst;
		i__3 = oldlst;
		for (j = oldfst; j <= i__3; ++j) {
		    if (j == oldlst || work[indgap + j] >= reltol * (d__1 = 
			    work[j], abs(d__1))) {
			newlst = j;
		    } else {

/*                    continue (to the next loop) */

			relgap = work[indgap + j] / (d__1 = work[j], abs(d__1)
				);
			if (j == newfrs) {
			    minrgp = relgap;
			} else {
			    minrgp = min(minrgp,relgap);
			}
			goto L140;
		    }
		    newsiz = newlst - newfrs + 1;
		    maxitr = 10;
		    newftt = oldien + newfrs;
		    if (newsiz > 1) {
			mgscls = newsiz <= 20 && minrgp >= mgstol;
			if (! mgscls) {
			    dlarrf_(&in, &d__[ibegin], &l[ibegin], &work[
				    indld + 1], &work[indlld + 1], &newfrs, &
				    newlst, &work[1], &z___ref(ibegin, newftt)
				    , &z___ref(ibegin, newftt + 1), &work[
				    indwrk], &iwork[iindwk], info);
			    if (*info == 0) {
				++nclus;
				k = newcls + (nclus << 1);
				iwork[k - 1] = newfrs;
				iwork[k] = newlst;
			    } else {
				*info = 0;
				if (minrgp >= mgstol) {
				    mgscls = TRUE_;
				} else {

/*                             Call DSTEIN to process this tight cluster.   
                               This happens only if MINRGP <= MGSTOL   
                               and DLARRF returns INFO = 1. The latter   
                               means that a new RRR to "break" the   
                               cluster could not be found. */

				    work[indwrk] = d__[ibegin];
				    i__4 = in - 1;
				    for (k = 1; k <= i__4; ++k) {
					work[indwrk + k] = d__[ibegin + k] + 
						work[indlld + k];
/* L60: */
				    }
				    i__4 = newsiz;
				    for (k = 1; k <= i__4; ++k) {
					iwork[iindwk + k - 1] = 1;
/* L70: */
				    }
				    i__4 = newlst;
				    for (k = newfrs; k <= i__4; ++k) {
					isuppz[(ibegin + k << 1) - 3] = 1;
					isuppz[(ibegin + k << 1) - 2] = in;
/* L80: */
				    }
				    temp[0] = in;
				    dstein_(&in, &work[indwrk], &work[indld + 
					    1], &newsiz, &work[newfrs], &
					    iwork[iindwk], temp, &z___ref(
					    ibegin, newftt), ldz, &work[
					    indwrk + in], &iwork[iindwk + in],
					     &iwork[iindwk + (in << 1)], &
					    iinfo);
				    if (iinfo != 0) {
					*info = 2;
					return 0;
				    }
				    ndone += newsiz;
				}
			    }
			}
		    } else {
			mgscls = FALSE_;
		    }
		    if (newsiz == 1 || mgscls) {
			ktot = newftt;
			i__4 = newlst;
			for (k = newfrs; k <= i__4; ++k) {
			    iter = 0;
L90:
			    lambda = work[k];
			    dlar1v_(&in, &c__1, &in, &lambda, &d__[ibegin], &
				    l[ibegin], &work[indld + 1], &work[indlld 
				    + 1], &gersch[(oldien << 1) + 1], &
				    z___ref(ibegin, ktot), &ztz, &mingma, &
				    iwork[iindr + ktot], &isuppz[(ktot << 1) 
				    - 1], &work[indwrk]);
			    tmp1 = 1. / ztz;
			    nrminv = sqrt(tmp1);
			    resid = abs(mingma) * nrminv;
			    rqcorr = mingma * tmp1;
			    if (k == in) {
				gap = work[indgap + k - 1];
			    } else if (k == 1) {
				gap = work[indgap + k];
			    } else {
/* Computing MIN */
				d__1 = work[indgap + k - 1], d__2 = work[
					indgap + k];
				gap = min(d__1,d__2);
			    }
			    ++iter;
			    if (resid > *tol * gap && abs(rqcorr) > eps * 4. *
				     abs(lambda)) {
				work[k] = lambda + rqcorr;
				if (iter < maxitr) {
				    goto L90;
				}
			    }
			    iwork[ktot] = 1;
			    if (newsiz == 1) {
				++ndone;
			    }
			    dscal_(&in, &nrminv, &z___ref(ibegin, ktot), &
				    c__1);
			    ++ktot;
/* L100: */
			}
			if (newsiz > 1) {
			    itmp1 = isuppz[(newftt << 1) - 1];
			    itmp2 = isuppz[newftt * 2];
			    ktot = oldien + newlst;
			    i__4 = ktot;
			    for (p = newftt + 1; p <= i__4; ++p) {
				i__5 = p - 1;
				for (q = newftt; q <= i__5; ++q) {
				    tmp1 = -ddot_(&in, &z___ref(ibegin, p), &
					    c__1, &z___ref(ibegin, q), &c__1);
				    daxpy_(&in, &tmp1, &z___ref(ibegin, q), &
					    c__1, &z___ref(ibegin, p), &c__1);
/* L110: */
				}
				tmp1 = 1. / dnrm2_(&in, &z___ref(ibegin, p), &
					c__1);
				dscal_(&in, &tmp1, &z___ref(ibegin, p), &c__1)
					;
/* Computing MIN */
				i__5 = itmp1, i__6 = isuppz[(p << 1) - 1];
				itmp1 = min(i__5,i__6);
/* Computing MAX */
				i__5 = itmp2, i__6 = isuppz[p * 2];
				itmp2 = max(i__5,i__6);
/* L120: */
			    }
			    i__4 = ktot;
			    for (p = newftt; p <= i__4; ++p) {
				isuppz[(p << 1) - 1] = itmp1;
				isuppz[p * 2] = itmp2;
/* L130: */
			    }
			    ndone += newsiz;
			}
		    }
		    newfrs = j + 1;
L140:
		    ;
		}
/* L150: */
	    }
	    ++ndepth;
	    goto L40;
	}
	j = ibegin << 1;
	i__2 = iend;
	for (i__ = ibegin; i__ <= i__2; ++i__) {
	    isuppz[j - 1] += oldien;
	    isuppz[j] += oldien;
	    j += 2;
/* L160: */
	}
	ibegin = iend + 1;
L170:
	;
    }

    return 0;

/*     End of DLARRV */

} /* dlarrv_ */

#undef z___ref


