#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int zlarrv_(integer *n, doublereal *d__, doublereal *l, 
	integer *isplit, integer *m, doublereal *w, integer *iblock, 
	doublereal *gersch, doublereal *tol, doublecomplex *z__, integer *ldz,
	 integer *isuppz, doublereal *work, integer *iwork, integer *info)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    ZLARRV computes the eigenvectors of the tridiagonal matrix   
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

    Z       (output) COMPLEX*16 array, dimension (LDZ, max(1,M) )   
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
                  if INFO = 2, internal error in ZSTEIN   

    Further Details   
    ===============   

    Based on contributions by   
       Inderjit Dhillon, IBM Almaden, USA   
       Osni Marques, LBNL/NERSC, USA   
       Ken Stanley, Computer Science Division, University of   
         California at Berkeley, USA   

    =====================================================================   


       Test the input parameters.   

       Parameter adjustments */
    /* Table of constant values */
    static doublecomplex c_b1 = {0.,0.};
    static integer c__1 = 1;
    
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2, i__3, i__4, i__5, i__6;
    doublereal d__1, d__2;
    doublecomplex z__1, z__2;
    /* Builtin functions */
    double sqrt(doublereal);
    /* Local variables */
    static integer iend, jblk, iter, temp[1], ktot, itmp1, itmp2, i__, j, k, 
	    p, q, indld;
    static doublereal sigma;
    static integer ndone, iinfo, iindr;
    static doublereal resid;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *);
    static integer nclus;
    extern /* Double Complex */ VOID zdotu_(doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    static integer iindc1, iindc2;
    extern /* Subroutine */ int zaxpy_(integer *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    static integer indin1, indin2;
    extern doublereal dznrm2_(integer *, doublecomplex *, integer *);
    extern /* Subroutine */ int zlar1v_(integer *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublecomplex *, doublereal *, 
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
	    doublereal *, doublereal *, doublereal *, integer *, integer *);
    static integer oldcls;
    extern /* Subroutine */ int zdscal_(integer *, doublereal *, 
	    doublecomplex *, integer *);
    static integer ndepth, inderr, iindwk;
    static logical mgscls;
    static integer lsbdpt, newcls, oldfst;
    static doublereal minrgp;
    static integer indwrk, oldlst;
    static doublereal reltol;
    extern /* Subroutine */ int zlaset_(char *, integer *, integer *, 
	    doublecomplex *, doublecomplex *, doublecomplex *, integer *);
    static integer maxitr, newfrs, newftt;
    static doublereal mgstol;
    static integer nsplit;
    static doublereal nrminv, rqcorr;
    static integer newlst;
    extern /* Subroutine */ int zstein_(integer *, doublereal *, doublereal *,
	     integer *, doublereal *, integer *, integer *, doublecomplex *, 
	    integer *, doublereal *, integer *, integer *, integer *);
    static integer newsiz;
    static doublereal gap, eps, ztz, tmp1;
#define z___subscr(a_1,a_2) (a_2)*z_dim1 + a_1
#define z___ref(a_1,a_2) z__[z___subscr(a_1,a_2)]


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
    indin1 = *n * 5 + 1;
    indin2 = *n * 6 + 1;
    indwrk = *n * 7 + 1;

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
    zlaset_("Full", n, n, &c_b1, &c_b1, &z__[z_offset], ldz);
    mgstol = eps * 5.;

    nsplit = iblock[*m];
    ibegin = 1;
    i__1 = nsplit;
    for (jblk = 1; jblk <= i__1; ++jblk) {
	iend = isplit[jblk];

/*        Find the eigenvectors of the submatrix indexed IBEGIN   
          through IEND. */

	if (ibegin == iend) {
	    i__2 = z___subscr(ibegin, ibegin);
	    z__[i__2].r = 1., z__[i__2].i = 0.;
	    isuppz[(ibegin << 1) - 1] = ibegin;
	    isuppz[ibegin * 2] = ibegin;
	    ibegin = iend + 1;
	    goto L190;
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
		    i__3 = in;
		    for (k = 1; k <= i__3; ++k) {
			i__4 = z___subscr(ibegin + k - 1, oldien + oldfst);
			d__[ibegin + k - 1] = z__[i__4].r;
			i__4 = z___subscr(ibegin + k - 1, oldien + oldfst + 1)
				;
			l[ibegin + k - 1] = z__[i__4].r;
/* L50: */
		    }
		    sigma = l[iend];
		}
		k = ibegin;
		i__3 = in - 1;
		for (j = 1; j <= i__3; ++j) {
		    work[indld + j] = d__[k] * l[k];
		    work[indlld + j] = work[indld + j] * l[k];
		    ++k;
/* L60: */
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
			goto L160;
		    }
		    newsiz = newlst - newfrs + 1;
		    maxitr = 10;
		    newftt = oldien + newfrs;
		    if (newsiz > 1) {
			mgscls = newsiz <= 20 && minrgp >= mgstol;
			if (! mgscls) {
			    i__4 = in;
			    for (k = 1; k <= i__4; ++k) {
				i__5 = z___subscr(ibegin + k - 1, newftt);
				work[indin1 + k - 1] = z__[i__5].r;
				i__5 = z___subscr(ibegin + k - 1, newftt + 1);
				work[indin2 + k - 1] = z__[i__5].r;
/* L70: */
			    }
			    dlarrf_(&in, &d__[ibegin], &l[ibegin], &work[
				    indld + 1], &work[indlld + 1], &newfrs, &
				    newlst, &work[1], &work[indin1], &work[
				    indin2], &work[indwrk], &iwork[iindwk], 
				    info);
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

/*                             Call ZSTEIN to process this tight cluster.   
                               This happens only if MINRGP <= MGSTOL   
                               and DLARRF returns INFO = 1. The latter   
                               means that a new RRR to "break" the   
                               cluster could not be found. */

				    work[indwrk] = d__[ibegin];
				    i__4 = in - 1;
				    for (k = 1; k <= i__4; ++k) {
					work[indwrk + k] = d__[ibegin + k] + 
						work[indlld + k];
/* L80: */
				    }
				    i__4 = newsiz;
				    for (k = 1; k <= i__4; ++k) {
					iwork[iindwk + k - 1] = 1;
/* L90: */
				    }
				    i__4 = newlst;
				    for (k = newfrs; k <= i__4; ++k) {
					isuppz[(ibegin + k << 1) - 3] = 1;
					isuppz[(ibegin + k << 1) - 2] = in;
/* L100: */
				    }
				    temp[0] = in;
				    zstein_(&in, &work[indwrk], &work[indld + 
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
L110:
			    lambda = work[k];
			    zlar1v_(&in, &c__1, &in, &lambda, &d__[ibegin], &
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
				    goto L110;
				}
			    }
			    iwork[ktot] = 1;
			    if (newsiz == 1) {
				++ndone;
			    }
			    zdscal_(&in, &nrminv, &z___ref(ibegin, ktot), &
				    c__1);
			    ++ktot;
/* L120: */
			}
			if (newsiz > 1) {
			    itmp1 = isuppz[(newftt << 1) - 1];
			    itmp2 = isuppz[newftt * 2];
			    ktot = oldien + newlst;
			    i__4 = ktot;
			    for (p = newftt + 1; p <= i__4; ++p) {
				i__5 = p - 1;
				for (q = newftt; q <= i__5; ++q) {
				    zdotu_(&z__2, &in, &z___ref(ibegin, p), &
					    c__1, &z___ref(ibegin, q), &c__1);
				    z__1.r = -z__2.r, z__1.i = -z__2.i;
				    tmp1 = z__1.r;
				    z__1.r = tmp1, z__1.i = 0.;
				    zaxpy_(&in, &z__1, &z___ref(ibegin, q), &
					    c__1, &z___ref(ibegin, p), &c__1);
/* L130: */
				}
				tmp1 = 1. / dznrm2_(&in, &z___ref(ibegin, p), 
					&c__1);
				zdscal_(&in, &tmp1, &z___ref(ibegin, p), &
					c__1);
/* Computing MIN */
				i__5 = itmp1, i__6 = isuppz[(p << 1) - 1];
				itmp1 = min(i__5,i__6);
/* Computing MAX */
				i__5 = itmp2, i__6 = isuppz[p * 2];
				itmp2 = max(i__5,i__6);
/* L140: */
			    }
			    i__4 = ktot;
			    for (p = newftt; p <= i__4; ++p) {
				isuppz[(p << 1) - 1] = itmp1;
				isuppz[p * 2] = itmp2;
/* L150: */
			    }
			    ndone += newsiz;
			}
		    }
		    newfrs = j + 1;
L160:
		    ;
		}
/* L170: */
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
/* L180: */
	}
	ibegin = iend + 1;
L190:
	;
    }

    return 0;

/*     End of ZLARRV */

} /* zlarrv_ */

#undef z___ref
#undef z___subscr


