#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int dlarre_(integer *n, doublereal *d__, doublereal *e, 
	doublereal *tol, integer *nsplit, integer *isplit, integer *m, 
	doublereal *w, doublereal *woff, doublereal *gersch, doublereal *work,
	 integer *info)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    Given the tridiagonal matrix T, DLARRE sets "small" off-diagonal   
    elements to zero, and for each unreduced block T_i, it finds   
    (i) the numbers sigma_i   
    (ii) the base T_i - sigma_i I = L_i D_i L_i^T representations and   
    (iii) eigenvalues of each L_i D_i L_i^T.   
    The representations and eigenvalues found are then used by   
    DSTEGR to compute the eigenvectors of a symmetric tridiagonal   
    matrix. Currently, the base representations are limited to being   
    positive or negative definite, and the eigenvalues of the definite   
    matrices are found by the dqds algorithm (subroutine DLASQ2). As   
    an added benefit, DLARRE also outputs the n Gerschgorin   
    intervals for each L_i D_i L_i^T.   

    Arguments   
    =========   

    N       (input) INTEGER   
            The order of the matrix.   

    D       (input/output) DOUBLE PRECISION array, dimension (N)   
            On entry, the n diagonal elements of the tridiagonal   
            matrix T.   
            On exit, the n diagonal elements of the diagonal   
            matrices D_i.   

    E       (input/output) DOUBLE PRECISION array, dimension (N)   
            On entry, the (n-1) subdiagonal elements of the tridiagonal   
            matrix T; E(N) need not be set.   
            On exit, the subdiagonal elements of the unit bidiagonal   
            matrices L_i.   

    TOL     (input) DOUBLE PRECISION   
            The threshold for splitting. If on input |E(i)| < TOL, then   
            the matrix T is split into smaller blocks.   

    NSPLIT  (input) INTEGER   
            The number of blocks T splits into. 1 <= NSPLIT <= N.   

    ISPLIT  (output) INTEGER array, dimension (2*N)   
            The splitting points, at which T breaks up into submatrices.   
            The first submatrix consists of rows/columns 1 to ISPLIT(1),   
            the second of rows/columns ISPLIT(1)+1 through ISPLIT(2),   
            etc., and the NSPLIT-th consists of rows/columns   
            ISPLIT(NSPLIT-1)+1 through ISPLIT(NSPLIT)=N.   

    M       (output) INTEGER   
            The total number of eigenvalues (of all the L_i D_i L_i^T)   
            found.   

    W       (output) DOUBLE PRECISION array, dimension (N)   
            The first M elements contain the eigenvalues. The   
            eigenvalues of each of the blocks, L_i D_i L_i^T, are   
            sorted in ascending order.   

    WOFF    (output) DOUBLE PRECISION array, dimension (N)   
            The NSPLIT base points sigma_i.   

    GERSCH  (output) DOUBLE PRECISION array, dimension (2*N)   
            The n Gerschgorin intervals.   

    WORK    (input) DOUBLE PRECISION array, dimension (4*N???)   
            Workspace.   

    INFO    (output) INTEGER   
            Output error code from DLASQ2   

    Further Details   
    ===============   

    Based on contributions by   
       Inderjit Dhillon, IBM Almaden, USA   
       Osni Marques, LBNL/NERSC, USA   

    =====================================================================   


       Parameter adjustments */
    /* Table of constant values */
    static integer c__1 = 1;
    
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;
    /* Local variables */
    static doublereal offd;
    static integer iend, jblk, i__, j;
    static doublereal s, delta, sigma;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *);
    static doublereal width;
    extern /* Subroutine */ int dlasq2_(integer *, doublereal *, integer *);
    static doublereal gl;
    static integer in;
    extern doublereal dlamch_(char *);
    static doublereal gu;
    static integer ibegin;
    static doublereal sgndef;
    static integer maxcnt, cnt;
    static doublereal eps, tau, nrm, tmp1;


    --work;
    --gersch;
    --woff;
    --w;
    --isplit;
    --e;
    --d__;

    /* Function Body */
    *info = 0;
    eps = dlamch_("Precision");

/*     Compute Splitting Points */

    *nsplit = 1;
    i__1 = *n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if ((d__1 = e[i__], abs(d__1)) <= *tol) {
	    isplit[*nsplit] = i__;
	    ++(*nsplit);
	}
/* L10: */
    }
    isplit[*nsplit] = *n;

    ibegin = 1;
    i__1 = *nsplit;
    for (jblk = 1; jblk <= i__1; ++jblk) {
	iend = isplit[jblk];
	if (ibegin == iend) {
	    w[ibegin] = d__[ibegin];
	    woff[jblk] = 0.;
	    ibegin = iend + 1;
	    goto L170;
	}
	in = iend - ibegin + 1;

/*        Form the n Gerschgorin intervals */

	gl = d__[ibegin] - (d__1 = e[ibegin], abs(d__1));
	gu = d__[ibegin] + (d__1 = e[ibegin], abs(d__1));
	gersch[(ibegin << 1) - 1] = gl;
	gersch[ibegin * 2] = gu;
	gersch[(iend << 1) - 1] = d__[iend] - (d__1 = e[iend - 1], abs(d__1));
	gersch[iend * 2] = d__[iend] + (d__1 = e[iend - 1], abs(d__1));
/* Computing MIN */
	d__1 = gersch[(iend << 1) - 1];
	gl = min(d__1,gl);
/* Computing MAX */
	d__1 = gersch[iend * 2];
	gu = max(d__1,gu);
	i__2 = iend - 1;
	for (i__ = ibegin + 1; i__ <= i__2; ++i__) {
	    offd = (d__1 = e[i__ - 1], abs(d__1)) + (d__2 = e[i__], abs(d__2))
		    ;
	    gersch[(i__ << 1) - 1] = d__[i__] - offd;
/* Computing MIN */
	    d__1 = gersch[(i__ << 1) - 1];
	    gl = min(d__1,gl);
	    gersch[i__ * 2] = d__[i__] + offd;
/* Computing MAX */
	    d__1 = gersch[i__ * 2];
	    gu = max(d__1,gu);
/* L20: */
	}
/* Computing MAX */
	d__1 = abs(gl), d__2 = abs(gu);
	nrm = max(d__1,d__2);

/*        Find the number SIGMA where the base representation   
          T - sigma I = L D L^T is to be formed. */

	width = gu - gl;
	i__2 = iend - 1;
	for (i__ = ibegin; i__ <= i__2; ++i__) {
	    work[i__] = e[i__] * e[i__];
/* L30: */
	}
	for (j = 1; j <= 2; ++j) {
	    if (j == 1) {
		tau = gl + width * .25;
	    } else {
		tau = gu - width * .25;
	    }
	    tmp1 = d__[ibegin] - tau;
	    if (tmp1 < 0.) {
		cnt = 1;
	    } else {
		cnt = 0;
	    }
	    i__2 = iend;
	    for (i__ = ibegin + 1; i__ <= i__2; ++i__) {
		tmp1 = d__[i__] - tau - work[i__ - 1] / tmp1;
		if (tmp1 < 0.) {
		    ++cnt;
		}
/* L40: */
	    }
	    if (cnt == 0) {
		gl = tau;
	    } else if (cnt == in) {
		gu = tau;
	    }
	    if (j == 1) {
		maxcnt = cnt;
		sigma = gl;
		sgndef = 1.;
	    } else {
		if (in - cnt > maxcnt) {
		    sigma = gu;
		    sgndef = -1.;
		}
	    }
/* L50: */
	}

/*        Find the base L D L^T representation */

	work[in * 3] = 1.;
	delta = eps;
	tau = sgndef * nrm;
L60:
	sigma -= delta * tau;
	work[1] = d__[ibegin] - sigma;
	j = ibegin;
	i__2 = in - 1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    work[(in << 1) + i__] = 1. / work[(i__ << 1) - 1];
	    tmp1 = e[j] * work[(in << 1) + i__];
	    work[(i__ << 1) + 1] = d__[j + 1] - sigma - tmp1 * e[j];
	    work[i__ * 2] = tmp1;
	    ++j;
/* L70: */
	}
	for (i__ = in; i__ >= 1; --i__) {
	    tmp1 = sgndef * work[(i__ << 1) - 1];
	    if (tmp1 < 0. || work[(in << 1) + i__] == 0. || ! (tmp1 > 0. || 
		    tmp1 < 1.)) {
		delta *= 2.;
		goto L60;
	    }
	    --j;
/* L80: */
	}

	j = ibegin;
	d__[ibegin] = work[1];
	work[1] = abs(work[1]);
	i__2 = in - 1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    tmp1 = e[j];
	    e[j] = work[i__ * 2];
	    work[i__ * 2] = (d__1 = tmp1 * work[i__ * 2], abs(d__1));
	    ++j;
	    d__[j] = work[(i__ << 1) + 1];
	    work[(i__ << 1) + 1] = (d__1 = work[(i__ << 1) + 1], abs(d__1));
/* L90: */
	}

	dlasq2_(&in, &work[1], info);

	tau = sgndef * work[in];
	work[in * 3] = 1.;
	delta = eps * 2.;
L100:
	tau *= 1. - delta;

	s = -tau;
	j = ibegin;
	i__2 = in - 1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    work[i__] = d__[j] + s;
	    work[(in << 1) + i__] = 1. / work[i__];
/*           WORK( N+I ) = ( E( I ) * D( I ) ) / WORK( I ) */
	    work[in + i__] = e[j] * d__[j] * work[(in << 1) + i__];
	    s = s * work[in + i__] * e[j] - tau;
	    ++j;
/* L110: */
	}
	work[in] = d__[iend] + s;

/*        Checking to see if all the diagonal elements of the new   
          L D L^T representation have the same sign */

	for (i__ = in; i__ >= 1; --i__) {
	    tmp1 = sgndef * work[i__];
	    if (tmp1 < 0. || work[(in << 1) + i__] == 0. || ! (tmp1 > 0. || 
		    tmp1 < 1.)) {
		delta *= 2.;
		goto L100;
	    }
/* L120: */
	}

	sigma += tau;
	dcopy_(&in, &work[1], &c__1, &d__[ibegin], &c__1);
	i__2 = in - 1;
	dcopy_(&i__2, &work[in + 1], &c__1, &e[ibegin], &c__1);
	woff[jblk] = sigma;

/*        Update the n Gerschgorin intervals */

	i__2 = iend;
	for (i__ = ibegin; i__ <= i__2; ++i__) {
	    gersch[(i__ << 1) - 1] -= sigma;
	    gersch[i__ * 2] -= sigma;
/* L130: */
	}

/*        Compute the eigenvalues of L D L^T. */

	j = ibegin;
	i__2 = in - 1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    work[(i__ << 1) - 1] = (d__1 = d__[j], abs(d__1));
	    work[i__ * 2] = e[j] * e[j] * work[(i__ << 1) - 1];
	    ++j;
/* L140: */
	}
	work[(in << 1) - 1] = (d__1 = d__[iend], abs(d__1));

	dlasq2_(&in, &work[1], info);

	j = ibegin;
	if (sgndef > 0.) {
	    i__2 = in;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		w[j] = work[in - i__ + 1];
		++j;
/* L150: */
	    }
	} else {
	    i__2 = in;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		w[j] = -work[i__];
		++j;
/* L160: */
	    }
	}
	ibegin = iend + 1;
L170:
	;
    }
    *m = *n;

    return 0;

/*     End of DLARRE */

} /* dlarre_ */

