#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int dlarrf_(integer *n, doublereal *d__, doublereal *l, 
	doublereal *ld, doublereal *lld, integer *ifirst, integer *ilast, 
	doublereal *w, doublereal *dplus, doublereal *lplus, doublereal *work,
	 integer *iwork, integer *info)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    Given the initial representation L D L^T and its cluster of close   
    eigenvalues (in a relative measure), W( IFIRST ), W( IFIRST+1 ), ...   
    W( ILAST ), DLARRF finds a new relatively robust representation   
    L D L^T - SIGMA I = L(+) D(+) L(+)^T such that at least one of the   
    eigenvalues of L(+) D(+) L(+)^T is relatively isolated.   

    Arguments   
    =========   

    N       (input) INTEGER   
            The order of the matrix.   

    D       (input) DOUBLE PRECISION array, dimension (N)   
            The n diagonal elements of the diagonal matrix D.   

    L       (input) DOUBLE PRECISION array, dimension (N-1)   
            The (n-1) subdiagonal elements of the unit bidiagonal   
            matrix L.   

    LD      (input) DOUBLE PRECISION array, dimension (N-1)   
            The n-1 elements L(i)*D(i).   

    LLD     (input) DOUBLE PRECISION array, dimension (N-1)   
            The n-1 elements L(i)*L(i)*D(i).   

    IFIRST  (input) INTEGER   
            The index of the first eigenvalue in the cluster.   

    ILAST   (input) INTEGER   
            The index of the last eigenvalue in the cluster.   

    W       (input/output) DOUBLE PRECISION array, dimension (N)   
            On input, the eigenvalues of L D L^T in ascending order.   
            W( IFIRST ) through W( ILAST ) form the cluster of relatively   
            close eigenalues.   
            On output, W( IFIRST ) thru' W( ILAST ) are estimates of the   
            corresponding eigenvalues of L(+) D(+) L(+)^T.   

    SIGMA   (input) DOUBLE PRECISION   
            The shift used to form L(+) D(+) L(+)^T.   

    DPLUS   (output) DOUBLE PRECISION array, dimension (N)   
            The n diagonal elements of the diagonal matrix D(+).   

    LPLUS   (output) DOUBLE PRECISION array, dimension (N)   
            The first (n-1) elements of LPLUS contain the subdiagonal   
            elements of the unit bidiagonal matrix L(+). LPLUS( N ) is   
            set to SIGMA.   

    WORK    (input) DOUBLE PRECISION array, dimension (???)   
            Workspace.   

    Further Details   
    ===============   

    Based on contributions by   
       Inderjit Dhillon, IBM Almaden, USA   
       Osni Marques, LBNL/NERSC, USA   

    =====================================================================   


       Parameter adjustments */
    /* System generated locals */
    integer i__1;
    /* Local variables */
    static integer i__;
    static doublereal s, delta, sigma;
    extern doublereal dlamch_(char *);
    static doublereal eps;

    --iwork;
    --work;
    --lplus;
    --dplus;
    --w;
    --lld;
    --ld;
    --l;
    --d__;

    /* Function Body */
    *info = 0;
    eps = dlamch_("Precision");
    if (*ifirst == 1) {
	sigma = w[*ifirst];
    } else if (*ilast == *n) {
	sigma = w[*ilast];
    } else {
	*info = 1;
	return 0;
    }

/*     Compute the new relatively robust representation (RRR) */

    delta = eps * 2.;
L10:
    if (*ifirst == 1) {
	sigma -= abs(sigma) * delta;
    } else {
	sigma += abs(sigma) * delta;
    }
    s = -sigma;
    i__1 = *n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	dplus[i__] = d__[i__] + s;
	lplus[i__] = ld[i__] / dplus[i__];
	s = s * lplus[i__] * l[i__] - sigma;
/* L20: */
    }
    dplus[*n] = d__[*n] + s;
    if (*ifirst == 1) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (dplus[i__] < 0.) {
		delta *= 2.;
		goto L10;
	    }
/* L30: */
	}
    } else {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (dplus[i__] > 0.) {
		delta *= 2.;
		goto L10;
	    }
/* L40: */
	}
    }
    i__1 = *ilast;
    for (i__ = *ifirst; i__ <= i__1; ++i__) {
	w[i__] -= sigma;
/* L50: */
    }
    lplus[*n] = sigma;

    return 0;

/*     End of DLARRF */

} /* dlarrf_ */

