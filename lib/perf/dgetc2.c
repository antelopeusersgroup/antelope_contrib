#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int dgetc2_(integer *n, doublereal *a, integer *lda, integer 
	*ipiv, integer *jpiv, integer *info)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    DGETC2 computes an LU factorization with complete pivoting of the   
    n-by-n matrix A. The factorization has the form A = P * L * U * Q,   
    where P and Q are permutation matrices, L is lower triangular with   
    unit diagonal elements and U is upper triangular.   

    This is the Level 2 BLAS algorithm.   

    Arguments   
    =========   

    N       (input) INTEGER   
            The order of the matrix A. N >= 0.   

    A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)   
            On entry, the n-by-n matrix A to be factored.   
            On exit, the factors L and U from the factorization   
            A = P*L*U*Q; the unit diagonal elements of L are not stored.   
            If U(k, k) appears to be less than SMIN, U(k, k) is given the   
            value of SMIN, i.e., giving a nonsingular perturbed system.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,N).   

    IPIV    (output) INTEGER array, dimension(N).   
            The pivot indices; for 1 <= i <= N, row i of the   
            matrix has been interchanged with row IPIV(i).   

    JPIV    (output) INTEGER array, dimension(N).   
            The pivot indices; for 1 <= j <= N, column j of the   
            matrix has been interchanged with column JPIV(j).   

    INFO    (output) INTEGER   
             = 0: successful exit   
             > 0: if INFO = k, U(k, k) is likely to produce owerflow if   
                  we try to solve for x in Ax = b. So U is perturbed to   
                  avoid the overflow.   

    Further Details   
    ===============   

    Based on contributions by   
       Bo Kagstrom and Peter Poromaa, Department of Computing Science,   
       Umea University, S-901 87 Umea, Sweden.   

    =====================================================================   


       Set constants to control overflow   

       Parameter adjustments */
    /* Table of constant values */
    static integer c__1 = 1;
    static doublereal c_b10 = -1.;
    
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1;
    /* Local variables */
    extern /* Subroutine */ int dger_(integer *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    static doublereal smin, xmax;
    static integer i__, j;
    extern /* Subroutine */ int dswap_(integer *, doublereal *, integer *, 
	    doublereal *, integer *), dlabad_(doublereal *, doublereal *);
    extern doublereal dlamch_(char *);
    static integer ip, jp;
    static doublereal bignum, smlnum, eps;
    static integer ipv, jpv;
#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]


    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --ipiv;
    --jpiv;

    /* Function Body */
    *info = 0;
    eps = dlamch_("P");
    smlnum = dlamch_("S") / eps;
    bignum = 1. / smlnum;
    dlabad_(&smlnum, &bignum);

/*     Factorize A using complete pivoting.   
       Set pivots less than SMIN to SMIN. */

    i__1 = *n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Find max element in matrix A */

	xmax = 0.;
	i__2 = *n;
	for (ip = i__; ip <= i__2; ++ip) {
	    i__3 = *n;
	    for (jp = i__; jp <= i__3; ++jp) {
		if ((d__1 = a_ref(ip, jp), abs(d__1)) >= xmax) {
		    xmax = (d__1 = a_ref(ip, jp), abs(d__1));
		    ipv = ip;
		    jpv = jp;
		}
/* L10: */
	    }
/* L20: */
	}
	if (i__ == 1) {
/* Computing MAX */
	    d__1 = eps * xmax;
	    smin = max(d__1,smlnum);
	}

/*        Swap rows */

	if (ipv != i__) {
	    dswap_(n, &a_ref(ipv, 1), lda, &a_ref(i__, 1), lda);
	}
	ipiv[i__] = ipv;

/*        Swap columns */

	if (jpv != i__) {
	    dswap_(n, &a_ref(1, jpv), &c__1, &a_ref(1, i__), &c__1);
	}
	jpiv[i__] = jpv;

/*        Check for singularity */

	if ((d__1 = a_ref(i__, i__), abs(d__1)) < smin) {
	    *info = i__;
	    a_ref(i__, i__) = smin;
	}
	i__2 = *n;
	for (j = i__ + 1; j <= i__2; ++j) {
	    a_ref(j, i__) = a_ref(j, i__) / a_ref(i__, i__);
/* L30: */
	}
	i__2 = *n - i__;
	i__3 = *n - i__;
	dger_(&i__2, &i__3, &c_b10, &a_ref(i__ + 1, i__), &c__1, &a_ref(i__, 
		i__ + 1), lda, &a_ref(i__ + 1, i__ + 1), lda);
/* L40: */
    }

    if ((d__1 = a_ref(*n, *n), abs(d__1)) < smin) {
	*info = *n;
	a_ref(*n, *n) = smin;
    }

    return 0;

/*     End of DGETC2 */

} /* dgetc2_ */

#undef a_ref


