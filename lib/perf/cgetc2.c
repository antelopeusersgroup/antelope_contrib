#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int cgetc2_(integer *n, complex *a, integer *lda, integer *
	ipiv, integer *jpiv, integer *info)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    CGETC2 computes an LU factorization, using complete pivoting, of the   
    n-by-n matrix A. The factorization has the form A = P * L * U * Q,   
    where P and Q are permutation matrices, L is lower triangular with   
    unit diagonal elements and U is upper triangular.   

    This is a level 1 BLAS version of the algorithm.   

    Arguments   
    =========   

    N       (input) INTEGER   
            The order of the matrix A. N >= 0.   

    A       (input/output) COMPLEX array, dimension (LDA, N)   
            On entry, the n-by-n matrix to be factored.   
            On exit, the factors L and U from the factorization   
            A = P*L*U*Q; the unit diagonal elements of L are not stored.   
            If U(k, k) appears to be less than SMIN, U(k, k) is given the   
            value of SMIN, giving a nonsingular perturbed system.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1, N).   

    IPIV    (output) INTEGER array, dimension (N).   
            The pivot indices; for 1 <= i <= N, row i of the   
            matrix has been interchanged with row IPIV(i).   

    JPIV    (output) INTEGER array, dimension (N).   
            The pivot indices; for 1 <= j <= N, column j of the   
            matrix has been interchanged with column JPIV(j).   

    INFO    (output) INTEGER   
             = 0: successful exit   
             > 0: if INFO = k, U(k, k) is likely to produce overflow if   
                  one tries to solve for x in Ax = b. So U is perturbed   
                  to avoid the overflow.   

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
    static complex c_b10 = {-1.f,0.f};
    
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    real r__1;
    complex q__1;
    /* Builtin functions */
    double c_abs(complex *);
    void c_div(complex *, complex *, complex *);
    /* Local variables */
    static real smin, xmax;
    static integer i__, j;
    extern /* Subroutine */ int cgeru_(integer *, integer *, complex *, 
	    complex *, integer *, complex *, integer *, complex *, integer *),
	     cswap_(integer *, complex *, integer *, complex *, integer *), 
	    slabad_(real *, real *);
    static integer ip, jp;
    extern doublereal slamch_(char *);
    static real bignum, smlnum, eps;
    static integer ipv, jpv;
#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]


    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --ipiv;
    --jpiv;

    /* Function Body */
    *info = 0;
    eps = slamch_("P");
    smlnum = slamch_("S") / eps;
    bignum = 1.f / smlnum;
    slabad_(&smlnum, &bignum);

/*     Factorize A using complete pivoting.   
       Set pivots less than SMIN to SMIN */

    i__1 = *n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Find max element in matrix A */

	xmax = 0.f;
	i__2 = *n;
	for (ip = i__; ip <= i__2; ++ip) {
	    i__3 = *n;
	    for (jp = i__; jp <= i__3; ++jp) {
		if (c_abs(&a_ref(ip, jp)) >= xmax) {
		    xmax = c_abs(&a_ref(ip, jp));
		    ipv = ip;
		    jpv = jp;
		}
/* L10: */
	    }
/* L20: */
	}
	if (i__ == 1) {
/* Computing MAX */
	    r__1 = eps * xmax;
	    smin = dmax(r__1,smlnum);
	}

/*        Swap rows */

	if (ipv != i__) {
	    cswap_(n, &a_ref(ipv, 1), lda, &a_ref(i__, 1), lda);
	}
	ipiv[i__] = ipv;

/*        Swap columns */

	if (jpv != i__) {
	    cswap_(n, &a_ref(1, jpv), &c__1, &a_ref(1, i__), &c__1);
	}
	jpiv[i__] = jpv;

/*        Check for singularity */

	if (c_abs(&a_ref(i__, i__)) < smin) {
	    *info = i__;
	    i__2 = a_subscr(i__, i__);
	    q__1.r = smin, q__1.i = 0.f;
	    a[i__2].r = q__1.r, a[i__2].i = q__1.i;
	}
	i__2 = *n;
	for (j = i__ + 1; j <= i__2; ++j) {
	    i__3 = a_subscr(j, i__);
	    c_div(&q__1, &a_ref(j, i__), &a_ref(i__, i__));
	    a[i__3].r = q__1.r, a[i__3].i = q__1.i;
/* L30: */
	}
	i__2 = *n - i__;
	i__3 = *n - i__;
	cgeru_(&i__2, &i__3, &c_b10, &a_ref(i__ + 1, i__), &c__1, &a_ref(i__, 
		i__ + 1), lda, &a_ref(i__ + 1, i__ + 1), lda);
/* L40: */
    }

    if (c_abs(&a_ref(*n, *n)) < smin) {
	*info = *n;
	i__1 = a_subscr(*n, *n);
	q__1.r = smin, q__1.i = 0.f;
	a[i__1].r = q__1.r, a[i__1].i = q__1.i;
    }
    return 0;

/*     End of CGETC2 */

} /* cgetc2_ */

#undef a_ref
#undef a_subscr


