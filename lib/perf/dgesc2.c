#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int dgesc2_(integer *n, doublereal *a, integer *lda, 
	doublereal *rhs, integer *ipiv, integer *jpiv, doublereal *scale)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    DGESC2 solves a system of linear equations   

              A * X = scale* RHS   

    with a general N-by-N matrix A using the LU factorization with   
    complete pivoting computed by DGETC2.   

    Arguments   
    =========   

    N       (input) INTEGER   
            The order of the matrix A.   

    A       (input) DOUBLE PRECISION array, dimension (LDA,N)   
            On entry, the  LU part of the factorization of the n-by-n   
            matrix A computed by DGETC2:  A = P * L * U * Q   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1, N).   

    RHS     (input/output) DOUBLE PRECISION array, dimension (N).   
            On entry, the right hand side vector b.   
            On exit, the solution vector X.   

    IPIV    (iput) INTEGER array, dimension (N).   
            The pivot indices; for 1 <= i <= N, row i of the   
            matrix has been interchanged with row IPIV(i).   

    JPIV    (iput) INTEGER array, dimension (N).   
            The pivot indices; for 1 <= j <= N, column j of the   
            matrix has been interchanged with column JPIV(j).   

    SCALE    (output) DOUBLE PRECISION   
             On exit, SCALE contains the scale factor. SCALE is chosen   
             0 <= SCALE <= 1 to prevent owerflow in the solution.   

    Further Details   
    ===============   

    Based on contributions by   
       Bo Kagstrom and Peter Poromaa, Department of Computing Science,   
       Umea University, S-901 87 Umea, Sweden.   

    =====================================================================   


        Set constant to control owerflow   

       Parameter adjustments */
    /* Table of constant values */
    static integer c__1 = 1;
    static integer c_n1 = -1;
    
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    doublereal d__1, d__2;
    /* Local variables */
    static doublereal temp;
    static integer i__, j;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *), dlabad_(doublereal *, doublereal *);
    extern doublereal dlamch_(char *);
    extern integer idamax_(integer *, doublereal *, integer *);
    static doublereal bignum;
    extern /* Subroutine */ int dlaswp_(integer *, doublereal *, integer *, 
	    integer *, integer *, integer *, integer *);
    static doublereal smlnum, eps;
#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]


    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --rhs;
    --ipiv;
    --jpiv;

    /* Function Body */
    eps = dlamch_("P");
    smlnum = dlamch_("S") / eps;
    bignum = 1. / smlnum;
    dlabad_(&smlnum, &bignum);

/*     Apply permutations IPIV to RHS */

    i__1 = *n - 1;
    dlaswp_(&c__1, &rhs[1], lda, &c__1, &i__1, &ipiv[1], &c__1);

/*     Solve for L part */

    i__1 = *n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = *n;
	for (j = i__ + 1; j <= i__2; ++j) {
	    rhs[j] -= a_ref(j, i__) * rhs[i__];
/* L10: */
	}
/* L20: */
    }

/*     Solve for U part */

    *scale = 1.;

/*     Check for scaling */

    i__ = idamax_(n, &rhs[1], &c__1);
    if (smlnum * 2. * (d__1 = rhs[i__], abs(d__1)) > (d__2 = a_ref(*n, *n), 
	    abs(d__2))) {
	temp = .5 / (d__1 = rhs[i__], abs(d__1));
	dscal_(n, &temp, &rhs[1], &c__1);
	*scale *= temp;
    }

    for (i__ = *n; i__ >= 1; --i__) {
	temp = 1. / a_ref(i__, i__);
	rhs[i__] *= temp;
	i__1 = *n;
	for (j = i__ + 1; j <= i__1; ++j) {
	    rhs[i__] -= rhs[j] * (a_ref(i__, j) * temp);
/* L30: */
	}
/* L40: */
    }

/*     Apply permutations JPIV to the solution (RHS) */

    i__1 = *n - 1;
    dlaswp_(&c__1, &rhs[1], lda, &c__1, &i__1, &jpiv[1], &c_n1);
    return 0;

/*     End of DGESC2 */

} /* dgesc2_ */

#undef a_ref


