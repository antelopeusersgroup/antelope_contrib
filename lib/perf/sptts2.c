#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int sptts2_(integer *n, integer *nrhs, real *d__, real *e, 
	real *b, integer *ldb)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    SPTTS2 solves a tridiagonal system of the form   
       A * X = B   
    using the L*D*L' factorization of A computed by SPTTRF.  D is a   
    diagonal matrix specified in the vector D, L is a unit bidiagonal   
    matrix whose subdiagonal is specified in the vector E, and X and B   
    are N by NRHS matrices.   

    Arguments   
    =========   

    N       (input) INTEGER   
            The order of the tridiagonal matrix A.  N >= 0.   

    NRHS    (input) INTEGER   
            The number of right hand sides, i.e., the number of columns   
            of the matrix B.  NRHS >= 0.   

    D       (input) REAL array, dimension (N)   
            The n diagonal elements of the diagonal matrix D from the   
            L*D*L' factorization of A.   

    E       (input) REAL array, dimension (N-1)   
            The (n-1) subdiagonal elements of the unit bidiagonal factor   
            L from the L*D*L' factorization of A.  E can also be regarded   
            as the superdiagonal of the unit bidiagonal factor U from the   
            factorization A = U'*D*U.   

    B       (input/output) REAL array, dimension (LDB,NRHS)   
            On entry, the right hand side vectors B for the system of   
            linear equations.   
            On exit, the solution vectors, X.   

    LDB     (input) INTEGER   
            The leading dimension of the array B.  LDB >= max(1,N).   

    =====================================================================   


       Quick return if possible   

       Parameter adjustments */
    /* System generated locals */
    integer b_dim1, b_offset, i__1, i__2;
    real r__1;
    /* Local variables */
    static integer i__, j;
    extern /* Subroutine */ int sscal_(integer *, real *, real *, integer *);
#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]

    --d__;
    --e;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;

    /* Function Body */
    if (*n <= 1) {
	if (*n == 1) {
	    r__1 = 1.f / d__[1];
	    sscal_(nrhs, &r__1, &b[b_offset], ldb);
	}
	return 0;
    }

/*     Solve A * X = B using the factorization A = L*D*L',   
       overwriting each right hand side vector with its solution. */

    i__1 = *nrhs;
    for (j = 1; j <= i__1; ++j) {

/*           Solve L * x = b. */

	i__2 = *n;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    b_ref(i__, j) = b_ref(i__, j) - b_ref(i__ - 1, j) * e[i__ - 1];
/* L10: */
	}

/*           Solve D * L' * x = b. */

	b_ref(*n, j) = b_ref(*n, j) / d__[*n];
	for (i__ = *n - 1; i__ >= 1; --i__) {
	    b_ref(i__, j) = b_ref(i__, j) / d__[i__] - b_ref(i__ + 1, j) * e[
		    i__];
/* L20: */
	}
/* L30: */
    }

    return 0;

/*     End of SPTTS2 */

} /* sptts2_ */

#undef b_ref


