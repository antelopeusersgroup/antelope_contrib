#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int dlagtm_(char *trans, integer *n, integer *nrhs, 
	doublereal *alpha, doublereal *dl, doublereal *d__, doublereal *du, 
	doublereal *x, integer *ldx, doublereal *beta, doublereal *b, integer 
	*ldb)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       October 31, 1992   


    Purpose   
    =======   

    DLAGTM performs a matrix-vector product of the form   

       B := alpha * A * X + beta * B   

    where A is a tridiagonal matrix of order N, B and X are N by NRHS   
    matrices, and alpha and beta are real scalars, each of which may be   
    0., 1., or -1.   

    Arguments   
    =========   

    TRANS   (input) CHARACTER   
            Specifies the operation applied to A.   
            = 'N':  No transpose, B := alpha * A * X + beta * B   
            = 'T':  Transpose,    B := alpha * A'* X + beta * B   
            = 'C':  Conjugate transpose = Transpose   

    N       (input) INTEGER   
            The order of the matrix A.  N >= 0.   

    NRHS    (input) INTEGER   
            The number of right hand sides, i.e., the number of columns   
            of the matrices X and B.   

    ALPHA   (input) DOUBLE PRECISION   
            The scalar alpha.  ALPHA must be 0., 1., or -1.; otherwise,   
            it is assumed to be 0.   

    DL      (input) DOUBLE PRECISION array, dimension (N-1)   
            The (n-1) sub-diagonal elements of T.   

    D       (input) DOUBLE PRECISION array, dimension (N)   
            The diagonal elements of T.   

    DU      (input) DOUBLE PRECISION array, dimension (N-1)   
            The (n-1) super-diagonal elements of T.   

    X       (input) DOUBLE PRECISION array, dimension (LDX,NRHS)   
            The N by NRHS matrix X.   
    LDX     (input) INTEGER   
            The leading dimension of the array X.  LDX >= max(N,1).   

    BETA    (input) DOUBLE PRECISION   
            The scalar beta.  BETA must be 0., 1., or -1.; otherwise,   
            it is assumed to be 1.   

    B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)   
            On entry, the N by NRHS matrix B.   
            On exit, B is overwritten by the matrix expression   
            B := alpha * A * X + beta * B.   

    LDB     (input) INTEGER   
            The leading dimension of the array B.  LDB >= max(N,1).   

    =====================================================================   


       Parameter adjustments */
    /* System generated locals */
    integer b_dim1, b_offset, x_dim1, x_offset, i__1, i__2;
    /* Local variables */
    static integer i__, j;
    extern logical lsame_(char *, char *);
#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]
#define x_ref(a_1,a_2) x[(a_2)*x_dim1 + a_1]

    --dl;
    --d__;
    --du;
    x_dim1 = *ldx;
    x_offset = 1 + x_dim1 * 1;
    x -= x_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;

    /* Function Body */
    if (*n == 0) {
	return 0;
    }

/*     Multiply B by BETA if BETA.NE.1. */

    if (*beta == 0.) {
	i__1 = *nrhs;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *n;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		b_ref(i__, j) = 0.;
/* L10: */
	    }
/* L20: */
	}
    } else if (*beta == -1.) {
	i__1 = *nrhs;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *n;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		b_ref(i__, j) = -b_ref(i__, j);
/* L30: */
	    }
/* L40: */
	}
    }

    if (*alpha == 1.) {
	if (lsame_(trans, "N")) {

/*           Compute B := B + A*X */

	    i__1 = *nrhs;
	    for (j = 1; j <= i__1; ++j) {
		if (*n == 1) {
		    b_ref(1, j) = b_ref(1, j) + d__[1] * x_ref(1, j);
		} else {
		    b_ref(1, j) = b_ref(1, j) + d__[1] * x_ref(1, j) + du[1] *
			     x_ref(2, j);
		    b_ref(*n, j) = b_ref(*n, j) + dl[*n - 1] * x_ref(*n - 1, 
			    j) + d__[*n] * x_ref(*n, j);
		    i__2 = *n - 1;
		    for (i__ = 2; i__ <= i__2; ++i__) {
			b_ref(i__, j) = b_ref(i__, j) + dl[i__ - 1] * x_ref(
				i__ - 1, j) + d__[i__] * x_ref(i__, j) + du[
				i__] * x_ref(i__ + 1, j);
/* L50: */
		    }
		}
/* L60: */
	    }
	} else {

/*           Compute B := B + A'*X */

	    i__1 = *nrhs;
	    for (j = 1; j <= i__1; ++j) {
		if (*n == 1) {
		    b_ref(1, j) = b_ref(1, j) + d__[1] * x_ref(1, j);
		} else {
		    b_ref(1, j) = b_ref(1, j) + d__[1] * x_ref(1, j) + dl[1] *
			     x_ref(2, j);
		    b_ref(*n, j) = b_ref(*n, j) + du[*n - 1] * x_ref(*n - 1, 
			    j) + d__[*n] * x_ref(*n, j);
		    i__2 = *n - 1;
		    for (i__ = 2; i__ <= i__2; ++i__) {
			b_ref(i__, j) = b_ref(i__, j) + du[i__ - 1] * x_ref(
				i__ - 1, j) + d__[i__] * x_ref(i__, j) + dl[
				i__] * x_ref(i__ + 1, j);
/* L70: */
		    }
		}
/* L80: */
	    }
	}
    } else if (*alpha == -1.) {
	if (lsame_(trans, "N")) {

/*           Compute B := B - A*X */

	    i__1 = *nrhs;
	    for (j = 1; j <= i__1; ++j) {
		if (*n == 1) {
		    b_ref(1, j) = b_ref(1, j) - d__[1] * x_ref(1, j);
		} else {
		    b_ref(1, j) = b_ref(1, j) - d__[1] * x_ref(1, j) - du[1] *
			     x_ref(2, j);
		    b_ref(*n, j) = b_ref(*n, j) - dl[*n - 1] * x_ref(*n - 1, 
			    j) - d__[*n] * x_ref(*n, j);
		    i__2 = *n - 1;
		    for (i__ = 2; i__ <= i__2; ++i__) {
			b_ref(i__, j) = b_ref(i__, j) - dl[i__ - 1] * x_ref(
				i__ - 1, j) - d__[i__] * x_ref(i__, j) - du[
				i__] * x_ref(i__ + 1, j);
/* L90: */
		    }
		}
/* L100: */
	    }
	} else {

/*           Compute B := B - A'*X */

	    i__1 = *nrhs;
	    for (j = 1; j <= i__1; ++j) {
		if (*n == 1) {
		    b_ref(1, j) = b_ref(1, j) - d__[1] * x_ref(1, j);
		} else {
		    b_ref(1, j) = b_ref(1, j) - d__[1] * x_ref(1, j) - dl[1] *
			     x_ref(2, j);
		    b_ref(*n, j) = b_ref(*n, j) - du[*n - 1] * x_ref(*n - 1, 
			    j) - d__[*n] * x_ref(*n, j);
		    i__2 = *n - 1;
		    for (i__ = 2; i__ <= i__2; ++i__) {
			b_ref(i__, j) = b_ref(i__, j) - du[i__ - 1] * x_ref(
				i__ - 1, j) - d__[i__] * x_ref(i__, j) - dl[
				i__] * x_ref(i__ + 1, j);
/* L110: */
		    }
		}
/* L120: */
	    }
	}
    }
    return 0;

/*     End of DLAGTM */

} /* dlagtm_ */

#undef x_ref
#undef b_ref


