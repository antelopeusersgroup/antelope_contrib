#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int sgtts2_(integer *itrans, integer *n, integer *nrhs, real 
	*dl, real *d__, real *du, real *du2, integer *ipiv, real *b, integer *
	ldb)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    SGTTS2 solves one of the systems of equations   
       A*X = B  or  A'*X = B,   
    with a tridiagonal matrix A using the LU factorization computed   
    by SGTTRF.   

    Arguments   
    =========   

    ITRANS  (input) INTEGER   
            Specifies the form of the system of equations.   
            = 0:  A * X = B  (No transpose)   
            = 1:  A'* X = B  (Transpose)   
            = 2:  A'* X = B  (Conjugate transpose = Transpose)   

    N       (input) INTEGER   
            The order of the matrix A.   

    NRHS    (input) INTEGER   
            The number of right hand sides, i.e., the number of columns   
            of the matrix B.  NRHS >= 0.   

    DL      (input) REAL array, dimension (N-1)   
            The (n-1) multipliers that define the matrix L from the   
            LU factorization of A.   

    D       (input) REAL array, dimension (N)   
            The n diagonal elements of the upper triangular matrix U from   
            the LU factorization of A.   

    DU      (input) REAL array, dimension (N-1)   
            The (n-1) elements of the first super-diagonal of U.   

    DU2     (input) REAL array, dimension (N-2)   
            The (n-2) elements of the second super-diagonal of U.   

    IPIV    (input) INTEGER array, dimension (N)   
            The pivot indices; for 1 <= i <= n, row i of the matrix was   
            interchanged with row IPIV(i).  IPIV(i) will always be either   
            i or i+1; IPIV(i) = i indicates a row interchange was not   
            required.   

    B       (input/output) REAL array, dimension (LDB,NRHS)   
            On entry, the matrix of right hand side vectors B.   
            On exit, B is overwritten by the solution vectors X.   

    LDB     (input) INTEGER   
            The leading dimension of the array B.  LDB >= max(1,N).   

    =====================================================================   


       Quick return if possible   

       Parameter adjustments */
    /* System generated locals */
    integer b_dim1, b_offset, i__1, i__2;
    /* Local variables */
    static real temp;
    static integer i__, j, ip;
#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]

    --dl;
    --d__;
    --du;
    --du2;
    --ipiv;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;

    /* Function Body */
    if (*n == 0 || *nrhs == 0) {
	return 0;
    }

    if (*itrans == 0) {

/*        Solve A*X = B using the LU factorization of A,   
          overwriting each right hand side vector with its solution. */

	if (*nrhs <= 1) {
	    j = 1;
L10:

/*           Solve L*x = b. */

	    i__1 = *n - 1;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		ip = ipiv[i__];
		temp = b_ref(i__ + 1 - ip + i__, j) - dl[i__] * b_ref(ip, j);
		b_ref(i__, j) = b_ref(ip, j);
		b_ref(i__ + 1, j) = temp;
/* L20: */
	    }

/*           Solve U*x = b. */

	    b_ref(*n, j) = b_ref(*n, j) / d__[*n];
	    if (*n > 1) {
		b_ref(*n - 1, j) = (b_ref(*n - 1, j) - du[*n - 1] * b_ref(*n, 
			j)) / d__[*n - 1];
	    }
	    for (i__ = *n - 2; i__ >= 1; --i__) {
		b_ref(i__, j) = (b_ref(i__, j) - du[i__] * b_ref(i__ + 1, j) 
			- du2[i__] * b_ref(i__ + 2, j)) / d__[i__];
/* L30: */
	    }
	    if (j < *nrhs) {
		++j;
		goto L10;
	    }
	} else {
	    i__1 = *nrhs;
	    for (j = 1; j <= i__1; ++j) {

/*              Solve L*x = b. */

		i__2 = *n - 1;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    if (ipiv[i__] == i__) {
			b_ref(i__ + 1, j) = b_ref(i__ + 1, j) - dl[i__] * 
				b_ref(i__, j);
		    } else {
			temp = b_ref(i__, j);
			b_ref(i__, j) = b_ref(i__ + 1, j);
			b_ref(i__ + 1, j) = temp - dl[i__] * b_ref(i__, j);
		    }
/* L40: */
		}

/*              Solve U*x = b. */

		b_ref(*n, j) = b_ref(*n, j) / d__[*n];
		if (*n > 1) {
		    b_ref(*n - 1, j) = (b_ref(*n - 1, j) - du[*n - 1] * b_ref(
			    *n, j)) / d__[*n - 1];
		}
		for (i__ = *n - 2; i__ >= 1; --i__) {
		    b_ref(i__, j) = (b_ref(i__, j) - du[i__] * b_ref(i__ + 1, 
			    j) - du2[i__] * b_ref(i__ + 2, j)) / d__[i__];
/* L50: */
		}
/* L60: */
	    }
	}
    } else {

/*        Solve A' * X = B. */

	if (*nrhs <= 1) {

/*           Solve U'*x = b. */

	    j = 1;
L70:
	    b_ref(1, j) = b_ref(1, j) / d__[1];
	    if (*n > 1) {
		b_ref(2, j) = (b_ref(2, j) - du[1] * b_ref(1, j)) / d__[2];
	    }
	    i__1 = *n;
	    for (i__ = 3; i__ <= i__1; ++i__) {
		b_ref(i__, j) = (b_ref(i__, j) - du[i__ - 1] * b_ref(i__ - 1, 
			j) - du2[i__ - 2] * b_ref(i__ - 2, j)) / d__[i__];
/* L80: */
	    }

/*           Solve L'*x = b. */

	    for (i__ = *n - 1; i__ >= 1; --i__) {
		ip = ipiv[i__];
		temp = b_ref(i__, j) - dl[i__] * b_ref(i__ + 1, j);
		b_ref(i__, j) = b_ref(ip, j);
		b_ref(ip, j) = temp;
/* L90: */
	    }
	    if (j < *nrhs) {
		++j;
		goto L70;
	    }

	} else {
	    i__1 = *nrhs;
	    for (j = 1; j <= i__1; ++j) {

/*              Solve U'*x = b. */

		b_ref(1, j) = b_ref(1, j) / d__[1];
		if (*n > 1) {
		    b_ref(2, j) = (b_ref(2, j) - du[1] * b_ref(1, j)) / d__[2]
			    ;
		}
		i__2 = *n;
		for (i__ = 3; i__ <= i__2; ++i__) {
		    b_ref(i__, j) = (b_ref(i__, j) - du[i__ - 1] * b_ref(i__ 
			    - 1, j) - du2[i__ - 2] * b_ref(i__ - 2, j)) / d__[
			    i__];
/* L100: */
		}
		for (i__ = *n - 1; i__ >= 1; --i__) {
		    if (ipiv[i__] == i__) {
			b_ref(i__, j) = b_ref(i__, j) - dl[i__] * b_ref(i__ + 
				1, j);
		    } else {
			temp = b_ref(i__ + 1, j);
			b_ref(i__ + 1, j) = b_ref(i__, j) - dl[i__] * temp;
			b_ref(i__, j) = temp;
		    }
/* L110: */
		}
/* L120: */
	    }
	}
    }

/*     End of SGTTS2 */

    return 0;
} /* sgtts2_ */

#undef b_ref


