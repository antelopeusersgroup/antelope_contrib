#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int zgtts2_(integer *itrans, integer *n, integer *nrhs, 
	doublecomplex *dl, doublecomplex *d__, doublecomplex *du, 
	doublecomplex *du2, integer *ipiv, doublecomplex *b, integer *ldb)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    ZGTTS2 solves one of the systems of equations   
       A * X = B,  A**T * X = B,  or  A**H * X = B,   
    with a tridiagonal matrix A using the LU factorization computed   
    by ZGTTRF.   

    Arguments   
    =========   

    ITRANS  (input) INTEGER   
            Specifies the form of the system of equations.   
            = 0:  A * X = B     (No transpose)   
            = 1:  A**T * X = B  (Transpose)   
            = 2:  A**H * X = B  (Conjugate transpose)   

    N       (input) INTEGER   
            The order of the matrix A.   

    NRHS    (input) INTEGER   
            The number of right hand sides, i.e., the number of columns   
            of the matrix B.  NRHS >= 0.   

    DL      (input) COMPLEX*16 array, dimension (N-1)   
            The (n-1) multipliers that define the matrix L from the   
            LU factorization of A.   

    D       (input) COMPLEX*16 array, dimension (N)   
            The n diagonal elements of the upper triangular matrix U from   
            the LU factorization of A.   

    DU      (input) COMPLEX*16 array, dimension (N-1)   
            The (n-1) elements of the first super-diagonal of U.   

    DU2     (input) COMPLEX*16 array, dimension (N-2)   
            The (n-2) elements of the second super-diagonal of U.   

    IPIV    (input) INTEGER array, dimension (N)   
            The pivot indices; for 1 <= i <= n, row i of the matrix was   
            interchanged with row IPIV(i).  IPIV(i) will always be either   
            i or i+1; IPIV(i) = i indicates a row interchange was not   
            required.   

    B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)   
            On entry, the matrix of right hand side vectors B.   
            On exit, B is overwritten by the solution vectors X.   

    LDB     (input) INTEGER   
            The leading dimension of the array B.  LDB >= max(1,N).   

    =====================================================================   


       Quick return if possible   

       Parameter adjustments */
    /* System generated locals */
    integer b_dim1, b_offset, i__1, i__2, i__3, i__4, i__5, i__6, i__7, i__8;
    doublecomplex z__1, z__2, z__3, z__4, z__5, z__6, z__7, z__8;
    /* Builtin functions */
    void z_div(doublecomplex *, doublecomplex *, doublecomplex *), d_cnjg(
	    doublecomplex *, doublecomplex *);
    /* Local variables */
    static doublecomplex temp;
    static integer i__, j;
#define b_subscr(a_1,a_2) (a_2)*b_dim1 + a_1
#define b_ref(a_1,a_2) b[b_subscr(a_1,a_2)]

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
		if (ipiv[i__] == i__) {
		    i__2 = b_subscr(i__ + 1, j);
		    i__3 = b_subscr(i__ + 1, j);
		    i__4 = i__;
		    i__5 = b_subscr(i__, j);
		    z__2.r = dl[i__4].r * b[i__5].r - dl[i__4].i * b[i__5].i, 
			    z__2.i = dl[i__4].r * b[i__5].i + dl[i__4].i * b[
			    i__5].r;
		    z__1.r = b[i__3].r - z__2.r, z__1.i = b[i__3].i - z__2.i;
		    b[i__2].r = z__1.r, b[i__2].i = z__1.i;
		} else {
		    i__2 = b_subscr(i__, j);
		    temp.r = b[i__2].r, temp.i = b[i__2].i;
		    i__2 = b_subscr(i__, j);
		    i__3 = b_subscr(i__ + 1, j);
		    b[i__2].r = b[i__3].r, b[i__2].i = b[i__3].i;
		    i__2 = b_subscr(i__ + 1, j);
		    i__3 = i__;
		    i__4 = b_subscr(i__, j);
		    z__2.r = dl[i__3].r * b[i__4].r - dl[i__3].i * b[i__4].i, 
			    z__2.i = dl[i__3].r * b[i__4].i + dl[i__3].i * b[
			    i__4].r;
		    z__1.r = temp.r - z__2.r, z__1.i = temp.i - z__2.i;
		    b[i__2].r = z__1.r, b[i__2].i = z__1.i;
		}
/* L20: */
	    }

/*           Solve U*x = b. */

	    i__1 = b_subscr(*n, j);
	    z_div(&z__1, &b_ref(*n, j), &d__[*n]);
	    b[i__1].r = z__1.r, b[i__1].i = z__1.i;
	    if (*n > 1) {
		i__1 = b_subscr(*n - 1, j);
		i__2 = b_subscr(*n - 1, j);
		i__3 = *n - 1;
		i__4 = b_subscr(*n, j);
		z__3.r = du[i__3].r * b[i__4].r - du[i__3].i * b[i__4].i, 
			z__3.i = du[i__3].r * b[i__4].i + du[i__3].i * b[i__4]
			.r;
		z__2.r = b[i__2].r - z__3.r, z__2.i = b[i__2].i - z__3.i;
		z_div(&z__1, &z__2, &d__[*n - 1]);
		b[i__1].r = z__1.r, b[i__1].i = z__1.i;
	    }
	    for (i__ = *n - 2; i__ >= 1; --i__) {
		i__1 = b_subscr(i__, j);
		i__2 = b_subscr(i__, j);
		i__3 = i__;
		i__4 = b_subscr(i__ + 1, j);
		z__4.r = du[i__3].r * b[i__4].r - du[i__3].i * b[i__4].i, 
			z__4.i = du[i__3].r * b[i__4].i + du[i__3].i * b[i__4]
			.r;
		z__3.r = b[i__2].r - z__4.r, z__3.i = b[i__2].i - z__4.i;
		i__5 = i__;
		i__6 = b_subscr(i__ + 2, j);
		z__5.r = du2[i__5].r * b[i__6].r - du2[i__5].i * b[i__6].i, 
			z__5.i = du2[i__5].r * b[i__6].i + du2[i__5].i * b[
			i__6].r;
		z__2.r = z__3.r - z__5.r, z__2.i = z__3.i - z__5.i;
		z_div(&z__1, &z__2, &d__[i__]);
		b[i__1].r = z__1.r, b[i__1].i = z__1.i;
/* L30: */
	    }
	    if (j < *nrhs) {
		++j;
		goto L10;
	    }
	} else {
	    i__1 = *nrhs;
	    for (j = 1; j <= i__1; ++j) {

/*           Solve L*x = b. */

		i__2 = *n - 1;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    if (ipiv[i__] == i__) {
			i__3 = b_subscr(i__ + 1, j);
			i__4 = b_subscr(i__ + 1, j);
			i__5 = i__;
			i__6 = b_subscr(i__, j);
			z__2.r = dl[i__5].r * b[i__6].r - dl[i__5].i * b[i__6]
				.i, z__2.i = dl[i__5].r * b[i__6].i + dl[i__5]
				.i * b[i__6].r;
			z__1.r = b[i__4].r - z__2.r, z__1.i = b[i__4].i - 
				z__2.i;
			b[i__3].r = z__1.r, b[i__3].i = z__1.i;
		    } else {
			i__3 = b_subscr(i__, j);
			temp.r = b[i__3].r, temp.i = b[i__3].i;
			i__3 = b_subscr(i__, j);
			i__4 = b_subscr(i__ + 1, j);
			b[i__3].r = b[i__4].r, b[i__3].i = b[i__4].i;
			i__3 = b_subscr(i__ + 1, j);
			i__4 = i__;
			i__5 = b_subscr(i__, j);
			z__2.r = dl[i__4].r * b[i__5].r - dl[i__4].i * b[i__5]
				.i, z__2.i = dl[i__4].r * b[i__5].i + dl[i__4]
				.i * b[i__5].r;
			z__1.r = temp.r - z__2.r, z__1.i = temp.i - z__2.i;
			b[i__3].r = z__1.r, b[i__3].i = z__1.i;
		    }
/* L40: */
		}

/*           Solve U*x = b. */

		i__2 = b_subscr(*n, j);
		z_div(&z__1, &b_ref(*n, j), &d__[*n]);
		b[i__2].r = z__1.r, b[i__2].i = z__1.i;
		if (*n > 1) {
		    i__2 = b_subscr(*n - 1, j);
		    i__3 = b_subscr(*n - 1, j);
		    i__4 = *n - 1;
		    i__5 = b_subscr(*n, j);
		    z__3.r = du[i__4].r * b[i__5].r - du[i__4].i * b[i__5].i, 
			    z__3.i = du[i__4].r * b[i__5].i + du[i__4].i * b[
			    i__5].r;
		    z__2.r = b[i__3].r - z__3.r, z__2.i = b[i__3].i - z__3.i;
		    z_div(&z__1, &z__2, &d__[*n - 1]);
		    b[i__2].r = z__1.r, b[i__2].i = z__1.i;
		}
		for (i__ = *n - 2; i__ >= 1; --i__) {
		    i__2 = b_subscr(i__, j);
		    i__3 = b_subscr(i__, j);
		    i__4 = i__;
		    i__5 = b_subscr(i__ + 1, j);
		    z__4.r = du[i__4].r * b[i__5].r - du[i__4].i * b[i__5].i, 
			    z__4.i = du[i__4].r * b[i__5].i + du[i__4].i * b[
			    i__5].r;
		    z__3.r = b[i__3].r - z__4.r, z__3.i = b[i__3].i - z__4.i;
		    i__6 = i__;
		    i__7 = b_subscr(i__ + 2, j);
		    z__5.r = du2[i__6].r * b[i__7].r - du2[i__6].i * b[i__7]
			    .i, z__5.i = du2[i__6].r * b[i__7].i + du2[i__6]
			    .i * b[i__7].r;
		    z__2.r = z__3.r - z__5.r, z__2.i = z__3.i - z__5.i;
		    z_div(&z__1, &z__2, &d__[i__]);
		    b[i__2].r = z__1.r, b[i__2].i = z__1.i;
/* L50: */
		}
/* L60: */
	    }
	}
    } else if (*itrans == 1) {

/*        Solve A**T * X = B. */

	if (*nrhs <= 1) {
	    j = 1;
L70:

/*           Solve U**T * x = b. */

	    i__1 = b_subscr(1, j);
	    z_div(&z__1, &b_ref(1, j), &d__[1]);
	    b[i__1].r = z__1.r, b[i__1].i = z__1.i;
	    if (*n > 1) {
		i__1 = b_subscr(2, j);
		i__2 = b_subscr(2, j);
		i__3 = b_subscr(1, j);
		z__3.r = du[1].r * b[i__3].r - du[1].i * b[i__3].i, z__3.i = 
			du[1].r * b[i__3].i + du[1].i * b[i__3].r;
		z__2.r = b[i__2].r - z__3.r, z__2.i = b[i__2].i - z__3.i;
		z_div(&z__1, &z__2, &d__[2]);
		b[i__1].r = z__1.r, b[i__1].i = z__1.i;
	    }
	    i__1 = *n;
	    for (i__ = 3; i__ <= i__1; ++i__) {
		i__2 = b_subscr(i__, j);
		i__3 = b_subscr(i__, j);
		i__4 = i__ - 1;
		i__5 = b_subscr(i__ - 1, j);
		z__4.r = du[i__4].r * b[i__5].r - du[i__4].i * b[i__5].i, 
			z__4.i = du[i__4].r * b[i__5].i + du[i__4].i * b[i__5]
			.r;
		z__3.r = b[i__3].r - z__4.r, z__3.i = b[i__3].i - z__4.i;
		i__6 = i__ - 2;
		i__7 = b_subscr(i__ - 2, j);
		z__5.r = du2[i__6].r * b[i__7].r - du2[i__6].i * b[i__7].i, 
			z__5.i = du2[i__6].r * b[i__7].i + du2[i__6].i * b[
			i__7].r;
		z__2.r = z__3.r - z__5.r, z__2.i = z__3.i - z__5.i;
		z_div(&z__1, &z__2, &d__[i__]);
		b[i__2].r = z__1.r, b[i__2].i = z__1.i;
/* L80: */
	    }

/*           Solve L**T * x = b. */

	    for (i__ = *n - 1; i__ >= 1; --i__) {
		if (ipiv[i__] == i__) {
		    i__1 = b_subscr(i__, j);
		    i__2 = b_subscr(i__, j);
		    i__3 = i__;
		    i__4 = b_subscr(i__ + 1, j);
		    z__2.r = dl[i__3].r * b[i__4].r - dl[i__3].i * b[i__4].i, 
			    z__2.i = dl[i__3].r * b[i__4].i + dl[i__3].i * b[
			    i__4].r;
		    z__1.r = b[i__2].r - z__2.r, z__1.i = b[i__2].i - z__2.i;
		    b[i__1].r = z__1.r, b[i__1].i = z__1.i;
		} else {
		    i__1 = b_subscr(i__ + 1, j);
		    temp.r = b[i__1].r, temp.i = b[i__1].i;
		    i__1 = b_subscr(i__ + 1, j);
		    i__2 = b_subscr(i__, j);
		    i__3 = i__;
		    z__2.r = dl[i__3].r * temp.r - dl[i__3].i * temp.i, 
			    z__2.i = dl[i__3].r * temp.i + dl[i__3].i * 
			    temp.r;
		    z__1.r = b[i__2].r - z__2.r, z__1.i = b[i__2].i - z__2.i;
		    b[i__1].r = z__1.r, b[i__1].i = z__1.i;
		    i__1 = b_subscr(i__, j);
		    b[i__1].r = temp.r, b[i__1].i = temp.i;
		}
/* L90: */
	    }
	    if (j < *nrhs) {
		++j;
		goto L70;
	    }
	} else {
	    i__1 = *nrhs;
	    for (j = 1; j <= i__1; ++j) {

/*           Solve U**T * x = b. */

		i__2 = b_subscr(1, j);
		z_div(&z__1, &b_ref(1, j), &d__[1]);
		b[i__2].r = z__1.r, b[i__2].i = z__1.i;
		if (*n > 1) {
		    i__2 = b_subscr(2, j);
		    i__3 = b_subscr(2, j);
		    i__4 = b_subscr(1, j);
		    z__3.r = du[1].r * b[i__4].r - du[1].i * b[i__4].i, 
			    z__3.i = du[1].r * b[i__4].i + du[1].i * b[i__4]
			    .r;
		    z__2.r = b[i__3].r - z__3.r, z__2.i = b[i__3].i - z__3.i;
		    z_div(&z__1, &z__2, &d__[2]);
		    b[i__2].r = z__1.r, b[i__2].i = z__1.i;
		}
		i__2 = *n;
		for (i__ = 3; i__ <= i__2; ++i__) {
		    i__3 = b_subscr(i__, j);
		    i__4 = b_subscr(i__, j);
		    i__5 = i__ - 1;
		    i__6 = b_subscr(i__ - 1, j);
		    z__4.r = du[i__5].r * b[i__6].r - du[i__5].i * b[i__6].i, 
			    z__4.i = du[i__5].r * b[i__6].i + du[i__5].i * b[
			    i__6].r;
		    z__3.r = b[i__4].r - z__4.r, z__3.i = b[i__4].i - z__4.i;
		    i__7 = i__ - 2;
		    i__8 = b_subscr(i__ - 2, j);
		    z__5.r = du2[i__7].r * b[i__8].r - du2[i__7].i * b[i__8]
			    .i, z__5.i = du2[i__7].r * b[i__8].i + du2[i__7]
			    .i * b[i__8].r;
		    z__2.r = z__3.r - z__5.r, z__2.i = z__3.i - z__5.i;
		    z_div(&z__1, &z__2, &d__[i__]);
		    b[i__3].r = z__1.r, b[i__3].i = z__1.i;
/* L100: */
		}

/*           Solve L**T * x = b. */

		for (i__ = *n - 1; i__ >= 1; --i__) {
		    if (ipiv[i__] == i__) {
			i__2 = b_subscr(i__, j);
			i__3 = b_subscr(i__, j);
			i__4 = i__;
			i__5 = b_subscr(i__ + 1, j);
			z__2.r = dl[i__4].r * b[i__5].r - dl[i__4].i * b[i__5]
				.i, z__2.i = dl[i__4].r * b[i__5].i + dl[i__4]
				.i * b[i__5].r;
			z__1.r = b[i__3].r - z__2.r, z__1.i = b[i__3].i - 
				z__2.i;
			b[i__2].r = z__1.r, b[i__2].i = z__1.i;
		    } else {
			i__2 = b_subscr(i__ + 1, j);
			temp.r = b[i__2].r, temp.i = b[i__2].i;
			i__2 = b_subscr(i__ + 1, j);
			i__3 = b_subscr(i__, j);
			i__4 = i__;
			z__2.r = dl[i__4].r * temp.r - dl[i__4].i * temp.i, 
				z__2.i = dl[i__4].r * temp.i + dl[i__4].i * 
				temp.r;
			z__1.r = b[i__3].r - z__2.r, z__1.i = b[i__3].i - 
				z__2.i;
			b[i__2].r = z__1.r, b[i__2].i = z__1.i;
			i__2 = b_subscr(i__, j);
			b[i__2].r = temp.r, b[i__2].i = temp.i;
		    }
/* L110: */
		}
/* L120: */
	    }
	}
    } else {

/*        Solve A**H * X = B. */

	if (*nrhs <= 1) {
	    j = 1;
L130:

/*           Solve U**H * x = b. */

	    i__1 = b_subscr(1, j);
	    d_cnjg(&z__2, &d__[1]);
	    z_div(&z__1, &b_ref(1, j), &z__2);
	    b[i__1].r = z__1.r, b[i__1].i = z__1.i;
	    if (*n > 1) {
		i__1 = b_subscr(2, j);
		i__2 = b_subscr(2, j);
		d_cnjg(&z__4, &du[1]);
		i__3 = b_subscr(1, j);
		z__3.r = z__4.r * b[i__3].r - z__4.i * b[i__3].i, z__3.i = 
			z__4.r * b[i__3].i + z__4.i * b[i__3].r;
		z__2.r = b[i__2].r - z__3.r, z__2.i = b[i__2].i - z__3.i;
		d_cnjg(&z__5, &d__[2]);
		z_div(&z__1, &z__2, &z__5);
		b[i__1].r = z__1.r, b[i__1].i = z__1.i;
	    }
	    i__1 = *n;
	    for (i__ = 3; i__ <= i__1; ++i__) {
		i__2 = b_subscr(i__, j);
		i__3 = b_subscr(i__, j);
		d_cnjg(&z__5, &du[i__ - 1]);
		i__4 = b_subscr(i__ - 1, j);
		z__4.r = z__5.r * b[i__4].r - z__5.i * b[i__4].i, z__4.i = 
			z__5.r * b[i__4].i + z__5.i * b[i__4].r;
		z__3.r = b[i__3].r - z__4.r, z__3.i = b[i__3].i - z__4.i;
		d_cnjg(&z__7, &du2[i__ - 2]);
		i__5 = b_subscr(i__ - 2, j);
		z__6.r = z__7.r * b[i__5].r - z__7.i * b[i__5].i, z__6.i = 
			z__7.r * b[i__5].i + z__7.i * b[i__5].r;
		z__2.r = z__3.r - z__6.r, z__2.i = z__3.i - z__6.i;
		d_cnjg(&z__8, &d__[i__]);
		z_div(&z__1, &z__2, &z__8);
		b[i__2].r = z__1.r, b[i__2].i = z__1.i;
/* L140: */
	    }

/*           Solve L**H * x = b. */

	    for (i__ = *n - 1; i__ >= 1; --i__) {
		if (ipiv[i__] == i__) {
		    i__1 = b_subscr(i__, j);
		    i__2 = b_subscr(i__, j);
		    d_cnjg(&z__3, &dl[i__]);
		    i__3 = b_subscr(i__ + 1, j);
		    z__2.r = z__3.r * b[i__3].r - z__3.i * b[i__3].i, z__2.i =
			     z__3.r * b[i__3].i + z__3.i * b[i__3].r;
		    z__1.r = b[i__2].r - z__2.r, z__1.i = b[i__2].i - z__2.i;
		    b[i__1].r = z__1.r, b[i__1].i = z__1.i;
		} else {
		    i__1 = b_subscr(i__ + 1, j);
		    temp.r = b[i__1].r, temp.i = b[i__1].i;
		    i__1 = b_subscr(i__ + 1, j);
		    i__2 = b_subscr(i__, j);
		    d_cnjg(&z__3, &dl[i__]);
		    z__2.r = z__3.r * temp.r - z__3.i * temp.i, z__2.i = 
			    z__3.r * temp.i + z__3.i * temp.r;
		    z__1.r = b[i__2].r - z__2.r, z__1.i = b[i__2].i - z__2.i;
		    b[i__1].r = z__1.r, b[i__1].i = z__1.i;
		    i__1 = b_subscr(i__, j);
		    b[i__1].r = temp.r, b[i__1].i = temp.i;
		}
/* L150: */
	    }
	    if (j < *nrhs) {
		++j;
		goto L130;
	    }
	} else {
	    i__1 = *nrhs;
	    for (j = 1; j <= i__1; ++j) {

/*           Solve U**H * x = b. */

		i__2 = b_subscr(1, j);
		d_cnjg(&z__2, &d__[1]);
		z_div(&z__1, &b_ref(1, j), &z__2);
		b[i__2].r = z__1.r, b[i__2].i = z__1.i;
		if (*n > 1) {
		    i__2 = b_subscr(2, j);
		    i__3 = b_subscr(2, j);
		    d_cnjg(&z__4, &du[1]);
		    i__4 = b_subscr(1, j);
		    z__3.r = z__4.r * b[i__4].r - z__4.i * b[i__4].i, z__3.i =
			     z__4.r * b[i__4].i + z__4.i * b[i__4].r;
		    z__2.r = b[i__3].r - z__3.r, z__2.i = b[i__3].i - z__3.i;
		    d_cnjg(&z__5, &d__[2]);
		    z_div(&z__1, &z__2, &z__5);
		    b[i__2].r = z__1.r, b[i__2].i = z__1.i;
		}
		i__2 = *n;
		for (i__ = 3; i__ <= i__2; ++i__) {
		    i__3 = b_subscr(i__, j);
		    i__4 = b_subscr(i__, j);
		    d_cnjg(&z__5, &du[i__ - 1]);
		    i__5 = b_subscr(i__ - 1, j);
		    z__4.r = z__5.r * b[i__5].r - z__5.i * b[i__5].i, z__4.i =
			     z__5.r * b[i__5].i + z__5.i * b[i__5].r;
		    z__3.r = b[i__4].r - z__4.r, z__3.i = b[i__4].i - z__4.i;
		    d_cnjg(&z__7, &du2[i__ - 2]);
		    i__6 = b_subscr(i__ - 2, j);
		    z__6.r = z__7.r * b[i__6].r - z__7.i * b[i__6].i, z__6.i =
			     z__7.r * b[i__6].i + z__7.i * b[i__6].r;
		    z__2.r = z__3.r - z__6.r, z__2.i = z__3.i - z__6.i;
		    d_cnjg(&z__8, &d__[i__]);
		    z_div(&z__1, &z__2, &z__8);
		    b[i__3].r = z__1.r, b[i__3].i = z__1.i;
/* L160: */
		}

/*           Solve L**H * x = b. */

		for (i__ = *n - 1; i__ >= 1; --i__) {
		    if (ipiv[i__] == i__) {
			i__2 = b_subscr(i__, j);
			i__3 = b_subscr(i__, j);
			d_cnjg(&z__3, &dl[i__]);
			i__4 = b_subscr(i__ + 1, j);
			z__2.r = z__3.r * b[i__4].r - z__3.i * b[i__4].i, 
				z__2.i = z__3.r * b[i__4].i + z__3.i * b[i__4]
				.r;
			z__1.r = b[i__3].r - z__2.r, z__1.i = b[i__3].i - 
				z__2.i;
			b[i__2].r = z__1.r, b[i__2].i = z__1.i;
		    } else {
			i__2 = b_subscr(i__ + 1, j);
			temp.r = b[i__2].r, temp.i = b[i__2].i;
			i__2 = b_subscr(i__ + 1, j);
			i__3 = b_subscr(i__, j);
			d_cnjg(&z__3, &dl[i__]);
			z__2.r = z__3.r * temp.r - z__3.i * temp.i, z__2.i = 
				z__3.r * temp.i + z__3.i * temp.r;
			z__1.r = b[i__3].r - z__2.r, z__1.i = b[i__3].i - 
				z__2.i;
			b[i__2].r = z__1.r, b[i__2].i = z__1.i;
			i__2 = b_subscr(i__, j);
			b[i__2].r = temp.r, b[i__2].i = temp.i;
		    }
/* L170: */
		}
/* L180: */
	    }
	}
    }

/*     End of ZGTTS2 */

    return 0;
} /* zgtts2_ */

#undef b_ref
#undef b_subscr


