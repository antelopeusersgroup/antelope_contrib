#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int dpbstf_(char *uplo, integer *n, integer *kd, doublereal *
	ab, integer *ldab, integer *info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    DPBSTF computes a split Cholesky factorization of a real   
    symmetric positive definite band matrix A.   

    This routine is designed to be used in conjunction with DSBGST.   

    The factorization has the form  A = S**T*S  where S is a band matrix   
    of the same bandwidth as A and the following structure:   

      S = ( U    )   
          ( M  L )   

    where U is upper triangular of order m = (n+kd)/2, and L is lower   
    triangular of order n-m.   

    Arguments   
    =========   

    UPLO    (input) CHARACTER*1   
            = 'U':  Upper triangle of A is stored;   
            = 'L':  Lower triangle of A is stored.   

    N       (input) INTEGER   
            The order of the matrix A.  N >= 0.   

    KD      (input) INTEGER   
            The number of superdiagonals of the matrix A if UPLO = 'U',   
            or the number of subdiagonals if UPLO = 'L'.  KD >= 0.   

    AB      (input/output) DOUBLE PRECISION array, dimension (LDAB,N)   
            On entry, the upper or lower triangle of the symmetric band   
            matrix A, stored in the first kd+1 rows of the array.  The   
            j-th column of A is stored in the j-th column of the array AB   
            as follows:   
            if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;   
            if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).   

            On exit, if INFO = 0, the factor S from the split Cholesky   
            factorization A = S**T*S. See Further Details.   

    LDAB    (input) INTEGER   
            The leading dimension of the array AB.  LDAB >= KD+1.   

    INFO    (output) INTEGER   
            = 0: successful exit   
            < 0: if INFO = -i, the i-th argument had an illegal value   
            > 0: if INFO = i, the factorization could not be completed,   
                 because the updated element a(i,i) was negative; the   
                 matrix A is not positive definite.   

    Further Details   
    ===============   

    The band storage scheme is illustrated by the following example, when   
    N = 7, KD = 2:   

    S = ( s11  s12  s13                     )   
        (      s22  s23  s24                )   
        (           s33  s34                )   
        (                s44                )   
        (           s53  s54  s55           )   
        (                s64  s65  s66      )   
        (                     s75  s76  s77 )   

    If UPLO = 'U', the array AB holds:   

    on entry:                          on exit:   

     *    *   a13  a24  a35  a46  a57   *    *   s13  s24  s53  s64  s75   
     *   a12  a23  a34  a45  a56  a67   *   s12  s23  s34  s54  s65  s76   
    a11  a22  a33  a44  a55  a66  a77  s11  s22  s33  s44  s55  s66  s77   

    If UPLO = 'L', the array AB holds:   

    on entry:                          on exit:   

    a11  a22  a33  a44  a55  a66  a77  s11  s22  s33  s44  s55  s66  s77   
    a21  a32  a43  a54  a65  a76   *   s12  s23  s34  s54  s65  s76   *   
    a31  a42  a53  a64  a64   *    *   s13  s24  s53  s64  s75   *    *   

    Array elements marked * are not used by the routine.   

    =====================================================================   


       Test the input parameters.   

       Parameter adjustments */
    /* Table of constant values */
    static integer c__1 = 1;
    static doublereal c_b9 = -1.;
    
    /* System generated locals */
    integer ab_dim1, ab_offset, i__1, i__2, i__3;
    doublereal d__1;
    /* Builtin functions */
    double sqrt(doublereal);
    /* Local variables */
    extern /* Subroutine */ int dsyr_(char *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *);
    static integer j, m;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *);
    extern logical lsame_(char *, char *);
    static logical upper;
    static integer km;
    extern /* Subroutine */ int xerbla_(char *, integer *);
    static doublereal ajj;
    static integer kld;
#define ab_ref(a_1,a_2) ab[(a_2)*ab_dim1 + a_1]


    ab_dim1 = *ldab;
    ab_offset = 1 + ab_dim1 * 1;
    ab -= ab_offset;

    /* Function Body */
    *info = 0;
    upper = lsame_(uplo, "U");
    if (! upper && ! lsame_(uplo, "L")) {
	*info = -1;
    } else if (*n < 0) {
	*info = -2;
    } else if (*kd < 0) {
	*info = -3;
    } else if (*ldab < *kd + 1) {
	*info = -5;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("DPBSTF", &i__1);
	return 0;
    }

/*     Quick return if possible */

    if (*n == 0) {
	return 0;
    }

/* Computing MAX */
    i__1 = 1, i__2 = *ldab - 1;
    kld = max(i__1,i__2);

/*     Set the splitting point m. */

    m = (*n + *kd) / 2;

    if (upper) {

/*        Factorize A(m+1:n,m+1:n) as L**T*L, and update A(1:m,1:m). */

	i__1 = m + 1;
	for (j = *n; j >= i__1; --j) {

/*           Compute s(j,j) and test for non-positive-definiteness. */

	    ajj = ab_ref(*kd + 1, j);
	    if (ajj <= 0.) {
		goto L50;
	    }
	    ajj = sqrt(ajj);
	    ab_ref(*kd + 1, j) = ajj;
/* Computing MIN */
	    i__2 = j - 1;
	    km = min(i__2,*kd);

/*           Compute elements j-km:j-1 of the j-th column and update the   
             the leading submatrix within the band. */

	    d__1 = 1. / ajj;
	    dscal_(&km, &d__1, &ab_ref(*kd + 1 - km, j), &c__1);
	    dsyr_("Upper", &km, &c_b9, &ab_ref(*kd + 1 - km, j), &c__1, &
		    ab_ref(*kd + 1, j - km), &kld);
/* L10: */
	}

/*        Factorize the updated submatrix A(1:m,1:m) as U**T*U. */

	i__1 = m;
	for (j = 1; j <= i__1; ++j) {

/*           Compute s(j,j) and test for non-positive-definiteness. */

	    ajj = ab_ref(*kd + 1, j);
	    if (ajj <= 0.) {
		goto L50;
	    }
	    ajj = sqrt(ajj);
	    ab_ref(*kd + 1, j) = ajj;
/* Computing MIN */
	    i__2 = *kd, i__3 = m - j;
	    km = min(i__2,i__3);

/*           Compute elements j+1:j+km of the j-th row and update the   
             trailing submatrix within the band. */

	    if (km > 0) {
		d__1 = 1. / ajj;
		dscal_(&km, &d__1, &ab_ref(*kd, j + 1), &kld);
		dsyr_("Upper", &km, &c_b9, &ab_ref(*kd, j + 1), &kld, &ab_ref(
			*kd + 1, j + 1), &kld);
	    }
/* L20: */
	}
    } else {

/*        Factorize A(m+1:n,m+1:n) as L**T*L, and update A(1:m,1:m). */

	i__1 = m + 1;
	for (j = *n; j >= i__1; --j) {

/*           Compute s(j,j) and test for non-positive-definiteness. */

	    ajj = ab_ref(1, j);
	    if (ajj <= 0.) {
		goto L50;
	    }
	    ajj = sqrt(ajj);
	    ab_ref(1, j) = ajj;
/* Computing MIN */
	    i__2 = j - 1;
	    km = min(i__2,*kd);

/*           Compute elements j-km:j-1 of the j-th row and update the   
             trailing submatrix within the band. */

	    d__1 = 1. / ajj;
	    dscal_(&km, &d__1, &ab_ref(km + 1, j - km), &kld);
	    dsyr_("Lower", &km, &c_b9, &ab_ref(km + 1, j - km), &kld, &ab_ref(
		    1, j - km), &kld);
/* L30: */
	}

/*        Factorize the updated submatrix A(1:m,1:m) as U**T*U. */

	i__1 = m;
	for (j = 1; j <= i__1; ++j) {

/*           Compute s(j,j) and test for non-positive-definiteness. */

	    ajj = ab_ref(1, j);
	    if (ajj <= 0.) {
		goto L50;
	    }
	    ajj = sqrt(ajj);
	    ab_ref(1, j) = ajj;
/* Computing MIN */
	    i__2 = *kd, i__3 = m - j;
	    km = min(i__2,i__3);

/*           Compute elements j+1:j+km of the j-th column and update the   
             trailing submatrix within the band. */

	    if (km > 0) {
		d__1 = 1. / ajj;
		dscal_(&km, &d__1, &ab_ref(2, j), &c__1);
		dsyr_("Lower", &km, &c_b9, &ab_ref(2, j), &c__1, &ab_ref(1, j 
			+ 1), &kld);
	    }
/* L40: */
	}
    }
    return 0;

L50:
    *info = j;
    return 0;

/*     End of DPBSTF */

} /* dpbstf_ */

#undef ab_ref


