#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int cpptri_(char *uplo, integer *n, complex *ap, integer *
	info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    CPPTRI computes the inverse of a complex Hermitian positive definite   
    matrix A using the Cholesky factorization A = U**H*U or A = L*L**H   
    computed by CPPTRF.   

    Arguments   
    =========   

    UPLO    (input) CHARACTER*1   
            = 'U':  Upper triangular factor is stored in AP;   
            = 'L':  Lower triangular factor is stored in AP.   

    N       (input) INTEGER   
            The order of the matrix A.  N >= 0.   

    AP      (input/output) COMPLEX array, dimension (N*(N+1)/2)   
            On entry, the triangular factor U or L from the Cholesky   
            factorization A = U**H*U or A = L*L**H, packed columnwise as   
            a linear array.  The j-th column of U or L is stored in the   
            array AP as follows:   
            if UPLO = 'U', AP(i + (j-1)*j/2) = U(i,j) for 1<=i<=j;   
            if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = L(i,j) for j<=i<=n.   

            On exit, the upper or lower triangle of the (Hermitian)   
            inverse of A, overwriting the input factor U or L.   

    INFO    (output) INTEGER   
            = 0:  successful exit   
            < 0:  if INFO = -i, the i-th argument had an illegal value   
            > 0:  if INFO = i, the (i,i) element of the factor U or L is   
                  zero, and the inverse could not be computed.   

    =====================================================================   


       Test the input parameters.   

       Parameter adjustments */
    /* Table of constant values */
    static real c_b8 = 1.f;
    static integer c__1 = 1;
    
    /* System generated locals */
    integer i__1, i__2, i__3;
    real r__1;
    complex q__1;
    /* Local variables */
    extern /* Subroutine */ int chpr_(char *, integer *, real *, complex *, 
	    integer *, complex *);
    static integer j;
    extern /* Complex */ VOID cdotc_(complex *, integer *, complex *, integer 
	    *, complex *, integer *);
    extern logical lsame_(char *, char *);
    extern /* Subroutine */ int ctpmv_(char *, char *, char *, integer *, 
	    complex *, complex *, integer *);
    static logical upper;
    static integer jc, jj;
    extern /* Subroutine */ int csscal_(integer *, real *, complex *, integer 
	    *), xerbla_(char *, integer *), ctptri_(char *, char *, 
	    integer *, complex *, integer *);
    static real ajj;
    static integer jjn;


    --ap;

    /* Function Body */
    *info = 0;
    upper = lsame_(uplo, "U");
    if (! upper && ! lsame_(uplo, "L")) {
	*info = -1;
    } else if (*n < 0) {
	*info = -2;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("CPPTRI", &i__1);
	return 0;
    }

/*     Quick return if possible */

    if (*n == 0) {
	return 0;
    }

/*     Invert the triangular Cholesky factor U or L. */

    ctptri_(uplo, "Non-unit", n, &ap[1], info);
    if (*info > 0) {
	return 0;
    }
    if (upper) {

/*        Compute the product inv(U) * inv(U)'. */

	jj = 0;
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    jc = jj + 1;
	    jj += j;
	    if (j > 1) {
		i__2 = j - 1;
		chpr_("Upper", &i__2, &c_b8, &ap[jc], &c__1, &ap[1]);
	    }
	    i__2 = jj;
	    ajj = ap[i__2].r;
	    csscal_(&j, &ajj, &ap[jc], &c__1);
/* L10: */
	}

    } else {

/*        Compute the product inv(L)' * inv(L). */

	jj = 1;
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    jjn = jj + *n - j + 1;
	    i__2 = jj;
	    i__3 = *n - j + 1;
	    cdotc_(&q__1, &i__3, &ap[jj], &c__1, &ap[jj], &c__1);
	    r__1 = q__1.r;
	    ap[i__2].r = r__1, ap[i__2].i = 0.f;
	    if (j < *n) {
		i__2 = *n - j;
		ctpmv_("Lower", "Conjugate transpose", "Non-unit", &i__2, &ap[
			jjn], &ap[jj + 1], &c__1);
	    }
	    jj = jjn;
/* L20: */
	}
    }

    return 0;

/*     End of CPPTRI */

} /* cpptri_ */

