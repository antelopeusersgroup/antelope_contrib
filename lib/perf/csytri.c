#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int csytri_(char *uplo, integer *n, complex *a, integer *lda,
	 integer *ipiv, complex *work, integer *info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    CSYTRI computes the inverse of a complex symmetric indefinite matrix   
    A using the factorization A = U*D*U**T or A = L*D*L**T computed by   
    CSYTRF.   

    Arguments   
    =========   

    UPLO    (input) CHARACTER*1   
            Specifies whether the details of the factorization are stored   
            as an upper or lower triangular matrix.   
            = 'U':  Upper triangular, form is A = U*D*U**T;   
            = 'L':  Lower triangular, form is A = L*D*L**T.   

    N       (input) INTEGER   
            The order of the matrix A.  N >= 0.   

    A       (input/output) COMPLEX array, dimension (LDA,N)   
            On entry, the block diagonal matrix D and the multipliers   
            used to obtain the factor U or L as computed by CSYTRF.   

            On exit, if INFO = 0, the (symmetric) inverse of the original   
            matrix.  If UPLO = 'U', the upper triangular part of the   
            inverse is formed and the part of A below the diagonal is not   
            referenced; if UPLO = 'L' the lower triangular part of the   
            inverse is formed and the part of A above the diagonal is   
            not referenced.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,N).   

    IPIV    (input) INTEGER array, dimension (N)   
            Details of the interchanges and the block structure of D   
            as determined by CSYTRF.   

    WORK    (workspace) COMPLEX array, dimension (2*N)   

    INFO    (output) INTEGER   
            = 0: successful exit   
            < 0: if INFO = -i, the i-th argument had an illegal value   
            > 0: if INFO = i, D(i,i) = 0; the matrix is singular and its   
                 inverse could not be computed.   

    =====================================================================   


       Test the input parameters.   

       Parameter adjustments */
    /* Table of constant values */
    static complex c_b1 = {1.f,0.f};
    static complex c_b2 = {0.f,0.f};
    static integer c__1 = 1;
    
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    complex q__1, q__2, q__3;
    /* Builtin functions */
    void c_div(complex *, complex *, complex *);
    /* Local variables */
    static complex temp, akkp1, d__;
    static integer k;
    static complex t;
    extern logical lsame_(char *, char *);
    extern /* Subroutine */ int ccopy_(integer *, complex *, integer *, 
	    complex *, integer *);
    extern /* Complex */ VOID cdotu_(complex *, integer *, complex *, integer 
	    *, complex *, integer *);
    extern /* Subroutine */ int cswap_(integer *, complex *, integer *, 
	    complex *, integer *);
    static integer kstep;
    static logical upper;
    extern /* Subroutine */ int csymv_(char *, integer *, complex *, complex *
	    , integer *, complex *, integer *, complex *, complex *, integer *
	    );
    static complex ak;
    static integer kp;
    extern /* Subroutine */ int xerbla_(char *, integer *);
    static complex akp1;
#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]


    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --ipiv;
    --work;

    /* Function Body */
    *info = 0;
    upper = lsame_(uplo, "U");
    if (! upper && ! lsame_(uplo, "L")) {
	*info = -1;
    } else if (*n < 0) {
	*info = -2;
    } else if (*lda < max(1,*n)) {
	*info = -4;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("CSYTRI", &i__1);
	return 0;
    }

/*     Quick return if possible */

    if (*n == 0) {
	return 0;
    }

/*     Check that the diagonal matrix D is nonsingular. */

    if (upper) {

/*        Upper triangular storage: examine D from bottom to top */

	for (*info = *n; *info >= 1; --(*info)) {
	    i__1 = a_subscr(*info, *info);
	    if (ipiv[*info] > 0 && (a[i__1].r == 0.f && a[i__1].i == 0.f)) {
		return 0;
	    }
/* L10: */
	}
    } else {

/*        Lower triangular storage: examine D from top to bottom. */

	i__1 = *n;
	for (*info = 1; *info <= i__1; ++(*info)) {
	    i__2 = a_subscr(*info, *info);
	    if (ipiv[*info] > 0 && (a[i__2].r == 0.f && a[i__2].i == 0.f)) {
		return 0;
	    }
/* L20: */
	}
    }
    *info = 0;

    if (upper) {

/*        Compute inv(A) from the factorization A = U*D*U'.   

          K is the main loop index, increasing from 1 to N in steps of   
          1 or 2, depending on the size of the diagonal blocks. */

	k = 1;
L30:

/*        If K > N, exit from loop. */

	if (k > *n) {
	    goto L40;
	}

	if (ipiv[k] > 0) {

/*           1 x 1 diagonal block   

             Invert the diagonal block. */

	    i__1 = a_subscr(k, k);
	    c_div(&q__1, &c_b1, &a_ref(k, k));
	    a[i__1].r = q__1.r, a[i__1].i = q__1.i;

/*           Compute column K of the inverse. */

	    if (k > 1) {
		i__1 = k - 1;
		ccopy_(&i__1, &a_ref(1, k), &c__1, &work[1], &c__1);
		i__1 = k - 1;
		q__1.r = -1.f, q__1.i = 0.f;
		csymv_(uplo, &i__1, &q__1, &a[a_offset], lda, &work[1], &c__1,
			 &c_b2, &a_ref(1, k), &c__1);
		i__1 = a_subscr(k, k);
		i__2 = a_subscr(k, k);
		i__3 = k - 1;
		cdotu_(&q__2, &i__3, &work[1], &c__1, &a_ref(1, k), &c__1);
		q__1.r = a[i__2].r - q__2.r, q__1.i = a[i__2].i - q__2.i;
		a[i__1].r = q__1.r, a[i__1].i = q__1.i;
	    }
	    kstep = 1;
	} else {

/*           2 x 2 diagonal block   

             Invert the diagonal block. */

	    i__1 = a_subscr(k, k + 1);
	    t.r = a[i__1].r, t.i = a[i__1].i;
	    c_div(&q__1, &a_ref(k, k), &t);
	    ak.r = q__1.r, ak.i = q__1.i;
	    c_div(&q__1, &a_ref(k + 1, k + 1), &t);
	    akp1.r = q__1.r, akp1.i = q__1.i;
	    c_div(&q__1, &a_ref(k, k + 1), &t);
	    akkp1.r = q__1.r, akkp1.i = q__1.i;
	    q__3.r = ak.r * akp1.r - ak.i * akp1.i, q__3.i = ak.r * akp1.i + 
		    ak.i * akp1.r;
	    q__2.r = q__3.r - 1.f, q__2.i = q__3.i + 0.f;
	    q__1.r = t.r * q__2.r - t.i * q__2.i, q__1.i = t.r * q__2.i + t.i 
		    * q__2.r;
	    d__.r = q__1.r, d__.i = q__1.i;
	    i__1 = a_subscr(k, k);
	    c_div(&q__1, &akp1, &d__);
	    a[i__1].r = q__1.r, a[i__1].i = q__1.i;
	    i__1 = a_subscr(k + 1, k + 1);
	    c_div(&q__1, &ak, &d__);
	    a[i__1].r = q__1.r, a[i__1].i = q__1.i;
	    i__1 = a_subscr(k, k + 1);
	    q__2.r = -akkp1.r, q__2.i = -akkp1.i;
	    c_div(&q__1, &q__2, &d__);
	    a[i__1].r = q__1.r, a[i__1].i = q__1.i;

/*           Compute columns K and K+1 of the inverse. */

	    if (k > 1) {
		i__1 = k - 1;
		ccopy_(&i__1, &a_ref(1, k), &c__1, &work[1], &c__1);
		i__1 = k - 1;
		q__1.r = -1.f, q__1.i = 0.f;
		csymv_(uplo, &i__1, &q__1, &a[a_offset], lda, &work[1], &c__1,
			 &c_b2, &a_ref(1, k), &c__1);
		i__1 = a_subscr(k, k);
		i__2 = a_subscr(k, k);
		i__3 = k - 1;
		cdotu_(&q__2, &i__3, &work[1], &c__1, &a_ref(1, k), &c__1);
		q__1.r = a[i__2].r - q__2.r, q__1.i = a[i__2].i - q__2.i;
		a[i__1].r = q__1.r, a[i__1].i = q__1.i;
		i__1 = a_subscr(k, k + 1);
		i__2 = a_subscr(k, k + 1);
		i__3 = k - 1;
		cdotu_(&q__2, &i__3, &a_ref(1, k), &c__1, &a_ref(1, k + 1), &
			c__1);
		q__1.r = a[i__2].r - q__2.r, q__1.i = a[i__2].i - q__2.i;
		a[i__1].r = q__1.r, a[i__1].i = q__1.i;
		i__1 = k - 1;
		ccopy_(&i__1, &a_ref(1, k + 1), &c__1, &work[1], &c__1);
		i__1 = k - 1;
		q__1.r = -1.f, q__1.i = 0.f;
		csymv_(uplo, &i__1, &q__1, &a[a_offset], lda, &work[1], &c__1,
			 &c_b2, &a_ref(1, k + 1), &c__1);
		i__1 = a_subscr(k + 1, k + 1);
		i__2 = a_subscr(k + 1, k + 1);
		i__3 = k - 1;
		cdotu_(&q__2, &i__3, &work[1], &c__1, &a_ref(1, k + 1), &c__1)
			;
		q__1.r = a[i__2].r - q__2.r, q__1.i = a[i__2].i - q__2.i;
		a[i__1].r = q__1.r, a[i__1].i = q__1.i;
	    }
	    kstep = 2;
	}

	kp = (i__1 = ipiv[k], abs(i__1));
	if (kp != k) {

/*           Interchange rows and columns K and KP in the leading   
             submatrix A(1:k+1,1:k+1) */

	    i__1 = kp - 1;
	    cswap_(&i__1, &a_ref(1, k), &c__1, &a_ref(1, kp), &c__1);
	    i__1 = k - kp - 1;
	    cswap_(&i__1, &a_ref(kp + 1, k), &c__1, &a_ref(kp, kp + 1), lda);
	    i__1 = a_subscr(k, k);
	    temp.r = a[i__1].r, temp.i = a[i__1].i;
	    i__1 = a_subscr(k, k);
	    i__2 = a_subscr(kp, kp);
	    a[i__1].r = a[i__2].r, a[i__1].i = a[i__2].i;
	    i__1 = a_subscr(kp, kp);
	    a[i__1].r = temp.r, a[i__1].i = temp.i;
	    if (kstep == 2) {
		i__1 = a_subscr(k, k + 1);
		temp.r = a[i__1].r, temp.i = a[i__1].i;
		i__1 = a_subscr(k, k + 1);
		i__2 = a_subscr(kp, k + 1);
		a[i__1].r = a[i__2].r, a[i__1].i = a[i__2].i;
		i__1 = a_subscr(kp, k + 1);
		a[i__1].r = temp.r, a[i__1].i = temp.i;
	    }
	}

	k += kstep;
	goto L30;
L40:

	;
    } else {

/*        Compute inv(A) from the factorization A = L*D*L'.   

          K is the main loop index, increasing from 1 to N in steps of   
          1 or 2, depending on the size of the diagonal blocks. */

	k = *n;
L50:

/*        If K < 1, exit from loop. */

	if (k < 1) {
	    goto L60;
	}

	if (ipiv[k] > 0) {

/*           1 x 1 diagonal block   

             Invert the diagonal block. */

	    i__1 = a_subscr(k, k);
	    c_div(&q__1, &c_b1, &a_ref(k, k));
	    a[i__1].r = q__1.r, a[i__1].i = q__1.i;

/*           Compute column K of the inverse. */

	    if (k < *n) {
		i__1 = *n - k;
		ccopy_(&i__1, &a_ref(k + 1, k), &c__1, &work[1], &c__1);
		i__1 = *n - k;
		q__1.r = -1.f, q__1.i = 0.f;
		csymv_(uplo, &i__1, &q__1, &a_ref(k + 1, k + 1), lda, &work[1]
			, &c__1, &c_b2, &a_ref(k + 1, k), &c__1);
		i__1 = a_subscr(k, k);
		i__2 = a_subscr(k, k);
		i__3 = *n - k;
		cdotu_(&q__2, &i__3, &work[1], &c__1, &a_ref(k + 1, k), &c__1)
			;
		q__1.r = a[i__2].r - q__2.r, q__1.i = a[i__2].i - q__2.i;
		a[i__1].r = q__1.r, a[i__1].i = q__1.i;
	    }
	    kstep = 1;
	} else {

/*           2 x 2 diagonal block   

             Invert the diagonal block. */

	    i__1 = a_subscr(k, k - 1);
	    t.r = a[i__1].r, t.i = a[i__1].i;
	    c_div(&q__1, &a_ref(k - 1, k - 1), &t);
	    ak.r = q__1.r, ak.i = q__1.i;
	    c_div(&q__1, &a_ref(k, k), &t);
	    akp1.r = q__1.r, akp1.i = q__1.i;
	    c_div(&q__1, &a_ref(k, k - 1), &t);
	    akkp1.r = q__1.r, akkp1.i = q__1.i;
	    q__3.r = ak.r * akp1.r - ak.i * akp1.i, q__3.i = ak.r * akp1.i + 
		    ak.i * akp1.r;
	    q__2.r = q__3.r - 1.f, q__2.i = q__3.i + 0.f;
	    q__1.r = t.r * q__2.r - t.i * q__2.i, q__1.i = t.r * q__2.i + t.i 
		    * q__2.r;
	    d__.r = q__1.r, d__.i = q__1.i;
	    i__1 = a_subscr(k - 1, k - 1);
	    c_div(&q__1, &akp1, &d__);
	    a[i__1].r = q__1.r, a[i__1].i = q__1.i;
	    i__1 = a_subscr(k, k);
	    c_div(&q__1, &ak, &d__);
	    a[i__1].r = q__1.r, a[i__1].i = q__1.i;
	    i__1 = a_subscr(k, k - 1);
	    q__2.r = -akkp1.r, q__2.i = -akkp1.i;
	    c_div(&q__1, &q__2, &d__);
	    a[i__1].r = q__1.r, a[i__1].i = q__1.i;

/*           Compute columns K-1 and K of the inverse. */

	    if (k < *n) {
		i__1 = *n - k;
		ccopy_(&i__1, &a_ref(k + 1, k), &c__1, &work[1], &c__1);
		i__1 = *n - k;
		q__1.r = -1.f, q__1.i = 0.f;
		csymv_(uplo, &i__1, &q__1, &a_ref(k + 1, k + 1), lda, &work[1]
			, &c__1, &c_b2, &a_ref(k + 1, k), &c__1);
		i__1 = a_subscr(k, k);
		i__2 = a_subscr(k, k);
		i__3 = *n - k;
		cdotu_(&q__2, &i__3, &work[1], &c__1, &a_ref(k + 1, k), &c__1)
			;
		q__1.r = a[i__2].r - q__2.r, q__1.i = a[i__2].i - q__2.i;
		a[i__1].r = q__1.r, a[i__1].i = q__1.i;
		i__1 = a_subscr(k, k - 1);
		i__2 = a_subscr(k, k - 1);
		i__3 = *n - k;
		cdotu_(&q__2, &i__3, &a_ref(k + 1, k), &c__1, &a_ref(k + 1, k 
			- 1), &c__1);
		q__1.r = a[i__2].r - q__2.r, q__1.i = a[i__2].i - q__2.i;
		a[i__1].r = q__1.r, a[i__1].i = q__1.i;
		i__1 = *n - k;
		ccopy_(&i__1, &a_ref(k + 1, k - 1), &c__1, &work[1], &c__1);
		i__1 = *n - k;
		q__1.r = -1.f, q__1.i = 0.f;
		csymv_(uplo, &i__1, &q__1, &a_ref(k + 1, k + 1), lda, &work[1]
			, &c__1, &c_b2, &a_ref(k + 1, k - 1), &c__1);
		i__1 = a_subscr(k - 1, k - 1);
		i__2 = a_subscr(k - 1, k - 1);
		i__3 = *n - k;
		cdotu_(&q__2, &i__3, &work[1], &c__1, &a_ref(k + 1, k - 1), &
			c__1);
		q__1.r = a[i__2].r - q__2.r, q__1.i = a[i__2].i - q__2.i;
		a[i__1].r = q__1.r, a[i__1].i = q__1.i;
	    }
	    kstep = 2;
	}

	kp = (i__1 = ipiv[k], abs(i__1));
	if (kp != k) {

/*           Interchange rows and columns K and KP in the trailing   
             submatrix A(k-1:n,k-1:n) */

	    if (kp < *n) {
		i__1 = *n - kp;
		cswap_(&i__1, &a_ref(kp + 1, k), &c__1, &a_ref(kp + 1, kp), &
			c__1);
	    }
	    i__1 = kp - k - 1;
	    cswap_(&i__1, &a_ref(k + 1, k), &c__1, &a_ref(kp, k + 1), lda);
	    i__1 = a_subscr(k, k);
	    temp.r = a[i__1].r, temp.i = a[i__1].i;
	    i__1 = a_subscr(k, k);
	    i__2 = a_subscr(kp, kp);
	    a[i__1].r = a[i__2].r, a[i__1].i = a[i__2].i;
	    i__1 = a_subscr(kp, kp);
	    a[i__1].r = temp.r, a[i__1].i = temp.i;
	    if (kstep == 2) {
		i__1 = a_subscr(k, k - 1);
		temp.r = a[i__1].r, temp.i = a[i__1].i;
		i__1 = a_subscr(k, k - 1);
		i__2 = a_subscr(kp, k - 1);
		a[i__1].r = a[i__2].r, a[i__1].i = a[i__2].i;
		i__1 = a_subscr(kp, k - 1);
		a[i__1].r = temp.r, a[i__1].i = temp.i;
	    }
	}

	k -= kstep;
	goto L50;
L60:
	;
    }

    return 0;

/*     End of CSYTRI */

} /* csytri_ */

#undef a_ref
#undef a_subscr


