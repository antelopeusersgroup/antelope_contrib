#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int cgelq2_(integer *m, integer *n, complex *a, integer *lda,
	 complex *tau, complex *work, integer *info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    CGELQ2 computes an LQ factorization of a complex m by n matrix A:   
    A = L * Q.   

    Arguments   
    =========   

    M       (input) INTEGER   
            The number of rows of the matrix A.  M >= 0.   

    N       (input) INTEGER   
            The number of columns of the matrix A.  N >= 0.   

    A       (input/output) COMPLEX array, dimension (LDA,N)   
            On entry, the m by n matrix A.   
            On exit, the elements on and below the diagonal of the array   
            contain the m by min(m,n) lower trapezoidal matrix L (L is   
            lower triangular if m <= n); the elements above the diagonal,   
            with the array TAU, represent the unitary matrix Q as a   
            product of elementary reflectors (see Further Details).   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,M).   

    TAU     (output) COMPLEX array, dimension (min(M,N))   
            The scalar factors of the elementary reflectors (see Further   
            Details).   

    WORK    (workspace) COMPLEX array, dimension (M)   

    INFO    (output) INTEGER   
            = 0: successful exit   
            < 0: if INFO = -i, the i-th argument had an illegal value   

    Further Details   
    ===============   

    The matrix Q is represented as a product of elementary reflectors   

       Q = H(k)' . . . H(2)' H(1)', where k = min(m,n).   

    Each H(i) has the form   

       H(i) = I - tau * v * v'   

    where tau is a complex scalar, and v is a complex vector with   
    v(1:i-1) = 0 and v(i) = 1; conjg(v(i+1:n)) is stored on exit in   
    A(i,i+1:n), and tau in TAU(i).   

    =====================================================================   


       Test the input arguments   

       Parameter adjustments */
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    /* Local variables */
    static integer i__, k;
    static complex alpha;
    extern /* Subroutine */ int clarf_(char *, integer *, integer *, complex *
	    , integer *, complex *, complex *, integer *, complex *), 
	    clarfg_(integer *, complex *, complex *, integer *, complex *), 
	    clacgv_(integer *, complex *, integer *), xerbla_(char *, integer 
	    *);
#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]

    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --tau;
    --work;

    /* Function Body */
    *info = 0;
    if (*m < 0) {
	*info = -1;
    } else if (*n < 0) {
	*info = -2;
    } else if (*lda < max(1,*m)) {
	*info = -4;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("CGELQ2", &i__1);
	return 0;
    }

    k = min(*m,*n);

    i__1 = k;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Generate elementary reflector H(i) to annihilate A(i,i+1:n) */

	i__2 = *n - i__ + 1;
	clacgv_(&i__2, &a_ref(i__, i__), lda);
	i__2 = a_subscr(i__, i__);
	alpha.r = a[i__2].r, alpha.i = a[i__2].i;
/* Computing MIN */
	i__2 = i__ + 1;
	i__3 = *n - i__ + 1;
	clarfg_(&i__3, &alpha, &a_ref(i__, min(i__2,*n)), lda, &tau[i__]);
	if (i__ < *m) {

/*           Apply H(i) to A(i+1:m,i:n) from the right */

	    i__2 = a_subscr(i__, i__);
	    a[i__2].r = 1.f, a[i__2].i = 0.f;
	    i__2 = *m - i__;
	    i__3 = *n - i__ + 1;
	    clarf_("Right", &i__2, &i__3, &a_ref(i__, i__), lda, &tau[i__], &
		    a_ref(i__ + 1, i__), lda, &work[1]);
	}
	i__2 = a_subscr(i__, i__);
	a[i__2].r = alpha.r, a[i__2].i = alpha.i;
	i__2 = *n - i__ + 1;
	clacgv_(&i__2, &a_ref(i__, i__), lda);
/* L10: */
    }
    return 0;

/*     End of CGELQ2 */

} /* cgelq2_ */

#undef a_ref
#undef a_subscr


