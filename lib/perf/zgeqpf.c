#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int zgeqpf_(integer *m, integer *n, doublecomplex *a, 
	integer *lda, integer *jpvt, doublecomplex *tau, doublecomplex *work, 
	doublereal *rwork, integer *info)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    This routine is deprecated and has been replaced by routine ZGEQP3.   

    ZGEQPF computes a QR factorization with column pivoting of a   
    complex M-by-N matrix A: A*P = Q*R.   

    Arguments   
    =========   

    M       (input) INTEGER   
            The number of rows of the matrix A. M >= 0.   

    N       (input) INTEGER   
            The number of columns of the matrix A. N >= 0   

    A       (input/output) COMPLEX*16 array, dimension (LDA,N)   
            On entry, the M-by-N matrix A.   
            On exit, the upper triangle of the array contains the   
            min(M,N)-by-N upper triangular matrix R; the elements   
            below the diagonal, together with the array TAU,   
            represent the unitary matrix Q as a product of   
            min(m,n) elementary reflectors.   

    LDA     (input) INTEGER   
            The leading dimension of the array A. LDA >= max(1,M).   

    JPVT    (input/output) INTEGER array, dimension (N)   
            On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted   
            to the front of A*P (a leading column); if JPVT(i) = 0,   
            the i-th column of A is a free column.   
            On exit, if JPVT(i) = k, then the i-th column of A*P   
            was the k-th column of A.   

    TAU     (output) COMPLEX*16 array, dimension (min(M,N))   
            The scalar factors of the elementary reflectors.   

    WORK    (workspace) COMPLEX*16 array, dimension (N)   

    RWORK   (workspace) DOUBLE PRECISION array, dimension (2*N)   

    INFO    (output) INTEGER   
            = 0:  successful exit   
            < 0:  if INFO = -i, the i-th argument had an illegal value   

    Further Details   
    ===============   

    The matrix Q is represented as a product of elementary reflectors   

       Q = H(1) H(2) . . . H(n)   

    Each H(i) has the form   

       H = I - tau * v * v'   

    where tau is a complex scalar, and v is a complex vector with   
    v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i).   

    The matrix P is represented in jpvt as follows: If   
       jpvt(j) = i   
    then the jth column of P is the ith canonical unit vector.   

    =====================================================================   


       Test the input arguments   

       Parameter adjustments */
    /* Table of constant values */
    static integer c__1 = 1;
    
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1;
    doublecomplex z__1;
    /* Builtin functions */
    void d_cnjg(doublecomplex *, doublecomplex *);
    double z_abs(doublecomplex *), sqrt(doublereal);
    /* Local variables */
    static doublereal temp, temp2;
    static integer i__, j, itemp;
    extern /* Subroutine */ int zlarf_(char *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    integer *, doublecomplex *), zswap_(integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *), zgeqr2_(
	    integer *, integer *, doublecomplex *, integer *, doublecomplex *,
	     doublecomplex *, integer *);
    extern doublereal dznrm2_(integer *, doublecomplex *, integer *);
    static integer ma, mn;
    extern /* Subroutine */ int zunm2r_(char *, char *, integer *, integer *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    extern integer idamax_(integer *, doublereal *, integer *);
    extern /* Subroutine */ int xerbla_(char *, integer *), zlarfg_(
	    integer *, doublecomplex *, doublecomplex *, integer *, 
	    doublecomplex *);
    static doublecomplex aii;
    static integer pvt;
#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]


    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --jpvt;
    --tau;
    --work;
    --rwork;

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
	xerbla_("ZGEQPF", &i__1);
	return 0;
    }

    mn = min(*m,*n);

/*     Move initial columns up front */

    itemp = 1;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (jpvt[i__] != 0) {
	    if (i__ != itemp) {
		zswap_(m, &a_ref(1, i__), &c__1, &a_ref(1, itemp), &c__1);
		jpvt[i__] = jpvt[itemp];
		jpvt[itemp] = i__;
	    } else {
		jpvt[i__] = i__;
	    }
	    ++itemp;
	} else {
	    jpvt[i__] = i__;
	}
/* L10: */
    }
    --itemp;

/*     Compute the QR factorization and update remaining columns */

    if (itemp > 0) {
	ma = min(itemp,*m);
	zgeqr2_(m, &ma, &a[a_offset], lda, &tau[1], &work[1], info);
	if (ma < *n) {
	    i__1 = *n - ma;
	    zunm2r_("Left", "Conjugate transpose", m, &i__1, &ma, &a[a_offset]
		    , lda, &tau[1], &a_ref(1, ma + 1), lda, &work[1], info);
	}
    }

    if (itemp < mn) {

/*        Initialize partial column norms. The first n elements of   
          work store the exact column norms. */

	i__1 = *n;
	for (i__ = itemp + 1; i__ <= i__1; ++i__) {
	    i__2 = *m - itemp;
	    rwork[i__] = dznrm2_(&i__2, &a_ref(itemp + 1, i__), &c__1);
	    rwork[*n + i__] = rwork[i__];
/* L20: */
	}

/*        Compute factorization */

	i__1 = mn;
	for (i__ = itemp + 1; i__ <= i__1; ++i__) {

/*           Determine ith pivot column and swap if necessary */

	    i__2 = *n - i__ + 1;
	    pvt = i__ - 1 + idamax_(&i__2, &rwork[i__], &c__1);

	    if (pvt != i__) {
		zswap_(m, &a_ref(1, pvt), &c__1, &a_ref(1, i__), &c__1);
		itemp = jpvt[pvt];
		jpvt[pvt] = jpvt[i__];
		jpvt[i__] = itemp;
		rwork[pvt] = rwork[i__];
		rwork[*n + pvt] = rwork[*n + i__];
	    }

/*           Generate elementary reflector H(i) */

	    i__2 = a_subscr(i__, i__);
	    aii.r = a[i__2].r, aii.i = a[i__2].i;
/* Computing MIN */
	    i__2 = i__ + 1;
	    i__3 = *m - i__ + 1;
	    zlarfg_(&i__3, &aii, &a_ref(min(i__2,*m), i__), &c__1, &tau[i__]);
	    i__2 = a_subscr(i__, i__);
	    a[i__2].r = aii.r, a[i__2].i = aii.i;

	    if (i__ < *n) {

/*              Apply H(i) to A(i:m,i+1:n) from the left */

		i__2 = a_subscr(i__, i__);
		aii.r = a[i__2].r, aii.i = a[i__2].i;
		i__2 = a_subscr(i__, i__);
		a[i__2].r = 1., a[i__2].i = 0.;
		i__2 = *m - i__ + 1;
		i__3 = *n - i__;
		d_cnjg(&z__1, &tau[i__]);
		zlarf_("Left", &i__2, &i__3, &a_ref(i__, i__), &c__1, &z__1, &
			a_ref(i__, i__ + 1), lda, &work[1]);
		i__2 = a_subscr(i__, i__);
		a[i__2].r = aii.r, a[i__2].i = aii.i;
	    }

/*           Update partial column norms */

	    i__2 = *n;
	    for (j = i__ + 1; j <= i__2; ++j) {
		if (rwork[j] != 0.) {
/* Computing 2nd power */
		    d__1 = z_abs(&a_ref(i__, j)) / rwork[j];
		    temp = 1. - d__1 * d__1;
		    temp = max(temp,0.);
/* Computing 2nd power */
		    d__1 = rwork[j] / rwork[*n + j];
		    temp2 = temp * .05 * (d__1 * d__1) + 1.;
		    if (temp2 == 1.) {
			if (*m - i__ > 0) {
			    i__3 = *m - i__;
			    rwork[j] = dznrm2_(&i__3, &a_ref(i__ + 1, j), &
				    c__1);
			    rwork[*n + j] = rwork[j];
			} else {
			    rwork[j] = 0.;
			    rwork[*n + j] = 0.;
			}
		    } else {
			rwork[j] *= sqrt(temp);
		    }
		}
/* L30: */
	    }

/* L40: */
	}
    }
    return 0;

/*     End of ZGEQPF */

} /* zgeqpf_ */

#undef a_ref
#undef a_subscr


