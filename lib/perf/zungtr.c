#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int zungtr_(char *uplo, integer *n, doublecomplex *a, 
	integer *lda, doublecomplex *tau, doublecomplex *work, integer *lwork,
	 integer *info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    ZUNGTR generates a complex unitary matrix Q which is defined as the   
    product of n-1 elementary reflectors of order N, as returned by   
    ZHETRD:   

    if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),   

    if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).   

    Arguments   
    =========   

    UPLO    (input) CHARACTER*1   
            = 'U': Upper triangle of A contains elementary reflectors   
                   from ZHETRD;   
            = 'L': Lower triangle of A contains elementary reflectors   
                   from ZHETRD.   

    N       (input) INTEGER   
            The order of the matrix Q. N >= 0.   

    A       (input/output) COMPLEX*16 array, dimension (LDA,N)   
            On entry, the vectors which define the elementary reflectors,   
            as returned by ZHETRD.   
            On exit, the N-by-N unitary matrix Q.   

    LDA     (input) INTEGER   
            The leading dimension of the array A. LDA >= N.   

    TAU     (input) COMPLEX*16 array, dimension (N-1)   
            TAU(i) must contain the scalar factor of the elementary   
            reflector H(i), as returned by ZHETRD.   

    WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)   
            On exit, if INFO = 0, WORK(1) returns the optimal LWORK.   

    LWORK   (input) INTEGER   
            The dimension of the array WORK. LWORK >= N-1.   
            For optimum performance LWORK >= (N-1)*NB, where NB is   
            the optimal blocksize.   

            If LWORK = -1, then a workspace query is assumed; the routine   
            only calculates the optimal size of the WORK array, returns   
            this value as the first entry of the WORK array, and no error   
            message related to LWORK is issued by XERBLA.   

    INFO    (output) INTEGER   
            = 0:  successful exit   
            < 0:  if INFO = -i, the i-th argument had an illegal value   

    =====================================================================   


       Test the input arguments   

       Parameter adjustments */
    /* Table of constant values */
    static integer c__1 = 1;
    static integer c_n1 = -1;
    
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;
    /* Local variables */
    static integer i__, j;
    extern logical lsame_(char *, char *);
    static integer iinfo;
    static logical upper;
    static integer nb;
    extern /* Subroutine */ int xerbla_(char *, integer *);
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *, 
	    integer *, integer *, ftnlen, ftnlen);
    static integer lwkopt;
    static logical lquery;
    extern /* Subroutine */ int zungql_(integer *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    integer *, integer *), zungqr_(integer *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    integer *, integer *);
#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]


    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --tau;
    --work;

    /* Function Body */
    *info = 0;
    lquery = *lwork == -1;
    upper = lsame_(uplo, "U");
    if (! upper && ! lsame_(uplo, "L")) {
	*info = -1;
    } else if (*n < 0) {
	*info = -2;
    } else if (*lda < max(1,*n)) {
	*info = -4;
    } else /* if(complicated condition) */ {
/* Computing MAX */
	i__1 = 1, i__2 = *n - 1;
	if (*lwork < max(i__1,i__2) && ! lquery) {
	    *info = -7;
	}
    }

    if (*info == 0) {
	if (upper) {
	    i__1 = *n - 1;
	    i__2 = *n - 1;
	    i__3 = *n - 1;
	    nb = ilaenv_(&c__1, "ZUNGQL", " ", &i__1, &i__2, &i__3, &c_n1, (
		    ftnlen)6, (ftnlen)1);
	} else {
	    i__1 = *n - 1;
	    i__2 = *n - 1;
	    i__3 = *n - 1;
	    nb = ilaenv_(&c__1, "ZUNGQR", " ", &i__1, &i__2, &i__3, &c_n1, (
		    ftnlen)6, (ftnlen)1);
	}
/* Computing MAX */
	i__1 = 1, i__2 = *n - 1;
	lwkopt = max(i__1,i__2) * nb;
	work[1].r = (doublereal) lwkopt, work[1].i = 0.;
    }

    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("ZUNGTR", &i__1);
	return 0;
    } else if (lquery) {
	return 0;
    }

/*     Quick return if possible */

    if (*n == 0) {
	work[1].r = 1., work[1].i = 0.;
	return 0;
    }

    if (upper) {

/*        Q was determined by a call to ZHETRD with UPLO = 'U'   

          Shift the vectors which define the elementary reflectors one   
          column to the left, and set the last row and column of Q to   
          those of the unit matrix */

	i__1 = *n - 1;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = j - 1;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		i__3 = a_subscr(i__, j);
		i__4 = a_subscr(i__, j + 1);
		a[i__3].r = a[i__4].r, a[i__3].i = a[i__4].i;
/* L10: */
	    }
	    i__2 = a_subscr(*n, j);
	    a[i__2].r = 0., a[i__2].i = 0.;
/* L20: */
	}
	i__1 = *n - 1;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    i__2 = a_subscr(i__, *n);
	    a[i__2].r = 0., a[i__2].i = 0.;
/* L30: */
	}
	i__1 = a_subscr(*n, *n);
	a[i__1].r = 1., a[i__1].i = 0.;

/*        Generate Q(1:n-1,1:n-1) */

	i__1 = *n - 1;
	i__2 = *n - 1;
	i__3 = *n - 1;
	zungql_(&i__1, &i__2, &i__3, &a[a_offset], lda, &tau[1], &work[1], 
		lwork, &iinfo);

    } else {

/*        Q was determined by a call to ZHETRD with UPLO = 'L'.   

          Shift the vectors which define the elementary reflectors one   
          column to the right, and set the first row and column of Q to   
          those of the unit matrix */

	for (j = *n; j >= 2; --j) {
	    i__1 = a_subscr(1, j);
	    a[i__1].r = 0., a[i__1].i = 0.;
	    i__1 = *n;
	    for (i__ = j + 1; i__ <= i__1; ++i__) {
		i__2 = a_subscr(i__, j);
		i__3 = a_subscr(i__, j - 1);
		a[i__2].r = a[i__3].r, a[i__2].i = a[i__3].i;
/* L40: */
	    }
/* L50: */
	}
	i__1 = a_subscr(1, 1);
	a[i__1].r = 1., a[i__1].i = 0.;
	i__1 = *n;
	for (i__ = 2; i__ <= i__1; ++i__) {
	    i__2 = a_subscr(i__, 1);
	    a[i__2].r = 0., a[i__2].i = 0.;
/* L60: */
	}
	if (*n > 1) {

/*           Generate Q(2:n,2:n) */

	    i__1 = *n - 1;
	    i__2 = *n - 1;
	    i__3 = *n - 1;
	    zungqr_(&i__1, &i__2, &i__3, &a_ref(2, 2), lda, &tau[1], &work[1],
		     lwork, &iinfo);
	}
    }
    work[1].r = (doublereal) lwkopt, work[1].i = 0.;
    return 0;

/*     End of ZUNGTR */

} /* zungtr_ */

#undef a_ref
#undef a_subscr


