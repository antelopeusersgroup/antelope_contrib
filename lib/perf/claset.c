#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int claset_(char *uplo, integer *m, integer *n, complex *
	alpha, complex *beta, complex *a, integer *lda)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       October 31, 1992   


    Purpose   
    =======   

    CLASET initializes a 2-D array A to BETA on the diagonal and   
    ALPHA on the offdiagonals.   

    Arguments   
    =========   

    UPLO    (input) CHARACTER*1   
            Specifies the part of the matrix A to be set.   
            = 'U':      Upper triangular part is set. The lower triangle   
                        is unchanged.   
            = 'L':      Lower triangular part is set. The upper triangle   
                        is unchanged.   
            Otherwise:  All of the matrix A is set.   

    M       (input) INTEGER   
            On entry, M specifies the number of rows of A.   

    N       (input) INTEGER   
            On entry, N specifies the number of columns of A.   

    ALPHA   (input) COMPLEX   
            All the offdiagonal array elements are set to ALPHA.   

    BETA    (input) COMPLEX   
            All the diagonal array elements are set to BETA.   

    A       (input/output) COMPLEX array, dimension (LDA,N)   
            On entry, the m by n matrix A.   
            On exit, A(i,j) = ALPHA, 1 <= i <= m, 1 <= j <= n, i.ne.j;   
                     A(i,i) = BETA , 1 <= i <= min(m,n)   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,M).   

    =====================================================================   


       Parameter adjustments */
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    /* Local variables */
    static integer i__, j;
    extern logical lsame_(char *, char *);
#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]

    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;

    /* Function Body */
    if (lsame_(uplo, "U")) {

/*        Set the diagonal to BETA and the strictly upper triangular   
          part of the array to ALPHA. */

	i__1 = *n;
	for (j = 2; j <= i__1; ++j) {
/* Computing MIN */
	    i__3 = j - 1;
	    i__2 = min(i__3,*m);
	    for (i__ = 1; i__ <= i__2; ++i__) {
		i__3 = a_subscr(i__, j);
		a[i__3].r = alpha->r, a[i__3].i = alpha->i;
/* L10: */
	    }
/* L20: */
	}
	i__1 = min(*n,*m);
	for (i__ = 1; i__ <= i__1; ++i__) {
	    i__2 = a_subscr(i__, i__);
	    a[i__2].r = beta->r, a[i__2].i = beta->i;
/* L30: */
	}

    } else if (lsame_(uplo, "L")) {

/*        Set the diagonal to BETA and the strictly lower triangular   
          part of the array to ALPHA. */

	i__1 = min(*m,*n);
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *m;
	    for (i__ = j + 1; i__ <= i__2; ++i__) {
		i__3 = a_subscr(i__, j);
		a[i__3].r = alpha->r, a[i__3].i = alpha->i;
/* L40: */
	    }
/* L50: */
	}
	i__1 = min(*n,*m);
	for (i__ = 1; i__ <= i__1; ++i__) {
	    i__2 = a_subscr(i__, i__);
	    a[i__2].r = beta->r, a[i__2].i = beta->i;
/* L60: */
	}

    } else {

/*        Set the array to BETA on the diagonal and ALPHA on the   
          offdiagonal. */

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *m;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		i__3 = a_subscr(i__, j);
		a[i__3].r = alpha->r, a[i__3].i = alpha->i;
/* L70: */
	    }
/* L80: */
	}
	i__1 = min(*m,*n);
	for (i__ = 1; i__ <= i__1; ++i__) {
	    i__2 = a_subscr(i__, i__);
	    a[i__2].r = beta->r, a[i__2].i = beta->i;
/* L90: */
	}
    }

    return 0;

/*     End of CLASET */

} /* claset_ */

#undef a_ref
#undef a_subscr


