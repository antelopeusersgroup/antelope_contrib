#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int zlapmt_(logical *forwrd, integer *m, integer *n, 
	doublecomplex *x, integer *ldx, integer *k)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    ZLAPMT rearranges the columns of the M by N matrix X as specified   
    by the permutation K(1),K(2),...,K(N) of the integers 1,...,N.   
    If FORWRD = .TRUE.,  forward permutation:   

         X(*,K(J)) is moved X(*,J) for J = 1,2,...,N.   

    If FORWRD = .FALSE., backward permutation:   

         X(*,J) is moved to X(*,K(J)) for J = 1,2,...,N.   

    Arguments   
    =========   

    FORWRD  (input) LOGICAL   
            = .TRUE., forward permutation   
            = .FALSE., backward permutation   

    M       (input) INTEGER   
            The number of rows of the matrix X. M >= 0.   

    N       (input) INTEGER   
            The number of columns of the matrix X. N >= 0.   

    X       (input/output) COMPLEX*16 array, dimension (LDX,N)   
            On entry, the M by N matrix X.   
            On exit, X contains the permuted matrix X.   

    LDX     (input) INTEGER   
            The leading dimension of the array X, LDX >= MAX(1,M).   

    K       (input) INTEGER array, dimension (N)   
            On entry, K contains the permutation vector.   

    =====================================================================   


       Parameter adjustments */
    /* System generated locals */
    integer x_dim1, x_offset, i__1, i__2, i__3, i__4;
    /* Local variables */
    static doublecomplex temp;
    static integer i__, j, ii, in;
#define x_subscr(a_1,a_2) (a_2)*x_dim1 + a_1
#define x_ref(a_1,a_2) x[x_subscr(a_1,a_2)]

    x_dim1 = *ldx;
    x_offset = 1 + x_dim1 * 1;
    x -= x_offset;
    --k;

    /* Function Body */
    if (*n <= 1) {
	return 0;
    }

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	k[i__] = -k[i__];
/* L10: */
    }

    if (*forwrd) {

/*        Forward permutation */

	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {

	    if (k[i__] > 0) {
		goto L40;
	    }

	    j = i__;
	    k[j] = -k[j];
	    in = k[j];

L20:
	    if (k[in] > 0) {
		goto L40;
	    }

	    i__2 = *m;
	    for (ii = 1; ii <= i__2; ++ii) {
		i__3 = x_subscr(ii, j);
		temp.r = x[i__3].r, temp.i = x[i__3].i;
		i__3 = x_subscr(ii, j);
		i__4 = x_subscr(ii, in);
		x[i__3].r = x[i__4].r, x[i__3].i = x[i__4].i;
		i__3 = x_subscr(ii, in);
		x[i__3].r = temp.r, x[i__3].i = temp.i;
/* L30: */
	    }

	    k[in] = -k[in];
	    j = in;
	    in = k[in];
	    goto L20;

L40:

/* L50: */
	    ;
	}

    } else {

/*        Backward permutation */

	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {

	    if (k[i__] > 0) {
		goto L80;
	    }

	    k[i__] = -k[i__];
	    j = k[i__];
L60:
	    if (j == i__) {
		goto L80;
	    }

	    i__2 = *m;
	    for (ii = 1; ii <= i__2; ++ii) {
		i__3 = x_subscr(ii, i__);
		temp.r = x[i__3].r, temp.i = x[i__3].i;
		i__3 = x_subscr(ii, i__);
		i__4 = x_subscr(ii, j);
		x[i__3].r = x[i__4].r, x[i__3].i = x[i__4].i;
		i__3 = x_subscr(ii, j);
		x[i__3].r = temp.r, x[i__3].i = temp.i;
/* L70: */
	    }

	    k[j] = -k[j];
	    j = k[j];
	    goto L60;

L80:

/* L90: */
	    ;
	}

    }

    return 0;

/*     End of ZLAPMT */

} /* zlapmt_ */

#undef x_ref
#undef x_subscr


