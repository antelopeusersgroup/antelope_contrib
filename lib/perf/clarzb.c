#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int clarzb_(char *side, char *trans, char *direct, char *
	storev, integer *m, integer *n, integer *k, integer *l, complex *v, 
	integer *ldv, complex *t, integer *ldt, complex *c__, integer *ldc, 
	complex *work, integer *ldwork)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       December 1, 1999   


    Purpose   
    =======   

    CLARZB applies a complex block reflector H or its transpose H**H   
    to a complex distributed M-by-N  C from the left or the right.   

    Currently, only STOREV = 'R' and DIRECT = 'B' are supported.   

    Arguments   
    =========   

    SIDE    (input) CHARACTER*1   
            = 'L': apply H or H' from the Left   
            = 'R': apply H or H' from the Right   

    TRANS   (input) CHARACTER*1   
            = 'N': apply H (No transpose)   
            = 'C': apply H' (Conjugate transpose)   

    DIRECT  (input) CHARACTER*1   
            Indicates how H is formed from a product of elementary   
            reflectors   
            = 'F': H = H(1) H(2) . . . H(k) (Forward, not supported yet)   
            = 'B': H = H(k) . . . H(2) H(1) (Backward)   

    STOREV  (input) CHARACTER*1   
            Indicates how the vectors which define the elementary   
            reflectors are stored:   
            = 'C': Columnwise                        (not supported yet)   
            = 'R': Rowwise   

    M       (input) INTEGER   
            The number of rows of the matrix C.   

    N       (input) INTEGER   
            The number of columns of the matrix C.   

    K       (input) INTEGER   
            The order of the matrix T (= the number of elementary   
            reflectors whose product defines the block reflector).   

    L       (input) INTEGER   
            The number of columns of the matrix V containing the   
            meaningful part of the Householder reflectors.   
            If SIDE = 'L', M >= L >= 0, if SIDE = 'R', N >= L >= 0.   

    V       (input) COMPLEX array, dimension (LDV,NV).   
            If STOREV = 'C', NV = K; if STOREV = 'R', NV = L.   

    LDV     (input) INTEGER   
            The leading dimension of the array V.   
            If STOREV = 'C', LDV >= L; if STOREV = 'R', LDV >= K.   

    T       (input) COMPLEX array, dimension (LDT,K)   
            The triangular K-by-K matrix T in the representation of the   
            block reflector.   

    LDT     (input) INTEGER   
            The leading dimension of the array T. LDT >= K.   

    C       (input/output) COMPLEX array, dimension (LDC,N)   
            On entry, the M-by-N matrix C.   
            On exit, C is overwritten by H*C or H'*C or C*H or C*H'.   

    LDC     (input) INTEGER   
            The leading dimension of the array C. LDC >= max(1,M).   

    WORK    (workspace) COMPLEX array, dimension (LDWORK,K)   

    LDWORK  (input) INTEGER   
            The leading dimension of the array WORK.   
            If SIDE = 'L', LDWORK >= max(1,N);   
            if SIDE = 'R', LDWORK >= max(1,M).   

    Further Details   
    ===============   

    Based on contributions by   
      A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA   

    =====================================================================   


       Quick return if possible   

       Parameter adjustments */
    /* Table of constant values */
    static complex c_b1 = {1.f,0.f};
    static integer c__1 = 1;
    
    /* System generated locals */
    integer c_dim1, c_offset, t_dim1, t_offset, v_dim1, v_offset, work_dim1, 
	    work_offset, i__1, i__2, i__3, i__4, i__5;
    complex q__1;
    /* Local variables */
    static integer info, i__, j;
    extern /* Subroutine */ int cgemm_(char *, char *, integer *, integer *, 
	    integer *, complex *, complex *, integer *, complex *, integer *, 
	    complex *, complex *, integer *);
    extern logical lsame_(char *, char *);
    extern /* Subroutine */ int ccopy_(integer *, complex *, integer *, 
	    complex *, integer *), ctrmm_(char *, char *, char *, char *, 
	    integer *, integer *, complex *, complex *, integer *, complex *, 
	    integer *), clacgv_(integer *, 
	    complex *, integer *), xerbla_(char *, integer *);
    static char transt[1];
#define work_subscr(a_1,a_2) (a_2)*work_dim1 + a_1
#define work_ref(a_1,a_2) work[work_subscr(a_1,a_2)]
#define c___subscr(a_1,a_2) (a_2)*c_dim1 + a_1
#define c___ref(a_1,a_2) c__[c___subscr(a_1,a_2)]
#define t_subscr(a_1,a_2) (a_2)*t_dim1 + a_1
#define t_ref(a_1,a_2) t[t_subscr(a_1,a_2)]
#define v_subscr(a_1,a_2) (a_2)*v_dim1 + a_1
#define v_ref(a_1,a_2) v[v_subscr(a_1,a_2)]


    v_dim1 = *ldv;
    v_offset = 1 + v_dim1 * 1;
    v -= v_offset;
    t_dim1 = *ldt;
    t_offset = 1 + t_dim1 * 1;
    t -= t_offset;
    c_dim1 = *ldc;
    c_offset = 1 + c_dim1 * 1;
    c__ -= c_offset;
    work_dim1 = *ldwork;
    work_offset = 1 + work_dim1 * 1;
    work -= work_offset;

    /* Function Body */
    if (*m <= 0 || *n <= 0) {
	return 0;
    }

/*     Check for currently supported options */

    info = 0;
    if (! lsame_(direct, "B")) {
	info = -3;
    } else if (! lsame_(storev, "R")) {
	info = -4;
    }
    if (info != 0) {
	i__1 = -info;
	xerbla_("CLARZB", &i__1);
	return 0;
    }

    if (lsame_(trans, "N")) {
	*(unsigned char *)transt = 'C';
    } else {
	*(unsigned char *)transt = 'N';
    }

    if (lsame_(side, "L")) {

/*        Form  H * C  or  H' * C   

          W( 1:n, 1:k ) = conjg( C( 1:k, 1:n )' ) */

	i__1 = *k;
	for (j = 1; j <= i__1; ++j) {
	    ccopy_(n, &c___ref(j, 1), ldc, &work_ref(1, j), &c__1);
/* L10: */
	}

/*        W( 1:n, 1:k ) = W( 1:n, 1:k ) + ...   
                          conjg( C( m-l+1:m, 1:n )' ) * V( 1:k, 1:l )' */

	if (*l > 0) {
	    cgemm_("Transpose", "Conjugate transpose", n, k, l, &c_b1, &
		    c___ref(*m - *l + 1, 1), ldc, &v[v_offset], ldv, &c_b1, &
		    work[work_offset], ldwork);
	}

/*        W( 1:n, 1:k ) = W( 1:n, 1:k ) * T'  or  W( 1:m, 1:k ) * T */

	ctrmm_("Right", "Lower", transt, "Non-unit", n, k, &c_b1, &t[t_offset]
		, ldt, &work[work_offset], ldwork);

/*        C( 1:k, 1:n ) = C( 1:k, 1:n ) - conjg( W( 1:n, 1:k )' ) */

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *k;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		i__3 = c___subscr(i__, j);
		i__4 = c___subscr(i__, j);
		i__5 = work_subscr(j, i__);
		q__1.r = c__[i__4].r - work[i__5].r, q__1.i = c__[i__4].i - 
			work[i__5].i;
		c__[i__3].r = q__1.r, c__[i__3].i = q__1.i;
/* L20: */
	    }
/* L30: */
	}

/*        C( m-l+1:m, 1:n ) = C( m-l+1:m, 1:n ) - ...   
                      conjg( V( 1:k, 1:l )' ) * conjg( W( 1:n, 1:k )' ) */

	if (*l > 0) {
	    q__1.r = -1.f, q__1.i = 0.f;
	    cgemm_("Transpose", "Transpose", l, n, k, &q__1, &v[v_offset], 
		    ldv, &work[work_offset], ldwork, &c_b1, &c___ref(*m - *l 
		    + 1, 1), ldc);
	}

    } else if (lsame_(side, "R")) {

/*        Form  C * H  or  C * H'   

          W( 1:m, 1:k ) = C( 1:m, 1:k ) */

	i__1 = *k;
	for (j = 1; j <= i__1; ++j) {
	    ccopy_(m, &c___ref(1, j), &c__1, &work_ref(1, j), &c__1);
/* L40: */
	}

/*        W( 1:m, 1:k ) = W( 1:m, 1:k ) + ...   
                          C( 1:m, n-l+1:n ) * conjg( V( 1:k, 1:l )' ) */

	if (*l > 0) {
	    cgemm_("No transpose", "Transpose", m, k, l, &c_b1, &c___ref(1, *
		    n - *l + 1), ldc, &v[v_offset], ldv, &c_b1, &work[
		    work_offset], ldwork);
	}

/*        W( 1:m, 1:k ) = W( 1:m, 1:k ) * conjg( T )  or   
                          W( 1:m, 1:k ) * conjg( T' ) */

	i__1 = *k;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *k - j + 1;
	    clacgv_(&i__2, &t_ref(j, j), &c__1);
/* L50: */
	}
	ctrmm_("Right", "Lower", trans, "Non-unit", m, k, &c_b1, &t[t_offset],
		 ldt, &work[work_offset], ldwork);
	i__1 = *k;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *k - j + 1;
	    clacgv_(&i__2, &t_ref(j, j), &c__1);
/* L60: */
	}

/*        C( 1:m, 1:k ) = C( 1:m, 1:k ) - W( 1:m, 1:k ) */

	i__1 = *k;
	for (j = 1; j <= i__1; ++j) {
	    i__2 = *m;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		i__3 = c___subscr(i__, j);
		i__4 = c___subscr(i__, j);
		i__5 = work_subscr(i__, j);
		q__1.r = c__[i__4].r - work[i__5].r, q__1.i = c__[i__4].i - 
			work[i__5].i;
		c__[i__3].r = q__1.r, c__[i__3].i = q__1.i;
/* L70: */
	    }
/* L80: */
	}

/*        C( 1:m, n-l+1:n ) = C( 1:m, n-l+1:n ) - ...   
                              W( 1:m, 1:k ) * conjg( V( 1:k, 1:l ) ) */

	i__1 = *l;
	for (j = 1; j <= i__1; ++j) {
	    clacgv_(k, &v_ref(1, j), &c__1);
/* L90: */
	}
	if (*l > 0) {
	    q__1.r = -1.f, q__1.i = 0.f;
	    cgemm_("No transpose", "No transpose", m, l, k, &q__1, &work[
		    work_offset], ldwork, &v[v_offset], ldv, &c_b1, &c___ref(
		    1, *n - *l + 1), ldc);
	}
	i__1 = *l;
	for (j = 1; j <= i__1; ++j) {
	    clacgv_(k, &v_ref(1, j), &c__1);
/* L100: */
	}

    }

    return 0;

/*     End of CLARZB */

} /* clarzb_ */

#undef v_ref
#undef v_subscr
#undef t_ref
#undef t_subscr
#undef c___ref
#undef c___subscr
#undef work_ref
#undef work_subscr


