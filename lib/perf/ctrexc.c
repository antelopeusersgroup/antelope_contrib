#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int ctrexc_(char *compq, integer *n, complex *t, integer *
	ldt, complex *q, integer *ldq, integer *ifst, integer *ilst, integer *
	info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    CTREXC reorders the Schur factorization of a complex matrix   
    A = Q*T*Q**H, so that the diagonal element of T with row index IFST   
    is moved to row ILST.   

    The Schur form T is reordered by a unitary similarity transformation   
    Z**H*T*Z, and optionally the matrix Q of Schur vectors is updated by   
    postmultplying it with Z.   

    Arguments   
    =========   

    COMPQ   (input) CHARACTER*1   
            = 'V':  update the matrix Q of Schur vectors;   
            = 'N':  do not update Q.   

    N       (input) INTEGER   
            The order of the matrix T. N >= 0.   

    T       (input/output) COMPLEX array, dimension (LDT,N)   
            On entry, the upper triangular matrix T.   
            On exit, the reordered upper triangular matrix.   

    LDT     (input) INTEGER   
            The leading dimension of the array T. LDT >= max(1,N).   

    Q       (input/output) COMPLEX array, dimension (LDQ,N)   
            On entry, if COMPQ = 'V', the matrix Q of Schur vectors.   
            On exit, if COMPQ = 'V', Q has been postmultiplied by the   
            unitary transformation matrix Z which reorders T.   
            If COMPQ = 'N', Q is not referenced.   

    LDQ     (input) INTEGER   
            The leading dimension of the array Q.  LDQ >= max(1,N).   

    IFST    (input) INTEGER   
    ILST    (input) INTEGER   
            Specify the reordering of the diagonal elements of T:   
            The element with row index IFST is moved to row ILST by a   
            sequence of transpositions between adjacent elements.   
            1 <= IFST <= N; 1 <= ILST <= N.   

    INFO    (output) INTEGER   
            = 0:  successful exit   
            < 0:  if INFO = -i, the i-th argument had an illegal value   

    =====================================================================   


       Decode and test the input parameters.   

       Parameter adjustments */
    /* Table of constant values */
    static integer c__1 = 1;
    
    /* System generated locals */
    integer q_dim1, q_offset, t_dim1, t_offset, i__1, i__2, i__3;
    complex q__1;
    /* Builtin functions */
    void r_cnjg(complex *, complex *);
    /* Local variables */
    static complex temp;
    extern /* Subroutine */ int crot_(integer *, complex *, integer *, 
	    complex *, integer *, real *, complex *);
    static integer k;
    extern logical lsame_(char *, char *);
    static logical wantq;
    static integer m1, m2, m3;
    static real cs;
    static complex t11, t22, sn;
    extern /* Subroutine */ int clartg_(complex *, complex *, real *, complex 
	    *, complex *), xerbla_(char *, integer *);
#define q_subscr(a_1,a_2) (a_2)*q_dim1 + a_1
#define q_ref(a_1,a_2) q[q_subscr(a_1,a_2)]
#define t_subscr(a_1,a_2) (a_2)*t_dim1 + a_1
#define t_ref(a_1,a_2) t[t_subscr(a_1,a_2)]


    t_dim1 = *ldt;
    t_offset = 1 + t_dim1 * 1;
    t -= t_offset;
    q_dim1 = *ldq;
    q_offset = 1 + q_dim1 * 1;
    q -= q_offset;

    /* Function Body */
    *info = 0;
    wantq = lsame_(compq, "V");
    if (! lsame_(compq, "N") && ! wantq) {
	*info = -1;
    } else if (*n < 0) {
	*info = -2;
    } else if (*ldt < max(1,*n)) {
	*info = -4;
    } else if (*ldq < 1 || wantq && *ldq < max(1,*n)) {
	*info = -6;
    } else if (*ifst < 1 || *ifst > *n) {
	*info = -7;
    } else if (*ilst < 1 || *ilst > *n) {
	*info = -8;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("CTREXC", &i__1);
	return 0;
    }

/*     Quick return if possible */

    if (*n == 1 || *ifst == *ilst) {
	return 0;
    }

    if (*ifst < *ilst) {

/*        Move the IFST-th diagonal element forward down the diagonal. */

	m1 = 0;
	m2 = -1;
	m3 = 1;
    } else {

/*        Move the IFST-th diagonal element backward up the diagonal. */

	m1 = -1;
	m2 = 0;
	m3 = -1;
    }

    i__1 = *ilst + m2;
    i__2 = m3;
    for (k = *ifst + m1; i__2 < 0 ? k >= i__1 : k <= i__1; k += i__2) {

/*        Interchange the k-th and (k+1)-th diagonal elements. */

	i__3 = t_subscr(k, k);
	t11.r = t[i__3].r, t11.i = t[i__3].i;
	i__3 = t_subscr(k + 1, k + 1);
	t22.r = t[i__3].r, t22.i = t[i__3].i;

/*        Determine the transformation to perform the interchange. */

	q__1.r = t22.r - t11.r, q__1.i = t22.i - t11.i;
	clartg_(&t_ref(k, k + 1), &q__1, &cs, &sn, &temp);

/*        Apply transformation to the matrix T. */

	if (k + 2 <= *n) {
	    i__3 = *n - k - 1;
	    crot_(&i__3, &t_ref(k, k + 2), ldt, &t_ref(k + 1, k + 2), ldt, &
		    cs, &sn);
	}
	i__3 = k - 1;
	r_cnjg(&q__1, &sn);
	crot_(&i__3, &t_ref(1, k), &c__1, &t_ref(1, k + 1), &c__1, &cs, &q__1)
		;

	i__3 = t_subscr(k, k);
	t[i__3].r = t22.r, t[i__3].i = t22.i;
	i__3 = t_subscr(k + 1, k + 1);
	t[i__3].r = t11.r, t[i__3].i = t11.i;

	if (wantq) {

/*           Accumulate transformation in the matrix Q. */

	    r_cnjg(&q__1, &sn);
	    crot_(n, &q_ref(1, k), &c__1, &q_ref(1, k + 1), &c__1, &cs, &q__1)
		    ;
	}

/* L10: */
    }

    return 0;

/*     End of CTREXC */

} /* ctrexc_ */

#undef t_ref
#undef t_subscr
#undef q_ref
#undef q_subscr


