#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int ctrevc_(char *side, char *howmny, logical *select, 
	integer *n, complex *t, integer *ldt, complex *vl, integer *ldvl, 
	complex *vr, integer *ldvr, integer *mm, integer *m, complex *work, 
	real *rwork, integer *info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    CTREVC computes some or all of the right and/or left eigenvectors of   
    a complex upper triangular matrix T.   

    The right eigenvector x and the left eigenvector y of T corresponding   
    to an eigenvalue w are defined by:   

                 T*x = w*x,     y'*T = w*y'   

    where y' denotes the conjugate transpose of the vector y.   

    If all eigenvectors are requested, the routine may either return the   
    matrices X and/or Y of right or left eigenvectors of T, or the   
    products Q*X and/or Q*Y, where Q is an input unitary   
    matrix. If T was obtained from the Schur factorization of an   
    original matrix A = Q*T*Q', then Q*X and Q*Y are the matrices of   
    right or left eigenvectors of A.   

    Arguments   
    =========   

    SIDE    (input) CHARACTER*1   
            = 'R':  compute right eigenvectors only;   
            = 'L':  compute left eigenvectors only;   
            = 'B':  compute both right and left eigenvectors.   

    HOWMNY  (input) CHARACTER*1   
            = 'A':  compute all right and/or left eigenvectors;   
            = 'B':  compute all right and/or left eigenvectors,   
                    and backtransform them using the input matrices   
                    supplied in VR and/or VL;   
            = 'S':  compute selected right and/or left eigenvectors,   
                    specified by the logical array SELECT.   

    SELECT  (input) LOGICAL array, dimension (N)   
            If HOWMNY = 'S', SELECT specifies the eigenvectors to be   
            computed.   
            If HOWMNY = 'A' or 'B', SELECT is not referenced.   
            To select the eigenvector corresponding to the j-th   
            eigenvalue, SELECT(j) must be set to .TRUE..   

    N       (input) INTEGER   
            The order of the matrix T. N >= 0.   

    T       (input/output) COMPLEX array, dimension (LDT,N)   
            The upper triangular matrix T.  T is modified, but restored   
            on exit.   

    LDT     (input) INTEGER   
            The leading dimension of the array T. LDT >= max(1,N).   

    VL      (input/output) COMPLEX array, dimension (LDVL,MM)   
            On entry, if SIDE = 'L' or 'B' and HOWMNY = 'B', VL must   
            contain an N-by-N matrix Q (usually the unitary matrix Q of   
            Schur vectors returned by CHSEQR).   
            On exit, if SIDE = 'L' or 'B', VL contains:   
            if HOWMNY = 'A', the matrix Y of left eigenvectors of T;   
                             VL is lower triangular. The i-th column   
                             VL(i) of VL is the eigenvector corresponding   
                             to T(i,i).   
            if HOWMNY = 'B', the matrix Q*Y;   
            if HOWMNY = 'S', the left eigenvectors of T specified by   
                             SELECT, stored consecutively in the columns   
                             of VL, in the same order as their   
                             eigenvalues.   
            If SIDE = 'R', VL is not referenced.   

    LDVL    (input) INTEGER   
            The leading dimension of the array VL.  LDVL >= max(1,N) if   
            SIDE = 'L' or 'B'; LDVL >= 1 otherwise.   

    VR      (input/output) COMPLEX array, dimension (LDVR,MM)   
            On entry, if SIDE = 'R' or 'B' and HOWMNY = 'B', VR must   
            contain an N-by-N matrix Q (usually the unitary matrix Q of   
            Schur vectors returned by CHSEQR).   
            On exit, if SIDE = 'R' or 'B', VR contains:   
            if HOWMNY = 'A', the matrix X of right eigenvectors of T;   
                             VR is upper triangular. The i-th column   
                             VR(i) of VR is the eigenvector corresponding   
                             to T(i,i).   
            if HOWMNY = 'B', the matrix Q*X;   
            if HOWMNY = 'S', the right eigenvectors of T specified by   
                             SELECT, stored consecutively in the columns   
                             of VR, in the same order as their   
                             eigenvalues.   
            If SIDE = 'L', VR is not referenced.   

    LDVR    (input) INTEGER   
            The leading dimension of the array VR.  LDVR >= max(1,N) if   
             SIDE = 'R' or 'B'; LDVR >= 1 otherwise.   

    MM      (input) INTEGER   
            The number of columns in the arrays VL and/or VR. MM >= M.   

    M       (output) INTEGER   
            The number of columns in the arrays VL and/or VR actually   
            used to store the eigenvectors.  If HOWMNY = 'A' or 'B', M   
            is set to N.  Each selected eigenvector occupies one   
            column.   

    WORK    (workspace) COMPLEX array, dimension (2*N)   

    RWORK   (workspace) REAL array, dimension (N)   

    INFO    (output) INTEGER   
            = 0:  successful exit   
            < 0:  if INFO = -i, the i-th argument had an illegal value   

    Further Details   
    ===============   

    The algorithm used in this program is basically backward (forward)   
    substitution, with scaling to make the the code robust against   
    possible overflow.   

    Each eigenvector is normalized so that the element of largest   
    magnitude has magnitude 1; here the magnitude of a complex number   
    (x,y) is taken to be |x| + |y|.   

    =====================================================================   


       Decode and test the input parameters   

       Parameter adjustments */
    /* Table of constant values */
    static complex c_b2 = {1.f,0.f};
    static integer c__1 = 1;
    
    /* System generated locals */
    integer t_dim1, t_offset, vl_dim1, vl_offset, vr_dim1, vr_offset, i__1, 
	    i__2, i__3, i__4, i__5;
    real r__1, r__2, r__3;
    complex q__1, q__2;
    /* Builtin functions */
    double r_imag(complex *);
    void r_cnjg(complex *, complex *);
    /* Local variables */
    static logical allv;
    static real unfl, ovfl, smin;
    static logical over;
    static integer i__, j, k;
    static real scale;
    extern logical lsame_(char *, char *);
    extern /* Subroutine */ int cgemv_(char *, integer *, integer *, complex *
	    , complex *, integer *, complex *, integer *, complex *, complex *
	    , integer *);
    static real remax;
    extern /* Subroutine */ int ccopy_(integer *, complex *, integer *, 
	    complex *, integer *);
    static logical leftv, bothv, somev;
    static integer ii, ki;
    extern /* Subroutine */ int slabad_(real *, real *);
    static integer is;
    extern integer icamax_(integer *, complex *, integer *);
    extern doublereal slamch_(char *);
    extern /* Subroutine */ int csscal_(integer *, real *, complex *, integer 
	    *), xerbla_(char *, integer *), clatrs_(char *, char *, 
	    char *, char *, integer *, complex *, integer *, complex *, real *
	    , real *, integer *);
    extern doublereal scasum_(integer *, complex *, integer *);
    static logical rightv;
    static real smlnum, ulp;
#define t_subscr(a_1,a_2) (a_2)*t_dim1 + a_1
#define t_ref(a_1,a_2) t[t_subscr(a_1,a_2)]
#define vl_subscr(a_1,a_2) (a_2)*vl_dim1 + a_1
#define vl_ref(a_1,a_2) vl[vl_subscr(a_1,a_2)]
#define vr_subscr(a_1,a_2) (a_2)*vr_dim1 + a_1
#define vr_ref(a_1,a_2) vr[vr_subscr(a_1,a_2)]


    --select;
    t_dim1 = *ldt;
    t_offset = 1 + t_dim1 * 1;
    t -= t_offset;
    vl_dim1 = *ldvl;
    vl_offset = 1 + vl_dim1 * 1;
    vl -= vl_offset;
    vr_dim1 = *ldvr;
    vr_offset = 1 + vr_dim1 * 1;
    vr -= vr_offset;
    --work;
    --rwork;

    /* Function Body */
    bothv = lsame_(side, "B");
    rightv = lsame_(side, "R") || bothv;
    leftv = lsame_(side, "L") || bothv;

    allv = lsame_(howmny, "A");
    over = lsame_(howmny, "B");
    somev = lsame_(howmny, "S");

/*     Set M to the number of columns required to store the selected   
       eigenvectors. */

    if (somev) {
	*m = 0;
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    if (select[j]) {
		++(*m);
	    }
/* L10: */
	}
    } else {
	*m = *n;
    }

    *info = 0;
    if (! rightv && ! leftv) {
	*info = -1;
    } else if (! allv && ! over && ! somev) {
	*info = -2;
    } else if (*n < 0) {
	*info = -4;
    } else if (*ldt < max(1,*n)) {
	*info = -6;
    } else if (*ldvl < 1 || leftv && *ldvl < *n) {
	*info = -8;
    } else if (*ldvr < 1 || rightv && *ldvr < *n) {
	*info = -10;
    } else if (*mm < *m) {
	*info = -11;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("CTREVC", &i__1);
	return 0;
    }

/*     Quick return if possible. */

    if (*n == 0) {
	return 0;
    }

/*     Set the constants to control overflow. */

    unfl = slamch_("Safe minimum");
    ovfl = 1.f / unfl;
    slabad_(&unfl, &ovfl);
    ulp = slamch_("Precision");
    smlnum = unfl * (*n / ulp);

/*     Store the diagonal elements of T in working array WORK. */

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = i__ + *n;
	i__3 = t_subscr(i__, i__);
	work[i__2].r = t[i__3].r, work[i__2].i = t[i__3].i;
/* L20: */
    }

/*     Compute 1-norm of each column of strictly upper triangular   
       part of T to control overflow in triangular solver. */

    rwork[1] = 0.f;
    i__1 = *n;
    for (j = 2; j <= i__1; ++j) {
	i__2 = j - 1;
	rwork[j] = scasum_(&i__2, &t_ref(1, j), &c__1);
/* L30: */
    }

    if (rightv) {

/*        Compute right eigenvectors. */

	is = *m;
	for (ki = *n; ki >= 1; --ki) {

	    if (somev) {
		if (! select[ki]) {
		    goto L80;
		}
	    }
/* Computing MAX */
	    i__1 = t_subscr(ki, ki);
	    r__3 = ulp * ((r__1 = t[i__1].r, dabs(r__1)) + (r__2 = r_imag(&
		    t_ref(ki, ki)), dabs(r__2)));
	    smin = dmax(r__3,smlnum);

	    work[1].r = 1.f, work[1].i = 0.f;

/*           Form right-hand side. */

	    i__1 = ki - 1;
	    for (k = 1; k <= i__1; ++k) {
		i__2 = k;
		i__3 = t_subscr(k, ki);
		q__1.r = -t[i__3].r, q__1.i = -t[i__3].i;
		work[i__2].r = q__1.r, work[i__2].i = q__1.i;
/* L40: */
	    }

/*           Solve the triangular system:   
                (T(1:KI-1,1:KI-1) - T(KI,KI))*X = SCALE*WORK. */

	    i__1 = ki - 1;
	    for (k = 1; k <= i__1; ++k) {
		i__2 = t_subscr(k, k);
		i__3 = t_subscr(k, k);
		i__4 = t_subscr(ki, ki);
		q__1.r = t[i__3].r - t[i__4].r, q__1.i = t[i__3].i - t[i__4]
			.i;
		t[i__2].r = q__1.r, t[i__2].i = q__1.i;
		i__2 = t_subscr(k, k);
		if ((r__1 = t[i__2].r, dabs(r__1)) + (r__2 = r_imag(&t_ref(k, 
			k)), dabs(r__2)) < smin) {
		    i__3 = t_subscr(k, k);
		    t[i__3].r = smin, t[i__3].i = 0.f;
		}
/* L50: */
	    }

	    if (ki > 1) {
		i__1 = ki - 1;
		clatrs_("Upper", "No transpose", "Non-unit", "Y", &i__1, &t[
			t_offset], ldt, &work[1], &scale, &rwork[1], info);
		i__1 = ki;
		work[i__1].r = scale, work[i__1].i = 0.f;
	    }

/*           Copy the vector x or Q*x to VR and normalize. */

	    if (! over) {
		ccopy_(&ki, &work[1], &c__1, &vr_ref(1, is), &c__1);

		ii = icamax_(&ki, &vr_ref(1, is), &c__1);
		i__1 = vr_subscr(ii, is);
		remax = 1.f / ((r__1 = vr[i__1].r, dabs(r__1)) + (r__2 = 
			r_imag(&vr_ref(ii, is)), dabs(r__2)));
		csscal_(&ki, &remax, &vr_ref(1, is), &c__1);

		i__1 = *n;
		for (k = ki + 1; k <= i__1; ++k) {
		    i__2 = vr_subscr(k, is);
		    vr[i__2].r = 0.f, vr[i__2].i = 0.f;
/* L60: */
		}
	    } else {
		if (ki > 1) {
		    i__1 = ki - 1;
		    q__1.r = scale, q__1.i = 0.f;
		    cgemv_("N", n, &i__1, &c_b2, &vr[vr_offset], ldvr, &work[
			    1], &c__1, &q__1, &vr_ref(1, ki), &c__1);
		}

		ii = icamax_(n, &vr_ref(1, ki), &c__1);
		i__1 = vr_subscr(ii, ki);
		remax = 1.f / ((r__1 = vr[i__1].r, dabs(r__1)) + (r__2 = 
			r_imag(&vr_ref(ii, ki)), dabs(r__2)));
		csscal_(n, &remax, &vr_ref(1, ki), &c__1);
	    }

/*           Set back the original diagonal elements of T. */

	    i__1 = ki - 1;
	    for (k = 1; k <= i__1; ++k) {
		i__2 = t_subscr(k, k);
		i__3 = k + *n;
		t[i__2].r = work[i__3].r, t[i__2].i = work[i__3].i;
/* L70: */
	    }

	    --is;
L80:
	    ;
	}
    }

    if (leftv) {

/*        Compute left eigenvectors. */

	is = 1;
	i__1 = *n;
	for (ki = 1; ki <= i__1; ++ki) {

	    if (somev) {
		if (! select[ki]) {
		    goto L130;
		}
	    }
/* Computing MAX */
	    i__2 = t_subscr(ki, ki);
	    r__3 = ulp * ((r__1 = t[i__2].r, dabs(r__1)) + (r__2 = r_imag(&
		    t_ref(ki, ki)), dabs(r__2)));
	    smin = dmax(r__3,smlnum);

	    i__2 = *n;
	    work[i__2].r = 1.f, work[i__2].i = 0.f;

/*           Form right-hand side. */

	    i__2 = *n;
	    for (k = ki + 1; k <= i__2; ++k) {
		i__3 = k;
		r_cnjg(&q__2, &t_ref(ki, k));
		q__1.r = -q__2.r, q__1.i = -q__2.i;
		work[i__3].r = q__1.r, work[i__3].i = q__1.i;
/* L90: */
	    }

/*           Solve the triangular system:   
                (T(KI+1:N,KI+1:N) - T(KI,KI))'*X = SCALE*WORK. */

	    i__2 = *n;
	    for (k = ki + 1; k <= i__2; ++k) {
		i__3 = t_subscr(k, k);
		i__4 = t_subscr(k, k);
		i__5 = t_subscr(ki, ki);
		q__1.r = t[i__4].r - t[i__5].r, q__1.i = t[i__4].i - t[i__5]
			.i;
		t[i__3].r = q__1.r, t[i__3].i = q__1.i;
		i__3 = t_subscr(k, k);
		if ((r__1 = t[i__3].r, dabs(r__1)) + (r__2 = r_imag(&t_ref(k, 
			k)), dabs(r__2)) < smin) {
		    i__4 = t_subscr(k, k);
		    t[i__4].r = smin, t[i__4].i = 0.f;
		}
/* L100: */
	    }

	    if (ki < *n) {
		i__2 = *n - ki;
		clatrs_("Upper", "Conjugate transpose", "Non-unit", "Y", &
			i__2, &t_ref(ki + 1, ki + 1), ldt, &work[ki + 1], &
			scale, &rwork[1], info);
		i__2 = ki;
		work[i__2].r = scale, work[i__2].i = 0.f;
	    }

/*           Copy the vector x or Q*x to VL and normalize. */

	    if (! over) {
		i__2 = *n - ki + 1;
		ccopy_(&i__2, &work[ki], &c__1, &vl_ref(ki, is), &c__1);

		i__2 = *n - ki + 1;
		ii = icamax_(&i__2, &vl_ref(ki, is), &c__1) + ki - 1;
		i__2 = vl_subscr(ii, is);
		remax = 1.f / ((r__1 = vl[i__2].r, dabs(r__1)) + (r__2 = 
			r_imag(&vl_ref(ii, is)), dabs(r__2)));
		i__2 = *n - ki + 1;
		csscal_(&i__2, &remax, &vl_ref(ki, is), &c__1);

		i__2 = ki - 1;
		for (k = 1; k <= i__2; ++k) {
		    i__3 = vl_subscr(k, is);
		    vl[i__3].r = 0.f, vl[i__3].i = 0.f;
/* L110: */
		}
	    } else {
		if (ki < *n) {
		    i__2 = *n - ki;
		    q__1.r = scale, q__1.i = 0.f;
		    cgemv_("N", n, &i__2, &c_b2, &vl_ref(1, ki + 1), ldvl, &
			    work[ki + 1], &c__1, &q__1, &vl_ref(1, ki), &c__1);
		}

		ii = icamax_(n, &vl_ref(1, ki), &c__1);
		i__2 = vl_subscr(ii, ki);
		remax = 1.f / ((r__1 = vl[i__2].r, dabs(r__1)) + (r__2 = 
			r_imag(&vl_ref(ii, ki)), dabs(r__2)));
		csscal_(n, &remax, &vl_ref(1, ki), &c__1);
	    }

/*           Set back the original diagonal elements of T. */

	    i__2 = *n;
	    for (k = ki + 1; k <= i__2; ++k) {
		i__3 = t_subscr(k, k);
		i__4 = k + *n;
		t[i__3].r = work[i__4].r, t[i__3].i = work[i__4].i;
/* L120: */
	    }

	    ++is;
L130:
	    ;
	}
    }

    return 0;

/*     End of CTREVC */

} /* ctrevc_ */

#undef vr_ref
#undef vr_subscr
#undef vl_ref
#undef vl_subscr
#undef t_ref
#undef t_subscr


