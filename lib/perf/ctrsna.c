#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int ctrsna_(char *job, char *howmny, logical *select, 
	integer *n, complex *t, integer *ldt, complex *vl, integer *ldvl, 
	complex *vr, integer *ldvr, real *s, real *sep, integer *mm, integer *
	m, complex *work, integer *ldwork, real *rwork, integer *info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    CTRSNA estimates reciprocal condition numbers for specified   
    eigenvalues and/or right eigenvectors of a complex upper triangular   
    matrix T (or of any matrix Q*T*Q**H with Q unitary).   

    Arguments   
    =========   

    JOB     (input) CHARACTER*1   
            Specifies whether condition numbers are required for   
            eigenvalues (S) or eigenvectors (SEP):   
            = 'E': for eigenvalues only (S);   
            = 'V': for eigenvectors only (SEP);   
            = 'B': for both eigenvalues and eigenvectors (S and SEP).   

    HOWMNY  (input) CHARACTER*1   
            = 'A': compute condition numbers for all eigenpairs;   
            = 'S': compute condition numbers for selected eigenpairs   
                   specified by the array SELECT.   

    SELECT  (input) LOGICAL array, dimension (N)   
            If HOWMNY = 'S', SELECT specifies the eigenpairs for which   
            condition numbers are required. To select condition numbers   
            for the j-th eigenpair, SELECT(j) must be set to .TRUE..   
            If HOWMNY = 'A', SELECT is not referenced.   

    N       (input) INTEGER   
            The order of the matrix T. N >= 0.   

    T       (input) COMPLEX array, dimension (LDT,N)   
            The upper triangular matrix T.   

    LDT     (input) INTEGER   
            The leading dimension of the array T. LDT >= max(1,N).   

    VL      (input) COMPLEX array, dimension (LDVL,M)   
            If JOB = 'E' or 'B', VL must contain left eigenvectors of T   
            (or of any Q*T*Q**H with Q unitary), corresponding to the   
            eigenpairs specified by HOWMNY and SELECT. The eigenvectors   
            must be stored in consecutive columns of VL, as returned by   
            CHSEIN or CTREVC.   
            If JOB = 'V', VL is not referenced.   

    LDVL    (input) INTEGER   
            The leading dimension of the array VL.   
            LDVL >= 1; and if JOB = 'E' or 'B', LDVL >= N.   

    VR      (input) COMPLEX array, dimension (LDVR,M)   
            If JOB = 'E' or 'B', VR must contain right eigenvectors of T   
            (or of any Q*T*Q**H with Q unitary), corresponding to the   
            eigenpairs specified by HOWMNY and SELECT. The eigenvectors   
            must be stored in consecutive columns of VR, as returned by   
            CHSEIN or CTREVC.   
            If JOB = 'V', VR is not referenced.   

    LDVR    (input) INTEGER   
            The leading dimension of the array VR.   
            LDVR >= 1; and if JOB = 'E' or 'B', LDVR >= N.   

    S       (output) REAL array, dimension (MM)   
            If JOB = 'E' or 'B', the reciprocal condition numbers of the   
            selected eigenvalues, stored in consecutive elements of the   
            array. Thus S(j), SEP(j), and the j-th columns of VL and VR   
            all correspond to the same eigenpair (but not in general the   
            j-th eigenpair, unless all eigenpairs are selected).   
            If JOB = 'V', S is not referenced.   

    SEP     (output) REAL array, dimension (MM)   
            If JOB = 'V' or 'B', the estimated reciprocal condition   
            numbers of the selected eigenvectors, stored in consecutive   
            elements of the array.   
            If JOB = 'E', SEP is not referenced.   

    MM      (input) INTEGER   
            The number of elements in the arrays S (if JOB = 'E' or 'B')   
             and/or SEP (if JOB = 'V' or 'B'). MM >= M.   

    M       (output) INTEGER   
            The number of elements of the arrays S and/or SEP actually   
            used to store the estimated condition numbers.   
            If HOWMNY = 'A', M is set to N.   

    WORK    (workspace) COMPLEX array, dimension (LDWORK,N+1)   
            If JOB = 'E', WORK is not referenced.   

    LDWORK  (input) INTEGER   
            The leading dimension of the array WORK.   
            LDWORK >= 1; and if JOB = 'V' or 'B', LDWORK >= N.   

    RWORK   (workspace) REAL array, dimension (N)   
            If JOB = 'E', RWORK is not referenced.   

    INFO    (output) INTEGER   
            = 0: successful exit   
            < 0: if INFO = -i, the i-th argument had an illegal value   

    Further Details   
    ===============   

    The reciprocal of the condition number of an eigenvalue lambda is   
    defined as   

            S(lambda) = |v'*u| / (norm(u)*norm(v))   

    where u and v are the right and left eigenvectors of T corresponding   
    to lambda; v' denotes the conjugate transpose of v, and norm(u)   
    denotes the Euclidean norm. These reciprocal condition numbers always   
    lie between zero (very badly conditioned) and one (very well   
    conditioned). If n = 1, S(lambda) is defined to be 1.   

    An approximate error bound for a computed eigenvalue W(i) is given by   

                        EPS * norm(T) / S(i)   

    where EPS is the machine precision.   

    The reciprocal of the condition number of the right eigenvector u   
    corresponding to lambda is defined as follows. Suppose   

                T = ( lambda  c  )   
                    (   0    T22 )   

    Then the reciprocal condition number is   

            SEP( lambda, T22 ) = sigma-min( T22 - lambda*I )   

    where sigma-min denotes the smallest singular value. We approximate   
    the smallest singular value by the reciprocal of an estimate of the   
    one-norm of the inverse of T22 - lambda*I. If n = 1, SEP(1) is   
    defined to be abs(T(1,1)).   

    An approximate error bound for a computed right eigenvector VR(i)   
    is given by   

                        EPS * norm(T) / SEP(i)   

    =====================================================================   


       Decode and test the input parameters   

       Parameter adjustments */
    /* Table of constant values */
    static integer c__1 = 1;
    
    /* System generated locals */
    integer t_dim1, t_offset, vl_dim1, vl_offset, vr_dim1, vr_offset, 
	    work_dim1, work_offset, i__1, i__2, i__3, i__4, i__5;
    real r__1, r__2;
    complex q__1;
    /* Builtin functions */
    double c_abs(complex *), r_imag(complex *);
    /* Local variables */
    static integer kase, ierr;
    static complex prod;
    static real lnrm, rnrm;
    static integer i__, j, k;
    static real scale;
    extern /* Complex */ VOID cdotc_(complex *, integer *, complex *, integer 
	    *, complex *, integer *);
    extern logical lsame_(char *, char *);
    static complex dummy[1];
    static logical wants;
    static real xnorm;
    extern doublereal scnrm2_(integer *, complex *, integer *);
    extern /* Subroutine */ int slabad_(real *, real *);
    static integer ks;
    extern /* Subroutine */ int clacon_(integer *, complex *, complex *, real 
	    *, integer *);
    static integer ix;
    extern integer icamax_(integer *, complex *, integer *);
    extern doublereal slamch_(char *);
    extern /* Subroutine */ int clacpy_(char *, integer *, integer *, complex 
	    *, integer *, complex *, integer *), xerbla_(char *, 
	    integer *);
    static real bignum;
    static logical wantbh;
    extern /* Subroutine */ int clatrs_(char *, char *, char *, char *, 
	    integer *, complex *, integer *, complex *, real *, real *, 
	    integer *), csrscl_(integer *, 
	    real *, complex *, integer *), ctrexc_(char *, integer *, complex 
	    *, integer *, complex *, integer *, integer *, integer *, integer 
	    *);
    static logical somcon;
    static char normin[1];
    static real smlnum;
    static logical wantsp;
    static real eps, est;
#define work_subscr(a_1,a_2) (a_2)*work_dim1 + a_1
#define work_ref(a_1,a_2) work[work_subscr(a_1,a_2)]
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
    --s;
    --sep;
    work_dim1 = *ldwork;
    work_offset = 1 + work_dim1 * 1;
    work -= work_offset;
    --rwork;

    /* Function Body */
    wantbh = lsame_(job, "B");
    wants = lsame_(job, "E") || wantbh;
    wantsp = lsame_(job, "V") || wantbh;

    somcon = lsame_(howmny, "S");

/*     Set M to the number of eigenpairs for which condition numbers are   
       to be computed. */

    if (somcon) {
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
    if (! wants && ! wantsp) {
	*info = -1;
    } else if (! lsame_(howmny, "A") && ! somcon) {
	*info = -2;
    } else if (*n < 0) {
	*info = -4;
    } else if (*ldt < max(1,*n)) {
	*info = -6;
    } else if (*ldvl < 1 || wants && *ldvl < *n) {
	*info = -8;
    } else if (*ldvr < 1 || wants && *ldvr < *n) {
	*info = -10;
    } else if (*mm < *m) {
	*info = -13;
    } else if (*ldwork < 1 || wantsp && *ldwork < *n) {
	*info = -16;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("CTRSNA", &i__1);
	return 0;
    }

/*     Quick return if possible */

    if (*n == 0) {
	return 0;
    }

    if (*n == 1) {
	if (somcon) {
	    if (! select[1]) {
		return 0;
	    }
	}
	if (wants) {
	    s[1] = 1.f;
	}
	if (wantsp) {
	    sep[1] = c_abs(&t_ref(1, 1));
	}
	return 0;
    }

/*     Get machine constants */

    eps = slamch_("P");
    smlnum = slamch_("S") / eps;
    bignum = 1.f / smlnum;
    slabad_(&smlnum, &bignum);

    ks = 1;
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {

	if (somcon) {
	    if (! select[k]) {
		goto L50;
	    }
	}

	if (wants) {

/*           Compute the reciprocal condition number of the k-th   
             eigenvalue. */

	    cdotc_(&q__1, n, &vr_ref(1, ks), &c__1, &vl_ref(1, ks), &c__1);
	    prod.r = q__1.r, prod.i = q__1.i;
	    rnrm = scnrm2_(n, &vr_ref(1, ks), &c__1);
	    lnrm = scnrm2_(n, &vl_ref(1, ks), &c__1);
	    s[ks] = c_abs(&prod) / (rnrm * lnrm);

	}

	if (wantsp) {

/*           Estimate the reciprocal condition number of the k-th   
             eigenvector.   

             Copy the matrix T to the array WORK and swap the k-th   
             diagonal element to the (1,1) position. */

	    clacpy_("Full", n, n, &t[t_offset], ldt, &work[work_offset], 
		    ldwork);
	    ctrexc_("No Q", n, &work[work_offset], ldwork, dummy, &c__1, &k, &
		    c__1, &ierr);

/*           Form  C = T22 - lambda*I in WORK(2:N,2:N). */

	    i__2 = *n;
	    for (i__ = 2; i__ <= i__2; ++i__) {
		i__3 = work_subscr(i__, i__);
		i__4 = work_subscr(i__, i__);
		i__5 = work_subscr(1, 1);
		q__1.r = work[i__4].r - work[i__5].r, q__1.i = work[i__4].i - 
			work[i__5].i;
		work[i__3].r = q__1.r, work[i__3].i = q__1.i;
/* L20: */
	    }

/*           Estimate a lower bound for the 1-norm of inv(C'). The 1st   
             and (N+1)th columns of WORK are used to store work vectors. */

	    sep[ks] = 0.f;
	    est = 0.f;
	    kase = 0;
	    *(unsigned char *)normin = 'N';
L30:
	    i__2 = *n - 1;
	    clacon_(&i__2, &work_ref(1, *n + 1), &work[work_offset], &est, &
		    kase);

	    if (kase != 0) {
		if (kase == 1) {

/*                 Solve C'*x = scale*b */

		    i__2 = *n - 1;
		    clatrs_("Upper", "Conjugate transpose", "Nonunit", normin,
			     &i__2, &work_ref(2, 2), ldwork, &work[
			    work_offset], &scale, &rwork[1], &ierr);
		} else {

/*                 Solve C*x = scale*b */

		    i__2 = *n - 1;
		    clatrs_("Upper", "No transpose", "Nonunit", normin, &i__2,
			     &work_ref(2, 2), ldwork, &work[work_offset], &
			    scale, &rwork[1], &ierr);
		}
		*(unsigned char *)normin = 'Y';
		if (scale != 1.f) {

/*                 Multiply by 1/SCALE if doing so will not cause   
                   overflow. */

		    i__2 = *n - 1;
		    ix = icamax_(&i__2, &work[work_offset], &c__1);
		    i__2 = work_subscr(ix, 1);
		    xnorm = (r__1 = work[i__2].r, dabs(r__1)) + (r__2 = 
			    r_imag(&work_ref(ix, 1)), dabs(r__2));
		    if (scale < xnorm * smlnum || scale == 0.f) {
			goto L40;
		    }
		    csrscl_(n, &scale, &work[work_offset], &c__1);
		}
		goto L30;
	    }

	    sep[ks] = 1.f / dmax(est,smlnum);
	}

L40:
	++ks;
L50:
	;
    }
    return 0;

/*     End of CTRSNA */

} /* ctrsna_ */

#undef vr_ref
#undef vr_subscr
#undef vl_ref
#undef vl_subscr
#undef t_ref
#undef t_subscr
#undef work_ref
#undef work_subscr


