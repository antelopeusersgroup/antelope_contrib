#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int zggev_(char *jobvl, char *jobvr, integer *n, 
	doublecomplex *a, integer *lda, doublecomplex *b, integer *ldb, 
	doublecomplex *alpha, doublecomplex *beta, doublecomplex *vl, integer 
	*ldvl, doublecomplex *vr, integer *ldvr, doublecomplex *work, integer 
	*lwork, doublereal *rwork, integer *info)
{
/*  -- LAPACK driver routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    ZGGEV computes for a pair of N-by-N complex nonsymmetric matrices   
    (A,B), the generalized eigenvalues, and optionally, the left and/or   
    right generalized eigenvectors.   

    A generalized eigenvalue for a pair of matrices (A,B) is a scalar   
    lambda or a ratio alpha/beta = lambda, such that A - lambda*B is   
    singular. It is usually represented as the pair (alpha,beta), as   
    there is a reasonable interpretation for beta=0, and even for both   
    being zero.   

    The right generalized eigenvector v(j) corresponding to the   
    generalized eigenvalue lambda(j) of (A,B) satisfies   

                 A * v(j) = lambda(j) * B * v(j).   

    The left generalized eigenvector u(j) corresponding to the   
    generalized eigenvalues lambda(j) of (A,B) satisfies   

                 u(j)**H * A = lambda(j) * u(j)**H * B   

    where u(j)**H is the conjugate-transpose of u(j).   

    Arguments   
    =========   

    JOBVL   (input) CHARACTER*1   
            = 'N':  do not compute the left generalized eigenvectors;   
            = 'V':  compute the left generalized eigenvectors.   

    JOBVR   (input) CHARACTER*1   
            = 'N':  do not compute the right generalized eigenvectors;   
            = 'V':  compute the right generalized eigenvectors.   

    N       (input) INTEGER   
            The order of the matrices A, B, VL, and VR.  N >= 0.   

    A       (input/output) COMPLEX*16 array, dimension (LDA, N)   
            On entry, the matrix A in the pair (A,B).   
            On exit, A has been overwritten.   

    LDA     (input) INTEGER   
            The leading dimension of A.  LDA >= max(1,N).   

    B       (input/output) COMPLEX*16 array, dimension (LDB, N)   
            On entry, the matrix B in the pair (A,B).   
            On exit, B has been overwritten.   

    LDB     (input) INTEGER   
            The leading dimension of B.  LDB >= max(1,N).   

    ALPHA   (output) COMPLEX*16 array, dimension (N)   
    BETA    (output) COMPLEX*16 array, dimension (N)   
            On exit, ALPHA(j)/BETA(j), j=1,...,N, will be the   
            generalized eigenvalues.   

            Note: the quotients ALPHA(j)/BETA(j) may easily over- or   
            underflow, and BETA(j) may even be zero.  Thus, the user   
            should avoid naively computing the ratio alpha/beta.   
            However, ALPHA will be always less than and usually   
            comparable with norm(A) in magnitude, and BETA always less   
            than and usually comparable with norm(B).   

    VL      (output) COMPLEX*16 array, dimension (LDVL,N)   
            If JOBVL = 'V', the left generalized eigenvectors u(j) are   
            stored one after another in the columns of VL, in the same   
            order as their eigenvalues.   
            Each eigenvector will be scaled so the largest component   
            will have abs(real part) + abs(imag. part) = 1.   
            Not referenced if JOBVL = 'N'.   

    LDVL    (input) INTEGER   
            The leading dimension of the matrix VL. LDVL >= 1, and   
            if JOBVL = 'V', LDVL >= N.   

    VR      (output) COMPLEX*16 array, dimension (LDVR,N)   
            If JOBVR = 'V', the right generalized eigenvectors v(j) are   
            stored one after another in the columns of VR, in the same   
            order as their eigenvalues.   
            Each eigenvector will be scaled so the largest component   
            will have abs(real part) + abs(imag. part) = 1.   
            Not referenced if JOBVR = 'N'.   

    LDVR    (input) INTEGER   
            The leading dimension of the matrix VR. LDVR >= 1, and   
            if JOBVR = 'V', LDVR >= N.   

    WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)   
            On exit, if INFO = 0, WORK(1) returns the optimal LWORK.   

    LWORK   (input) INTEGER   
            The dimension of the array WORK.  LWORK >= max(1,2*N).   
            For good performance, LWORK must generally be larger.   

            If LWORK = -1, then a workspace query is assumed; the routine   
            only calculates the optimal size of the WORK array, returns   
            this value as the first entry of the WORK array, and no error   
            message related to LWORK is issued by XERBLA.   

    RWORK   (workspace/output) DOUBLE PRECISION array, dimension (8*N)   

    INFO    (output) INTEGER   
            = 0:  successful exit   
            < 0:  if INFO = -i, the i-th argument had an illegal value.   
            =1,...,N:   
                  The QZ iteration failed.  No eigenvectors have been   
                  calculated, but ALPHA(j) and BETA(j) should be   
                  correct for j=INFO+1,...,N.   
            > N:  =N+1: other then QZ iteration failed in DHGEQZ,   
                  =N+2: error return from DTGEVC.   

    =====================================================================   


       Decode the input arguments   

       Parameter adjustments */
    /* Table of constant values */
    static doublecomplex c_b1 = {0.,0.};
    static doublecomplex c_b2 = {1.,0.};
    static integer c__1 = 1;
    static integer c__0 = 0;
    
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, vl_dim1, vl_offset, vr_dim1, 
	    vr_offset, i__1, i__2, i__3, i__4;
    doublereal d__1, d__2, d__3, d__4;
    doublecomplex z__1;
    /* Builtin functions */
    double sqrt(doublereal), d_imag(doublecomplex *);
    /* Local variables */
    static doublereal anrm, bnrm;
    static integer ierr, itau;
    static doublereal temp;
    static logical ilvl, ilvr;
    static integer iwrk;
    extern logical lsame_(char *, char *);
    static integer ileft, icols, irwrk, irows;
    extern /* Subroutine */ int dlabad_(doublereal *, doublereal *);
    static integer jc, in;
    extern doublereal dlamch_(char *);
    static integer jr;
    extern /* Subroutine */ int zggbak_(char *, char *, integer *, integer *, 
	    integer *, doublereal *, doublereal *, integer *, doublecomplex *,
	     integer *, integer *), zggbal_(char *, integer *,
	     doublecomplex *, integer *, doublecomplex *, integer *, integer *
	    , integer *, doublereal *, doublereal *, doublereal *, integer *);
    static logical ilascl, ilbscl;
    extern /* Subroutine */ int xerbla_(char *, integer *);
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *, 
	    integer *, integer *, ftnlen, ftnlen);
    static logical ldumma[1];
    static char chtemp[1];
    static doublereal bignum;
    extern doublereal zlange_(char *, integer *, integer *, doublecomplex *, 
	    integer *, doublereal *);
    static integer ijobvl, iright;
    extern /* Subroutine */ int zgghrd_(char *, char *, integer *, integer *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *,
	     doublecomplex *, integer *, doublecomplex *, integer *, integer *
	    ), zlascl_(char *, integer *, integer *, 
	    doublereal *, doublereal *, integer *, integer *, doublecomplex *,
	     integer *, integer *);
    static integer ijobvr;
    extern /* Subroutine */ int zgeqrf_(integer *, integer *, doublecomplex *,
	     integer *, doublecomplex *, doublecomplex *, integer *, integer *
	    );
    static doublereal anrmto;
    static integer lwkmin;
    static doublereal bnrmto;
    extern /* Subroutine */ int zlacpy_(char *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *), 
	    zlaset_(char *, integer *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, integer *), ztgevc_(
	    char *, char *, logical *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, integer *, integer *, doublecomplex *,
	     doublereal *, integer *), zhgeqz_(char *, char *,
	     char *, integer *, integer *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, doublecomplex *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, integer *, doublereal *, integer *);
    static doublereal smlnum;
    static integer lwkopt;
    static logical lquery;
    extern /* Subroutine */ int zungqr_(integer *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    integer *, integer *), zunmqr_(char *, char *, integer *, integer 
	    *, integer *, doublecomplex *, integer *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, integer *);
    static integer ihi, ilo;
    static doublereal eps;
    static logical ilv;
#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]
#define b_subscr(a_1,a_2) (a_2)*b_dim1 + a_1
#define b_ref(a_1,a_2) b[b_subscr(a_1,a_2)]
#define vl_subscr(a_1,a_2) (a_2)*vl_dim1 + a_1
#define vl_ref(a_1,a_2) vl[vl_subscr(a_1,a_2)]
#define vr_subscr(a_1,a_2) (a_2)*vr_dim1 + a_1
#define vr_ref(a_1,a_2) vr[vr_subscr(a_1,a_2)]


    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    --alpha;
    --beta;
    vl_dim1 = *ldvl;
    vl_offset = 1 + vl_dim1 * 1;
    vl -= vl_offset;
    vr_dim1 = *ldvr;
    vr_offset = 1 + vr_dim1 * 1;
    vr -= vr_offset;
    --work;
    --rwork;

    /* Function Body */
    if (lsame_(jobvl, "N")) {
	ijobvl = 1;
	ilvl = FALSE_;
    } else if (lsame_(jobvl, "V")) {
	ijobvl = 2;
	ilvl = TRUE_;
    } else {
	ijobvl = -1;
	ilvl = FALSE_;
    }

    if (lsame_(jobvr, "N")) {
	ijobvr = 1;
	ilvr = FALSE_;
    } else if (lsame_(jobvr, "V")) {
	ijobvr = 2;
	ilvr = TRUE_;
    } else {
	ijobvr = -1;
	ilvr = FALSE_;
    }
    ilv = ilvl || ilvr;

/*     Test the input arguments */

    *info = 0;
    lquery = *lwork == -1;
    if (ijobvl <= 0) {
	*info = -1;
    } else if (ijobvr <= 0) {
	*info = -2;
    } else if (*n < 0) {
	*info = -3;
    } else if (*lda < max(1,*n)) {
	*info = -5;
    } else if (*ldb < max(1,*n)) {
	*info = -7;
    } else if (*ldvl < 1 || ilvl && *ldvl < *n) {
	*info = -11;
    } else if (*ldvr < 1 || ilvr && *ldvr < *n) {
	*info = -13;
    }

/*     Compute workspace   
        (Note: Comments in the code beginning "Workspace:" describe the   
         minimal amount of workspace needed at that point in the code,   
         as well as the preferred amount for good performance.   
         NB refers to the optimal block size for the immediately   
         following subroutine, as returned by ILAENV. The workspace is   
         computed assuming ILO = 1 and IHI = N, the worst case.) */

    lwkmin = 1;
    if (*info == 0 && (*lwork >= 1 || lquery)) {
	lwkopt = *n + *n * ilaenv_(&c__1, "ZGEQRF", " ", n, &c__1, n, &c__0, (
		ftnlen)6, (ftnlen)1);
/* Computing MAX */
	i__1 = 1, i__2 = *n << 1;
	lwkmin = max(i__1,i__2);
	work[1].r = (doublereal) lwkopt, work[1].i = 0.;
    }

    if (*lwork < lwkmin && ! lquery) {
	*info = -15;
    }

    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("ZGGEV ", &i__1);
	return 0;
    } else if (lquery) {
	return 0;
    }

/*     Quick return if possible */

    work[1].r = (doublereal) lwkopt, work[1].i = 0.;
    if (*n == 0) {
	return 0;
    }

/*     Get machine constants */

    eps = dlamch_("E") * dlamch_("B");
    smlnum = dlamch_("S");
    bignum = 1. / smlnum;
    dlabad_(&smlnum, &bignum);
    smlnum = sqrt(smlnum) / eps;
    bignum = 1. / smlnum;

/*     Scale A if max element outside range [SMLNUM,BIGNUM] */

    anrm = zlange_("M", n, n, &a[a_offset], lda, &rwork[1]);
    ilascl = FALSE_;
    if (anrm > 0. && anrm < smlnum) {
	anrmto = smlnum;
	ilascl = TRUE_;
    } else if (anrm > bignum) {
	anrmto = bignum;
	ilascl = TRUE_;
    }
    if (ilascl) {
	zlascl_("G", &c__0, &c__0, &anrm, &anrmto, n, n, &a[a_offset], lda, &
		ierr);
    }

/*     Scale B if max element outside range [SMLNUM,BIGNUM] */

    bnrm = zlange_("M", n, n, &b[b_offset], ldb, &rwork[1]);
    ilbscl = FALSE_;
    if (bnrm > 0. && bnrm < smlnum) {
	bnrmto = smlnum;
	ilbscl = TRUE_;
    } else if (bnrm > bignum) {
	bnrmto = bignum;
	ilbscl = TRUE_;
    }
    if (ilbscl) {
	zlascl_("G", &c__0, &c__0, &bnrm, &bnrmto, n, n, &b[b_offset], ldb, &
		ierr);
    }

/*     Permute the matrices A, B to isolate eigenvalues if possible   
       (Real Workspace: need 6*N) */

    ileft = 1;
    iright = *n + 1;
    irwrk = iright + *n;
    zggbal_("P", n, &a[a_offset], lda, &b[b_offset], ldb, &ilo, &ihi, &rwork[
	    ileft], &rwork[iright], &rwork[irwrk], &ierr);

/*     Reduce B to triangular form (QR decomposition of B)   
       (Complex Workspace: need N, prefer N*NB) */

    irows = ihi + 1 - ilo;
    if (ilv) {
	icols = *n + 1 - ilo;
    } else {
	icols = irows;
    }
    itau = 1;
    iwrk = itau + irows;
    i__1 = *lwork + 1 - iwrk;
    zgeqrf_(&irows, &icols, &b_ref(ilo, ilo), ldb, &work[itau], &work[iwrk], &
	    i__1, &ierr);

/*     Apply the orthogonal transformation to matrix A   
       (Complex Workspace: need N, prefer N*NB) */

    i__1 = *lwork + 1 - iwrk;
    zunmqr_("L", "C", &irows, &icols, &irows, &b_ref(ilo, ilo), ldb, &work[
	    itau], &a_ref(ilo, ilo), lda, &work[iwrk], &i__1, &ierr);

/*     Initialize VL   
       (Complex Workspace: need N, prefer N*NB) */

    if (ilvl) {
	zlaset_("Full", n, n, &c_b1, &c_b2, &vl[vl_offset], ldvl);
	i__1 = irows - 1;
	i__2 = irows - 1;
	zlacpy_("L", &i__1, &i__2, &b_ref(ilo + 1, ilo), ldb, &vl_ref(ilo + 1,
		 ilo), ldvl);
	i__1 = *lwork + 1 - iwrk;
	zungqr_(&irows, &irows, &irows, &vl_ref(ilo, ilo), ldvl, &work[itau], 
		&work[iwrk], &i__1, &ierr);
    }

/*     Initialize VR */

    if (ilvr) {
	zlaset_("Full", n, n, &c_b1, &c_b2, &vr[vr_offset], ldvr);
    }

/*     Reduce to generalized Hessenberg form */

    if (ilv) {

/*        Eigenvectors requested -- work on whole matrix. */

	zgghrd_(jobvl, jobvr, n, &ilo, &ihi, &a[a_offset], lda, &b[b_offset], 
		ldb, &vl[vl_offset], ldvl, &vr[vr_offset], ldvr, &ierr);
    } else {
	zgghrd_("N", "N", &irows, &c__1, &irows, &a_ref(ilo, ilo), lda, &
		b_ref(ilo, ilo), ldb, &vl[vl_offset], ldvl, &vr[vr_offset], 
		ldvr, &ierr);
    }

/*     Perform QZ algorithm (Compute eigenvalues, and optionally, the   
       Schur form and Schur vectors)   
       (Complex Workspace: need N)   
       (Real Workspace: need N) */

    iwrk = itau;
    if (ilv) {
	*(unsigned char *)chtemp = 'S';
    } else {
	*(unsigned char *)chtemp = 'E';
    }
    i__1 = *lwork + 1 - iwrk;
    zhgeqz_(chtemp, jobvl, jobvr, n, &ilo, &ihi, &a[a_offset], lda, &b[
	    b_offset], ldb, &alpha[1], &beta[1], &vl[vl_offset], ldvl, &vr[
	    vr_offset], ldvr, &work[iwrk], &i__1, &rwork[irwrk], &ierr);
    if (ierr != 0) {
	if (ierr > 0 && ierr <= *n) {
	    *info = ierr;
	} else if (ierr > *n && ierr <= *n << 1) {
	    *info = ierr - *n;
	} else {
	    *info = *n + 1;
	}
	goto L70;
    }

/*     Compute Eigenvectors   
       (Real Workspace: need 2*N)   
       (Complex Workspace: need 2*N) */

    if (ilv) {
	if (ilvl) {
	    if (ilvr) {
		*(unsigned char *)chtemp = 'B';
	    } else {
		*(unsigned char *)chtemp = 'L';
	    }
	} else {
	    *(unsigned char *)chtemp = 'R';
	}

	ztgevc_(chtemp, "B", ldumma, n, &a[a_offset], lda, &b[b_offset], ldb, 
		&vl[vl_offset], ldvl, &vr[vr_offset], ldvr, n, &in, &work[
		iwrk], &rwork[irwrk], &ierr);
	if (ierr != 0) {
	    *info = *n + 2;
	    goto L70;
	}

/*        Undo balancing on VL and VR and normalization   
          (Workspace: none needed) */

	if (ilvl) {
	    zggbak_("P", "L", n, &ilo, &ihi, &rwork[ileft], &rwork[iright], n,
		     &vl[vl_offset], ldvl, &ierr);
	    i__1 = *n;
	    for (jc = 1; jc <= i__1; ++jc) {
		temp = 0.;
		i__2 = *n;
		for (jr = 1; jr <= i__2; ++jr) {
/* Computing MAX */
		    i__3 = vl_subscr(jr, jc);
		    d__3 = temp, d__4 = (d__1 = vl[i__3].r, abs(d__1)) + (
			    d__2 = d_imag(&vl_ref(jr, jc)), abs(d__2));
		    temp = max(d__3,d__4);
/* L10: */
		}
		if (temp < smlnum) {
		    goto L30;
		}
		temp = 1. / temp;
		i__2 = *n;
		for (jr = 1; jr <= i__2; ++jr) {
		    i__3 = vl_subscr(jr, jc);
		    i__4 = vl_subscr(jr, jc);
		    z__1.r = temp * vl[i__4].r, z__1.i = temp * vl[i__4].i;
		    vl[i__3].r = z__1.r, vl[i__3].i = z__1.i;
/* L20: */
		}
L30:
		;
	    }
	}
	if (ilvr) {
	    zggbak_("P", "R", n, &ilo, &ihi, &rwork[ileft], &rwork[iright], n,
		     &vr[vr_offset], ldvr, &ierr);
	    i__1 = *n;
	    for (jc = 1; jc <= i__1; ++jc) {
		temp = 0.;
		i__2 = *n;
		for (jr = 1; jr <= i__2; ++jr) {
/* Computing MAX */
		    i__3 = vr_subscr(jr, jc);
		    d__3 = temp, d__4 = (d__1 = vr[i__3].r, abs(d__1)) + (
			    d__2 = d_imag(&vr_ref(jr, jc)), abs(d__2));
		    temp = max(d__3,d__4);
/* L40: */
		}
		if (temp < smlnum) {
		    goto L60;
		}
		temp = 1. / temp;
		i__2 = *n;
		for (jr = 1; jr <= i__2; ++jr) {
		    i__3 = vr_subscr(jr, jc);
		    i__4 = vr_subscr(jr, jc);
		    z__1.r = temp * vr[i__4].r, z__1.i = temp * vr[i__4].i;
		    vr[i__3].r = z__1.r, vr[i__3].i = z__1.i;
/* L50: */
		}
L60:
		;
	    }
	}
    }

/*     Undo scaling if necessary */

    if (ilascl) {
	zlascl_("G", &c__0, &c__0, &anrmto, &anrm, n, &c__1, &alpha[1], n, &
		ierr);
    }

    if (ilbscl) {
	zlascl_("G", &c__0, &c__0, &bnrmto, &bnrm, n, &c__1, &beta[1], n, &
		ierr);
    }

L70:
    work[1].r = (doublereal) lwkopt, work[1].i = 0.;

    return 0;

/*     End of ZGGEV */

} /* zggev_ */

#undef vr_ref
#undef vr_subscr
#undef vl_ref
#undef vl_subscr
#undef b_ref
#undef b_subscr
#undef a_ref
#undef a_subscr


