#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int cgeesx_(char *jobvs, char *sort, L_fp select, char *
	sense, integer *n, complex *a, integer *lda, integer *sdim, complex *
	w, complex *vs, integer *ldvs, real *rconde, real *rcondv, complex *
	work, integer *lwork, real *rwork, logical *bwork, integer *info)
{
/*  -- LAPACK driver routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    CGEESX computes for an N-by-N complex nonsymmetric matrix A, the   
    eigenvalues, the Schur form T, and, optionally, the matrix of Schur   
    vectors Z.  This gives the Schur factorization A = Z*T*(Z**H).   

    Optionally, it also orders the eigenvalues on the diagonal of the   
    Schur form so that selected eigenvalues are at the top left;   
    computes a reciprocal condition number for the average of the   
    selected eigenvalues (RCONDE); and computes a reciprocal condition   
    number for the right invariant subspace corresponding to the   
    selected eigenvalues (RCONDV).  The leading columns of Z form an   
    orthonormal basis for this invariant subspace.   

    For further explanation of the reciprocal condition numbers RCONDE   
    and RCONDV, see Section 4.10 of the LAPACK Users' Guide (where   
    these quantities are called s and sep respectively).   

    A complex matrix is in Schur form if it is upper triangular.   

    Arguments   
    =========   

    JOBVS   (input) CHARACTER*1   
            = 'N': Schur vectors are not computed;   
            = 'V': Schur vectors are computed.   

    SORT    (input) CHARACTER*1   
            Specifies whether or not to order the eigenvalues on the   
            diagonal of the Schur form.   
            = 'N': Eigenvalues are not ordered;   
            = 'S': Eigenvalues are ordered (see SELECT).   

    SELECT  (input) LOGICAL FUNCTION of one COMPLEX argument   
            SELECT must be declared EXTERNAL in the calling subroutine.   
            If SORT = 'S', SELECT is used to select eigenvalues to order   
            to the top left of the Schur form.   
            If SORT = 'N', SELECT is not referenced.   
            An eigenvalue W(j) is selected if SELECT(W(j)) is true.   

    SENSE   (input) CHARACTER*1   
            Determines which reciprocal condition numbers are computed.   
            = 'N': None are computed;   
            = 'E': Computed for average of selected eigenvalues only;   
            = 'V': Computed for selected right invariant subspace only;   
            = 'B': Computed for both.   
            If SENSE = 'E', 'V' or 'B', SORT must equal 'S'.   

    N       (input) INTEGER   
            The order of the matrix A. N >= 0.   

    A       (input/output) COMPLEX array, dimension (LDA, N)   
            On entry, the N-by-N matrix A.   
            On exit, A is overwritten by its Schur form T.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,N).   

    SDIM    (output) INTEGER   
            If SORT = 'N', SDIM = 0.   
            If SORT = 'S', SDIM = number of eigenvalues for which   
                           SELECT is true.   

    W       (output) COMPLEX array, dimension (N)   
            W contains the computed eigenvalues, in the same order   
            that they appear on the diagonal of the output Schur form T.   

    VS      (output) COMPLEX array, dimension (LDVS,N)   
            If JOBVS = 'V', VS contains the unitary matrix Z of Schur   
            vectors.   
            If JOBVS = 'N', VS is not referenced.   

    LDVS    (input) INTEGER   
            The leading dimension of the array VS.  LDVS >= 1, and if   
            JOBVS = 'V', LDVS >= N.   

    RCONDE  (output) REAL   
            If SENSE = 'E' or 'B', RCONDE contains the reciprocal   
            condition number for the average of the selected eigenvalues.   
            Not referenced if SENSE = 'N' or 'V'.   

    RCONDV  (output) REAL   
            If SENSE = 'V' or 'B', RCONDV contains the reciprocal   
            condition number for the selected right invariant subspace.   
            Not referenced if SENSE = 'N' or 'E'.   

    WORK    (workspace/output) COMPLEX array, dimension (LWORK)   
            On exit, if INFO = 0, WORK(1) returns the optimal LWORK.   

    LWORK   (input) INTEGER   
            The dimension of the array WORK.  LWORK >= max(1,2*N).   
            Also, if SENSE = 'E' or 'V' or 'B', LWORK >= 2*SDIM*(N-SDIM),   
            where SDIM is the number of selected eigenvalues computed by   
            this routine.  Note that 2*SDIM*(N-SDIM) <= N*N/2.   
            For good performance, LWORK must generally be larger.   

    RWORK   (workspace) REAL array, dimension (N)   

    BWORK   (workspace) LOGICAL array, dimension (N)   
            Not referenced if SORT = 'N'.   

    INFO    (output) INTEGER   
            = 0: successful exit   
            < 0: if INFO = -i, the i-th argument had an illegal value.   
            > 0: if INFO = i, and i is   
               <= N: the QR algorithm failed to compute all the   
                     eigenvalues; elements 1:ILO-1 and i+1:N of W   
                     contain those eigenvalues which have converged; if   
                     JOBVS = 'V', VS contains the transformation which   
                     reduces A to its partially converged Schur form.   
               = N+1: the eigenvalues could not be reordered because some   
                     eigenvalues were too close to separate (the problem   
                     is very ill-conditioned);   
               = N+2: after reordering, roundoff changed values of some   
                     complex eigenvalues so that leading eigenvalues in   
                     the Schur form no longer satisfy SELECT=.TRUE.  This   
                     could also be caused by underflow due to scaling.   

    =====================================================================   


       Test the input arguments   

       Parameter adjustments */
    /* Table of constant values */
    static integer c__1 = 1;
    static integer c__0 = 0;
    static integer c__8 = 8;
    static integer c_n1 = -1;
    static integer c__4 = 4;
    
    /* System generated locals */
    integer a_dim1, a_offset, vs_dim1, vs_offset, i__1, i__2, i__3, i__4;
    /* Builtin functions */
    double sqrt(doublereal);
    /* Local variables */
    static integer ibal, maxb;
    static real anrm;
    static integer ierr, itau, iwrk, i__, k, icond, ieval;
    extern logical lsame_(char *, char *);
    extern /* Subroutine */ int ccopy_(integer *, complex *, integer *, 
	    complex *, integer *), cgebak_(char *, char *, integer *, integer 
	    *, integer *, real *, integer *, complex *, integer *, integer *), cgebal_(char *, integer *, complex *, integer *, 
	    integer *, integer *, real *, integer *), slabad_(real *, 
	    real *);
    static logical scalea;
    extern doublereal clange_(char *, integer *, integer *, complex *, 
	    integer *, real *);
    static real cscale;
    extern /* Subroutine */ int cgehrd_(integer *, integer *, integer *, 
	    complex *, integer *, complex *, complex *, integer *, integer *),
	     clascl_(char *, integer *, integer *, real *, real *, integer *, 
	    integer *, complex *, integer *, integer *);
    extern doublereal slamch_(char *);
    extern /* Subroutine */ int clacpy_(char *, integer *, integer *, complex 
	    *, integer *, complex *, integer *), xerbla_(char *, 
	    integer *);
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *, 
	    integer *, integer *, ftnlen, ftnlen);
    static real bignum;
    extern /* Subroutine */ int slascl_(char *, integer *, integer *, real *, 
	    real *, integer *, integer *, real *, integer *, integer *), chseqr_(char *, char *, integer *, integer *, integer *, 
	    complex *, integer *, complex *, complex *, integer *, complex *, 
	    integer *, integer *), cunghr_(integer *, integer 
	    *, integer *, complex *, integer *, complex *, complex *, integer 
	    *, integer *);
    static logical wantsb;
    extern /* Subroutine */ int ctrsen_(char *, char *, logical *, integer *, 
	    complex *, integer *, complex *, integer *, complex *, integer *, 
	    real *, real *, complex *, integer *, integer *);
    static logical wantse;
    static integer minwrk, maxwrk;
    static logical wantsn;
    static real smlnum;
    static integer hswork;
    static logical wantst, wantsv, wantvs;
    static integer ihi, ilo;
    static real dum[1], eps;


    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --w;
    vs_dim1 = *ldvs;
    vs_offset = 1 + vs_dim1 * 1;
    vs -= vs_offset;
    --work;
    --rwork;
    --bwork;

    /* Function Body */
    *info = 0;
    wantvs = lsame_(jobvs, "V");
    wantst = lsame_(sort, "S");
    wantsn = lsame_(sense, "N");
    wantse = lsame_(sense, "E");
    wantsv = lsame_(sense, "V");
    wantsb = lsame_(sense, "B");
    if (! wantvs && ! lsame_(jobvs, "N")) {
	*info = -1;
    } else if (! wantst && ! lsame_(sort, "N")) {
	*info = -2;
    } else if (! (wantsn || wantse || wantsv || wantsb) || ! wantst && ! 
	    wantsn) {
	*info = -4;
    } else if (*n < 0) {
	*info = -5;
    } else if (*lda < max(1,*n)) {
	*info = -7;
    } else if (*ldvs < 1 || wantvs && *ldvs < *n) {
	*info = -11;
    }

/*     Compute workspace   
        (Note: Comments in the code beginning "Workspace:" describe the   
         minimal amount of real workspace needed at that point in the   
         code, as well as the preferred amount for good performance.   
         CWorkspace refers to complex workspace, and RWorkspace to real   
         workspace. NB refers to the optimal block size for the   
         immediately following subroutine, as returned by ILAENV.   
         HSWORK refers to the workspace preferred by CHSEQR, as   
         calculated below. HSWORK is computed assuming ILO=1 and IHI=N,   
         the worst case.   
         If SENSE = 'E', 'V' or 'B', then the amount of workspace needed   
         depends on SDIM, which is computed by the routine CTRSEN later   
         in the code.) */

    minwrk = 1;
    if (*info == 0 && *lwork >= 1) {
	maxwrk = *n + *n * ilaenv_(&c__1, "CGEHRD", " ", n, &c__1, n, &c__0, (
		ftnlen)6, (ftnlen)1);
/* Computing MAX */
	i__1 = 1, i__2 = *n << 1;
	minwrk = max(i__1,i__2);
	if (! wantvs) {
/* Computing MAX */
	    i__1 = ilaenv_(&c__8, "CHSEQR", "SN", n, &c__1, n, &c_n1, (ftnlen)
		    6, (ftnlen)2);
	    maxb = max(i__1,2);
/* Computing MIN   
   Computing MAX */
	    i__3 = 2, i__4 = ilaenv_(&c__4, "CHSEQR", "SN", n, &c__1, n, &
		    c_n1, (ftnlen)6, (ftnlen)2);
	    i__1 = min(maxb,*n), i__2 = max(i__3,i__4);
	    k = min(i__1,i__2);
/* Computing MAX */
	    i__1 = k * (k + 2), i__2 = *n << 1;
	    hswork = max(i__1,i__2);
/* Computing MAX */
	    i__1 = max(maxwrk,hswork);
	    maxwrk = max(i__1,1);
	} else {
/* Computing MAX */
	    i__1 = maxwrk, i__2 = *n + (*n - 1) * ilaenv_(&c__1, "CUNGHR", 
		    " ", n, &c__1, n, &c_n1, (ftnlen)6, (ftnlen)1);
	    maxwrk = max(i__1,i__2);
/* Computing MAX */
	    i__1 = ilaenv_(&c__8, "CHSEQR", "SV", n, &c__1, n, &c_n1, (ftnlen)
		    6, (ftnlen)2);
	    maxb = max(i__1,2);
/* Computing MIN   
   Computing MAX */
	    i__3 = 2, i__4 = ilaenv_(&c__4, "CHSEQR", "SV", n, &c__1, n, &
		    c_n1, (ftnlen)6, (ftnlen)2);
	    i__1 = min(maxb,*n), i__2 = max(i__3,i__4);
	    k = min(i__1,i__2);
/* Computing MAX */
	    i__1 = k * (k + 2), i__2 = *n << 1;
	    hswork = max(i__1,i__2);
/* Computing MAX */
	    i__1 = max(maxwrk,hswork);
	    maxwrk = max(i__1,1);
	}
	work[1].r = (real) maxwrk, work[1].i = 0.f;
    }
    if (*lwork < minwrk) {
	*info = -15;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("CGEESX", &i__1);
	return 0;
    }

/*     Quick return if possible */

    if (*n == 0) {
	*sdim = 0;
	return 0;
    }

/*     Get machine constants */

    eps = slamch_("P");
    smlnum = slamch_("S");
    bignum = 1.f / smlnum;
    slabad_(&smlnum, &bignum);
    smlnum = sqrt(smlnum) / eps;
    bignum = 1.f / smlnum;

/*     Scale A if max element outside range [SMLNUM,BIGNUM] */

    anrm = clange_("M", n, n, &a[a_offset], lda, dum);
    scalea = FALSE_;
    if (anrm > 0.f && anrm < smlnum) {
	scalea = TRUE_;
	cscale = smlnum;
    } else if (anrm > bignum) {
	scalea = TRUE_;
	cscale = bignum;
    }
    if (scalea) {
	clascl_("G", &c__0, &c__0, &anrm, &cscale, n, n, &a[a_offset], lda, &
		ierr);
    }


/*     Permute the matrix to make it more nearly triangular   
       (CWorkspace: none)   
       (RWorkspace: need N) */

    ibal = 1;
    cgebal_("P", n, &a[a_offset], lda, &ilo, &ihi, &rwork[ibal], &ierr);

/*     Reduce to upper Hessenberg form   
       (CWorkspace: need 2*N, prefer N+N*NB)   
       (RWorkspace: none) */

    itau = 1;
    iwrk = *n + itau;
    i__1 = *lwork - iwrk + 1;
    cgehrd_(n, &ilo, &ihi, &a[a_offset], lda, &work[itau], &work[iwrk], &i__1,
	     &ierr);

    if (wantvs) {

/*        Copy Householder vectors to VS */

	clacpy_("L", n, n, &a[a_offset], lda, &vs[vs_offset], ldvs)
		;

/*        Generate unitary matrix in VS   
          (CWorkspace: need 2*N-1, prefer N+(N-1)*NB)   
          (RWorkspace: none) */

	i__1 = *lwork - iwrk + 1;
	cunghr_(n, &ilo, &ihi, &vs[vs_offset], ldvs, &work[itau], &work[iwrk],
		 &i__1, &ierr);
    }

    *sdim = 0;

/*     Perform QR iteration, accumulating Schur vectors in VS if desired   
       (CWorkspace: need 1, prefer HSWORK (see comments) )   
       (RWorkspace: none) */

    iwrk = itau;
    i__1 = *lwork - iwrk + 1;
    chseqr_("S", jobvs, n, &ilo, &ihi, &a[a_offset], lda, &w[1], &vs[
	    vs_offset], ldvs, &work[iwrk], &i__1, &ieval);
    if (ieval > 0) {
	*info = ieval;
    }

/*     Sort eigenvalues if desired */

    if (wantst && *info == 0) {
	if (scalea) {
	    clascl_("G", &c__0, &c__0, &cscale, &anrm, n, &c__1, &w[1], n, &
		    ierr);
	}
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    bwork[i__] = (*select)(&w[i__]);
/* L10: */
	}

/*        Reorder eigenvalues, transform Schur vectors, and compute   
          reciprocal condition numbers   
          (CWorkspace: if SENSE is not 'N', need 2*SDIM*(N-SDIM)   
                       otherwise, need none )   
          (RWorkspace: none) */

	i__1 = *lwork - iwrk + 1;
	ctrsen_(sense, jobvs, &bwork[1], n, &a[a_offset], lda, &vs[vs_offset],
		 ldvs, &w[1], sdim, rconde, rcondv, &work[iwrk], &i__1, &
		icond);
	if (! wantsn) {
/* Computing MAX */
	    i__1 = maxwrk, i__2 = (*sdim << 1) * (*n - *sdim);
	    maxwrk = max(i__1,i__2);
	}
	if (icond == -14) {

/*           Not enough complex workspace */

	    *info = -15;
	}
    }

    if (wantvs) {

/*        Undo balancing   
          (CWorkspace: none)   
          (RWorkspace: need N) */

	cgebak_("P", "R", n, &ilo, &ihi, &rwork[ibal], n, &vs[vs_offset], 
		ldvs, &ierr);
    }

    if (scalea) {

/*        Undo scaling for the Schur form of A */

	clascl_("U", &c__0, &c__0, &cscale, &anrm, n, n, &a[a_offset], lda, &
		ierr);
	i__1 = *lda + 1;
	ccopy_(n, &a[a_offset], &i__1, &w[1], &c__1);
	if ((wantsv || wantsb) && *info == 0) {
	    dum[0] = *rcondv;
	    slascl_("G", &c__0, &c__0, &cscale, &anrm, &c__1, &c__1, dum, &
		    c__1, &ierr);
	    *rcondv = dum[0];
	}
    }

    work[1].r = (real) maxwrk, work[1].i = 0.f;
    return 0;

/*     End of CGEESX */

} /* cgeesx_ */

