#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int dgeesx_(char *jobvs, char *sort, L_fp select, char *
	sense, integer *n, doublereal *a, integer *lda, integer *sdim, 
	doublereal *wr, doublereal *wi, doublereal *vs, integer *ldvs, 
	doublereal *rconde, doublereal *rcondv, doublereal *work, integer *
	lwork, integer *iwork, integer *liwork, logical *bwork, integer *info)
{
/*  -- LAPACK driver routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    DGEESX computes for an N-by-N real nonsymmetric matrix A, the   
    eigenvalues, the real Schur form T, and, optionally, the matrix of   
    Schur vectors Z.  This gives the Schur factorization A = Z*T*(Z**T).   

    Optionally, it also orders the eigenvalues on the diagonal of the   
    real Schur form so that selected eigenvalues are at the top left;   
    computes a reciprocal condition number for the average of the   
    selected eigenvalues (RCONDE); and computes a reciprocal condition   
    number for the right invariant subspace corresponding to the   
    selected eigenvalues (RCONDV).  The leading columns of Z form an   
    orthonormal basis for this invariant subspace.   

    For further explanation of the reciprocal condition numbers RCONDE   
    and RCONDV, see Section 4.10 of the LAPACK Users' Guide (where   
    these quantities are called s and sep respectively).   

    A real matrix is in real Schur form if it is upper quasi-triangular   
    with 1-by-1 and 2-by-2 blocks. 2-by-2 blocks will be standardized in   
    the form   
              [  a  b  ]   
              [  c  a  ]   

    where b*c < 0. The eigenvalues of such a block are a +- sqrt(bc).   

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

    SELECT  (input) LOGICAL FUNCTION of two DOUBLE PRECISION arguments   
            SELECT must be declared EXTERNAL in the calling subroutine.   
            If SORT = 'S', SELECT is used to select eigenvalues to sort   
            to the top left of the Schur form.   
            If SORT = 'N', SELECT is not referenced.   
            An eigenvalue WR(j)+sqrt(-1)*WI(j) is selected if   
            SELECT(WR(j),WI(j)) is true; i.e., if either one of a   
            complex conjugate pair of eigenvalues is selected, then both   
            are.  Note that a selected complex eigenvalue may no longer   
            satisfy SELECT(WR(j),WI(j)) = .TRUE. after ordering, since   
            ordering may change the value of complex eigenvalues   
            (especially if the eigenvalue is ill-conditioned); in this   
            case INFO may be set to N+3 (see INFO below).   

    SENSE   (input) CHARACTER*1   
            Determines which reciprocal condition numbers are computed.   
            = 'N': None are computed;   
            = 'E': Computed for average of selected eigenvalues only;   
            = 'V': Computed for selected right invariant subspace only;   
            = 'B': Computed for both.   
            If SENSE = 'E', 'V' or 'B', SORT must equal 'S'.   

    N       (input) INTEGER   
            The order of the matrix A. N >= 0.   

    A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)   
            On entry, the N-by-N matrix A.   
            On exit, A is overwritten by its real Schur form T.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,N).   

    SDIM    (output) INTEGER   
            If SORT = 'N', SDIM = 0.   
            If SORT = 'S', SDIM = number of eigenvalues (after sorting)   
                           for which SELECT is true. (Complex conjugate   
                           pairs for which SELECT is true for either   
                           eigenvalue count as 2.)   

    WR      (output) DOUBLE PRECISION array, dimension (N)   
    WI      (output) DOUBLE PRECISION array, dimension (N)   
            WR and WI contain the real and imaginary parts, respectively,   
            of the computed eigenvalues, in the same order that they   
            appear on the diagonal of the output Schur form T.  Complex   
            conjugate pairs of eigenvalues appear consecutively with the   
            eigenvalue having the positive imaginary part first.   

    VS      (output) DOUBLE PRECISION array, dimension (LDVS,N)   
            If JOBVS = 'V', VS contains the orthogonal matrix Z of Schur   
            vectors.   
            If JOBVS = 'N', VS is not referenced.   

    LDVS    (input) INTEGER   
            The leading dimension of the array VS.  LDVS >= 1, and if   
            JOBVS = 'V', LDVS >= N.   

    RCONDE  (output) DOUBLE PRECISION   
            If SENSE = 'E' or 'B', RCONDE contains the reciprocal   
            condition number for the average of the selected eigenvalues.   
            Not referenced if SENSE = 'N' or 'V'.   

    RCONDV  (output) DOUBLE PRECISION   
            If SENSE = 'V' or 'B', RCONDV contains the reciprocal   
            condition number for the selected right invariant subspace.   
            Not referenced if SENSE = 'N' or 'E'.   

    WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)   
            On exit, if INFO = 0, WORK(1) returns the optimal LWORK.   

    LWORK   (input) INTEGER   
            The dimension of the array WORK.  LWORK >= max(1,3*N).   
            Also, if SENSE = 'E' or 'V' or 'B',   
            LWORK >= N+2*SDIM*(N-SDIM), where SDIM is the number of   
            selected eigenvalues computed by this routine.  Note that   
            N+2*SDIM*(N-SDIM) <= N+N*N/2.   
            For good performance, LWORK must generally be larger.   

    IWORK   (workspace/output) INTEGER array, dimension (LIWORK)   
            Not referenced if SENSE = 'N' or 'E'.   
            On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.   

    LIWORK  (input) INTEGER   
            The dimension of the array IWORK.   
            LIWORK >= 1; if SENSE = 'V' or 'B', LIWORK >= SDIM*(N-SDIM).   

    BWORK   (workspace) LOGICAL array, dimension (N)   
            Not referenced if SORT = 'N'.   

    INFO    (output) INTEGER   
            = 0: successful exit   
            < 0: if INFO = -i, the i-th argument had an illegal value.   
            > 0: if INFO = i, and i is   
               <= N: the QR algorithm failed to compute all the   
                     eigenvalues; elements 1:ILO-1 and i+1:N of WR and WI   
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
    static doublereal anrm;
    static integer ierr, itau, iwrk, inxt, i__, k, icond, ieval;
    extern logical lsame_(char *, char *);
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *), dswap_(integer *, doublereal *, integer 
	    *, doublereal *, integer *);
    static logical cursl;
    static integer i1, i2;
    extern /* Subroutine */ int dlabad_(doublereal *, doublereal *), dgebak_(
	    char *, char *, integer *, integer *, integer *, doublereal *, 
	    integer *, doublereal *, integer *, integer *), 
	    dgebal_(char *, integer *, doublereal *, integer *, integer *, 
	    integer *, doublereal *, integer *);
    static logical lst2sl, scalea;
    static integer ip;
    static doublereal cscale;
    extern doublereal dlamch_(char *), dlange_(char *, integer *, 
	    integer *, doublereal *, integer *, doublereal *);
    extern /* Subroutine */ int dgehrd_(integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    integer *), dlascl_(char *, integer *, integer *, doublereal *, 
	    doublereal *, integer *, integer *, doublereal *, integer *, 
	    integer *), dlacpy_(char *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, integer *), 
	    xerbla_(char *, integer *);
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *, 
	    integer *, integer *, ftnlen, ftnlen);
    static doublereal bignum;
    extern /* Subroutine */ int dorghr_(integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    integer *), dhseqr_(char *, char *, integer *, integer *, integer 
	    *, doublereal *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, integer *);
    static logical wantsb;
    extern /* Subroutine */ int dtrsen_(char *, char *, logical *, integer *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *,
	     integer *, integer *, integer *, integer *);
    static logical wantse, lastsl;
    static integer minwrk, maxwrk;
    static logical wantsn;
    static doublereal smlnum;
    static integer hswork;
    static logical wantst, wantsv, wantvs;
    static integer ihi, ilo;
    static doublereal dum[1], eps;
#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define vs_ref(a_1,a_2) vs[(a_2)*vs_dim1 + a_1]


    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --wr;
    --wi;
    vs_dim1 = *ldvs;
    vs_offset = 1 + vs_dim1 * 1;
    vs -= vs_offset;
    --work;
    --iwork;
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
	*info = -12;
    }

/*     Compute workspace   
        (Note: Comments in the code beginning "RWorkspace:" describe the   
         minimal amount of real workspace needed at that point in the   
         code, as well as the preferred amount for good performance.   
         IWorkspace refers to integer workspace.   
         NB refers to the optimal block size for the immediately   
         following subroutine, as returned by ILAENV.   
         HSWORK refers to the workspace preferred by DHSEQR, as   
         calculated below. HSWORK is computed assuming ILO=1 and IHI=N,   
         the worst case.   
         If SENSE = 'E', 'V' or 'B', then the amount of workspace needed   
         depends on SDIM, which is computed by the routine DTRSEN later   
         in the code.) */

    minwrk = 1;
    if (*info == 0 && *lwork >= 1) {
	maxwrk = (*n << 1) + *n * ilaenv_(&c__1, "DGEHRD", " ", n, &c__1, n, &
		c__0, (ftnlen)6, (ftnlen)1);
/* Computing MAX */
	i__1 = 1, i__2 = *n * 3;
	minwrk = max(i__1,i__2);
	if (! wantvs) {
/* Computing MAX */
	    i__1 = ilaenv_(&c__8, "DHSEQR", "SN", n, &c__1, n, &c_n1, (ftnlen)
		    6, (ftnlen)2);
	    maxb = max(i__1,2);
/* Computing MIN   
   Computing MAX */
	    i__3 = 2, i__4 = ilaenv_(&c__4, "DHSEQR", "SN", n, &c__1, n, &
		    c_n1, (ftnlen)6, (ftnlen)2);
	    i__1 = min(maxb,*n), i__2 = max(i__3,i__4);
	    k = min(i__1,i__2);
/* Computing MAX */
	    i__1 = k * (k + 2), i__2 = *n << 1;
	    hswork = max(i__1,i__2);
/* Computing MAX */
	    i__1 = maxwrk, i__2 = *n + hswork, i__1 = max(i__1,i__2);
	    maxwrk = max(i__1,1);
	} else {
/* Computing MAX */
	    i__1 = maxwrk, i__2 = (*n << 1) + (*n - 1) * ilaenv_(&c__1, "DOR"
		    "GHR", " ", n, &c__1, n, &c_n1, (ftnlen)6, (ftnlen)1);
	    maxwrk = max(i__1,i__2);
/* Computing MAX */
	    i__1 = ilaenv_(&c__8, "DHSEQR", "SV", n, &c__1, n, &c_n1, (ftnlen)
		    6, (ftnlen)2);
	    maxb = max(i__1,2);
/* Computing MIN   
   Computing MAX */
	    i__3 = 2, i__4 = ilaenv_(&c__4, "DHSEQR", "SV", n, &c__1, n, &
		    c_n1, (ftnlen)6, (ftnlen)2);
	    i__1 = min(maxb,*n), i__2 = max(i__3,i__4);
	    k = min(i__1,i__2);
/* Computing MAX */
	    i__1 = k * (k + 2), i__2 = *n << 1;
	    hswork = max(i__1,i__2);
/* Computing MAX */
	    i__1 = maxwrk, i__2 = *n + hswork, i__1 = max(i__1,i__2);
	    maxwrk = max(i__1,1);
	}
	work[1] = (doublereal) maxwrk;
    }
    if (*lwork < minwrk) {
	*info = -16;
    }
    if (*liwork < 1) {
	*info = -18;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("DGEESX", &i__1);
	return 0;
    }

/*     Quick return if possible */

    if (*n == 0) {
	*sdim = 0;
	return 0;
    }

/*     Get machine constants */

    eps = dlamch_("P");
    smlnum = dlamch_("S");
    bignum = 1. / smlnum;
    dlabad_(&smlnum, &bignum);
    smlnum = sqrt(smlnum) / eps;
    bignum = 1. / smlnum;

/*     Scale A if max element outside range [SMLNUM,BIGNUM] */

    anrm = dlange_("M", n, n, &a[a_offset], lda, dum);
    scalea = FALSE_;
    if (anrm > 0. && anrm < smlnum) {
	scalea = TRUE_;
	cscale = smlnum;
    } else if (anrm > bignum) {
	scalea = TRUE_;
	cscale = bignum;
    }
    if (scalea) {
	dlascl_("G", &c__0, &c__0, &anrm, &cscale, n, n, &a[a_offset], lda, &
		ierr);
    }

/*     Permute the matrix to make it more nearly triangular   
       (RWorkspace: need N) */

    ibal = 1;
    dgebal_("P", n, &a[a_offset], lda, &ilo, &ihi, &work[ibal], &ierr);

/*     Reduce to upper Hessenberg form   
       (RWorkspace: need 3*N, prefer 2*N+N*NB) */

    itau = *n + ibal;
    iwrk = *n + itau;
    i__1 = *lwork - iwrk + 1;
    dgehrd_(n, &ilo, &ihi, &a[a_offset], lda, &work[itau], &work[iwrk], &i__1,
	     &ierr);

    if (wantvs) {

/*        Copy Householder vectors to VS */

	dlacpy_("L", n, n, &a[a_offset], lda, &vs[vs_offset], ldvs)
		;

/*        Generate orthogonal matrix in VS   
          (RWorkspace: need 3*N-1, prefer 2*N+(N-1)*NB) */

	i__1 = *lwork - iwrk + 1;
	dorghr_(n, &ilo, &ihi, &vs[vs_offset], ldvs, &work[itau], &work[iwrk],
		 &i__1, &ierr);
    }

    *sdim = 0;

/*     Perform QR iteration, accumulating Schur vectors in VS if desired   
       (RWorkspace: need N+1, prefer N+HSWORK (see comments) ) */

    iwrk = itau;
    i__1 = *lwork - iwrk + 1;
    dhseqr_("S", jobvs, n, &ilo, &ihi, &a[a_offset], lda, &wr[1], &wi[1], &vs[
	    vs_offset], ldvs, &work[iwrk], &i__1, &ieval);
    if (ieval > 0) {
	*info = ieval;
    }

/*     Sort eigenvalues if desired */

    if (wantst && *info == 0) {
	if (scalea) {
	    dlascl_("G", &c__0, &c__0, &cscale, &anrm, n, &c__1, &wr[1], n, &
		    ierr);
	    dlascl_("G", &c__0, &c__0, &cscale, &anrm, n, &c__1, &wi[1], n, &
		    ierr);
	}
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    bwork[i__] = (*select)(&wr[i__], &wi[i__]);
/* L10: */
	}

/*        Reorder eigenvalues, transform Schur vectors, and compute   
          reciprocal condition numbers   
          (RWorkspace: if SENSE is not 'N', need N+2*SDIM*(N-SDIM)   
                       otherwise, need N )   
          (IWorkspace: if SENSE is 'V' or 'B', need SDIM*(N-SDIM)   
                       otherwise, need 0 ) */

	i__1 = *lwork - iwrk + 1;
	dtrsen_(sense, jobvs, &bwork[1], n, &a[a_offset], lda, &vs[vs_offset],
		 ldvs, &wr[1], &wi[1], sdim, rconde, rcondv, &work[iwrk], &
		i__1, &iwork[1], liwork, &icond);
	if (! wantsn) {
/* Computing MAX */
	    i__1 = maxwrk, i__2 = *n + (*sdim << 1) * (*n - *sdim);
	    maxwrk = max(i__1,i__2);
	}
	if (icond == -15) {

/*           Not enough real workspace */

	    *info = -16;
	} else if (icond == -17) {

/*           Not enough integer workspace */

	    *info = -18;
	} else if (icond > 0) {

/*           DTRSEN failed to reorder or to restore standard Schur form */

	    *info = icond + *n;
	}
    }

    if (wantvs) {

/*        Undo balancing   
          (RWorkspace: need N) */

	dgebak_("P", "R", n, &ilo, &ihi, &work[ibal], n, &vs[vs_offset], ldvs,
		 &ierr);
    }

    if (scalea) {

/*        Undo scaling for the Schur form of A */

	dlascl_("H", &c__0, &c__0, &cscale, &anrm, n, n, &a[a_offset], lda, &
		ierr);
	i__1 = *lda + 1;
	dcopy_(n, &a[a_offset], &i__1, &wr[1], &c__1);
	if ((wantsv || wantsb) && *info == 0) {
	    dum[0] = *rcondv;
	    dlascl_("G", &c__0, &c__0, &cscale, &anrm, &c__1, &c__1, dum, &
		    c__1, &ierr);
	    *rcondv = dum[0];
	}
	if (cscale == smlnum) {

/*           If scaling back towards underflow, adjust WI if an   
             offdiagonal element of a 2-by-2 block in the Schur form   
             underflows. */

	    if (ieval > 0) {
		i1 = ieval + 1;
		i2 = ihi - 1;
		i__1 = ilo - 1;
		dlascl_("G", &c__0, &c__0, &cscale, &anrm, &i__1, &c__1, &wi[
			1], n, &ierr);
	    } else if (wantst) {
		i1 = 1;
		i2 = *n - 1;
	    } else {
		i1 = ilo;
		i2 = ihi - 1;
	    }
	    inxt = i1 - 1;
	    i__1 = i2;
	    for (i__ = i1; i__ <= i__1; ++i__) {
		if (i__ < inxt) {
		    goto L20;
		}
		if (wi[i__] == 0.) {
		    inxt = i__ + 1;
		} else {
		    if (a_ref(i__ + 1, i__) == 0.) {
			wi[i__] = 0.;
			wi[i__ + 1] = 0.;
		    } else if (a_ref(i__ + 1, i__) != 0. && a_ref(i__, i__ + 
			    1) == 0.) {
			wi[i__] = 0.;
			wi[i__ + 1] = 0.;
			if (i__ > 1) {
			    i__2 = i__ - 1;
			    dswap_(&i__2, &a_ref(1, i__), &c__1, &a_ref(1, 
				    i__ + 1), &c__1);
			}
			if (*n > i__ + 1) {
			    i__2 = *n - i__ - 1;
			    dswap_(&i__2, &a_ref(i__, i__ + 2), lda, &a_ref(
				    i__ + 1, i__ + 2), lda);
			}
			dswap_(n, &vs_ref(1, i__), &c__1, &vs_ref(1, i__ + 1),
				 &c__1);
			a_ref(i__, i__ + 1) = a_ref(i__ + 1, i__);
			a_ref(i__ + 1, i__) = 0.;
		    }
		    inxt = i__ + 2;
		}
L20:
		;
	    }
	}
	i__1 = *n - ieval;
/* Computing MAX */
	i__3 = *n - ieval;
	i__2 = max(i__3,1);
	dlascl_("G", &c__0, &c__0, &cscale, &anrm, &i__1, &c__1, &wi[ieval + 
		1], &i__2, &ierr);
    }

    if (wantst && *info == 0) {

/*        Check if reordering successful */

	lastsl = TRUE_;
	lst2sl = TRUE_;
	*sdim = 0;
	ip = 0;
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    cursl = (*select)(&wr[i__], &wi[i__]);
	    if (wi[i__] == 0.) {
		if (cursl) {
		    ++(*sdim);
		}
		ip = 0;
		if (cursl && ! lastsl) {
		    *info = *n + 2;
		}
	    } else {
		if (ip == 1) {

/*                 Last eigenvalue of conjugate pair */

		    cursl = cursl || lastsl;
		    lastsl = cursl;
		    if (cursl) {
			*sdim += 2;
		    }
		    ip = -1;
		    if (cursl && ! lst2sl) {
			*info = *n + 2;
		    }
		} else {

/*                 First eigenvalue of conjugate pair */

		    ip = 1;
		}
	    }
	    lst2sl = lastsl;
	    lastsl = cursl;
/* L30: */
	}
    }

    work[1] = (doublereal) maxwrk;
    if (wantsv || wantsb) {
	iwork[1] = *sdim * (*n - *sdim);
    } else {
	iwork[1] = 1;
    }

    return 0;

/*     End of DGEESX */

} /* dgeesx_ */

#undef vs_ref
#undef a_ref


