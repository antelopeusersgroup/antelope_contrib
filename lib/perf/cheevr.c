#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int cheevr_(char *jobz, char *range, char *uplo, integer *n, 
	complex *a, integer *lda, real *vl, real *vu, integer *il, integer *
	iu, real *abstol, integer *m, real *w, complex *z__, integer *ldz, 
	integer *isuppz, complex *work, integer *lwork, real *rwork, integer *
	lrwork, integer *iwork, integer *liwork, integer *info)
{
/*  -- LAPACK driver routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 20, 2000   


    Purpose   
    =======   

    CHEEVR computes selected eigenvalues and, optionally, eigenvectors   
    of a complex Hermitian matrix T.  Eigenvalues and eigenvectors can   
    be selected by specifying either a range of values or a range of   
    indices for the desired eigenvalues.   

    Whenever possible, CHEEVR calls CSTEGR to compute the   
    eigenspectrum using Relatively Robust Representations.  CSTEGR   
    computes eigenvalues by the dqds algorithm, while orthogonal   
    eigenvectors are computed from various "good" L D L^T representations   
    (also known as Relatively Robust Representations). Gram-Schmidt   
    orthogonalization is avoided as far as possible. More specifically,   
    the various steps of the algorithm are as follows. For the i-th   
    unreduced block of T,   
       (a) Compute T - sigma_i = L_i D_i L_i^T, such that L_i D_i L_i^T   
            is a relatively robust representation,   
       (b) Compute the eigenvalues, lambda_j, of L_i D_i L_i^T to high   
           relative accuracy by the dqds algorithm,   
       (c) If there is a cluster of close eigenvalues, "choose" sigma_i   
           close to the cluster, and go to step (a),   
       (d) Given the approximate eigenvalue lambda_j of L_i D_i L_i^T,   
           compute the corresponding eigenvector by forming a   
           rank-revealing twisted factorization.   
    The desired accuracy of the output can be specified by the input   
    parameter ABSTOL.   

    For more details, see "A new O(n^2) algorithm for the symmetric   
    tridiagonal eigenvalue/eigenvector problem", by Inderjit Dhillon,   
    Computer Science Division Technical Report No. UCB//CSD-97-971,   
    UC Berkeley, May 1997.   


    Note 1 : CHEEVR calls CSTEGR when the full spectrum is requested   
    on machines which conform to the ieee-754 floating point standard.   
    CHEEVR calls SSTEBZ and CSTEIN on non-ieee machines and   
    when partial spectrum requests are made.   

    Normal execution of CSTEGR may create NaNs and infinities and   
    hence may abort due to a floating point exception in environments   
    which do not handle NaNs and infinities in the ieee standard default   
    manner.   

    Arguments   
    =========   

    JOBZ    (input) CHARACTER*1   
            = 'N':  Compute eigenvalues only;   
            = 'V':  Compute eigenvalues and eigenvectors.   

    RANGE   (input) CHARACTER*1   
            = 'A': all eigenvalues will be found.   
            = 'V': all eigenvalues in the half-open interval (VL,VU]   
                   will be found.   
            = 'I': the IL-th through IU-th eigenvalues will be found.   
   ********* For RANGE = 'V' or 'I' and IU - IL < N - 1, SSTEBZ and   
   ********* CSTEIN are called   

    UPLO    (input) CHARACTER*1   
            = 'U':  Upper triangle of A is stored;   
            = 'L':  Lower triangle of A is stored.   

    N       (input) INTEGER   
            The order of the matrix A.  N >= 0.   

    A       (input/output) COMPLEX array, dimension (LDA, N)   
            On entry, the Hermitian matrix A.  If UPLO = 'U', the   
            leading N-by-N upper triangular part of A contains the   
            upper triangular part of the matrix A.  If UPLO = 'L',   
            the leading N-by-N lower triangular part of A contains   
            the lower triangular part of the matrix A.   
            On exit, the lower triangle (if UPLO='L') or the upper   
            triangle (if UPLO='U') of A, including the diagonal, is   
            destroyed.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,N).   

    VL      (input) REAL   
    VU      (input) REAL   
            If RANGE='V', the lower and upper bounds of the interval to   
            be searched for eigenvalues. VL < VU.   
            Not referenced if RANGE = 'A' or 'I'.   

    IL      (input) INTEGER   
    IU      (input) INTEGER   
            If RANGE='I', the indices (in ascending order) of the   
            smallest and largest eigenvalues to be returned.   
            1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.   
            Not referenced if RANGE = 'A' or 'V'.   

    ABSTOL  (input) REAL   
            The absolute error tolerance for the eigenvalues.   
            An approximate eigenvalue is accepted as converged   
            when it is determined to lie in an interval [a,b]   
            of width less than or equal to   

                    ABSTOL + EPS *   max( |a|,|b| ) ,   

            where EPS is the machine precision.  If ABSTOL is less than   
            or equal to zero, then  EPS*|T|  will be used in its place,   
            where |T| is the 1-norm of the tridiagonal matrix obtained   
            by reducing A to tridiagonal form.   

            See "Computing Small Singular Values of Bidiagonal Matrices   
            with Guaranteed High Relative Accuracy," by Demmel and   
            Kahan, LAPACK Working Note #3.   

            If high relative accuracy is important, set ABSTOL to   
            SLAMCH( 'Safe minimum' ).  Doing so will guarantee that   
            eigenvalues are computed to high relative accuracy when   
            possible in future releases.  The current code does not   
            make any guarantees about high relative accuracy, but   
            furutre releases will. See J. Barlow and J. Demmel,   
            "Computing Accurate Eigensystems of Scaled Diagonally   
            Dominant Matrices", LAPACK Working Note #7, for a discussion   
            of which matrices define their eigenvalues to high relative   
            accuracy.   

    M       (output) INTEGER   
            The total number of eigenvalues found.  0 <= M <= N.   
            If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.   

    W       (output) REAL array, dimension (N)   
            The first M elements contain the selected eigenvalues in   
            ascending order.   

    Z       (output) COMPLEX array, dimension (LDZ, max(1,M))   
            If JOBZ = 'V', then if INFO = 0, the first M columns of Z   
            contain the orthonormal eigenvectors of the matrix A   
            corresponding to the selected eigenvalues, with the i-th   
            column of Z holding the eigenvector associated with W(i).   
            If JOBZ = 'N', then Z is not referenced.   
            Note: the user must ensure that at least max(1,M) columns are   
            supplied in the array Z; if RANGE = 'V', the exact value of M   
            is not known in advance and an upper bound must be used.   

    LDZ     (input) INTEGER   
            The leading dimension of the array Z.  LDZ >= 1, and if   
            JOBZ = 'V', LDZ >= max(1,N).   

    ISUPPZ  (output) INTEGER ARRAY, dimension ( 2*max(1,M) )   
            The support of the eigenvectors in Z, i.e., the indices   
            indicating the nonzero elements in Z. The i-th eigenvector   
            is nonzero only in elements ISUPPZ( 2*i-1 ) through   
            ISUPPZ( 2*i ).   
   ********* Implemented only for RANGE = 'A' or 'I' and IU - IL = N - 1   

    WORK    (workspace/output) COMPLEX array, dimension (LWORK)   
            On exit, if INFO = 0, WORK(1) returns the optimal LWORK.   

    LWORK   (input) INTEGER   
            The length of the array WORK.  LWORK >= max(1,2*N).   
            For optimal efficiency, LWORK >= (NB+1)*N,   
            where NB is the max of the blocksize for CHETRD and for   
            CUNMTR as returned by ILAENV.   

            If LWORK = -1, then a workspace query is assumed; the routine   
            only calculates the optimal size of the WORK array, returns   
            this value as the first entry of the WORK array, and no error   
            message related to LWORK is issued by XERBLA.   

    RWORK   (workspace/output) REAL array, dimension (LRWORK)   
            On exit, if INFO = 0, RWORK(1) returns the optimal   
            (and minimal) LRWORK.   

   LRWORK  (input) INTEGER   
           The length of the array RWORK.  LRWORK >= max(1,24*N).   

           If LRWORK = -1, then a workspace query is assumed; the routine   
           only calculates the optimal size of the RWORK array, returns   
           this value as the first entry of the RWORK array, and no error   
           message related to LRWORK is issued by XERBLA.   

    IWORK   (workspace/output) INTEGER array, dimension (LIWORK)   
            On exit, if INFO = 0, IWORK(1) returns the optimal   
            (and minimal) LIWORK.   

   LIWORK  (input) INTEGER   
           The dimension of the array IWORK.  LIWORK >= max(1,10*N).   

           If LIWORK = -1, then a workspace query is assumed; the   
           routine only calculates the optimal size of the IWORK array,   
           returns this value as the first entry of the IWORK array, and   
           no error message related to LIWORK is issued by XERBLA.   

    INFO    (output) INTEGER   
            = 0:  successful exit   
            < 0:  if INFO = -i, the i-th argument had an illegal value   
            > 0:  Internal error   

    Further Details   
    ===============   

    Based on contributions by   
       Inderjit Dhillon, IBM Almaden, USA   
       Osni Marques, LBNL/NERSC, USA   
       Ken Stanley, Computer Science Division, University of   
         California at Berkeley, USA   

   =====================================================================   


       Test the input parameters.   

       Parameter adjustments */
    /* Table of constant values */
    static integer c__10 = 10;
    static integer c__1 = 1;
    static integer c__2 = 2;
    static integer c__3 = 3;
    static integer c__4 = 4;
    static integer c_n1 = -1;
    
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2;
    real r__1, r__2;
    /* Builtin functions */
    double sqrt(doublereal);
    /* Local variables */
    static real anrm;
    static integer imax;
    static real rmin, rmax;
    static integer itmp1, i__, j, indrd, indre;
    static real sigma;
    extern logical lsame_(char *, char *);
    static integer iinfo;
    extern /* Subroutine */ int sscal_(integer *, real *, real *, integer *);
    static char order[1];
    static integer indwk;
    extern /* Subroutine */ int cswap_(integer *, complex *, integer *, 
	    complex *, integer *);
    static integer lwmin;
    static logical lower;
    extern /* Subroutine */ int scopy_(integer *, real *, integer *, real *, 
	    integer *);
    static logical wantz;
    static integer nb, jj;
    static logical alleig, indeig;
    static integer iscale, ieeeok, indibl, indrdd, indifl, indree;
    static logical valeig;
    extern doublereal slamch_(char *);
    extern /* Subroutine */ int chetrd_(char *, integer *, complex *, integer 
	    *, real *, real *, complex *, complex *, integer *, integer *), csscal_(integer *, real *, complex *, integer *);
    static real safmin;
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *, 
	    integer *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int xerbla_(char *, integer *);
    static real abstll, bignum;
    static integer indtau, indisp;
    extern /* Subroutine */ int cstegr_(char *, char *, integer *, real *, 
	    real *, real *, real *, integer *, integer *, real *, integer *, 
	    real *, complex *, integer *, integer *, real *, integer *, 
	    integer *, integer *, integer *), cstein_(integer 
	    *, real *, real *, integer *, real *, integer *, integer *, 
	    complex *, integer *, real *, integer *, integer *, integer *);
    static integer indiwo, indwkn;
    extern doublereal clansy_(char *, char *, integer *, complex *, integer *,
	     real *);
    static integer indrwk, liwmin;
    extern /* Subroutine */ int ssterf_(integer *, real *, real *, integer *);
    static integer lrwmin, llwrkn, llwork, nsplit;
    static real smlnum;
    extern /* Subroutine */ int cunmtr_(char *, char *, char *, integer *, 
	    integer *, complex *, integer *, complex *, complex *, integer *, 
	    complex *, integer *, integer *), sstebz_(
	    char *, char *, integer *, real *, real *, integer *, integer *, 
	    real *, real *, real *, integer *, integer *, real *, integer *, 
	    integer *, real *, integer *, integer *);
    static logical lquery;
    static integer lwkopt;
    static real eps, vll, vuu, tmp1;
#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]
#define z___subscr(a_1,a_2) (a_2)*z_dim1 + a_1
#define z___ref(a_1,a_2) z__[z___subscr(a_1,a_2)]


    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --w;
    z_dim1 = *ldz;
    z_offset = 1 + z_dim1 * 1;
    z__ -= z_offset;
    --isuppz;
    --work;
    --rwork;
    --iwork;

    /* Function Body */
    ieeeok = ilaenv_(&c__10, "CHEEVR", "N", &c__1, &c__2, &c__3, &c__4, (
	    ftnlen)6, (ftnlen)1);

    lower = lsame_(uplo, "L");
    wantz = lsame_(jobz, "V");
    alleig = lsame_(range, "A");
    valeig = lsame_(range, "V");
    indeig = lsame_(range, "I");

    lquery = *lwork == -1 || *lrwork == -1 || *liwork == -1;

/* Computing MAX */
    i__1 = 1, i__2 = *n * 24;
    lrwmin = max(i__1,i__2);
/* Computing MAX */
    i__1 = 1, i__2 = *n * 10;
    liwmin = max(i__1,i__2);
/* Computing MAX */
    i__1 = 1, i__2 = *n << 1;
    lwmin = max(i__1,i__2);

    *info = 0;
    if (! (wantz || lsame_(jobz, "N"))) {
	*info = -1;
    } else if (! (alleig || valeig || indeig)) {
	*info = -2;
    } else if (! (lower || lsame_(uplo, "U"))) {
	*info = -3;
    } else if (*n < 0) {
	*info = -4;
    } else if (*lda < max(1,*n)) {
	*info = -6;
    } else {
	if (valeig) {
	    if (*n > 0 && *vu <= *vl) {
		*info = -8;
	    }
	} else if (indeig) {
	    if (*il < 1 || *il > max(1,*n)) {
		*info = -9;
	    } else if (*iu < min(*n,*il) || *iu > *n) {
		*info = -10;
	    }
	}
    }
    if (*info == 0) {
	if (*ldz < 1 || wantz && *ldz < *n) {
	    *info = -15;
	} else if (*lwork < lwmin && ! lquery) {
	    *info = -18;
	} else if (*lrwork < lrwmin && ! lquery) {
	    *info = -20;
	} else if (*liwork < liwmin && ! lquery) {
	    *info = -22;
	}
    }

    if (*info == 0) {
	nb = ilaenv_(&c__1, "CHETRD", uplo, n, &c_n1, &c_n1, &c_n1, (ftnlen)6,
		 (ftnlen)1);
/* Computing MAX */
	i__1 = nb, i__2 = ilaenv_(&c__1, "CUNMTR", uplo, n, &c_n1, &c_n1, &
		c_n1, (ftnlen)6, (ftnlen)1);
	nb = max(i__1,i__2);
/* Computing MAX */
	i__1 = (nb + 1) * *n;
	lwkopt = max(i__1,lwmin);
	work[1].r = (real) lwkopt, work[1].i = 0.f;
	rwork[1] = (real) lrwmin;
	iwork[1] = liwmin;
    }

    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("CHEEVR", &i__1);
	return 0;
    } else if (lquery) {
	return 0;
    }

/*     Quick return if possible */

    *m = 0;
    if (*n == 0) {
	work[1].r = 1.f, work[1].i = 0.f;
	return 0;
    }

    if (*n == 1) {
	work[1].r = 7.f, work[1].i = 0.f;
	if (alleig || indeig) {
	    *m = 1;
	    i__1 = a_subscr(1, 1);
	    w[1] = a[i__1].r;
	} else {
	    i__1 = a_subscr(1, 1);
	    i__2 = a_subscr(1, 1);
	    if (*vl < a[i__1].r && *vu >= a[i__2].r) {
		*m = 1;
		i__1 = a_subscr(1, 1);
		w[1] = a[i__1].r;
	    }
	}
	if (wantz) {
	    i__1 = z___subscr(1, 1);
	    z__[i__1].r = 1.f, z__[i__1].i = 0.f;
	}
	return 0;
    }

/*     Get machine constants. */

    safmin = slamch_("Safe minimum");
    eps = slamch_("Precision");
    smlnum = safmin / eps;
    bignum = 1.f / smlnum;
    rmin = sqrt(smlnum);
/* Computing MIN */
    r__1 = sqrt(bignum), r__2 = 1.f / sqrt(sqrt(safmin));
    rmax = dmin(r__1,r__2);

/*     Scale matrix to allowable range, if necessary. */

    iscale = 0;
    abstll = *abstol;
    vll = *vl;
    vuu = *vu;
    anrm = clansy_("M", uplo, n, &a[a_offset], lda, &rwork[1]);
    if (anrm > 0.f && anrm < rmin) {
	iscale = 1;
	sigma = rmin / anrm;
    } else if (anrm > rmax) {
	iscale = 1;
	sigma = rmax / anrm;
    }
    if (iscale == 1) {
	if (lower) {
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		i__2 = *n - j + 1;
		csscal_(&i__2, &sigma, &a_ref(j, j), &c__1);
/* L10: */
	    }
	} else {
	    i__1 = *n;
	    for (j = 1; j <= i__1; ++j) {
		csscal_(&j, &sigma, &a_ref(1, j), &c__1);
/* L20: */
	    }
	}
	if (*abstol > 0.f) {
	    abstll = *abstol * sigma;
	}
	if (valeig) {
	    vll = *vl * sigma;
	    vuu = *vu * sigma;
	}
    }

/*     Call CHETRD to reduce Hermitian matrix to tridiagonal form. */

    indtau = 1;
    indwk = indtau + *n;

    indre = 1;
    indrd = indre + *n;
    indree = indrd + *n;
    indrdd = indree + *n;
    indrwk = indrdd + *n;
    llwork = *lwork - indwk + 1;
    chetrd_(uplo, n, &a[a_offset], lda, &rwork[indrd], &rwork[indre], &work[
	    indtau], &work[indwk], &llwork, &iinfo);

/*     If all eigenvalues are desired   
       then call SSTERF or CSTEGR and CUNMTR. */

    if ((alleig || indeig && *il == 1 && *iu == *n) && ieeeok == 1) {
	if (! wantz) {
	    scopy_(n, &rwork[indrd], &c__1, &w[1], &c__1);
	    i__1 = *n - 1;
	    scopy_(&i__1, &rwork[indre], &c__1, &rwork[indree], &c__1);
	    ssterf_(n, &w[1], &rwork[indree], info);
	} else {
	    i__1 = *n - 1;
	    scopy_(&i__1, &rwork[indre], &c__1, &rwork[indree], &c__1);
	    scopy_(n, &rwork[indrd], &c__1, &rwork[indrdd], &c__1);

	    cstegr_(jobz, "A", n, &rwork[indrdd], &rwork[indree], vl, vu, il, 
		    iu, abstol, m, &w[1], &z__[z_offset], ldz, &isuppz[1], &
		    rwork[indrwk], lwork, &iwork[1], liwork, info);



/*        Apply unitary matrix used in reduction to tridiagonal   
          form to eigenvectors returned by CSTEIN. */

	    if (wantz && *info == 0) {
		indwkn = indwk;
		llwrkn = *lwork - indwkn + 1;
		cunmtr_("L", uplo, "N", n, m, &a[a_offset], lda, &work[indtau]
			, &z__[z_offset], ldz, &work[indwkn], &llwrkn, &iinfo);
	    }
	}


	if (*info == 0) {
	    *m = *n;
	    goto L30;
	}
	*info = 0;
    }

/*     Otherwise, call SSTEBZ and, if eigenvectors are desired, CSTEIN.   
       Also call SSTEBZ and CSTEIN if CSTEGR fails. */

    if (wantz) {
	*(unsigned char *)order = 'B';
    } else {
	*(unsigned char *)order = 'E';
    }
    indifl = 1;
    indibl = indifl + *n;
    indisp = indibl + *n;
    indiwo = indisp + *n;
    sstebz_(range, order, n, &vll, &vuu, il, iu, &abstll, &rwork[indrd], &
	    rwork[indre], m, &nsplit, &w[1], &iwork[indibl], &iwork[indisp], &
	    rwork[indrwk], &iwork[indiwo], info);

    if (wantz) {
	cstein_(n, &rwork[indrd], &rwork[indre], m, &w[1], &iwork[indibl], &
		iwork[indisp], &z__[z_offset], ldz, &rwork[indrwk], &iwork[
		indiwo], &iwork[indifl], info);

/*        Apply unitary matrix used in reduction to tridiagonal   
          form to eigenvectors returned by CSTEIN. */

	indwkn = indwk;
	llwrkn = *lwork - indwkn + 1;
	cunmtr_("L", uplo, "N", n, m, &a[a_offset], lda, &work[indtau], &z__[
		z_offset], ldz, &work[indwkn], &llwrkn, &iinfo);
    }

/*     If matrix was scaled, then rescale eigenvalues appropriately. */

L30:
    if (iscale == 1) {
	if (*info == 0) {
	    imax = *m;
	} else {
	    imax = *info - 1;
	}
	r__1 = 1.f / sigma;
	sscal_(&imax, &r__1, &w[1], &c__1);
    }

/*     If eigenvalues are not in order, then sort them, along with   
       eigenvectors. */

    if (wantz) {
	i__1 = *m - 1;
	for (j = 1; j <= i__1; ++j) {
	    i__ = 0;
	    tmp1 = w[j];
	    i__2 = *m;
	    for (jj = j + 1; jj <= i__2; ++jj) {
		if (w[jj] < tmp1) {
		    i__ = jj;
		    tmp1 = w[jj];
		}
/* L40: */
	    }

	    if (i__ != 0) {
		itmp1 = iwork[indibl + i__ - 1];
		w[i__] = w[j];
		iwork[indibl + i__ - 1] = iwork[indibl + j - 1];
		w[j] = tmp1;
		iwork[indibl + j - 1] = itmp1;
		cswap_(n, &z___ref(1, i__), &c__1, &z___ref(1, j), &c__1);
	    }
/* L50: */
	}
    }

/*     Set WORK(1) to optimal workspace size. */

    work[1].r = (real) lwkopt, work[1].i = 0.f;
    rwork[1] = (real) lrwmin;
    iwork[1] = liwmin;

    return 0;

/*     End of CHEEVR */

} /* cheevr_ */

#undef z___ref
#undef z___subscr
#undef a_ref
#undef a_subscr


