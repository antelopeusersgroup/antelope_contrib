#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static complex c_b1 = {0.f,0.f};
static integer c__1 = 1;

/* Subroutine */ int cstegr_(char *jobz, char *range, integer *n, real *d__, 
	real *e, real *vl, real *vu, integer *il, integer *iu, real *abstol, 
	integer *m, real *w, complex *z__, integer *ldz, integer *isuppz, 
	real *work, integer *lwork, integer *iwork, integer *liwork, integer *
	info)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer iend;
    static real rmin, rmax;
    static integer itmp;
    static real tnrm;
    static integer i__, j;
    static real scale;
    extern logical lsame_(char *, char *);
    static integer iinfo;
    extern /* Subroutine */ int sscal_(integer *, real *, real *, integer *), 
	    cswap_(integer *, complex *, integer *, complex *, integer *);
    static integer lwmin;
    static logical wantz;
    static integer jj;
    static logical alleig, indeig;
    static integer ibegin, iindbl;
    static logical valeig;
    extern doublereal slamch_(char *);
    extern /* Subroutine */ int claset_(char *, integer *, integer *, complex 
	    *, complex *, complex *, integer *);
    static real safmin;
    extern /* Subroutine */ int xerbla_(char *, integer *);
    static real bignum;
    static integer iindwk, indgrs, indwof;
    extern /* Subroutine */ int clarrv_(integer *, real *, real *, integer *, 
	    integer *, real *, integer *, real *, real *, complex *, integer *
	    , integer *, real *, integer *, integer *), slarre_(integer *, 
	    real *, real *, real *, integer *, integer *, integer *, real *, 
	    real *, real *, real *, integer *);
    static real thresh;
    static integer iinspl, indwrk, liwmin;
    extern doublereal slanst_(char *, integer *, real *, real *);
    static integer nsplit;
    static real smlnum;
    static logical lquery;
    static real eps, tol, tmp;


#define z___subscr(a_1,a_2) (a_2)*z_dim1 + a_1
#define z___ref(a_1,a_2) z__[z___subscr(a_1,a_2)]


/*  -- LAPACK computational routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       October 31, 1999   


    Purpose   
    =======   

   CSTEGR computes selected eigenvalues and, optionally, eigenvectors   
   of a real symmetric tridiagonal matrix T.  Eigenvalues and   
   eigenvectors can be selected by specifying either a range of values   
   or a range of indices for the desired eigenvalues. The eigenvalues   
   are computed by the dqds algorithm, while orthogonal eigenvectors are   
   computed from various ``good'' L D L^T representations (also known as   
   Relatively Robust Representations). Gram-Schmidt orthogonalization is   
   avoided as far as possible. More specifically, the various steps of   
   the algorithm are as follows. For the i-th unreduced block of T,   
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
    Computer Science Division Technical Report No. UCB/CSD-97-971,   
    UC Berkeley, May 1997.   

    Note 1 : Currently CSTEGR is only set up to find ALL the n   
    eigenvalues and eigenvectors of T in O(n^2) time   
    Note 2 : Currently the routine CSTEIN is called when an appropriate   
    sigma_i cannot be chosen in step (c) above. CSTEIN invokes modified   
    Gram-Schmidt when eigenvalues are close.   
    Note 3 : CSTEGR works only on machines which follow ieee-754   
    floating-point standard in their handling of infinities and NaNs.   
    Normal execution of CSTEGR may create NaNs and infinities and hence   
    may abort due to a floating point exception in environments which   
    do not conform to the ieee standard.   

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
   ********* Only RANGE = 'A' is currently supported *********************   

    N       (input) INTEGER   
            The order of the matrix.  N >= 0.   

    D       (input/output) REAL array, dimension (N)   
            On entry, the n diagonal elements of the tridiagonal matrix   
            T. On exit, D is overwritten.   

    E       (input/output) REAL array, dimension (N)   
            On entry, the (n-1) subdiagonal elements of the tridiagonal   
            matrix T in elements 1 to N-1 of E; E(N) need not be set.   
            On exit, E is overwritten.   

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
            The absolute error tolerance for the   
            eigenvalues/eigenvectors. IF JOBZ = 'V', the eigenvalues and   
            eigenvectors output have residual norms bounded by ABSTOL,   
            and the dot products between different eigenvectors are   
            bounded by ABSTOL. If ABSTOL is less than N*EPS*|T|, then   
            N*EPS*|T| will be used in its place, where EPS is the   
            machine precision and |T| is the 1-norm of the tridiagonal   
            matrix. The eigenvalues are computed to an accuracy of   
            EPS*|T| irrespective of ABSTOL. If high relative accuracy   
            is important, set ABSTOL to DLAMCH( 'Safe minimum' ).   
            See Barlow and Demmel "Computing Accurate Eigensystems of   
            Scaled Diagonally Dominant Matrices", LAPACK Working Note #7   
            for a discussion of which matrices define their eigenvalues   
            to high relative accuracy.   

    M       (output) INTEGER   
            The total number of eigenvalues found.  0 <= M <= N.   
            If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.   

    W       (output) REAL array, dimension (N)   
            The first M elements contain the selected eigenvalues in   
            ascending order.   

    Z       (output) COMPLEX array, dimension (LDZ, max(1,M) )   
            If JOBZ = 'V', then if INFO = 0, the first M columns of Z   
            contain the orthonormal eigenvectors of the matrix T   
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

    WORK    (workspace/output) REAL array, dimension (LWORK)   
            On exit, if INFO = 0, WORK(1) returns the optimal   
            (and minimal) LWORK.   

    LWORK   (input) INTEGER   
            The dimension of the array WORK.  LWORK >= max(1,18*N)   

            If LWORK = -1, then a workspace query is assumed; the routine   
            only calculates the optimal size of the WORK array, returns   
            this value as the first entry of the WORK array, and no error   
            message related to LWORK is issued by XERBLA.   

    IWORK   (workspace/output) INTEGER array, dimension (LIWORK)   
            On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.   

    LIWORK  (input) INTEGER   
            The dimension of the array IWORK.  LIWORK >= max(1,10*N)   

            If LIWORK = -1, then a workspace query is assumed; the   
            routine only calculates the optimal size of the IWORK array,   
            returns this value as the first entry of the IWORK array, and   
            no error message related to LIWORK is issued by XERBLA.   

    INFO    (output) INTEGER   
            = 0:  successful exit   
            < 0:  if INFO = -i, the i-th argument had an illegal value   
            > 0:  if INFO = 1, internal error in SLARRE,   
                  if INFO = 2, internal error in CLARRV.   

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
    --d__;
    --e;
    --w;
    z_dim1 = *ldz;
    z_offset = 1 + z_dim1 * 1;
    z__ -= z_offset;
    --isuppz;
    --work;
    --iwork;

    /* Function Body */
    wantz = lsame_(jobz, "V");
    alleig = lsame_(range, "A");
    valeig = lsame_(range, "V");
    indeig = lsame_(range, "I");

    lquery = *lwork == -1 || *liwork == -1;
    lwmin = *n * 18;
    liwmin = *n * 10;

    *info = 0;
    if (! (wantz || lsame_(jobz, "N"))) {
	*info = -1;
    } else if (! (alleig || valeig || indeig)) {
	*info = -2;

/*     The following two lines need to be removed once the   
       RANGE = 'V' and RANGE = 'I' options are provided. */

    } else if (valeig || indeig) {
	*info = -2;
    } else if (*n < 0) {
	*info = -3;
    } else if (valeig && *n > 0 && *vu <= *vl) {
	*info = -7;
    } else if (indeig && *il < 1) {
	*info = -8;
/*     The following change should be made in DSTEVX also, otherwise   
       IL can be specified as N+1 and IU as N.   
       ELSE IF( INDEIG .AND. ( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) ) THEN */
    } else if (indeig && (*iu < *il || *iu > *n)) {
	*info = -9;
    } else if (*ldz < 1 || wantz && *ldz < *n) {
	*info = -14;
    } else if (*lwork < lwmin && ! lquery) {
	*info = -17;
    } else if (*liwork < liwmin && ! lquery) {
	*info = -19;
    }
    if (*info == 0) {
	work[1] = (real) lwmin;
	iwork[1] = liwmin;
    }

    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("CSTEGR", &i__1);
	return 0;
    } else if (lquery) {
	return 0;
    }

/*     Quick return if possible */

    *m = 0;
    if (*n == 0) {
	return 0;
    }

    if (*n == 1) {
	if (alleig || indeig) {
	    *m = 1;
	    w[1] = d__[1];
	} else {
	    if (*vl < d__[1] && *vu >= d__[1]) {
		*m = 1;
		w[1] = d__[1];
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

    scale = 1.f;
    tnrm = slanst_("M", n, &d__[1], &e[1]);
    if (tnrm > 0.f && tnrm < rmin) {
	scale = rmin / tnrm;
    } else if (tnrm > rmax) {
	scale = rmax / tnrm;
    }
    if (scale != 1.f) {
	sscal_(n, &scale, &d__[1], &c__1);
	i__1 = *n - 1;
	sscal_(&i__1, &scale, &e[1], &c__1);
	tnrm *= scale;
    }
    indgrs = 1;
    indwof = (*n << 1) + 1;
    indwrk = *n * 3 + 1;

    iinspl = 1;
    iindbl = *n + 1;
    iindwk = (*n << 1) + 1;

    claset_("Full", n, n, &c_b1, &c_b1, &z__[z_offset], ldz);

/*     Compute the desired eigenvalues of the tridiagonal after splitting   
       into smaller subblocks if the corresponding of-diagonal elements   
       are small */

    thresh = eps * tnrm;
    slarre_(n, &d__[1], &e[1], &thresh, &nsplit, &iwork[iinspl], m, &w[1], &
	    work[indwof], &work[indgrs], &work[indwrk], &iinfo);
    if (iinfo != 0) {
	*info = 1;
	return 0;
    }

    if (wantz) {

/*        Compute the desired eigenvectors corresponding to the computed   
          eigenvalues   

   Computing MAX */
	r__1 = *abstol, r__2 = (real) (*n) * thresh;
	tol = dmax(r__1,r__2);
	ibegin = 1;
	i__1 = nsplit;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    iend = iwork[iinspl + i__ - 1];
	    i__2 = iend;
	    for (j = ibegin; j <= i__2; ++j) {
		iwork[iindbl + j - 1] = i__;
/* L10: */
	    }
	    ibegin = iend + 1;
/* L20: */
	}

	clarrv_(n, &d__[1], &e[1], &iwork[iinspl], m, &w[1], &iwork[iindbl], &
		work[indgrs], &tol, &z__[z_offset], ldz, &isuppz[1], &work[
		indwrk], &iwork[iindwk], &iinfo);
	if (iinfo != 0) {
	    *info = 2;
	    return 0;
	}

    }

    ibegin = 1;
    i__1 = nsplit;
    for (i__ = 1; i__ <= i__1; ++i__) {
	iend = iwork[iinspl + i__ - 1];
	i__2 = iend;
	for (j = ibegin; j <= i__2; ++j) {
	    w[j] += work[indwof + i__ - 1];
/* L30: */
	}
	ibegin = iend + 1;
/* L40: */
    }

/*     If matrix was scaled, then rescale eigenvalues appropriately. */

    if (scale != 1.f) {
	r__1 = 1.f / scale;
	sscal_(m, &r__1, &w[1], &c__1);
    }

/*     If eigenvalues are not in order, then sort them, along with   
       eigenvectors. */

    if (nsplit > 1) {
	i__1 = *m - 1;
	for (j = 1; j <= i__1; ++j) {
	    i__ = 0;
	    tmp = w[j];
	    i__2 = *m;
	    for (jj = j + 1; jj <= i__2; ++jj) {
		if (w[jj] < tmp) {
		    i__ = jj;
		    tmp = w[jj];
		}
/* L50: */
	    }
	    if (i__ != 0) {
		w[i__] = w[j];
		w[j] = tmp;
		if (wantz) {
		    cswap_(n, &z___ref(1, i__), &c__1, &z___ref(1, j), &c__1);
		    itmp = isuppz[(i__ << 1) - 1];
		    isuppz[(i__ << 1) - 1] = isuppz[(j << 1) - 1];
		    isuppz[(j << 1) - 1] = itmp;
		    itmp = isuppz[i__ * 2];
		    isuppz[i__ * 2] = isuppz[j * 2];
		    isuppz[j * 2] = itmp;
		}
	    }
/* L60: */
	}
    }

    work[1] = (real) lwmin;
    iwork[1] = liwmin;
    return 0;

/*     End of CSTEGR */

} /* cstegr_ */

#undef z___ref
#undef z___subscr


