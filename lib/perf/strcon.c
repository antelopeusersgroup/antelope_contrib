#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int strcon_(char *norm, char *uplo, char *diag, integer *n, 
	real *a, integer *lda, real *rcond, real *work, integer *iwork, 
	integer *info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    STRCON estimates the reciprocal of the condition number of a   
    triangular matrix A, in either the 1-norm or the infinity-norm.   

    The norm of A is computed and an estimate is obtained for   
    norm(inv(A)), then the reciprocal of the condition number is   
    computed as   
       RCOND = 1 / ( norm(A) * norm(inv(A)) ).   

    Arguments   
    =========   

    NORM    (input) CHARACTER*1   
            Specifies whether the 1-norm condition number or the   
            infinity-norm condition number is required:   
            = '1' or 'O':  1-norm;   
            = 'I':         Infinity-norm.   

    UPLO    (input) CHARACTER*1   
            = 'U':  A is upper triangular;   
            = 'L':  A is lower triangular.   

    DIAG    (input) CHARACTER*1   
            = 'N':  A is non-unit triangular;   
            = 'U':  A is unit triangular.   

    N       (input) INTEGER   
            The order of the matrix A.  N >= 0.   

    A       (input) REAL array, dimension (LDA,N)   
            The triangular matrix A.  If UPLO = 'U', the leading N-by-N   
            upper triangular part of the array A contains the upper   
            triangular matrix, and the strictly lower triangular part of   
            A is not referenced.  If UPLO = 'L', the leading N-by-N lower   
            triangular part of the array A contains the lower triangular   
            matrix, and the strictly upper triangular part of A is not   
            referenced.  If DIAG = 'U', the diagonal elements of A are   
            also not referenced and are assumed to be 1.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max(1,N).   

    RCOND   (output) REAL   
            The reciprocal of the condition number of the matrix A,   
            computed as RCOND = 1/(norm(A) * norm(inv(A))).   

    WORK    (workspace) REAL array, dimension (3*N)   

    IWORK   (workspace) INTEGER array, dimension (N)   

    INFO    (output) INTEGER   
            = 0:  successful exit   
            < 0:  if INFO = -i, the i-th argument had an illegal value   

    =====================================================================   


       Test the input parameters.   

       Parameter adjustments */
    /* Table of constant values */
    static integer c__1 = 1;
    
    /* System generated locals */
    integer a_dim1, a_offset, i__1;
    real r__1;
    /* Local variables */
    static integer kase, kase1;
    static real scale;
    extern logical lsame_(char *, char *);
    static real anorm;
    extern /* Subroutine */ int srscl_(integer *, real *, real *, integer *);
    static logical upper;
    static real xnorm;
    static integer ix;
    extern doublereal slamch_(char *);
    extern /* Subroutine */ int xerbla_(char *, integer *), slacon_(
	    integer *, real *, real *, integer *, real *, integer *);
    extern integer isamax_(integer *, real *, integer *);
    static real ainvnm;
    static logical onenrm;
    static char normin[1];
    extern doublereal slantr_(char *, char *, char *, integer *, integer *, 
	    real *, integer *, real *);
    extern /* Subroutine */ int slatrs_(char *, char *, char *, char *, 
	    integer *, real *, integer *, real *, real *, real *, integer *);
    static real smlnum;
    static logical nounit;


    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --work;
    --iwork;

    /* Function Body */
    *info = 0;
    upper = lsame_(uplo, "U");
    onenrm = *(unsigned char *)norm == '1' || lsame_(norm, "O");
    nounit = lsame_(diag, "N");

    if (! onenrm && ! lsame_(norm, "I")) {
	*info = -1;
    } else if (! upper && ! lsame_(uplo, "L")) {
	*info = -2;
    } else if (! nounit && ! lsame_(diag, "U")) {
	*info = -3;
    } else if (*n < 0) {
	*info = -4;
    } else if (*lda < max(1,*n)) {
	*info = -6;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("STRCON", &i__1);
	return 0;
    }

/*     Quick return if possible */

    if (*n == 0) {
	*rcond = 1.f;
	return 0;
    }

    *rcond = 0.f;
    smlnum = slamch_("Safe minimum") * (real) max(1,*n);

/*     Compute the norm of the triangular matrix A. */

    anorm = slantr_(norm, uplo, diag, n, n, &a[a_offset], lda, &work[1]);

/*     Continue only if ANORM > 0. */

    if (anorm > 0.f) {

/*        Estimate the norm of the inverse of A. */

	ainvnm = 0.f;
	*(unsigned char *)normin = 'N';
	if (onenrm) {
	    kase1 = 1;
	} else {
	    kase1 = 2;
	}
	kase = 0;
L10:
	slacon_(n, &work[*n + 1], &work[1], &iwork[1], &ainvnm, &kase);
	if (kase != 0) {
	    if (kase == kase1) {

/*              Multiply by inv(A). */

		slatrs_(uplo, "No transpose", diag, normin, n, &a[a_offset], 
			lda, &work[1], &scale, &work[(*n << 1) + 1], info);
	    } else {

/*              Multiply by inv(A'). */

		slatrs_(uplo, "Transpose", diag, normin, n, &a[a_offset], lda,
			 &work[1], &scale, &work[(*n << 1) + 1], info);
	    }
	    *(unsigned char *)normin = 'Y';

/*           Multiply by 1/SCALE if doing so will not cause overflow. */

	    if (scale != 1.f) {
		ix = isamax_(n, &work[1], &c__1);
		xnorm = (r__1 = work[ix], dabs(r__1));
		if (scale < xnorm * smlnum || scale == 0.f) {
		    goto L20;
		}
		srscl_(n, &scale, &work[1], &c__1);
	    }
	    goto L10;
	}

/*        Compute the estimate of the reciprocal condition number. */

	if (ainvnm != 0.f) {
	    *rcond = 1.f / anorm / ainvnm;
	}
    }

L20:
    return 0;

/*     End of STRCON */

} /* strcon_ */

