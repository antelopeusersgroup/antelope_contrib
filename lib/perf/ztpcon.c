#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int ztpcon_(char *norm, char *uplo, char *diag, integer *n, 
	doublecomplex *ap, doublereal *rcond, doublecomplex *work, doublereal 
	*rwork, integer *info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    ZTPCON estimates the reciprocal of the condition number of a packed   
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

    AP      (input) COMPLEX*16 array, dimension (N*(N+1)/2)   
            The upper or lower triangular matrix A, packed columnwise in   
            a linear array.  The j-th column of A is stored in the array   
            AP as follows:   
            if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;   
            if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.   
            If DIAG = 'U', the diagonal elements of A are not referenced   
            and are assumed to be 1.   

    RCOND   (output) DOUBLE PRECISION   
            The reciprocal of the condition number of the matrix A,   
            computed as RCOND = 1/(norm(A) * norm(inv(A))).   

    WORK    (workspace) COMPLEX*16 array, dimension (2*N)   

    RWORK   (workspace) DOUBLE PRECISION array, dimension (N)   

    INFO    (output) INTEGER   
            = 0:  successful exit   
            < 0:  if INFO = -i, the i-th argument had an illegal value   

    =====================================================================   


       Test the input parameters.   

       Parameter adjustments */
    /* Table of constant values */
    static integer c__1 = 1;
    
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;
    /* Builtin functions */
    double d_imag(doublecomplex *);
    /* Local variables */
    static integer kase, kase1;
    static doublereal scale;
    extern logical lsame_(char *, char *);
    static doublereal anorm;
    static logical upper;
    static doublereal xnorm;
    extern doublereal dlamch_(char *);
    static integer ix;
    extern /* Subroutine */ int xerbla_(char *, integer *), zlacon_(
	    integer *, doublecomplex *, doublecomplex *, doublereal *, 
	    integer *);
    static doublereal ainvnm;
    extern integer izamax_(integer *, doublecomplex *, integer *);
    static logical onenrm;
    extern /* Subroutine */ int zdrscl_(integer *, doublereal *, 
	    doublecomplex *, integer *);
    static char normin[1];
    extern doublereal zlantp_(char *, char *, char *, integer *, 
	    doublecomplex *, doublereal *);
    static doublereal smlnum;
    static logical nounit;
    extern /* Subroutine */ int zlatps_(char *, char *, char *, char *, 
	    integer *, doublecomplex *, doublecomplex *, doublereal *, 
	    doublereal *, integer *);


    --rwork;
    --work;
    --ap;

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
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("ZTPCON", &i__1);
	return 0;
    }

/*     Quick return if possible */

    if (*n == 0) {
	*rcond = 1.;
	return 0;
    }

    *rcond = 0.;
    smlnum = dlamch_("Safe minimum") * (doublereal) max(1,*n);

/*     Compute the norm of the triangular matrix A. */

    anorm = zlantp_(norm, uplo, diag, n, &ap[1], &rwork[1]);

/*     Continue only if ANORM > 0. */

    if (anorm > 0.) {

/*        Estimate the norm of the inverse of A. */

	ainvnm = 0.;
	*(unsigned char *)normin = 'N';
	if (onenrm) {
	    kase1 = 1;
	} else {
	    kase1 = 2;
	}
	kase = 0;
L10:
	zlacon_(n, &work[*n + 1], &work[1], &ainvnm, &kase);
	if (kase != 0) {
	    if (kase == kase1) {

/*              Multiply by inv(A). */

		zlatps_(uplo, "No transpose", diag, normin, n, &ap[1], &work[
			1], &scale, &rwork[1], info);
	    } else {

/*              Multiply by inv(A'). */

		zlatps_(uplo, "Conjugate transpose", diag, normin, n, &ap[1], 
			&work[1], &scale, &rwork[1], info);
	    }
	    *(unsigned char *)normin = 'Y';

/*           Multiply by 1/SCALE if doing so will not cause overflow. */

	    if (scale != 1.) {
		ix = izamax_(n, &work[1], &c__1);
		i__1 = ix;
		xnorm = (d__1 = work[i__1].r, abs(d__1)) + (d__2 = d_imag(&
			work[ix]), abs(d__2));
		if (scale < xnorm * smlnum || scale == 0.) {
		    goto L20;
		}
		zdrscl_(n, &scale, &work[1], &c__1);
	    }
	    goto L10;
	}

/*        Compute the estimate of the reciprocal condition number. */

	if (ainvnm != 0.) {
	    *rcond = 1. / anorm / ainvnm;
	}
    }

L20:
    return 0;

/*     End of ZTPCON */

} /* ztpcon_ */

