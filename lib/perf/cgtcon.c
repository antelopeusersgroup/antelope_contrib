#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int cgtcon_(char *norm, integer *n, complex *dl, complex *
	d__, complex *du, complex *du2, integer *ipiv, real *anorm, real *
	rcond, complex *work, integer *info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    CGTCON estimates the reciprocal of the condition number of a complex   
    tridiagonal matrix A using the LU factorization as computed by   
    CGTTRF.   

    An estimate is obtained for norm(inv(A)), and the reciprocal of the   
    condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).   

    Arguments   
    =========   

    NORM    (input) CHARACTER*1   
            Specifies whether the 1-norm condition number or the   
            infinity-norm condition number is required:   
            = '1' or 'O':  1-norm;   
            = 'I':         Infinity-norm.   

    N       (input) INTEGER   
            The order of the matrix A.  N >= 0.   

    DL      (input) COMPLEX array, dimension (N-1)   
            The (n-1) multipliers that define the matrix L from the   
            LU factorization of A as computed by CGTTRF.   

    D       (input) COMPLEX array, dimension (N)   
            The n diagonal elements of the upper triangular matrix U from   
            the LU factorization of A.   

    DU      (input) COMPLEX array, dimension (N-1)   
            The (n-1) elements of the first superdiagonal of U.   

    DU2     (input) COMPLEX array, dimension (N-2)   
            The (n-2) elements of the second superdiagonal of U.   

    IPIV    (input) INTEGER array, dimension (N)   
            The pivot indices; for 1 <= i <= n, row i of the matrix was   
            interchanged with row IPIV(i).  IPIV(i) will always be either   
            i or i+1; IPIV(i) = i indicates a row interchange was not   
            required.   

    ANORM   (input) REAL   
            If NORM = '1' or 'O', the 1-norm of the original matrix A.   
            If NORM = 'I', the infinity-norm of the original matrix A.   

    RCOND   (output) REAL   
            The reciprocal of the condition number of the matrix A,   
            computed as RCOND = 1/(ANORM * AINVNM), where AINVNM is an   
            estimate of the 1-norm of inv(A) computed in this routine.   

    WORK    (workspace) COMPLEX array, dimension (2*N)   

    INFO    (output) INTEGER   
            = 0:  successful exit   
            < 0:  if INFO = -i, the i-th argument had an illegal value   

    =====================================================================   


       Test the input arguments.   

       Parameter adjustments */
    /* Table of constant values */
    static integer c__1 = 1;
    
    /* System generated locals */
    integer i__1, i__2;
    /* Local variables */
    static integer kase, kase1, i__;
    extern logical lsame_(char *, char *);
    extern /* Subroutine */ int clacon_(integer *, complex *, complex *, real 
	    *, integer *), xerbla_(char *, integer *);
    static real ainvnm;
    static logical onenrm;
    extern /* Subroutine */ int cgttrs_(char *, integer *, integer *, complex 
	    *, complex *, complex *, complex *, integer *, complex *, integer 
	    *, integer *);


    --work;
    --ipiv;
    --du2;
    --du;
    --d__;
    --dl;

    /* Function Body */
    *info = 0;
    onenrm = *(unsigned char *)norm == '1' || lsame_(norm, "O");
    if (! onenrm && ! lsame_(norm, "I")) {
	*info = -1;
    } else if (*n < 0) {
	*info = -2;
    } else if (*anorm < 0.f) {
	*info = -8;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("CGTCON", &i__1);
	return 0;
    }

/*     Quick return if possible */

    *rcond = 0.f;
    if (*n == 0) {
	*rcond = 1.f;
	return 0;
    } else if (*anorm == 0.f) {
	return 0;
    }

/*     Check that D(1:N) is non-zero. */

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = i__;
	if (d__[i__2].r == 0.f && d__[i__2].i == 0.f) {
	    return 0;
	}
/* L10: */
    }

    ainvnm = 0.f;
    if (onenrm) {
	kase1 = 1;
    } else {
	kase1 = 2;
    }
    kase = 0;
L20:
    clacon_(n, &work[*n + 1], &work[1], &ainvnm, &kase);
    if (kase != 0) {
	if (kase == kase1) {

/*           Multiply by inv(U)*inv(L). */

	    cgttrs_("No transpose", n, &c__1, &dl[1], &d__[1], &du[1], &du2[1]
		    , &ipiv[1], &work[1], n, info);
	} else {

/*           Multiply by inv(L')*inv(U'). */

	    cgttrs_("Conjugate transpose", n, &c__1, &dl[1], &d__[1], &du[1], 
		    &du2[1], &ipiv[1], &work[1], n, info);
	}
	goto L20;
    }

/*     Compute the estimate of the reciprocal condition number. */

    if (ainvnm != 0.f) {
	*rcond = 1.f / ainvnm / *anorm;
    }

    return 0;

/*     End of CGTCON */

} /* cgtcon_ */

