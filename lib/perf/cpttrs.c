#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int cpttrs_(char *uplo, integer *n, integer *nrhs, real *d__,
	 complex *e, complex *b, integer *ldb, integer *info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    CPTTRS solves a tridiagonal system of the form   
       A * X = B   
    using the factorization A = U'*D*U or A = L*D*L' computed by CPTTRF.   
    D is a diagonal matrix specified in the vector D, U (or L) is a unit   
    bidiagonal matrix whose superdiagonal (subdiagonal) is specified in   
    the vector E, and X and B are N by NRHS matrices.   

    Arguments   
    =========   

    UPLO    (input) CHARACTER*1   
            Specifies the form of the factorization and whether the   
            vector E is the superdiagonal of the upper bidiagonal factor   
            U or the subdiagonal of the lower bidiagonal factor L.   
            = 'U':  A = U'*D*U, E is the superdiagonal of U   
            = 'L':  A = L*D*L', E is the subdiagonal of L   

    N       (input) INTEGER   
            The order of the tridiagonal matrix A.  N >= 0.   

    NRHS    (input) INTEGER   
            The number of right hand sides, i.e., the number of columns   
            of the matrix B.  NRHS >= 0.   

    D       (input) REAL array, dimension (N)   
            The n diagonal elements of the diagonal matrix D from the   
            factorization A = U'*D*U or A = L*D*L'.   

    E       (input) COMPLEX array, dimension (N-1)   
            If UPLO = 'U', the (n-1) superdiagonal elements of the unit   
            bidiagonal factor U from the factorization A = U'*D*U.   
            If UPLO = 'L', the (n-1) subdiagonal elements of the unit   
            bidiagonal factor L from the factorization A = L*D*L'.   

    B       (input/output) REAL array, dimension (LDB,NRHS)   
            On entry, the right hand side vectors B for the system of   
            linear equations.   
            On exit, the solution vectors, X.   

    LDB     (input) INTEGER   
            The leading dimension of the array B.  LDB >= max(1,N).   

    INFO    (output) INTEGER   
            = 0: successful exit   
            < 0: if INFO = -k, the k-th argument had an illegal value   

    =====================================================================   


       Test the input arguments.   

       Parameter adjustments */
    /* Table of constant values */
    static integer c__1 = 1;
    static integer c_n1 = -1;
    
    /* System generated locals */
    integer b_dim1, b_offset, i__1, i__2, i__3;
    /* Local variables */
    static integer j, iuplo;
    static logical upper;
    static integer jb;
    extern /* Subroutine */ int cptts2_(integer *, integer *, integer *, real 
	    *, complex *, complex *, integer *);
    static integer nb;
    extern /* Subroutine */ int xerbla_(char *, integer *);
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *, 
	    integer *, integer *, ftnlen, ftnlen);
#define b_subscr(a_1,a_2) (a_2)*b_dim1 + a_1
#define b_ref(a_1,a_2) b[b_subscr(a_1,a_2)]


    --d__;
    --e;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;

    /* Function Body */
    *info = 0;
    upper = *(unsigned char *)uplo == 'U' || *(unsigned char *)uplo == 'u';
    if (! upper && ! (*(unsigned char *)uplo == 'L' || *(unsigned char *)uplo 
	    == 'l')) {
	*info = -1;
    } else if (*n < 0) {
	*info = -2;
    } else if (*nrhs < 0) {
	*info = -3;
    } else if (*ldb < max(1,*n)) {
	*info = -7;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("CPTTRS", &i__1);
	return 0;
    }

/*     Quick return if possible */

    if (*n == 0 || *nrhs == 0) {
	return 0;
    }

/*     Determine the number of right-hand sides to solve at a time. */

    if (*nrhs == 1) {
	nb = 1;
    } else {
/* Computing MAX */
	i__1 = 1, i__2 = ilaenv_(&c__1, "CPTTRS", uplo, n, nrhs, &c_n1, &c_n1,
		 (ftnlen)6, (ftnlen)1);
	nb = max(i__1,i__2);
    }

/*     Decode UPLO */

    if (upper) {
	iuplo = 1;
    } else {
	iuplo = 0;
    }

    if (nb >= *nrhs) {
	cptts2_(&iuplo, n, nrhs, &d__[1], &e[1], &b[b_offset], ldb);
    } else {
	i__1 = *nrhs;
	i__2 = nb;
	for (j = 1; i__2 < 0 ? j >= i__1 : j <= i__1; j += i__2) {
/* Computing MIN */
	    i__3 = *nrhs - j + 1;
	    jb = min(i__3,nb);
	    cptts2_(&iuplo, n, &jb, &d__[1], &e[1], &b_ref(1, j), ldb);
/* L10: */
	}
    }

    return 0;

/*     End of CPTTRS */

} /* cpttrs_ */

#undef b_ref
#undef b_subscr


