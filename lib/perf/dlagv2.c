#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int dlagv2_(doublereal *a, integer *lda, doublereal *b, 
	integer *ldb, doublereal *alphar, doublereal *alphai, doublereal *
	beta, doublereal *csl, doublereal *snl, doublereal *csr, doublereal *
	snr)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    DLAGV2 computes the Generalized Schur factorization of a real 2-by-2   
    matrix pencil (A,B) where B is upper triangular. This routine   
    computes orthogonal (rotation) matrices given by CSL, SNL and CSR,   
    SNR such that   

    1) if the pencil (A,B) has two real eigenvalues (include 0/0 or 1/0   
       types), then   

       [ a11 a12 ] := [  CSL  SNL ] [ a11 a12 ] [  CSR -SNR ]   
       [  0  a22 ]    [ -SNL  CSL ] [ a21 a22 ] [  SNR  CSR ]   

       [ b11 b12 ] := [  CSL  SNL ] [ b11 b12 ] [  CSR -SNR ]   
       [  0  b22 ]    [ -SNL  CSL ] [  0  b22 ] [  SNR  CSR ],   

    2) if the pencil (A,B) has a pair of complex conjugate eigenvalues,   
       then   

       [ a11 a12 ] := [  CSL  SNL ] [ a11 a12 ] [  CSR -SNR ]   
       [ a21 a22 ]    [ -SNL  CSL ] [ a21 a22 ] [  SNR  CSR ]   

       [ b11  0  ] := [  CSL  SNL ] [ b11 b12 ] [  CSR -SNR ]   
       [  0  b22 ]    [ -SNL  CSL ] [  0  b22 ] [  SNR  CSR ]   

       where b11 >= b22 > 0.   


    Arguments   
    =========   

    A       (input/output) DOUBLE PRECISION array, dimension (LDA, 2)   
            On entry, the 2 x 2 matrix A.   
            On exit, A is overwritten by the ``A-part'' of the   
            generalized Schur form.   

    LDA     (input) INTEGER   
            THe leading dimension of the array A.  LDA >= 2.   

    B       (input/output) DOUBLE PRECISION array, dimension (LDB, 2)   
            On entry, the upper triangular 2 x 2 matrix B.   
            On exit, B is overwritten by the ``B-part'' of the   
            generalized Schur form.   

    LDB     (input) INTEGER   
            THe leading dimension of the array B.  LDB >= 2.   

    ALPHAR  (output) DOUBLE PRECISION array, dimension (2)   
    ALPHAI  (output) DOUBLE PRECISION array, dimension (2)   
    BETA    (output) DOUBLE PRECISION array, dimension (2)   
            (ALPHAR(k)+i*ALPHAI(k))/BETA(k) are the eigenvalues of the   
            pencil (A,B), k=1,2, i = sqrt(-1).  Note that BETA(k) may   
            be zero.   

    CSL     (output) DOUBLE PRECISION   
            The cosine of the left rotation matrix.   

    SNL     (output) DOUBLE PRECISION   
            The sine of the left rotation matrix.   

    CSR     (output) DOUBLE PRECISION   
            The cosine of the right rotation matrix.   

    SNR     (output) DOUBLE PRECISION   
            The sine of the right rotation matrix.   

    Further Details   
    ===============   

    Based on contributions by   
       Mark Fahey, Department of Mathematics, Univ. of Kentucky, USA   

    =====================================================================   


       Parameter adjustments */
    /* Table of constant values */
    static integer c__2 = 2;
    static integer c__1 = 1;
    
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset;
    doublereal d__1, d__2, d__3, d__4, d__5, d__6;
    /* Local variables */
    extern /* Subroutine */ int drot_(integer *, doublereal *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *), dlag2_(
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    static doublereal r__, t, anorm, bnorm, h1, h2, h3, scale1, scale2;
    extern /* Subroutine */ int dlasv2_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    extern doublereal dlapy2_(doublereal *, doublereal *);
    static doublereal ascale, bscale;
    extern doublereal dlamch_(char *);
    static doublereal wi, qq, rr, safmin;
    extern /* Subroutine */ int dlartg_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static doublereal wr1, wr2, ulp;
#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]


    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    --alphar;
    --alphai;
    --beta;

    /* Function Body */
    safmin = dlamch_("S");
    ulp = dlamch_("P");

/*     Scale A   

   Computing MAX */
    d__5 = (d__1 = a_ref(1, 1), abs(d__1)) + (d__2 = a_ref(2, 1), abs(d__2)), 
	    d__6 = (d__3 = a_ref(1, 2), abs(d__3)) + (d__4 = a_ref(2, 2), abs(
	    d__4)), d__5 = max(d__5,d__6);
    anorm = max(d__5,safmin);
    ascale = 1. / anorm;
    a_ref(1, 1) = ascale * a_ref(1, 1);
    a_ref(1, 2) = ascale * a_ref(1, 2);
    a_ref(2, 1) = ascale * a_ref(2, 1);
    a_ref(2, 2) = ascale * a_ref(2, 2);

/*     Scale B   

   Computing MAX */
    d__4 = (d__3 = b_ref(1, 1), abs(d__3)), d__5 = (d__1 = b_ref(1, 2), abs(
	    d__1)) + (d__2 = b_ref(2, 2), abs(d__2)), d__4 = max(d__4,d__5);
    bnorm = max(d__4,safmin);
    bscale = 1. / bnorm;
    b_ref(1, 1) = bscale * b_ref(1, 1);
    b_ref(1, 2) = bscale * b_ref(1, 2);
    b_ref(2, 2) = bscale * b_ref(2, 2);

/*     Check if A can be deflated */

    if ((d__1 = a_ref(2, 1), abs(d__1)) <= ulp) {
	*csl = 1.;
	*snl = 0.;
	*csr = 1.;
	*snr = 0.;
	a_ref(2, 1) = 0.;
	b_ref(2, 1) = 0.;

/*     Check if B is singular */

    } else if ((d__1 = b_ref(1, 1), abs(d__1)) <= ulp) {
	dlartg_(&a_ref(1, 1), &a_ref(2, 1), csl, snl, &r__);
	*csr = 1.;
	*snr = 0.;
	drot_(&c__2, &a_ref(1, 1), lda, &a_ref(2, 1), lda, csl, snl);
	drot_(&c__2, &b_ref(1, 1), ldb, &b_ref(2, 1), ldb, csl, snl);
	a_ref(2, 1) = 0.;
	b_ref(1, 1) = 0.;
	b_ref(2, 1) = 0.;

    } else if ((d__1 = b_ref(2, 2), abs(d__1)) <= ulp) {
	dlartg_(&a_ref(2, 2), &a_ref(2, 1), csr, snr, &t);
	*snr = -(*snr);
	drot_(&c__2, &a_ref(1, 1), &c__1, &a_ref(1, 2), &c__1, csr, snr);
	drot_(&c__2, &b_ref(1, 1), &c__1, &b_ref(1, 2), &c__1, csr, snr);
	*csl = 1.;
	*snl = 0.;
	a_ref(2, 1) = 0.;
	b_ref(2, 1) = 0.;
	b_ref(2, 2) = 0.;

    } else {

/*        B is nonsingular, first compute the eigenvalues of (A,B) */

	dlag2_(&a[a_offset], lda, &b[b_offset], ldb, &safmin, &scale1, &
		scale2, &wr1, &wr2, &wi);

	if (wi == 0.) {

/*           two real eigenvalues, compute s*A-w*B */

	    h1 = scale1 * a_ref(1, 1) - wr1 * b_ref(1, 1);
	    h2 = scale1 * a_ref(1, 2) - wr1 * b_ref(1, 2);
	    h3 = scale1 * a_ref(2, 2) - wr1 * b_ref(2, 2);

	    rr = dlapy2_(&h1, &h2);
	    d__1 = scale1 * a_ref(2, 1);
	    qq = dlapy2_(&d__1, &h3);

	    if (rr > qq) {

/*              find right rotation matrix to zero 1,1 element of   
                (sA - wB) */

		dlartg_(&h2, &h1, csr, snr, &t);

	    } else {

/*              find right rotation matrix to zero 2,1 element of   
                (sA - wB) */

		d__1 = scale1 * a_ref(2, 1);
		dlartg_(&h3, &d__1, csr, snr, &t);

	    }

	    *snr = -(*snr);
	    drot_(&c__2, &a_ref(1, 1), &c__1, &a_ref(1, 2), &c__1, csr, snr);
	    drot_(&c__2, &b_ref(1, 1), &c__1, &b_ref(1, 2), &c__1, csr, snr);

/*           compute inf norms of A and B   

   Computing MAX */
	    d__5 = (d__1 = a_ref(1, 1), abs(d__1)) + (d__2 = a_ref(1, 2), abs(
		    d__2)), d__6 = (d__3 = a_ref(2, 1), abs(d__3)) + (d__4 = 
		    a_ref(2, 2), abs(d__4));
	    h1 = max(d__5,d__6);
/* Computing MAX */
	    d__5 = (d__1 = b_ref(1, 1), abs(d__1)) + (d__2 = b_ref(1, 2), abs(
		    d__2)), d__6 = (d__3 = b_ref(2, 1), abs(d__3)) + (d__4 = 
		    b_ref(2, 2), abs(d__4));
	    h2 = max(d__5,d__6);

	    if (scale1 * h1 >= abs(wr1) * h2) {

/*              find left rotation matrix Q to zero out B(2,1) */

		dlartg_(&b_ref(1, 1), &b_ref(2, 1), csl, snl, &r__);

	    } else {

/*              find left rotation matrix Q to zero out A(2,1) */

		dlartg_(&a_ref(1, 1), &a_ref(2, 1), csl, snl, &r__);

	    }

	    drot_(&c__2, &a_ref(1, 1), lda, &a_ref(2, 1), lda, csl, snl);
	    drot_(&c__2, &b_ref(1, 1), ldb, &b_ref(2, 1), ldb, csl, snl);

	    a_ref(2, 1) = 0.;
	    b_ref(2, 1) = 0.;

	} else {

/*           a pair of complex conjugate eigenvalues   
             first compute the SVD of the matrix B */

	    dlasv2_(&b_ref(1, 1), &b_ref(1, 2), &b_ref(2, 2), &r__, &t, snr, 
		    csr, snl, csl);

/*           Form (A,B) := Q(A,B)Z' where Q is left rotation matrix and   
             Z is right rotation matrix computed from DLASV2 */

	    drot_(&c__2, &a_ref(1, 1), lda, &a_ref(2, 1), lda, csl, snl);
	    drot_(&c__2, &b_ref(1, 1), ldb, &b_ref(2, 1), ldb, csl, snl);
	    drot_(&c__2, &a_ref(1, 1), &c__1, &a_ref(1, 2), &c__1, csr, snr);
	    drot_(&c__2, &b_ref(1, 1), &c__1, &b_ref(1, 2), &c__1, csr, snr);

	    b_ref(2, 1) = 0.;
	    b_ref(1, 2) = 0.;

	}

    }

/*     Unscaling */

    a_ref(1, 1) = anorm * a_ref(1, 1);
    a_ref(2, 1) = anorm * a_ref(2, 1);
    a_ref(1, 2) = anorm * a_ref(1, 2);
    a_ref(2, 2) = anorm * a_ref(2, 2);
    b_ref(1, 1) = bnorm * b_ref(1, 1);
    b_ref(2, 1) = bnorm * b_ref(2, 1);
    b_ref(1, 2) = bnorm * b_ref(1, 2);
    b_ref(2, 2) = bnorm * b_ref(2, 2);

    if (wi == 0.) {
	alphar[1] = a_ref(1, 1);
	alphar[2] = a_ref(2, 2);
	alphai[1] = 0.;
	alphai[2] = 0.;
	beta[1] = b_ref(1, 1);
	beta[2] = b_ref(2, 2);
    } else {
	alphar[1] = anorm * wr1 / scale1 / bnorm;
	alphai[1] = anorm * wi / scale1 / bnorm;
	alphar[2] = alphar[1];
	alphai[2] = -alphai[1];
	beta[1] = 1.;
	beta[2] = 1.;
    }

/* L10: */

    return 0;

/*     End of DLAGV2 */

} /* dlagv2_ */

#undef b_ref
#undef a_ref


