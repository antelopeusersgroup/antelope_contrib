#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int claqp2_(integer *m, integer *n, integer *offset, complex 
	*a, integer *lda, integer *jpvt, complex *tau, real *vn1, real *vn2, 
	complex *work)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    CLAQP2 computes a QR factorization with column pivoting of   
    the block A(OFFSET+1:M,1:N).   
    The block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.   

    Arguments   
    =========   

    M       (input) INTEGER   
            The number of rows of the matrix A. M >= 0.   

    N       (input) INTEGER   
            The number of columns of the matrix A. N >= 0.   

    OFFSET  (input) INTEGER   
            The number of rows of the matrix A that must be pivoted   
            but no factorized. OFFSET >= 0.   

    A       (input/output) COMPLEX array, dimension (LDA,N)   
            On entry, the M-by-N matrix A.   
            On exit, the upper triangle of block A(OFFSET+1:M,1:N) is   
            the triangular factor obtained; the elements in block   
            A(OFFSET+1:M,1:N) below the diagonal, together with the   
            array TAU, represent the orthogonal matrix Q as a product of   
            elementary reflectors. Block A(1:OFFSET,1:N) has been   
            accordingly pivoted, but no factorized.   

    LDA     (input) INTEGER   
            The leading dimension of the array A. LDA >= max(1,M).   

    JPVT    (input/output) INTEGER array, dimension (N)   
            On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted   
            to the front of A*P (a leading column); if JPVT(i) = 0,   
            the i-th column of A is a free column.   
            On exit, if JPVT(i) = k, then the i-th column of A*P   
            was the k-th column of A.   

    TAU     (output) COMPLEX array, dimension (min(M,N))   
            The scalar factors of the elementary reflectors.   

    VN1     (input/output) REAL array, dimension (N)   
            The vector with the partial column norms.   

    VN2     (input/output) REAL array, dimension (N)   
            The vector with the exact column norms.   

    WORK    (workspace) COMPLEX array, dimension (N)   

    Further Details   
    ===============   

    Based on contributions by   
      G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain   
      X. Sun, Computer Science Dept., Duke University, USA   

    =====================================================================   


       Parameter adjustments */
    /* Table of constant values */
    static integer c__1 = 1;
    
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    real r__1;
    complex q__1;
    /* Builtin functions */
    void r_cnjg(complex *, complex *);
    double c_abs(complex *), sqrt(doublereal);
    /* Local variables */
    static real temp, temp2;
    static integer i__, j;
    extern /* Subroutine */ int clarf_(char *, integer *, integer *, complex *
	    , integer *, complex *, complex *, integer *, complex *);
    static integer offpi;
    extern /* Subroutine */ int cswap_(integer *, complex *, integer *, 
	    complex *, integer *);
    static integer itemp;
    extern doublereal scnrm2_(integer *, complex *, integer *);
    static integer mn;
    extern /* Subroutine */ int clarfg_(integer *, complex *, complex *, 
	    integer *, complex *);
    extern integer isamax_(integer *, real *, integer *);
    static complex aii;
    static integer pvt;
#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]


    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --jpvt;
    --tau;
    --vn1;
    --vn2;
    --work;

    /* Function Body   
   Computing MIN */
    i__1 = *m - *offset;
    mn = min(i__1,*n);

/*     Compute factorization. */

    i__1 = mn;
    for (i__ = 1; i__ <= i__1; ++i__) {

	offpi = *offset + i__;

/*        Determine ith pivot column and swap if necessary. */

	i__2 = *n - i__ + 1;
	pvt = i__ - 1 + isamax_(&i__2, &vn1[i__], &c__1);

	if (pvt != i__) {
	    cswap_(m, &a_ref(1, pvt), &c__1, &a_ref(1, i__), &c__1);
	    itemp = jpvt[pvt];
	    jpvt[pvt] = jpvt[i__];
	    jpvt[i__] = itemp;
	    vn1[pvt] = vn1[i__];
	    vn2[pvt] = vn2[i__];
	}

/*        Generate elementary reflector H(i). */

	if (offpi < *m) {
	    i__2 = *m - offpi + 1;
	    clarfg_(&i__2, &a_ref(offpi, i__), &a_ref(offpi + 1, i__), &c__1, 
		    &tau[i__]);
	} else {
	    clarfg_(&c__1, &a_ref(*m, i__), &a_ref(*m, i__), &c__1, &tau[i__])
		    ;
	}

	if (i__ < *n) {

/*           Apply H(i)' to A(offset+i:m,i+1:n) from the left. */

	    i__2 = a_subscr(offpi, i__);
	    aii.r = a[i__2].r, aii.i = a[i__2].i;
	    i__2 = a_subscr(offpi, i__);
	    a[i__2].r = 1.f, a[i__2].i = 0.f;
	    i__2 = *m - offpi + 1;
	    i__3 = *n - i__;
	    r_cnjg(&q__1, &tau[i__]);
	    clarf_("Left", &i__2, &i__3, &a_ref(offpi, i__), &c__1, &q__1, &
		    a_ref(offpi, i__ + 1), lda, &work[1]);
	    i__2 = a_subscr(offpi, i__);
	    a[i__2].r = aii.r, a[i__2].i = aii.i;
	}

/*        Update partial column norms. */

	i__2 = *n;
	for (j = i__ + 1; j <= i__2; ++j) {
	    if (vn1[j] != 0.f) {
/* Computing 2nd power */
		r__1 = c_abs(&a_ref(offpi, j)) / vn1[j];
		temp = 1.f - r__1 * r__1;
		temp = dmax(temp,0.f);
/* Computing 2nd power */
		r__1 = vn1[j] / vn2[j];
		temp2 = temp * .05f * (r__1 * r__1) + 1.f;
		if (temp2 == 1.f) {
		    if (offpi < *m) {
			i__3 = *m - offpi;
			vn1[j] = scnrm2_(&i__3, &a_ref(offpi + 1, j), &c__1);
			vn2[j] = vn1[j];
		    } else {
			vn1[j] = 0.f;
			vn2[j] = 0.f;
		    }
		} else {
		    vn1[j] *= sqrt(temp);
		}
	    }
/* L10: */
	}

/* L20: */
    }

    return 0;

/*     End of CLAQP2 */

} /* claqp2_ */

#undef a_ref
#undef a_subscr


