#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int slaqps_(integer *m, integer *n, integer *offset, integer 
	*nb, integer *kb, real *a, integer *lda, integer *jpvt, real *tau, 
	real *vn1, real *vn2, real *auxv, real *f, integer *ldf)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    SLAQPS computes a step of QR factorization with column pivoting   
    of a real M-by-N matrix A by using Blas-3.  It tries to factorize   
    NB columns from A starting from the row OFFSET+1, and updates all   
    of the matrix with Blas-3 xGEMM.   

    In some cases, due to catastrophic cancellations, it cannot   
    factorize NB columns.  Hence, the actual number of factorized   
    columns is returned in KB.   

    Block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.   

    Arguments   
    =========   

    M       (input) INTEGER   
            The number of rows of the matrix A. M >= 0.   

    N       (input) INTEGER   
            The number of columns of the matrix A. N >= 0   

    OFFSET  (input) INTEGER   
            The number of rows of A that have been factorized in   
            previous steps.   

    NB      (input) INTEGER   
            The number of columns to factorize.   

    KB      (output) INTEGER   
            The number of columns actually factorized.   

    A       (input/output) REAL array, dimension (LDA,N)   
            On entry, the M-by-N matrix A.   
            On exit, block A(OFFSET+1:M,1:KB) is the triangular   
            factor obtained and block A(1:OFFSET,1:N) has been   
            accordingly pivoted, but no factorized.   
            The rest of the matrix, block A(OFFSET+1:M,KB+1:N) has   
            been updated.   

    LDA     (input) INTEGER   
            The leading dimension of the array A. LDA >= max(1,M).   

    JPVT    (input/output) INTEGER array, dimension (N)   
            JPVT(I) = K <==> Column K of the full matrix A has been   
            permuted into position I in AP.   

    TAU     (output) REAL array, dimension (KB)   
            The scalar factors of the elementary reflectors.   

    VN1     (input/output) REAL array, dimension (N)   
            The vector with the partial column norms.   

    VN2     (input/output) REAL array, dimension (N)   
            The vector with the exact column norms.   

    AUXV    (input/output) REAL array, dimension (NB)   
            Auxiliar vector.   

    F       (input/output) REAL array, dimension (LDF,NB)   
            Matrix F' = L*Y'*A.   

    LDF     (input) INTEGER   
            The leading dimension of the array F. LDF >= max(1,N).   

    Further Details   
    ===============   

    Based on contributions by   
      G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain   
      X. Sun, Computer Science Dept., Duke University, USA   

    =====================================================================   


       Parameter adjustments */
    /* Table of constant values */
    static integer c__1 = 1;
    static real c_b7 = -1.f;
    static real c_b8 = 1.f;
    static real c_b15 = 0.f;
    
    /* System generated locals */
    integer a_dim1, a_offset, f_dim1, f_offset, i__1, i__2;
    real r__1, r__2;
    /* Builtin functions */
    double sqrt(doublereal);
    integer i_nint(real *);
    /* Local variables */
    static real temp, temp2;
    extern doublereal snrm2_(integer *, real *, integer *);
    static integer j, k;
    extern /* Subroutine */ int sgemm_(char *, char *, integer *, integer *, 
	    integer *, real *, real *, integer *, real *, integer *, real *, 
	    real *, integer *);
    static integer itemp;
    extern /* Subroutine */ int sgemv_(char *, integer *, integer *, real *, 
	    real *, integer *, real *, integer *, real *, real *, integer *), sswap_(integer *, real *, integer *, real *, integer *);
    static integer rk;
    extern /* Subroutine */ int slarfg_(integer *, real *, real *, integer *, 
	    real *);
    static integer lsticc;
    extern integer isamax_(integer *, real *, integer *);
    static integer lastrk;
    static real akk;
    static integer pvt;
#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define f_ref(a_1,a_2) f[(a_2)*f_dim1 + a_1]


    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    --jpvt;
    --tau;
    --vn1;
    --vn2;
    --auxv;
    f_dim1 = *ldf;
    f_offset = 1 + f_dim1 * 1;
    f -= f_offset;

    /* Function Body   
   Computing MIN */
    i__1 = *m, i__2 = *n + *offset;
    lastrk = min(i__1,i__2);
    lsticc = 0;
    k = 0;

/*     Beginning of while loop. */

L10:
    if (k < *nb && lsticc == 0) {
	++k;
	rk = *offset + k;

/*        Determine ith pivot column and swap if necessary */

	i__1 = *n - k + 1;
	pvt = k - 1 + isamax_(&i__1, &vn1[k], &c__1);
	if (pvt != k) {
	    sswap_(m, &a_ref(1, pvt), &c__1, &a_ref(1, k), &c__1);
	    i__1 = k - 1;
	    sswap_(&i__1, &f_ref(pvt, 1), ldf, &f_ref(k, 1), ldf);
	    itemp = jpvt[pvt];
	    jpvt[pvt] = jpvt[k];
	    jpvt[k] = itemp;
	    vn1[pvt] = vn1[k];
	    vn2[pvt] = vn2[k];
	}

/*        Apply previous Householder reflectors to column K:   
          A(RK:M,K) := A(RK:M,K) - A(RK:M,1:K-1)*F(K,1:K-1)'. */

	if (k > 1) {
	    i__1 = *m - rk + 1;
	    i__2 = k - 1;
	    sgemv_("No transpose", &i__1, &i__2, &c_b7, &a_ref(rk, 1), lda, &
		    f_ref(k, 1), ldf, &c_b8, &a_ref(rk, k), &c__1)
		    ;
	}

/*        Generate elementary reflector H(k). */

	if (rk < *m) {
	    i__1 = *m - rk + 1;
	    slarfg_(&i__1, &a_ref(rk, k), &a_ref(rk + 1, k), &c__1, &tau[k]);
	} else {
	    slarfg_(&c__1, &a_ref(rk, k), &a_ref(rk, k), &c__1, &tau[k]);
	}

	akk = a_ref(rk, k);
	a_ref(rk, k) = 1.f;

/*        Compute Kth column of F:   

          Compute  F(K+1:N,K) := tau(K)*A(RK:M,K+1:N)'*A(RK:M,K). */

	if (k < *n) {
	    i__1 = *m - rk + 1;
	    i__2 = *n - k;
	    sgemv_("Transpose", &i__1, &i__2, &tau[k], &a_ref(rk, k + 1), lda,
		     &a_ref(rk, k), &c__1, &c_b15, &f_ref(k + 1, k), &c__1);
	}

/*        Padding F(1:K,K) with zeros. */

	i__1 = k;
	for (j = 1; j <= i__1; ++j) {
	    f_ref(j, k) = 0.f;
/* L20: */
	}

/*        Incremental updating of F:   
          F(1:N,K) := F(1:N,K) - tau(K)*F(1:N,1:K-1)*A(RK:M,1:K-1)'   
                      *A(RK:M,K). */

	if (k > 1) {
	    i__1 = *m - rk + 1;
	    i__2 = k - 1;
	    r__1 = -tau[k];
	    sgemv_("Transpose", &i__1, &i__2, &r__1, &a_ref(rk, 1), lda, &
		    a_ref(rk, k), &c__1, &c_b15, &auxv[1], &c__1);

	    i__1 = k - 1;
	    sgemv_("No transpose", n, &i__1, &c_b8, &f_ref(1, 1), ldf, &auxv[
		    1], &c__1, &c_b8, &f_ref(1, k), &c__1);
	}

/*        Update the current row of A:   
          A(RK,K+1:N) := A(RK,K+1:N) - A(RK,1:K)*F(K+1:N,1:K)'. */

	if (k < *n) {
	    i__1 = *n - k;
	    sgemv_("No transpose", &i__1, &k, &c_b7, &f_ref(k + 1, 1), ldf, &
		    a_ref(rk, 1), lda, &c_b8, &a_ref(rk, k + 1), lda);
	}

/*        Update partial column norms. */

	if (rk < lastrk) {
	    i__1 = *n;
	    for (j = k + 1; j <= i__1; ++j) {
		if (vn1[j] != 0.f) {
		    temp = (r__1 = a_ref(rk, j), dabs(r__1)) / vn1[j];
/* Computing MAX */
		    r__1 = 0.f, r__2 = (temp + 1.f) * (1.f - temp);
		    temp = dmax(r__1,r__2);
/* Computing 2nd power */
		    r__1 = vn1[j] / vn2[j];
		    temp2 = temp * .05f * (r__1 * r__1) + 1.f;
		    if (temp2 == 1.f) {
			vn2[j] = (real) lsticc;
			lsticc = j;
		    } else {
			vn1[j] *= sqrt(temp);
		    }
		}
/* L30: */
	    }
	}

	a_ref(rk, k) = akk;

/*        End of while loop. */

	goto L10;
    }
    *kb = k;
    rk = *offset + *kb;

/*     Apply the block reflector to the rest of the matrix:   
       A(OFFSET+KB+1:M,KB+1:N) := A(OFFSET+KB+1:M,KB+1:N) -   
                           A(OFFSET+KB+1:M,1:KB)*F(KB+1:N,1:KB)'.   

   Computing MIN */
    i__1 = *n, i__2 = *m - *offset;
    if (*kb < min(i__1,i__2)) {
	i__1 = *m - rk;
	i__2 = *n - *kb;
	sgemm_("No transpose", "Transpose", &i__1, &i__2, kb, &c_b7, &a_ref(
		rk + 1, 1), lda, &f_ref(*kb + 1, 1), ldf, &c_b8, &a_ref(rk + 
		1, *kb + 1), lda);
    }

/*     Recomputation of difficult columns. */

L40:
    if (lsticc > 0) {
	itemp = i_nint(&vn2[lsticc]);
	i__1 = *m - rk;
	vn1[lsticc] = snrm2_(&i__1, &a_ref(rk + 1, lsticc), &c__1);
	vn2[lsticc] = vn1[lsticc];
	lsticc = itemp;
	goto L40;
    }

    return 0;

/*     End of SLAQPS */

} /* slaqps_ */

#undef f_ref
#undef a_ref


