#include "blaswrap.h"
/*  -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;
static doublereal c_b13 = 1.;
static doublereal c_b26 = 0.;

/* Subroutine */ int dlasd3_(integer *nl, integer *nr, integer *sqre, integer 
	*k, doublereal *d__, doublereal *q, integer *ldq, doublereal *dsigma, 
	doublereal *u, integer *ldu, doublereal *u2, integer *ldu2, 
	doublereal *vt, integer *ldvt, doublereal *vt2, integer *ldvt2, 
	integer *idxc, integer *ctot, doublereal *z__, integer *info)
{
    /* System generated locals */
    integer q_dim1, q_offset, u_dim1, u_offset, u2_dim1, u2_offset, vt_dim1, 
	    vt_offset, vt2_dim1, vt2_offset, i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal temp;
    extern doublereal dnrm2_(integer *, doublereal *, integer *);
    static integer i__, j, m, n;
    extern /* Subroutine */ int dgemm_(char *, char *, integer *, integer *, 
	    integer *, doublereal *, doublereal *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, integer *);
    static integer ctemp;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *);
    static integer ktemp;
    extern doublereal dlamc3_(doublereal *, doublereal *);
    extern /* Subroutine */ int dlasd4_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *);
    static integer jc;
    extern /* Subroutine */ int dlascl_(char *, integer *, integer *, 
	    doublereal *, doublereal *, integer *, integer *, doublereal *, 
	    integer *, integer *), dlacpy_(char *, integer *, integer 
	    *, doublereal *, integer *, doublereal *, integer *), 
	    xerbla_(char *, integer *);
    static doublereal rho;
    static integer nlp1, nlp2, nrp1;


#define q_ref(a_1,a_2) q[(a_2)*q_dim1 + a_1]
#define u_ref(a_1,a_2) u[(a_2)*u_dim1 + a_1]
#define u2_ref(a_1,a_2) u2[(a_2)*u2_dim1 + a_1]
#define vt_ref(a_1,a_2) vt[(a_2)*vt_dim1 + a_1]
#define vt2_ref(a_1,a_2) vt2[(a_2)*vt2_dim1 + a_1]


/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,   
       Courant Institute, NAG Ltd., and Rice University   
       October 31, 1999   


    Purpose   
    =======   

    DLASD3 finds all the square roots of the roots of the secular   
    equation, as defined by the values in D and Z.  It makes the   
    appropriate calls to DLASD4 and then updates the singular   
    vectors by matrix multiplication.   

    This code makes very mild assumptions about floating point   
    arithmetic. It will work on machines with a guard digit in   
    add/subtract, or on those binary machines without guard digits   
    which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.   
    It could conceivably fail on hexadecimal or decimal machines   
    without guard digits, but we know of none.   

    DLASD3 is called from DLASD1.   

    Arguments   
    =========   

    NL     (input) INTEGER   
           The row dimension of the upper block.  NL >= 1.   

    NR     (input) INTEGER   
           The row dimension of the lower block.  NR >= 1.   

    SQRE   (input) INTEGER   
           = 0: the lower block is an NR-by-NR square matrix.   
           = 1: the lower block is an NR-by-(NR+1) rectangular matrix.   

           The bidiagonal matrix has N = NL + NR + 1 rows and   
           M = N + SQRE >= N columns.   

    K      (input) INTEGER   
           The size of the secular equation, 1 =< K = < N.   

    D      (output) DOUBLE PRECISION array, dimension(K)   
           On exit the square roots of the roots of the secular equation,   
           in ascending order.   

    Q      (workspace) DOUBLE PRECISION array,   
                       dimension at least (LDQ,K).   

    LDQ    (input) INTEGER   
           The leading dimension of the array Q.  LDQ >= K.   

    DSIGMA (input) DOUBLE PRECISION array, dimension(K)   
           The first K elements of this array contain the old roots   
           of the deflated updating problem.  These are the poles   
           of the secular equation.   

    U      (input) DOUBLE PRECISION array, dimension (LDU, N)   
           The last N - K columns of this matrix contain the deflated   
           left singular vectors.   

    LDU    (input) INTEGER   
           The leading dimension of the array U.  LDU >= N.   

    U2     (input) DOUBLE PRECISION array, dimension (LDU2, N)   
           The first K columns of this matrix contain the non-deflated   
           left singular vectors for the split problem.   

    LDU2   (input) INTEGER   
           The leading dimension of the array U2.  LDU2 >= N.   

    VT     (input) DOUBLE PRECISION array, dimension (LDVT, M)   
           The last M - K columns of VT' contain the deflated   
           right singular vectors.   

    LDVT   (input) INTEGER   
           The leading dimension of the array VT.  LDVT >= N.   

    VT2    (input) DOUBLE PRECISION array, dimension (LDVT2, N)   
           The first K columns of VT2' contain the non-deflated   
           right singular vectors for the split problem.   

    LDVT2  (input) INTEGER   
           The leading dimension of the array VT2.  LDVT2 >= N.   

    IDXC   (input) INTEGER array, dimension ( N )   
           The permutation used to arrange the columns of U (and rows of   
           VT) into three groups:  the first group contains non-zero   
           entries only at and above (or before) NL +1; the second   
           contains non-zero entries only at and below (or after) NL+2;   
           and the third is dense. The first column of U and the row of   
           VT are treated separately, however.   

           The rows of the singular vectors found by DLASD4   
           must be likewise permuted before the matrix multiplies can   
           take place.   

    CTOT   (input) INTEGER array, dimension ( 4 )   
           A count of the total number of the various types of columns   
           in U (or rows in VT), as described in IDXC. The fourth column   
           type is any column which has been deflated.   

    Z      (input) DOUBLE PRECISION array, dimension (K)   
           The first K elements of this array contain the components   
           of the deflation-adjusted updating row vector.   

    INFO   (output) INTEGER   
           = 0:  successful exit.   
           < 0:  if INFO = -i, the i-th argument had an illegal value.   
           > 0:  if INFO = 1, an singular value did not converge   

    Further Details   
    ===============   

    Based on contributions by   
       Ming Gu and Huan Ren, Computer Science Division, University of   
       California at Berkeley, USA   

    =====================================================================   


       Test the input parameters.   

       Parameter adjustments */
    --d__;
    q_dim1 = *ldq;
    q_offset = 1 + q_dim1 * 1;
    q -= q_offset;
    --dsigma;
    u_dim1 = *ldu;
    u_offset = 1 + u_dim1 * 1;
    u -= u_offset;
    u2_dim1 = *ldu2;
    u2_offset = 1 + u2_dim1 * 1;
    u2 -= u2_offset;
    vt_dim1 = *ldvt;
    vt_offset = 1 + vt_dim1 * 1;
    vt -= vt_offset;
    vt2_dim1 = *ldvt2;
    vt2_offset = 1 + vt2_dim1 * 1;
    vt2 -= vt2_offset;
    --idxc;
    --ctot;
    --z__;

    /* Function Body */
    *info = 0;

    if (*nl < 1) {
	*info = -1;
    } else if (*nr < 1) {
	*info = -2;
    } else if (*sqre != 1 && *sqre != 0) {
	*info = -3;
    }

    n = *nl + *nr + 1;
    m = n + *sqre;
    nlp1 = *nl + 1;
    nlp2 = *nl + 2;

    if (*k < 1 || *k > n) {
	*info = -4;
    } else if (*ldq < *k) {
	*info = -7;
    } else if (*ldu < n) {
	*info = -10;
    } else if (*ldu2 < n) {
	*info = -12;
    } else if (*ldvt < m) {
	*info = -14;
    } else if (*ldvt2 < m) {
	*info = -16;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("DLASD3", &i__1);
	return 0;
    }

/*     Quick return if possible */

    if (*k == 1) {
	d__[1] = abs(z__[1]);
	dcopy_(&m, &vt2_ref(1, 1), ldvt2, &vt_ref(1, 1), ldvt);
	if (z__[1] > 0.) {
	    dcopy_(&n, &u2_ref(1, 1), &c__1, &u_ref(1, 1), &c__1);
	} else {
	    i__1 = n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		u_ref(i__, 1) = -u2_ref(i__, 1);
/* L10: */
	    }
	}
	return 0;
    }

/*     Modify values DSIGMA(i) to make sure all DSIGMA(i)-DSIGMA(j) can   
       be computed with high relative accuracy (barring over/underflow).   
       This is a problem on machines without a guard digit in   
       add/subtract (Cray XMP, Cray YMP, Cray C 90 and Cray 2).   
       The following code replaces DSIGMA(I) by 2*DSIGMA(I)-DSIGMA(I),   
       which on any of these machines zeros out the bottommost   
       bit of DSIGMA(I) if it is 1; this makes the subsequent   
       subtractions DSIGMA(I)-DSIGMA(J) unproblematic when cancellation   
       occurs. On binary machines with a guard digit (almost all   
       machines) it does not change DSIGMA(I) at all. On hexadecimal   
       and decimal machines with a guard digit, it slightly   
       changes the bottommost bits of DSIGMA(I). It does not account   
       for hexadecimal or decimal machines without guard digits   
       (we know of none). We use a subroutine call to compute   
       2*DLAMBDA(I) to prevent optimizing compilers from eliminating   
       this code. */

    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
	dsigma[i__] = dlamc3_(&dsigma[i__], &dsigma[i__]) - dsigma[i__];
/* L20: */
    }

/*     Keep a copy of Z. */

    dcopy_(k, &z__[1], &c__1, &q[q_offset], &c__1);

/*     Normalize Z. */

    rho = dnrm2_(k, &z__[1], &c__1);
    dlascl_("G", &c__0, &c__0, &rho, &c_b13, k, &c__1, &z__[1], k, info);
    rho *= rho;

/*     Find the new singular values. */

    i__1 = *k;
    for (j = 1; j <= i__1; ++j) {
	dlasd4_(k, &j, &dsigma[1], &z__[1], &u_ref(1, j), &rho, &d__[j], &
		vt_ref(1, j), info);

/*        If the zero finder fails, the computation is terminated. */

	if (*info != 0) {
	    return 0;
	}
/* L30: */
    }

/*     Compute updated Z. */

    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
	z__[i__] = u_ref(i__, *k) * vt_ref(i__, *k);
	i__2 = i__ - 1;
	for (j = 1; j <= i__2; ++j) {
	    z__[i__] *= u_ref(i__, j) * vt_ref(i__, j) / (dsigma[i__] - 
		    dsigma[j]) / (dsigma[i__] + dsigma[j]);
/* L40: */
	}
	i__2 = *k - 1;
	for (j = i__; j <= i__2; ++j) {
	    z__[i__] *= u_ref(i__, j) * vt_ref(i__, j) / (dsigma[i__] - 
		    dsigma[j + 1]) / (dsigma[i__] + dsigma[j + 1]);
/* L50: */
	}
	d__2 = sqrt((d__1 = z__[i__], abs(d__1)));
	z__[i__] = d_sign(&d__2, &q_ref(i__, 1));
/* L60: */
    }

/*     Compute left singular vectors of the modified diagonal matrix,   
       and store related information for the right singular vectors. */

    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
	vt_ref(1, i__) = z__[1] / u_ref(1, i__) / vt_ref(1, i__);
	u_ref(1, i__) = -1.;
	i__2 = *k;
	for (j = 2; j <= i__2; ++j) {
	    vt_ref(j, i__) = z__[j] / u_ref(j, i__) / vt_ref(j, i__);
	    u_ref(j, i__) = dsigma[j] * vt_ref(j, i__);
/* L70: */
	}
	temp = dnrm2_(k, &u_ref(1, i__), &c__1);
	q_ref(1, i__) = u_ref(1, i__) / temp;
	i__2 = *k;
	for (j = 2; j <= i__2; ++j) {
	    jc = idxc[j];
	    q_ref(j, i__) = u_ref(jc, i__) / temp;
/* L80: */
	}
/* L90: */
    }

/*     Update the left singular vector matrix. */

    if (*k == 2) {
	dgemm_("N", "N", &n, k, k, &c_b13, &u2[u2_offset], ldu2, &q[q_offset],
		 ldq, &c_b26, &u[u_offset], ldu);
	goto L100;
    }
    if (ctot[1] > 0) {
	dgemm_("N", "N", nl, k, &ctot[1], &c_b13, &u2_ref(1, 2), ldu2, &q_ref(
		2, 1), ldq, &c_b26, &u_ref(1, 1), ldu);
	if (ctot[3] > 0) {
	    ktemp = ctot[1] + 2 + ctot[2];
	    dgemm_("N", "N", nl, k, &ctot[3], &c_b13, &u2_ref(1, ktemp), ldu2,
		     &q_ref(ktemp, 1), ldq, &c_b13, &u_ref(1, 1), ldu);
	}
    } else if (ctot[3] > 0) {
	ktemp = ctot[1] + 2 + ctot[2];
	dgemm_("N", "N", nl, k, &ctot[3], &c_b13, &u2_ref(1, ktemp), ldu2, &
		q_ref(ktemp, 1), ldq, &c_b26, &u_ref(1, 1), ldu);
    } else {
	dlacpy_("F", nl, k, &u2[u2_offset], ldu2, &u[u_offset], ldu);
    }
    dcopy_(k, &q_ref(1, 1), ldq, &u_ref(nlp1, 1), ldu);
    ktemp = ctot[1] + 2;
    ctemp = ctot[2] + ctot[3];
    dgemm_("N", "N", nr, k, &ctemp, &c_b13, &u2_ref(nlp2, ktemp), ldu2, &
	    q_ref(ktemp, 1), ldq, &c_b26, &u_ref(nlp2, 1), ldu);

/*     Generate the right singular vectors. */

L100:
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
	temp = dnrm2_(k, &vt_ref(1, i__), &c__1);
	q_ref(i__, 1) = vt_ref(1, i__) / temp;
	i__2 = *k;
	for (j = 2; j <= i__2; ++j) {
	    jc = idxc[j];
	    q_ref(i__, j) = vt_ref(jc, i__) / temp;
/* L110: */
	}
/* L120: */
    }

/*     Update the right singular vector matrix. */

    if (*k == 2) {
	dgemm_("N", "N", k, &m, k, &c_b13, &q[q_offset], ldq, &vt2[vt2_offset]
		, ldvt2, &c_b26, &vt[vt_offset], ldvt);
	return 0;
    }
    ktemp = ctot[1] + 1;
    dgemm_("N", "N", k, &nlp1, &ktemp, &c_b13, &q_ref(1, 1), ldq, &vt2_ref(1, 
	    1), ldvt2, &c_b26, &vt_ref(1, 1), ldvt);
    ktemp = ctot[1] + 2 + ctot[2];
    if (ktemp <= *ldvt2) {
	dgemm_("N", "N", k, &nlp1, &ctot[3], &c_b13, &q_ref(1, ktemp), ldq, &
		vt2_ref(ktemp, 1), ldvt2, &c_b13, &vt_ref(1, 1), ldvt);
    }

    ktemp = ctot[1] + 1;
    nrp1 = *nr + *sqre;
    if (ktemp > 1) {
	i__1 = *k;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    q_ref(i__, ktemp) = q_ref(i__, 1);
/* L130: */
	}
	i__1 = m;
	for (i__ = nlp2; i__ <= i__1; ++i__) {
	    vt2_ref(ktemp, i__) = vt2_ref(1, i__);
/* L140: */
	}
    }
    ctemp = ctot[2] + 1 + ctot[3];
    dgemm_("N", "N", k, &nrp1, &ctemp, &c_b13, &q_ref(1, ktemp), ldq, &
	    vt2_ref(ktemp, nlp2), ldvt2, &c_b26, &vt_ref(1, nlp2), ldvt);

    return 0;

/*     End of DLASD3 */

} /* dlasd3_ */

#undef vt2_ref
#undef vt_ref
#undef u2_ref
#undef u_ref
#undef q_ref


