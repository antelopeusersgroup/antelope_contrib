#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int slals0_(integer *icompq, integer *nl, integer *nr, 
	integer *sqre, integer *nrhs, real *b, integer *ldb, real *bx, 
	integer *ldbx, integer *perm, integer *givptr, integer *givcol, 
	integer *ldgcol, real *givnum, integer *ldgnum, real *poles, real *
	difl, real *difr, real *z__, integer *k, real *c__, real *s, real *
	work, integer *info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       December 1, 1999   


    Purpose   
    =======   

    SLALS0 applies back the multiplying factors of either the left or the   
    right singular vector matrix of a diagonal matrix appended by a row   
    to the right hand side matrix B in solving the least squares problem   
    using the divide-and-conquer SVD approach.   

    For the left singular vector matrix, three types of orthogonal   
    matrices are involved:   

    (1L) Givens rotations: the number of such rotations is GIVPTR; the   
         pairs of columns/rows they were applied to are stored in GIVCOL;   
         and the C- and S-values of these rotations are stored in GIVNUM.   

    (2L) Permutation. The (NL+1)-st row of B is to be moved to the first   
         row, and for J=2:N, PERM(J)-th row of B is to be moved to the   
         J-th row.   

    (3L) The left singular vector matrix of the remaining matrix.   

    For the right singular vector matrix, four types of orthogonal   
    matrices are involved:   

    (1R) The right singular vector matrix of the remaining matrix.   

    (2R) If SQRE = 1, one extra Givens rotation to generate the right   
         null space.   

    (3R) The inverse transformation of (2L).   

    (4R) The inverse transformation of (1L).   

    Arguments   
    =========   

    ICOMPQ (input) INTEGER   
           Specifies whether singular vectors are to be computed in   
           factored form:   
           = 0: Left singular vector matrix.   
           = 1: Right singular vector matrix.   

    NL     (input) INTEGER   
           The row dimension of the upper block. NL >= 1.   

    NR     (input) INTEGER   
           The row dimension of the lower block. NR >= 1.   

    SQRE   (input) INTEGER   
           = 0: the lower block is an NR-by-NR square matrix.   
           = 1: the lower block is an NR-by-(NR+1) rectangular matrix.   

           The bidiagonal matrix has row dimension N = NL + NR + 1,   
           and column dimension M = N + SQRE.   

    NRHS   (input) INTEGER   
           The number of columns of B and BX. NRHS must be at least 1.   

    B      (input/output) REAL array, dimension ( LDB, NRHS )   
           On input, B contains the right hand sides of the least   
           squares problem in rows 1 through M. On output, B contains   
           the solution X in rows 1 through N.   

    LDB    (input) INTEGER   
           The leading dimension of B. LDB must be at least   
           max(1,MAX( M, N ) ).   

    BX     (workspace) REAL array, dimension ( LDBX, NRHS )   

    LDBX   (input) INTEGER   
           The leading dimension of BX.   

    PERM   (input) INTEGER array, dimension ( N )   
           The permutations (from deflation and sorting) applied   
           to the two blocks.   

    GIVPTR (input) INTEGER   
           The number of Givens rotations which took place in this   
           subproblem.   

    GIVCOL (input) INTEGER array, dimension ( LDGCOL, 2 )   
           Each pair of numbers indicates a pair of rows/columns   
           involved in a Givens rotation.   

    LDGCOL (input) INTEGER   
           The leading dimension of GIVCOL, must be at least N.   

    GIVNUM (input) REAL array, dimension ( LDGNUM, 2 )   
           Each number indicates the C or S value used in the   
           corresponding Givens rotation.   

    LDGNUM (input) INTEGER   
           The leading dimension of arrays DIFR, POLES and   
           GIVNUM, must be at least K.   

    POLES  (input) REAL array, dimension ( LDGNUM, 2 )   
           On entry, POLES(1:K, 1) contains the new singular   
           values obtained from solving the secular equation, and   
           POLES(1:K, 2) is an array containing the poles in the secular   
           equation.   

    DIFL   (input) REAL array, dimension ( K ).   
           On entry, DIFL(I) is the distance between I-th updated   
           (undeflated) singular value and the I-th (undeflated) old   
           singular value.   

    DIFR   (input) REAL array, dimension ( LDGNUM, 2 ).   
           On entry, DIFR(I, 1) contains the distances between I-th   
           updated (undeflated) singular value and the I+1-th   
           (undeflated) old singular value. And DIFR(I, 2) is the   
           normalizing factor for the I-th right singular vector.   

    Z      (input) REAL array, dimension ( K )   
           Contain the components of the deflation-adjusted updating row   
           vector.   

    K      (input) INTEGER   
           Contains the dimension of the non-deflated matrix,   
           This is the order of the related secular equation. 1 <= K <=N.   

    C      (input) REAL   
           C contains garbage if SQRE =0 and the C-value of a Givens   
           rotation related to the right null space if SQRE = 1.   

    S      (input) REAL   
           S contains garbage if SQRE =0 and the S-value of a Givens   
           rotation related to the right null space if SQRE = 1.   

    WORK   (workspace) REAL array, dimension ( K )   

    INFO   (output) INTEGER   
            = 0:  successful exit.   
            < 0:  if INFO = -i, the i-th argument had an illegal value.   

    Further Details   
    ===============   

    Based on contributions by   
       Ming Gu and Ren-Cang Li, Computer Science Division, University of   
         California at Berkeley, USA   
       Osni Marques, LBNL/NERSC, USA   

    =====================================================================   


       Test the input parameters.   

       Parameter adjustments */
    /* Table of constant values */
    static real c_b5 = -1.f;
    static integer c__1 = 1;
    static real c_b11 = 1.f;
    static real c_b13 = 0.f;
    static integer c__0 = 0;
    
    /* System generated locals */
    integer givcol_dim1, givcol_offset, b_dim1, b_offset, bx_dim1, bx_offset, 
	    difr_dim1, difr_offset, givnum_dim1, givnum_offset, poles_dim1, 
	    poles_offset, i__1, i__2;
    real r__1;
    /* Local variables */
    static real temp;
    extern /* Subroutine */ int srot_(integer *, real *, integer *, real *, 
	    integer *, real *, real *);
    extern doublereal snrm2_(integer *, real *, integer *);
    static integer i__, j, m, n;
    static real diflj, difrj, dsigj;
    extern /* Subroutine */ int sscal_(integer *, real *, real *, integer *), 
	    sgemv_(char *, integer *, integer *, real *, real *, integer *, 
	    real *, integer *, real *, real *, integer *), scopy_(
	    integer *, real *, integer *, real *, integer *);
    extern doublereal slamc3_(real *, real *);
    static real dj;
    extern /* Subroutine */ int xerbla_(char *, integer *);
    static real dsigjp;
    extern /* Subroutine */ int slascl_(char *, integer *, integer *, real *, 
	    real *, integer *, integer *, real *, integer *, integer *), slacpy_(char *, integer *, integer *, real *, integer *, 
	    real *, integer *);
    static integer nlp1;
#define difr_ref(a_1,a_2) difr[(a_2)*difr_dim1 + a_1]
#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]
#define poles_ref(a_1,a_2) poles[(a_2)*poles_dim1 + a_1]
#define bx_ref(a_1,a_2) bx[(a_2)*bx_dim1 + a_1]
#define givcol_ref(a_1,a_2) givcol[(a_2)*givcol_dim1 + a_1]
#define givnum_ref(a_1,a_2) givnum[(a_2)*givnum_dim1 + a_1]


    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    bx_dim1 = *ldbx;
    bx_offset = 1 + bx_dim1 * 1;
    bx -= bx_offset;
    --perm;
    givcol_dim1 = *ldgcol;
    givcol_offset = 1 + givcol_dim1 * 1;
    givcol -= givcol_offset;
    difr_dim1 = *ldgnum;
    difr_offset = 1 + difr_dim1 * 1;
    difr -= difr_offset;
    poles_dim1 = *ldgnum;
    poles_offset = 1 + poles_dim1 * 1;
    poles -= poles_offset;
    givnum_dim1 = *ldgnum;
    givnum_offset = 1 + givnum_dim1 * 1;
    givnum -= givnum_offset;
    --difl;
    --z__;
    --work;

    /* Function Body */
    *info = 0;

    if (*icompq < 0 || *icompq > 1) {
	*info = -1;
    } else if (*nl < 1) {
	*info = -2;
    } else if (*nr < 1) {
	*info = -3;
    } else if (*sqre < 0 || *sqre > 1) {
	*info = -4;
    }

    n = *nl + *nr + 1;

    if (*nrhs < 1) {
	*info = -5;
    } else if (*ldb < n) {
	*info = -7;
    } else if (*ldbx < n) {
	*info = -9;
    } else if (*givptr < 0) {
	*info = -11;
    } else if (*ldgcol < n) {
	*info = -13;
    } else if (*ldgnum < n) {
	*info = -15;
    } else if (*k < 1) {
	*info = -20;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("SLALS0", &i__1);
	return 0;
    }

    m = n + *sqre;
    nlp1 = *nl + 1;

    if (*icompq == 0) {

/*        Apply back orthogonal transformations from the left.   

          Step (1L): apply back the Givens rotations performed. */

	i__1 = *givptr;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    srot_(nrhs, &b_ref(givcol_ref(i__, 2), 1), ldb, &b_ref(givcol_ref(
		    i__, 1), 1), ldb, &givnum_ref(i__, 2), &givnum_ref(i__, 1)
		    );
/* L10: */
	}

/*        Step (2L): permute rows of B. */

	scopy_(nrhs, &b_ref(nlp1, 1), ldb, &bx_ref(1, 1), ldbx);
	i__1 = n;
	for (i__ = 2; i__ <= i__1; ++i__) {
	    scopy_(nrhs, &b_ref(perm[i__], 1), ldb, &bx_ref(i__, 1), ldbx);
/* L20: */
	}

/*        Step (3L): apply the inverse of the left singular vector   
          matrix to BX. */

	if (*k == 1) {
	    scopy_(nrhs, &bx[bx_offset], ldbx, &b[b_offset], ldb);
	    if (z__[1] < 0.f) {
		sscal_(nrhs, &c_b5, &b[b_offset], ldb);
	    }
	} else {
	    i__1 = *k;
	    for (j = 1; j <= i__1; ++j) {
		diflj = difl[j];
		dj = poles_ref(j, 1);
		dsigj = -poles_ref(j, 2);
		if (j < *k) {
		    difrj = -difr_ref(j, 1);
		    dsigjp = -poles_ref(j + 1, 2);
		}
		if (z__[j] == 0.f || poles_ref(j, 2) == 0.f) {
		    work[j] = 0.f;
		} else {
		    work[j] = -poles_ref(j, 2) * z__[j] / diflj / (poles_ref(
			    j, 2) + dj);
		}
		i__2 = j - 1;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    if (z__[i__] == 0.f || poles_ref(i__, 2) == 0.f) {
			work[i__] = 0.f;
		    } else {
			work[i__] = poles_ref(i__, 2) * z__[i__] / (slamc3_(&
				poles_ref(i__, 2), &dsigj) - diflj) / (
				poles_ref(i__, 2) + dj);
		    }
/* L30: */
		}
		i__2 = *k;
		for (i__ = j + 1; i__ <= i__2; ++i__) {
		    if (z__[i__] == 0.f || poles_ref(i__, 2) == 0.f) {
			work[i__] = 0.f;
		    } else {
			work[i__] = poles_ref(i__, 2) * z__[i__] / (slamc3_(&
				poles_ref(i__, 2), &dsigjp) + difrj) / (
				poles_ref(i__, 2) + dj);
		    }
/* L40: */
		}
		work[1] = -1.f;
		temp = snrm2_(k, &work[1], &c__1);
		sgemv_("T", k, nrhs, &c_b11, &bx[bx_offset], ldbx, &work[1], &
			c__1, &c_b13, &b_ref(j, 1), ldb);
		slascl_("G", &c__0, &c__0, &temp, &c_b11, &c__1, nrhs, &b_ref(
			j, 1), ldb, info);
/* L50: */
	    }
	}

/*        Move the deflated rows of BX to B also. */

	if (*k < max(m,n)) {
	    i__1 = n - *k;
	    slacpy_("A", &i__1, nrhs, &bx_ref(*k + 1, 1), ldbx, &b_ref(*k + 1,
		     1), ldb);
	}
    } else {

/*        Apply back the right orthogonal transformations.   

          Step (1R): apply back the new right singular vector matrix   
          to B. */

	if (*k == 1) {
	    scopy_(nrhs, &b[b_offset], ldb, &bx[bx_offset], ldbx);
	} else {
	    i__1 = *k;
	    for (j = 1; j <= i__1; ++j) {
		dsigj = poles_ref(j, 2);
		if (z__[j] == 0.f) {
		    work[j] = 0.f;
		} else {
		    work[j] = -z__[j] / difl[j] / (dsigj + poles_ref(j, 1)) / 
			    difr_ref(j, 2);
		}
		i__2 = j - 1;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    if (z__[j] == 0.f) {
			work[i__] = 0.f;
		    } else {
			r__1 = -poles_ref(i__ + 1, 2);
			work[i__] = z__[j] / (slamc3_(&dsigj, &r__1) - 
				difr_ref(i__, 1)) / (dsigj + poles_ref(i__, 1)
				) / difr_ref(i__, 2);
		    }
/* L60: */
		}
		i__2 = *k;
		for (i__ = j + 1; i__ <= i__2; ++i__) {
		    if (z__[j] == 0.f) {
			work[i__] = 0.f;
		    } else {
			r__1 = -poles_ref(i__, 2);
			work[i__] = z__[j] / (slamc3_(&dsigj, &r__1) - difl[
				i__]) / (dsigj + poles_ref(i__, 1)) / 
				difr_ref(i__, 2);
		    }
/* L70: */
		}
		sgemv_("T", k, nrhs, &c_b11, &b[b_offset], ldb, &work[1], &
			c__1, &c_b13, &bx_ref(j, 1), ldbx);
/* L80: */
	    }
	}

/*        Step (2R): if SQRE = 1, apply back the rotation that is   
          related to the right null space of the subproblem. */

	if (*sqre == 1) {
	    scopy_(nrhs, &b_ref(m, 1), ldb, &bx_ref(m, 1), ldbx);
	    srot_(nrhs, &bx_ref(1, 1), ldbx, &bx_ref(m, 1), ldbx, c__, s);
	}
	if (*k < max(m,n)) {
	    i__1 = n - *k;
	    slacpy_("A", &i__1, nrhs, &b_ref(*k + 1, 1), ldb, &bx_ref(*k + 1, 
		    1), ldbx);
	}

/*        Step (3R): permute rows of B. */

	scopy_(nrhs, &bx_ref(1, 1), ldbx, &b_ref(nlp1, 1), ldb);
	if (*sqre == 1) {
	    scopy_(nrhs, &bx_ref(m, 1), ldbx, &b_ref(m, 1), ldb);
	}
	i__1 = n;
	for (i__ = 2; i__ <= i__1; ++i__) {
	    scopy_(nrhs, &bx_ref(i__, 1), ldbx, &b_ref(perm[i__], 1), ldb);
/* L90: */
	}

/*        Step (4R): apply back the Givens rotations performed. */

	for (i__ = *givptr; i__ >= 1; --i__) {
	    r__1 = -givnum_ref(i__, 1);
	    srot_(nrhs, &b_ref(givcol_ref(i__, 2), 1), ldb, &b_ref(givcol_ref(
		    i__, 1), 1), ldb, &givnum_ref(i__, 2), &r__1);
/* L100: */
	}
    }

    return 0;

/*     End of SLALS0 */

} /* slals0_ */

#undef givnum_ref
#undef givcol_ref
#undef bx_ref
#undef poles_ref
#undef b_ref
#undef difr_ref


