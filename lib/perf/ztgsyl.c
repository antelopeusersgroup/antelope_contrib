#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int ztgsyl_(char *trans, integer *ijob, integer *m, integer *
	n, doublecomplex *a, integer *lda, doublecomplex *b, integer *ldb, 
	doublecomplex *c__, integer *ldc, doublecomplex *d__, integer *ldd, 
	doublecomplex *e, integer *lde, doublecomplex *f, integer *ldf, 
	doublereal *scale, doublereal *dif, doublecomplex *work, integer *
	lwork, integer *iwork, integer *info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    ZTGSYL solves the generalized Sylvester equation:   

                A * R - L * B = scale * C            (1)   
                D * R - L * E = scale * F   

    where R and L are unknown m-by-n matrices, (A, D), (B, E) and   
    (C, F) are given matrix pairs of size m-by-m, n-by-n and m-by-n,   
    respectively, with complex entries. A, B, D and E are upper   
    triangular (i.e., (A,D) and (B,E) in generalized Schur form).   

    The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1   
    is an output scaling factor chosen to avoid overflow.   

    In matrix notation (1) is equivalent to solve Zx = scale*b, where Z   
    is defined as   

           Z = [ kron(In, A)  -kron(B', Im) ]        (2)   
               [ kron(In, D)  -kron(E', Im) ],   

    Here Ix is the identity matrix of size x and X' is the conjugate   
    transpose of X. Kron(X, Y) is the Kronecker product between the   
    matrices X and Y.   

    If TRANS = 'C', y in the conjugate transposed system Z'*y = scale*b   
    is solved for, which is equivalent to solve for R and L in   

                A' * R + D' * L = scale * C           (3)   
                R * B' + L * E' = scale * -F   

    This case (TRANS = 'C') is used to compute an one-norm-based estimate   
    of Dif[(A,D), (B,E)], the separation between the matrix pairs (A,D)   
    and (B,E), using ZLACON.   

    If IJOB >= 1, ZTGSYL computes a Frobenius norm-based estimate of   
    Dif[(A,D),(B,E)]. That is, the reciprocal of a lower bound on the   
    reciprocal of the smallest singular value of Z.   

    This is a level-3 BLAS algorithm.   

    Arguments   
    =========   

    TRANS   (input) CHARACTER*1   
            = 'N': solve the generalized sylvester equation (1).   
            = 'C': solve the "conjugate transposed" system (3).   

    IJOB    (input) INTEGER   
            Specifies what kind of functionality to be performed.   
            =0: solve (1) only.   
            =1: The functionality of 0 and 3.   
            =2: The functionality of 0 and 4.   
            =3: Only an estimate of Dif[(A,D), (B,E)] is computed.   
                (look ahead strategy is used).   
            =4: Only an estimate of Dif[(A,D), (B,E)] is computed.   
                (ZGECON on sub-systems is used).   
            Not referenced if TRANS = 'C'.   

    M       (input) INTEGER   
            The order of the matrices A and D, and the row dimension of   
            the matrices C, F, R and L.   

    N       (input) INTEGER   
            The order of the matrices B and E, and the column dimension   
            of the matrices C, F, R and L.   

    A       (input) COMPLEX*16 array, dimension (LDA, M)   
            The upper triangular matrix A.   

    LDA     (input) INTEGER   
            The leading dimension of the array A. LDA >= max(1, M).   

    B       (input) COMPLEX*16 array, dimension (LDB, N)   
            The upper triangular matrix B.   

    LDB     (input) INTEGER   
            The leading dimension of the array B. LDB >= max(1, N).   

    C       (input/output) COMPLEX*16 array, dimension (LDC, N)   
            On entry, C contains the right-hand-side of the first matrix   
            equation in (1) or (3).   
            On exit, if IJOB = 0, 1 or 2, C has been overwritten by   
            the solution R. If IJOB = 3 or 4 and TRANS = 'N', C holds R,   
            the solution achieved during the computation of the   
            Dif-estimate.   

    LDC     (input) INTEGER   
            The leading dimension of the array C. LDC >= max(1, M).   

    D       (input) COMPLEX*16 array, dimension (LDD, M)   
            The upper triangular matrix D.   

    LDD     (input) INTEGER   
            The leading dimension of the array D. LDD >= max(1, M).   

    E       (input) COMPLEX*16 array, dimension (LDE, N)   
            The upper triangular matrix E.   

    LDE     (input) INTEGER   
            The leading dimension of the array E. LDE >= max(1, N).   

    F       (input/output) COMPLEX*16 array, dimension (LDF, N)   
            On entry, F contains the right-hand-side of the second matrix   
            equation in (1) or (3).   
            On exit, if IJOB = 0, 1 or 2, F has been overwritten by   
            the solution L. If IJOB = 3 or 4 and TRANS = 'N', F holds L,   
            the solution achieved during the computation of the   
            Dif-estimate.   

    LDF     (input) INTEGER   
            The leading dimension of the array F. LDF >= max(1, M).   

    DIF     (output) DOUBLE PRECISION   
            On exit DIF is the reciprocal of a lower bound of the   
            reciprocal of the Dif-function, i.e. DIF is an upper bound of   
            Dif[(A,D), (B,E)] = sigma-min(Z), where Z as in (2).   
            IF IJOB = 0 or TRANS = 'C', DIF is not referenced.   

    SCALE   (output) DOUBLE PRECISION   
            On exit SCALE is the scaling factor in (1) or (3).   
            If 0 < SCALE < 1, C and F hold the solutions R and L, resp.,   
            to a slightly perturbed system but the input matrices A, B,   
            D and E have not been changed. If SCALE = 0, R and L will   
            hold the solutions to the homogenious system with C = F = 0.   

    WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)   
            IF IJOB = 0, WORK is not referenced.  Otherwise,   

    LWORK   (input) INTEGER   
            The dimension of the array WORK. LWORK > = 1.   
            If IJOB = 1 or 2 and TRANS = 'N', LWORK >= 2*M*N.   

            If LWORK = -1, then a workspace query is assumed; the routine   
            only calculates the optimal size of the WORK array, returns   
            this value as the first entry of the WORK array, and no error   
            message related to LWORK is issued by XERBLA.   

    IWORK   (workspace) INTEGER array, dimension (M+N+2)   
            If IJOB = 0, IWORK is not referenced.   

    INFO    (output) INTEGER   
              =0: successful exit   
              <0: If INFO = -i, the i-th argument had an illegal value.   
              >0: (A, D) and (B, E) have common or very close   
                  eigenvalues.   

    Further Details   
    ===============   

    Based on contributions by   
       Bo Kagstrom and Peter Poromaa, Department of Computing Science,   
       Umea University, S-901 87 Umea, Sweden.   

    [1] B. Kagstrom and P. Poromaa, LAPACK-Style Algorithms and Software   
        for Solving the Generalized Sylvester Equation and Estimating the   
        Separation between Regular Matrix Pairs, Report UMINF - 93.23,   
        Department of Computing Science, Umea University, S-901 87 Umea,   
        Sweden, December 1993, Revised April 1994, Also as LAPACK Working   
        Note 75.  To appear in ACM Trans. on Math. Software, Vol 22,   
        No 1, 1996.   

    [2] B. Kagstrom, A Perturbation Analysis of the Generalized Sylvester   
        Equation (AR - LB, DR - LE ) = (C, F), SIAM J. Matrix Anal.   
        Appl., 15(4):1045-1060, 1994.   

    [3] B. Kagstrom and L. Westin, Generalized Schur Methods with   
        Condition Estimators for Solving the Generalized Sylvester   
        Equation, IEEE Transactions on Automatic Control, Vol. 34, No. 7,   
        July 1989, pp 745-751.   

    =====================================================================   


       Decode and test input parameters   

       Parameter adjustments */
    /* Table of constant values */
    static integer c__2 = 2;
    static integer c_n1 = -1;
    static integer c__5 = 5;
    static integer c__0 = 0;
    static integer c__1 = 1;
    static doublecomplex c_b16 = {0.,0.};
    static doublecomplex c_b53 = {-1.,0.};
    static doublecomplex c_b54 = {1.,0.};
    
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, d_dim1, 
	    d_offset, e_dim1, e_offset, f_dim1, f_offset, i__1, i__2, i__3, 
	    i__4;
    doublecomplex z__1;
    /* Builtin functions */
    double sqrt(doublereal);
    /* Local variables */
    static doublereal dsum;
    static integer i__, j, k, p, q;
    extern logical lsame_(char *, char *);
    static integer ifunc, linfo;
    extern /* Subroutine */ int zscal_(integer *, doublecomplex *, 
	    doublecomplex *, integer *), zgemm_(char *, char *, integer *, 
	    integer *, integer *, doublecomplex *, doublecomplex *, integer *,
	     doublecomplex *, integer *, doublecomplex *, doublecomplex *, 
	    integer *);
    static integer lwmin;
    extern /* Subroutine */ int zcopy_(integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *);
    static doublereal scale2;
    static integer ie, je, mb, nb;
    static doublereal dscale;
    static integer is, js, pq;
    extern /* Subroutine */ int ztgsy2_(char *, integer *, integer *, integer 
	    *, doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *);
    static doublereal scaloc;
    extern /* Subroutine */ int xerbla_(char *, integer *);
    extern integer ilaenv_(integer *, char *, char *, integer *, integer *, 
	    integer *, integer *, ftnlen, ftnlen);
    static integer iround;
    static logical notran;
    static integer isolve;
    extern /* Subroutine */ int zlacpy_(char *, integer *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    static logical lquery;
#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]
#define b_subscr(a_1,a_2) (a_2)*b_dim1 + a_1
#define b_ref(a_1,a_2) b[b_subscr(a_1,a_2)]
#define c___subscr(a_1,a_2) (a_2)*c_dim1 + a_1
#define c___ref(a_1,a_2) c__[c___subscr(a_1,a_2)]
#define d___subscr(a_1,a_2) (a_2)*d_dim1 + a_1
#define d___ref(a_1,a_2) d__[d___subscr(a_1,a_2)]
#define e_subscr(a_1,a_2) (a_2)*e_dim1 + a_1
#define e_ref(a_1,a_2) e[e_subscr(a_1,a_2)]
#define f_subscr(a_1,a_2) (a_2)*f_dim1 + a_1
#define f_ref(a_1,a_2) f[f_subscr(a_1,a_2)]


    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    c_dim1 = *ldc;
    c_offset = 1 + c_dim1 * 1;
    c__ -= c_offset;
    d_dim1 = *ldd;
    d_offset = 1 + d_dim1 * 1;
    d__ -= d_offset;
    e_dim1 = *lde;
    e_offset = 1 + e_dim1 * 1;
    e -= e_offset;
    f_dim1 = *ldf;
    f_offset = 1 + f_dim1 * 1;
    f -= f_offset;
    --work;
    --iwork;

    /* Function Body */
    *info = 0;
    notran = lsame_(trans, "N");
    lquery = *lwork == -1;

    if ((*ijob == 1 || *ijob == 2) && notran) {
/* Computing MAX */
	i__1 = 1, i__2 = (*m << 1) * *n;
	lwmin = max(i__1,i__2);
    } else {
	lwmin = 1;
    }

    if (! notran && ! lsame_(trans, "C")) {
	*info = -1;
    } else if (*ijob < 0 || *ijob > 4) {
	*info = -2;
    } else if (*m <= 0) {
	*info = -3;
    } else if (*n <= 0) {
	*info = -4;
    } else if (*lda < max(1,*m)) {
	*info = -6;
    } else if (*ldb < max(1,*n)) {
	*info = -8;
    } else if (*ldc < max(1,*m)) {
	*info = -10;
    } else if (*ldd < max(1,*m)) {
	*info = -12;
    } else if (*lde < max(1,*n)) {
	*info = -14;
    } else if (*ldf < max(1,*m)) {
	*info = -16;
    } else if (*lwork < lwmin && ! lquery) {
	*info = -20;
    }

    if (*info == 0) {
	work[1].r = (doublereal) lwmin, work[1].i = 0.;
    }

    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("ZTGSYL", &i__1);
	return 0;
    } else if (lquery) {
	return 0;
    }

/*     Determine  optimal block sizes MB and NB */

    mb = ilaenv_(&c__2, "ZTGSYL", trans, m, n, &c_n1, &c_n1, (ftnlen)6, (
	    ftnlen)1);
    nb = ilaenv_(&c__5, "ZTGSYL", trans, m, n, &c_n1, &c_n1, (ftnlen)6, (
	    ftnlen)1);

    isolve = 1;
    ifunc = 0;
    if (*ijob >= 3 && notran) {
	ifunc = *ijob - 2;
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    zcopy_(m, &c_b16, &c__0, &c___ref(1, j), &c__1);
	    zcopy_(m, &c_b16, &c__0, &f_ref(1, j), &c__1);
/* L10: */
	}
    } else if (*ijob >= 1 && notran) {
	isolve = 2;
    }

    if (mb <= 1 && nb <= 1 || mb >= *m && nb >= *n) {

/*        Use unblocked Level 2 solver */

	i__1 = isolve;
	for (iround = 1; iround <= i__1; ++iround) {

	    *scale = 1.;
	    dscale = 0.;
	    dsum = 1.;
	    pq = *m * *n;
	    ztgsy2_(trans, &ifunc, m, n, &a[a_offset], lda, &b[b_offset], ldb,
		     &c__[c_offset], ldc, &d__[d_offset], ldd, &e[e_offset], 
		    lde, &f[f_offset], ldf, scale, &dsum, &dscale, info);
	    if (dscale != 0.) {
		if (*ijob == 1 || *ijob == 3) {
		    *dif = sqrt((doublereal) ((*m << 1) * *n)) / (dscale * 
			    sqrt(dsum));
		} else {
		    *dif = sqrt((doublereal) pq) / (dscale * sqrt(dsum));
		}
	    }
	    if (isolve == 2 && iround == 1) {
		ifunc = *ijob;
		scale2 = *scale;
		zlacpy_("F", m, n, &c__[c_offset], ldc, &work[1], m);
		zlacpy_("F", m, n, &f[f_offset], ldf, &work[*m * *n + 1], m);
		i__2 = *n;
		for (j = 1; j <= i__2; ++j) {
		    zcopy_(m, &c_b16, &c__0, &c___ref(1, j), &c__1);
		    zcopy_(m, &c_b16, &c__0, &f_ref(1, j), &c__1);
/* L20: */
		}
	    } else if (isolve == 2 && iround == 2) {
		zlacpy_("F", m, n, &work[1], m, &c__[c_offset], ldc);
		zlacpy_("F", m, n, &work[*m * *n + 1], m, &f[f_offset], ldf);
		*scale = scale2;
	    }
/* L30: */
	}

	return 0;

    }

/*     Determine block structure of A */

    p = 0;
    i__ = 1;
L40:
    if (i__ > *m) {
	goto L50;
    }
    ++p;
    iwork[p] = i__;
    i__ += mb;
    if (i__ >= *m) {
	goto L50;
    }
    goto L40;
L50:
    iwork[p + 1] = *m + 1;
    if (iwork[p] == iwork[p + 1]) {
	--p;
    }

/*     Determine block structure of B */

    q = p + 1;
    j = 1;
L60:
    if (j > *n) {
	goto L70;
    }

    ++q;
    iwork[q] = j;
    j += nb;
    if (j >= *n) {
	goto L70;
    }
    goto L60;

L70:
    iwork[q + 1] = *n + 1;
    if (iwork[q] == iwork[q + 1]) {
	--q;
    }

    if (notran) {
	i__1 = isolve;
	for (iround = 1; iround <= i__1; ++iround) {

/*           Solve (I, J) - subsystem   
                 A(I, I) * R(I, J) - L(I, J) * B(J, J) = C(I, J)   
                 D(I, I) * R(I, J) - L(I, J) * E(J, J) = F(I, J)   
             for I = P, P - 1, ..., 1; J = 1, 2, ..., Q */

	    pq = 0;
	    *scale = 1.;
	    dscale = 0.;
	    dsum = 1.;
	    i__2 = q;
	    for (j = p + 2; j <= i__2; ++j) {
		js = iwork[j];
		je = iwork[j + 1] - 1;
		nb = je - js + 1;
		for (i__ = p; i__ >= 1; --i__) {
		    is = iwork[i__];
		    ie = iwork[i__ + 1] - 1;
		    mb = ie - is + 1;
		    ztgsy2_(trans, &ifunc, &mb, &nb, &a_ref(is, is), lda, &
			    b_ref(js, js), ldb, &c___ref(is, js), ldc, &
			    d___ref(is, is), ldd, &e_ref(js, js), lde, &f_ref(
			    is, js), ldf, &scaloc, &dsum, &dscale, &linfo);
		    if (linfo > 0) {
			*info = linfo;
		    }
		    pq += mb * nb;
		    if (scaloc != 1.) {
			i__3 = js - 1;
			for (k = 1; k <= i__3; ++k) {
			    z__1.r = scaloc, z__1.i = 0.;
			    zscal_(m, &z__1, &c___ref(1, k), &c__1);
			    z__1.r = scaloc, z__1.i = 0.;
			    zscal_(m, &z__1, &f_ref(1, k), &c__1);
/* L80: */
			}
			i__3 = je;
			for (k = js; k <= i__3; ++k) {
			    i__4 = is - 1;
			    z__1.r = scaloc, z__1.i = 0.;
			    zscal_(&i__4, &z__1, &c___ref(1, k), &c__1);
			    i__4 = is - 1;
			    z__1.r = scaloc, z__1.i = 0.;
			    zscal_(&i__4, &z__1, &f_ref(1, k), &c__1);
/* L90: */
			}
			i__3 = je;
			for (k = js; k <= i__3; ++k) {
			    i__4 = *m - ie;
			    z__1.r = scaloc, z__1.i = 0.;
			    zscal_(&i__4, &z__1, &c___ref(ie + 1, k), &c__1);
			    i__4 = *m - ie;
			    z__1.r = scaloc, z__1.i = 0.;
			    zscal_(&i__4, &z__1, &f_ref(ie + 1, k), &c__1);
/* L100: */
			}
			i__3 = *n;
			for (k = je + 1; k <= i__3; ++k) {
			    z__1.r = scaloc, z__1.i = 0.;
			    zscal_(m, &z__1, &c___ref(1, k), &c__1);
			    z__1.r = scaloc, z__1.i = 0.;
			    zscal_(m, &z__1, &f_ref(1, k), &c__1);
/* L110: */
			}
			*scale *= scaloc;
		    }

/*                 Substitute R(I,J) and L(I,J) into remaining equation. */

		    if (i__ > 1) {
			i__3 = is - 1;
			zgemm_("N", "N", &i__3, &nb, &mb, &c_b53, &a_ref(1, 
				is), lda, &c___ref(is, js), ldc, &c_b54, &
				c___ref(1, js), ldc);
			i__3 = is - 1;
			zgemm_("N", "N", &i__3, &nb, &mb, &c_b53, &d___ref(1, 
				is), ldd, &c___ref(is, js), ldc, &c_b54, &
				f_ref(1, js), ldf);
		    }
		    if (j < q) {
			i__3 = *n - je;
			zgemm_("N", "N", &mb, &i__3, &nb, &c_b54, &f_ref(is, 
				js), ldf, &b_ref(js, je + 1), ldb, &c_b54, &
				c___ref(is, je + 1), ldc);
			i__3 = *n - je;
			zgemm_("N", "N", &mb, &i__3, &nb, &c_b54, &f_ref(is, 
				js), ldf, &e_ref(js, je + 1), lde, &c_b54, &
				f_ref(is, je + 1), ldf);
		    }
/* L120: */
		}
/* L130: */
	    }
	    if (dscale != 0.) {
		if (*ijob == 1 || *ijob == 3) {
		    *dif = sqrt((doublereal) ((*m << 1) * *n)) / (dscale * 
			    sqrt(dsum));
		} else {
		    *dif = sqrt((doublereal) pq) / (dscale * sqrt(dsum));
		}
	    }
	    if (isolve == 2 && iround == 1) {
		ifunc = *ijob;
		scale2 = *scale;
		zlacpy_("F", m, n, &c__[c_offset], ldc, &work[1], m);
		zlacpy_("F", m, n, &f[f_offset], ldf, &work[*m * *n + 1], m);
		i__2 = *n;
		for (j = 1; j <= i__2; ++j) {
		    zcopy_(m, &c_b16, &c__0, &c___ref(1, j), &c__1);
		    zcopy_(m, &c_b16, &c__0, &f_ref(1, j), &c__1);
/* L140: */
		}
	    } else if (isolve == 2 && iround == 2) {
		zlacpy_("F", m, n, &work[1], m, &c__[c_offset], ldc);
		zlacpy_("F", m, n, &work[*m * *n + 1], m, &f[f_offset], ldf);
		*scale = scale2;
	    }
/* L150: */
	}
    } else {

/*        Solve transposed (I, J)-subsystem   
              A(I, I)' * R(I, J) + D(I, I)' * L(I, J) = C(I, J)   
              R(I, J) * B(J, J)  + L(I, J) * E(J, J) = -F(I, J)   
          for I = 1,2,..., P; J = Q, Q-1,..., 1 */

	*scale = 1.;
	i__1 = p;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    is = iwork[i__];
	    ie = iwork[i__ + 1] - 1;
	    mb = ie - is + 1;
	    i__2 = p + 2;
	    for (j = q; j >= i__2; --j) {
		js = iwork[j];
		je = iwork[j + 1] - 1;
		nb = je - js + 1;
		ztgsy2_(trans, &ifunc, &mb, &nb, &a_ref(is, is), lda, &b_ref(
			js, js), ldb, &c___ref(is, js), ldc, &d___ref(is, is),
			 ldd, &e_ref(js, js), lde, &f_ref(is, js), ldf, &
			scaloc, &dsum, &dscale, &linfo);
		if (linfo > 0) {
		    *info = linfo;
		}
		if (scaloc != 1.) {
		    i__3 = js - 1;
		    for (k = 1; k <= i__3; ++k) {
			z__1.r = scaloc, z__1.i = 0.;
			zscal_(m, &z__1, &c___ref(1, k), &c__1);
			z__1.r = scaloc, z__1.i = 0.;
			zscal_(m, &z__1, &f_ref(1, k), &c__1);
/* L160: */
		    }
		    i__3 = je;
		    for (k = js; k <= i__3; ++k) {
			i__4 = is - 1;
			z__1.r = scaloc, z__1.i = 0.;
			zscal_(&i__4, &z__1, &c___ref(1, k), &c__1);
			i__4 = is - 1;
			z__1.r = scaloc, z__1.i = 0.;
			zscal_(&i__4, &z__1, &f_ref(1, k), &c__1);
/* L170: */
		    }
		    i__3 = je;
		    for (k = js; k <= i__3; ++k) {
			i__4 = *m - ie;
			z__1.r = scaloc, z__1.i = 0.;
			zscal_(&i__4, &z__1, &c___ref(ie + 1, k), &c__1);
			i__4 = *m - ie;
			z__1.r = scaloc, z__1.i = 0.;
			zscal_(&i__4, &z__1, &f_ref(ie + 1, k), &c__1);
/* L180: */
		    }
		    i__3 = *n;
		    for (k = je + 1; k <= i__3; ++k) {
			z__1.r = scaloc, z__1.i = 0.;
			zscal_(m, &z__1, &c___ref(1, k), &c__1);
			z__1.r = scaloc, z__1.i = 0.;
			zscal_(m, &z__1, &f_ref(1, k), &c__1);
/* L190: */
		    }
		    *scale *= scaloc;
		}

/*              Substitute R(I,J) and L(I,J) into remaining equation. */

		if (j > p + 2) {
		    i__3 = js - 1;
		    zgemm_("N", "C", &mb, &i__3, &nb, &c_b54, &c___ref(is, js)
			    , ldc, &b_ref(1, js), ldb, &c_b54, &f_ref(is, 1), 
			    ldf);
		    i__3 = js - 1;
		    zgemm_("N", "C", &mb, &i__3, &nb, &c_b54, &f_ref(is, js), 
			    ldf, &e_ref(1, js), lde, &c_b54, &f_ref(is, 1), 
			    ldf);
		}
		if (i__ < p) {
		    i__3 = *m - ie;
		    zgemm_("C", "N", &i__3, &nb, &mb, &c_b53, &a_ref(is, ie + 
			    1), lda, &c___ref(is, js), ldc, &c_b54, &c___ref(
			    ie + 1, js), ldc);
		    i__3 = *m - ie;
		    zgemm_("C", "N", &i__3, &nb, &mb, &c_b53, &d___ref(is, ie 
			    + 1), ldd, &f_ref(is, js), ldf, &c_b54, &c___ref(
			    ie + 1, js), ldc);
		}
/* L200: */
	    }
/* L210: */
	}
    }

    work[1].r = (doublereal) lwmin, work[1].i = 0.;

    return 0;

/*     End of ZTGSYL */

} /* ztgsyl_ */

#undef f_ref
#undef f_subscr
#undef e_ref
#undef e_subscr
#undef d___ref
#undef d___subscr
#undef c___ref
#undef c___subscr
#undef b_ref
#undef b_subscr
#undef a_ref
#undef a_subscr


