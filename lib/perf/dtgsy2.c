#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int dtgsy2_(char *trans, integer *ijob, integer *m, integer *
	n, doublereal *a, integer *lda, doublereal *b, integer *ldb, 
	doublereal *c__, integer *ldc, doublereal *d__, integer *ldd, 
	doublereal *e, integer *lde, doublereal *f, integer *ldf, doublereal *
	scale, doublereal *rdsum, doublereal *rdscal, integer *iwork, integer 
	*pq, integer *info)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    DTGSY2 solves the generalized Sylvester equation:   

                A * R - L * B = scale * C                (1)   
                D * R - L * E = scale * F,   

    using Level 1 and 2 BLAS. where R and L are unknown M-by-N matrices,   
    (A, D), (B, E) and (C, F) are given matrix pairs of size M-by-M,   
    N-by-N and M-by-N, respectively, with real entries. (A, D) and (B, E)   
    must be in generalized Schur canonical form, i.e. A, B are upper   
    quasi triangular and D, E are upper triangular. The solution (R, L)   
    overwrites (C, F). 0 <= SCALE <= 1 is an output scaling factor   
    chosen to avoid overflow.   

    In matrix notation solving equation (1) corresponds to solve   
    Z*x = scale*b, where Z is defined as   

           Z = [ kron(In, A)  -kron(B', Im) ]             (2)   
               [ kron(In, D)  -kron(E', Im) ],   

    Ik is the identity matrix of size k and X' is the transpose of X.   
    kron(X, Y) is the Kronecker product between the matrices X and Y.   
    In the process of solving (1), we solve a number of such systems   
    where Dim(In), Dim(In) = 1 or 2.   

    If TRANS = 'T', solve the transposed system Z'*y = scale*b for y,   
    which is equivalent to solve for R and L in   

                A' * R  + D' * L   = scale *  C           (3)   
                R  * B' + L  * E'  = scale * -F   

    This case is used to compute an estimate of Dif[(A, D), (B, E)] =   
    sigma_min(Z) using reverse communicaton with DLACON.   

    DTGSY2 also (IJOB >= 1) contributes to the computation in STGSYL   
    of an upper bound on the separation between to matrix pairs. Then   
    the input (A, D), (B, E) are sub-pencils of the matrix pair in   
    DTGSYL. See STGSYL for details.   

    Arguments   
    =========   

    TRANS   (input) CHARACTER   
            = 'N', solve the generalized Sylvester equation (1).   
            = 'T': solve the 'transposed' system (3).   

    IJOB    (input) INTEGER   
            Specifies what kind of functionality to be performed.   
            = 0: solve (1) only.   
            = 1: A contribution from this subsystem to a Frobenius   
                 norm-based estimate of the separation between two matrix   
                 pairs is computed. (look ahead strategy is used).   
            = 2: A contribution from this subsystem to a Frobenius   
                 norm-based estimate of the separation between two matrix   
                 pairs is computed. (DGECON on sub-systems is used.)   
            Not referenced if TRANS = 'T'.   

    M       (input) INTEGER   
            On entry, M specifies the order of A and D, and the row   
            dimension of C, F, R and L.   

    N       (input) INTEGER   
            On entry, N specifies the order of B and E, and the column   
            dimension of C, F, R and L.   

    A       (input) DOUBLE PRECISION array, dimension (LDA, M)   
            On entry, A contains an upper quasi triangular matrix.   

    LDA     (input) INTEGER   
            The leading dimension of the matrix A. LDA >= max(1, M).   

    B       (input) DOUBLE PRECISION array, dimension (LDB, N)   
            On entry, B contains an upper quasi triangular matrix.   

    LDB     (input) INTEGER   
            The leading dimension of the matrix B. LDB >= max(1, N).   

    C       (input/ output) DOUBLE PRECISION array, dimension (LDC, N)   
            On entry, C contains the right-hand-side of the first matrix   
            equation in (1).   
            On exit, if IJOB = 0, C has been overwritten by the   
            solution R.   

    LDC     (input) INTEGER   
            The leading dimension of the matrix C. LDC >= max(1, M).   

    D       (input) DOUBLE PRECISION array, dimension (LDD, M)   
            On entry, D contains an upper triangular matrix.   

    LDD     (input) INTEGER   
            The leading dimension of the matrix D. LDD >= max(1, M).   

    E       (input) DOUBLE PRECISION array, dimension (LDE, N)   
            On entry, E contains an upper triangular matrix.   

    LDE     (input) INTEGER   
            The leading dimension of the matrix E. LDE >= max(1, N).   

    F       (input/ output) DOUBLE PRECISION array, dimension (LDF, N)   
            On entry, F contains the right-hand-side of the second matrix   
            equation in (1).   
            On exit, if IJOB = 0, F has been overwritten by the   
            solution L.   

    LDF     (input) INTEGER   
            The leading dimension of the matrix F. LDF >= max(1, M).   

    SCALE   (output) DOUBLE PRECISION   
            On exit, 0 <= SCALE <= 1. If 0 < SCALE < 1, the solutions   
            R and L (C and F on entry) will hold the solutions to a   
            slightly perturbed system but the input matrices A, B, D and   
            E have not been changed. If SCALE = 0, R and L will hold the   
            solutions to the homogeneous system with C = F = 0. Normally,   
            SCALE = 1.   

    RDSUM   (input/output) DOUBLE PRECISION   
            On entry, the sum of squares of computed contributions to   
            the Dif-estimate under computation by DTGSYL, where the   
            scaling factor RDSCAL (see below) has been factored out.   
            On exit, the corresponding sum of squares updated with the   
            contributions from the current sub-system.   
            If TRANS = 'T' RDSUM is not touched.   
            NOTE: RDSUM only makes sense when DTGSY2 is called by STGSYL.   

    RDSCAL  (input/output) DOUBLE PRECISION   
            On entry, scaling factor used to prevent overflow in RDSUM.   
            On exit, RDSCAL is updated w.r.t. the current contributions   
            in RDSUM.   
            If TRANS = 'T', RDSCAL is not touched.   
            NOTE: RDSCAL only makes sense when DTGSY2 is called by   
                  DTGSYL.   

    IWORK   (workspace) INTEGER array, dimension (M+N+2)   

    PQ      (output) INTEGER   
            On exit, the number of subsystems (of size 2-by-2, 4-by-4 and   
            8-by-8) solved by this routine.   

    INFO    (output) INTEGER   
            On exit, if INFO is set to   
              =0: Successful exit   
              <0: If INFO = -i, the i-th argument had an illegal value.   
              >0: The matrix pairs (A, D) and (B, E) have common or very   
                  close eigenvalues.   

    Further Details   
    ===============   

    Based on contributions by   
       Bo Kagstrom and Peter Poromaa, Department of Computing Science,   
       Umea University, S-901 87 Umea, Sweden.   

    =====================================================================   


       Decode and test input parameters   

       Parameter adjustments */
    /* Table of constant values */
    static integer c__8 = 8;
    static integer c__1 = 1;
    static doublereal c_b27 = -1.;
    static doublereal c_b42 = 1.;
    static integer c__64 = 64;
    static doublereal c_b54 = 0.;
    static integer c__0 = 0;
    
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, d_dim1, 
	    d_offset, e_dim1, e_offset, f_dim1, f_offset, i__1, i__2, i__3;
    /* Local variables */
    extern /* Subroutine */ int dger_(integer *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, integer *, doublereal *, 
	    integer *);
    static integer ierr, zdim, ipiv[8], jpiv[8], i__, j, k, p, q;
    static doublereal alpha;
    extern /* Subroutine */ int dscal_(integer *, doublereal *, doublereal *, 
	    integer *), dgemm_(char *, char *, integer *, integer *, integer *
	    , doublereal *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, integer *);
    static doublereal z__[64]	/* was [8][8] */;
    extern logical lsame_(char *, char *);
    extern /* Subroutine */ int dgemv_(char *, integer *, integer *, 
	    doublereal *, doublereal *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, integer *), dcopy_(integer *, 
	    doublereal *, integer *, doublereal *, integer *), daxpy_(integer 
	    *, doublereal *, doublereal *, integer *, doublereal *, integer *)
	    , dgesc2_(integer *, doublereal *, integer *, doublereal *, 
	    integer *, integer *, doublereal *), dgetc2_(integer *, 
	    doublereal *, integer *, integer *, integer *, integer *);
    static integer ie, je, mb, nb, ii, jj, is, js;
    extern /* Subroutine */ int dlatdf_(integer *, integer *, doublereal *, 
	    integer *, doublereal *, doublereal *, doublereal *, integer *, 
	    integer *);
    static doublereal scaloc;
    extern /* Subroutine */ int xerbla_(char *, integer *);
    static logical notran;
    static doublereal rhs[8];
    static integer isp1, jsp1;
#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]
#define c___ref(a_1,a_2) c__[(a_2)*c_dim1 + a_1]
#define d___ref(a_1,a_2) d__[(a_2)*d_dim1 + a_1]
#define e_ref(a_1,a_2) e[(a_2)*e_dim1 + a_1]
#define f_ref(a_1,a_2) f[(a_2)*f_dim1 + a_1]
#define z___ref(a_1,a_2) z__[(a_2)*8 + a_1 - 9]


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
    --iwork;

    /* Function Body */
    *info = 0;
    ierr = 0;
    notran = lsame_(trans, "N");
    if (! notran && ! lsame_(trans, "T")) {
	*info = -1;
    } else if (*ijob < 0 || *ijob > 2) {
	*info = -2;
    } else if (*m <= 0) {
	*info = -3;
    } else if (*n <= 0) {
	*info = -4;
    } else if (*lda < max(1,*m)) {
	*info = -5;
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
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("DTGSY2", &i__1);
	return 0;
    }

/*     Determine block structure of A */

    *pq = 0;
    p = 0;
    i__ = 1;
L10:
    if (i__ > *m) {
	goto L20;
    }
    ++p;
    iwork[p] = i__;
    if (i__ == *m) {
	goto L20;
    }
    if (a_ref(i__ + 1, i__) != 0.) {
	i__ += 2;
    } else {
	++i__;
    }
    goto L10;
L20:
    iwork[p + 1] = *m + 1;

/*     Determine block structure of B */

    q = p + 1;
    j = 1;
L30:
    if (j > *n) {
	goto L40;
    }
    ++q;
    iwork[q] = j;
    if (j == *n) {
	goto L40;
    }
    if (b_ref(j + 1, j) != 0.) {
	j += 2;
    } else {
	++j;
    }
    goto L30;
L40:
    iwork[q + 1] = *n + 1;
    *pq = p * (q - p - 1);

    if (notran) {

/*        Solve (I, J) - subsystem   
             A(I, I) * R(I, J) - L(I, J) * B(J, J) = C(I, J)   
             D(I, I) * R(I, J) - L(I, J) * E(J, J) = F(I, J)   
          for I = P, P - 1, ..., 1; J = 1, 2, ..., Q */

	*scale = 1.;
	scaloc = 1.;
	i__1 = q;
	for (j = p + 2; j <= i__1; ++j) {
	    js = iwork[j];
	    jsp1 = js + 1;
	    je = iwork[j + 1] - 1;
	    nb = je - js + 1;
	    for (i__ = p; i__ >= 1; --i__) {

		is = iwork[i__];
		isp1 = is + 1;
		ie = iwork[i__ + 1] - 1;
		mb = ie - is + 1;
		zdim = mb * nb << 1;

		if (mb == 1 && nb == 1) {

/*                 Build a 2-by-2 system Z * x = RHS */

		    z___ref(1, 1) = a_ref(is, is);
		    z___ref(2, 1) = d___ref(is, is);
		    z___ref(1, 2) = -b_ref(js, js);
		    z___ref(2, 2) = -e_ref(js, js);

/*                 Set up right hand side(s) */

		    rhs[0] = c___ref(is, js);
		    rhs[1] = f_ref(is, js);

/*                 Solve Z * x = RHS */

		    dgetc2_(&zdim, z__, &c__8, ipiv, jpiv, &ierr);
		    if (ierr > 0) {
			*info = ierr;
		    }

		    if (*ijob == 0) {
			dgesc2_(&zdim, z__, &c__8, rhs, ipiv, jpiv, &scaloc);
			if (scaloc != 1.) {
			    i__2 = *n;
			    for (k = 1; k <= i__2; ++k) {
				dscal_(m, &scaloc, &c___ref(1, k), &c__1);
				dscal_(m, &scaloc, &f_ref(1, k), &c__1);
/* L50: */
			    }
			    *scale *= scaloc;
			}
		    } else {
			dlatdf_(ijob, &zdim, z__, &c__8, rhs, rdsum, rdscal, 
				ipiv, jpiv);
		    }

/*                 Unpack solution vector(s) */

		    c___ref(is, js) = rhs[0];
		    f_ref(is, js) = rhs[1];

/*                 Substitute R(I, J) and L(I, J) into remaining   
                   equation. */

		    if (i__ > 1) {
			alpha = -rhs[0];
			i__2 = is - 1;
			daxpy_(&i__2, &alpha, &a_ref(1, is), &c__1, &c___ref(
				1, js), &c__1);
			i__2 = is - 1;
			daxpy_(&i__2, &alpha, &d___ref(1, is), &c__1, &f_ref(
				1, js), &c__1);
		    }
		    if (j < q) {
			i__2 = *n - je;
			daxpy_(&i__2, &rhs[1], &b_ref(js, je + 1), ldb, &
				c___ref(is, je + 1), ldc);
			i__2 = *n - je;
			daxpy_(&i__2, &rhs[1], &e_ref(js, je + 1), lde, &
				f_ref(is, je + 1), ldf);
		    }

		} else if (mb == 1 && nb == 2) {

/*                 Build a 4-by-4 system Z * x = RHS */

		    z___ref(1, 1) = a_ref(is, is);
		    z___ref(2, 1) = 0.;
		    z___ref(3, 1) = d___ref(is, is);
		    z___ref(4, 1) = 0.;

		    z___ref(1, 2) = 0.;
		    z___ref(2, 2) = a_ref(is, is);
		    z___ref(3, 2) = 0.;
		    z___ref(4, 2) = d___ref(is, is);

		    z___ref(1, 3) = -b_ref(js, js);
		    z___ref(2, 3) = -b_ref(js, jsp1);
		    z___ref(3, 3) = -e_ref(js, js);
		    z___ref(4, 3) = -e_ref(js, jsp1);

		    z___ref(1, 4) = -b_ref(jsp1, js);
		    z___ref(2, 4) = -b_ref(jsp1, jsp1);
		    z___ref(3, 4) = 0.;
		    z___ref(4, 4) = -e_ref(jsp1, jsp1);

/*                 Set up right hand side(s) */

		    rhs[0] = c___ref(is, js);
		    rhs[1] = c___ref(is, jsp1);
		    rhs[2] = f_ref(is, js);
		    rhs[3] = f_ref(is, jsp1);

/*                 Solve Z * x = RHS */

		    dgetc2_(&zdim, z__, &c__8, ipiv, jpiv, &ierr);
		    if (ierr > 0) {
			*info = ierr;
		    }

		    if (*ijob == 0) {
			dgesc2_(&zdim, z__, &c__8, rhs, ipiv, jpiv, &scaloc);
			if (scaloc != 1.) {
			    i__2 = *n;
			    for (k = 1; k <= i__2; ++k) {
				dscal_(m, &scaloc, &c___ref(1, k), &c__1);
				dscal_(m, &scaloc, &f_ref(1, k), &c__1);
/* L60: */
			    }
			    *scale *= scaloc;
			}
		    } else {
			dlatdf_(ijob, &zdim, z__, &c__8, rhs, rdsum, rdscal, 
				ipiv, jpiv);
		    }

/*                 Unpack solution vector(s) */

		    c___ref(is, js) = rhs[0];
		    c___ref(is, jsp1) = rhs[1];
		    f_ref(is, js) = rhs[2];
		    f_ref(is, jsp1) = rhs[3];

/*                 Substitute R(I, J) and L(I, J) into remaining   
                   equation. */

		    if (i__ > 1) {
			i__2 = is - 1;
			dger_(&i__2, &nb, &c_b27, &a_ref(1, is), &c__1, rhs, &
				c__1, &c___ref(1, js), ldc);
			i__2 = is - 1;
			dger_(&i__2, &nb, &c_b27, &d___ref(1, is), &c__1, rhs,
				 &c__1, &f_ref(1, js), ldf);
		    }
		    if (j < q) {
			i__2 = *n - je;
			daxpy_(&i__2, &rhs[2], &b_ref(js, je + 1), ldb, &
				c___ref(is, je + 1), ldc);
			i__2 = *n - je;
			daxpy_(&i__2, &rhs[2], &e_ref(js, je + 1), lde, &
				f_ref(is, je + 1), ldf);
			i__2 = *n - je;
			daxpy_(&i__2, &rhs[3], &b_ref(jsp1, je + 1), ldb, &
				c___ref(is, je + 1), ldc);
			i__2 = *n - je;
			daxpy_(&i__2, &rhs[3], &e_ref(jsp1, je + 1), lde, &
				f_ref(is, je + 1), ldf);
		    }

		} else if (mb == 2 && nb == 1) {

/*                 Build a 4-by-4 system Z * x = RHS */

		    z___ref(1, 1) = a_ref(is, is);
		    z___ref(2, 1) = a_ref(isp1, is);
		    z___ref(3, 1) = d___ref(is, is);
		    z___ref(4, 1) = 0.;

		    z___ref(1, 2) = a_ref(is, isp1);
		    z___ref(2, 2) = a_ref(isp1, isp1);
		    z___ref(3, 2) = d___ref(is, isp1);
		    z___ref(4, 2) = d___ref(isp1, isp1);

		    z___ref(1, 3) = -b_ref(js, js);
		    z___ref(2, 3) = 0.;
		    z___ref(3, 3) = -e_ref(js, js);
		    z___ref(4, 3) = 0.;

		    z___ref(1, 4) = 0.;
		    z___ref(2, 4) = -b_ref(js, js);
		    z___ref(3, 4) = 0.;
		    z___ref(4, 4) = -e_ref(js, js);

/*                 Set up right hand side(s) */

		    rhs[0] = c___ref(is, js);
		    rhs[1] = c___ref(isp1, js);
		    rhs[2] = f_ref(is, js);
		    rhs[3] = f_ref(isp1, js);

/*                 Solve Z * x = RHS */

		    dgetc2_(&zdim, z__, &c__8, ipiv, jpiv, &ierr);
		    if (ierr > 0) {
			*info = ierr;
		    }
		    if (*ijob == 0) {
			dgesc2_(&zdim, z__, &c__8, rhs, ipiv, jpiv, &scaloc);
			if (scaloc != 1.) {
			    i__2 = *n;
			    for (k = 1; k <= i__2; ++k) {
				dscal_(m, &scaloc, &c___ref(1, k), &c__1);
				dscal_(m, &scaloc, &f_ref(1, k), &c__1);
/* L70: */
			    }
			    *scale *= scaloc;
			}
		    } else {
			dlatdf_(ijob, &zdim, z__, &c__8, rhs, rdsum, rdscal, 
				ipiv, jpiv);
		    }

/*                 Unpack solution vector(s) */

		    c___ref(is, js) = rhs[0];
		    c___ref(isp1, js) = rhs[1];
		    f_ref(is, js) = rhs[2];
		    f_ref(isp1, js) = rhs[3];

/*                 Substitute R(I, J) and L(I, J) into remaining   
                   equation. */

		    if (i__ > 1) {
			i__2 = is - 1;
			dgemv_("N", &i__2, &mb, &c_b27, &a_ref(1, is), lda, 
				rhs, &c__1, &c_b42, &c___ref(1, js), &c__1);
			i__2 = is - 1;
			dgemv_("N", &i__2, &mb, &c_b27, &d___ref(1, is), ldd, 
				rhs, &c__1, &c_b42, &f_ref(1, js), &c__1);
		    }
		    if (j < q) {
			i__2 = *n - je;
			dger_(&mb, &i__2, &c_b42, &rhs[2], &c__1, &b_ref(js, 
				je + 1), ldb, &c___ref(is, je + 1), ldc);
			i__2 = *n - je;
			dger_(&mb, &i__2, &c_b42, &rhs[2], &c__1, &e_ref(js, 
				je + 1), ldb, &f_ref(is, je + 1), ldc);
		    }

		} else if (mb == 2 && nb == 2) {

/*                 Build an 8-by-8 system Z * x = RHS */

		    dcopy_(&c__64, &c_b54, &c__0, z__, &c__1);

		    z___ref(1, 1) = a_ref(is, is);
		    z___ref(2, 1) = a_ref(isp1, is);
		    z___ref(5, 1) = d___ref(is, is);

		    z___ref(1, 2) = a_ref(is, isp1);
		    z___ref(2, 2) = a_ref(isp1, isp1);
		    z___ref(5, 2) = d___ref(is, isp1);
		    z___ref(6, 2) = d___ref(isp1, isp1);

		    z___ref(3, 3) = a_ref(is, is);
		    z___ref(4, 3) = a_ref(isp1, is);
		    z___ref(7, 3) = d___ref(is, is);

		    z___ref(3, 4) = a_ref(is, isp1);
		    z___ref(4, 4) = a_ref(isp1, isp1);
		    z___ref(7, 4) = d___ref(is, isp1);
		    z___ref(8, 4) = d___ref(isp1, isp1);

		    z___ref(1, 5) = -b_ref(js, js);
		    z___ref(3, 5) = -b_ref(js, jsp1);
		    z___ref(5, 5) = -e_ref(js, js);
		    z___ref(7, 5) = -e_ref(js, jsp1);

		    z___ref(2, 6) = -b_ref(js, js);
		    z___ref(4, 6) = -b_ref(js, jsp1);
		    z___ref(6, 6) = -e_ref(js, js);
		    z___ref(8, 6) = -e_ref(js, jsp1);

		    z___ref(1, 7) = -b_ref(jsp1, js);
		    z___ref(3, 7) = -b_ref(jsp1, jsp1);
		    z___ref(7, 7) = -e_ref(jsp1, jsp1);

		    z___ref(2, 8) = -b_ref(jsp1, js);
		    z___ref(4, 8) = -b_ref(jsp1, jsp1);
		    z___ref(8, 8) = -e_ref(jsp1, jsp1);

/*                 Set up right hand side(s) */

		    k = 1;
		    ii = mb * nb + 1;
		    i__2 = nb - 1;
		    for (jj = 0; jj <= i__2; ++jj) {
			dcopy_(&mb, &c___ref(is, js + jj), &c__1, &rhs[k - 1],
				 &c__1);
			dcopy_(&mb, &f_ref(is, js + jj), &c__1, &rhs[ii - 1], 
				&c__1);
			k += mb;
			ii += mb;
/* L80: */
		    }

/*                 Solve Z * x = RHS */

		    dgetc2_(&zdim, z__, &c__8, ipiv, jpiv, &ierr);
		    if (ierr > 0) {
			*info = ierr;
		    }
		    if (*ijob == 0) {
			dgesc2_(&zdim, z__, &c__8, rhs, ipiv, jpiv, &scaloc);
			if (scaloc != 1.) {
			    i__2 = *n;
			    for (k = 1; k <= i__2; ++k) {
				dscal_(m, &scaloc, &c___ref(1, k), &c__1);
				dscal_(m, &scaloc, &f_ref(1, k), &c__1);
/* L90: */
			    }
			    *scale *= scaloc;
			}
		    } else {
			dlatdf_(ijob, &zdim, z__, &c__8, rhs, rdsum, rdscal, 
				ipiv, jpiv);
		    }

/*                 Unpack solution vector(s) */

		    k = 1;
		    ii = mb * nb + 1;
		    i__2 = nb - 1;
		    for (jj = 0; jj <= i__2; ++jj) {
			dcopy_(&mb, &rhs[k - 1], &c__1, &c___ref(is, js + jj),
				 &c__1);
			dcopy_(&mb, &rhs[ii - 1], &c__1, &f_ref(is, js + jj), 
				&c__1);
			k += mb;
			ii += mb;
/* L100: */
		    }

/*                 Substitute R(I, J) and L(I, J) into remaining   
                   equation. */

		    if (i__ > 1) {
			i__2 = is - 1;
			dgemm_("N", "N", &i__2, &nb, &mb, &c_b27, &a_ref(1, 
				is), lda, rhs, &mb, &c_b42, &c___ref(1, js), 
				ldc);
			i__2 = is - 1;
			dgemm_("N", "N", &i__2, &nb, &mb, &c_b27, &d___ref(1, 
				is), ldd, rhs, &mb, &c_b42, &f_ref(1, js), 
				ldf);
		    }
		    if (j < q) {
			k = mb * nb + 1;
			i__2 = *n - je;
			dgemm_("N", "N", &mb, &i__2, &nb, &c_b42, &rhs[k - 1],
				 &mb, &b_ref(js, je + 1), ldb, &c_b42, &
				c___ref(is, je + 1), ldc);
			i__2 = *n - je;
			dgemm_("N", "N", &mb, &i__2, &nb, &c_b42, &rhs[k - 1],
				 &mb, &e_ref(js, je + 1), lde, &c_b42, &f_ref(
				is, je + 1), ldf);
		    }

		}

/* L110: */
	    }
/* L120: */
	}
    } else {

/*        Solve (I, J) - subsystem   
               A(I, I)' * R(I, J) + D(I, I)' * L(J, J)  =  C(I, J)   
               R(I, I)  * B(J, J) + L(I, J)  * E(J, J)  = -F(I, J)   
          for I = 1, 2, ..., P, J = Q, Q - 1, ..., 1 */

	*scale = 1.;
	scaloc = 1.;
	i__1 = p;
	for (i__ = 1; i__ <= i__1; ++i__) {

	    is = iwork[i__];
	    isp1 = is + 1;
	    ie = iwork[i__ + 1] - 1;
	    mb = ie - is + 1;
	    i__2 = p + 2;
	    for (j = q; j >= i__2; --j) {

		js = iwork[j];
		jsp1 = js + 1;
		je = iwork[j + 1] - 1;
		nb = je - js + 1;
		zdim = mb * nb << 1;
		if (mb == 1 && nb == 1) {

/*                 Build a 2-by-2 system Z' * x = RHS */

		    z___ref(1, 1) = a_ref(is, is);
		    z___ref(2, 1) = -b_ref(js, js);
		    z___ref(1, 2) = d___ref(is, is);
		    z___ref(2, 2) = -e_ref(js, js);

/*                 Set up right hand side(s) */

		    rhs[0] = c___ref(is, js);
		    rhs[1] = f_ref(is, js);

/*                 Solve Z' * x = RHS */

		    dgetc2_(&zdim, z__, &c__8, ipiv, jpiv, &ierr);
		    if (ierr > 0) {
			*info = ierr;
		    }

		    dgesc2_(&zdim, z__, &c__8, rhs, ipiv, jpiv, &scaloc);
		    if (scaloc != 1.) {
			i__3 = *n;
			for (k = 1; k <= i__3; ++k) {
			    dscal_(m, &scaloc, &c___ref(1, k), &c__1);
			    dscal_(m, &scaloc, &f_ref(1, k), &c__1);
/* L130: */
			}
			*scale *= scaloc;
		    }

/*                 Unpack solution vector(s) */

		    c___ref(is, js) = rhs[0];
		    f_ref(is, js) = rhs[1];

/*                 Substitute R(I, J) and L(I, J) into remaining   
                   equation. */

		    if (j > p + 2) {
			alpha = rhs[0];
			i__3 = js - 1;
			daxpy_(&i__3, &alpha, &b_ref(1, js), &c__1, &f_ref(is,
				 1), ldf);
			alpha = rhs[1];
			i__3 = js - 1;
			daxpy_(&i__3, &alpha, &e_ref(1, js), &c__1, &f_ref(is,
				 1), ldf);
		    }
		    if (i__ < p) {
			alpha = -rhs[0];
			i__3 = *m - ie;
			daxpy_(&i__3, &alpha, &a_ref(is, ie + 1), lda, &
				c___ref(ie + 1, js), &c__1);
			alpha = -rhs[1];
			i__3 = *m - ie;
			daxpy_(&i__3, &alpha, &d___ref(is, ie + 1), ldd, &
				c___ref(ie + 1, js), &c__1);
		    }

		} else if (mb == 1 && nb == 2) {

/*                 Build a 4-by-4 system Z' * x = RHS */

		    z___ref(1, 1) = a_ref(is, is);
		    z___ref(2, 1) = 0.;
		    z___ref(3, 1) = -b_ref(js, js);
		    z___ref(4, 1) = -b_ref(jsp1, js);

		    z___ref(1, 2) = 0.;
		    z___ref(2, 2) = a_ref(is, is);
		    z___ref(3, 2) = -b_ref(js, jsp1);
		    z___ref(4, 2) = -b_ref(jsp1, jsp1);

		    z___ref(1, 3) = d___ref(is, is);
		    z___ref(2, 3) = 0.;
		    z___ref(3, 3) = -e_ref(js, js);
		    z___ref(4, 3) = 0.;

		    z___ref(1, 4) = 0.;
		    z___ref(2, 4) = d___ref(is, is);
		    z___ref(3, 4) = -e_ref(js, jsp1);
		    z___ref(4, 4) = -e_ref(jsp1, jsp1);

/*                 Set up right hand side(s) */

		    rhs[0] = c___ref(is, js);
		    rhs[1] = c___ref(is, jsp1);
		    rhs[2] = f_ref(is, js);
		    rhs[3] = f_ref(is, jsp1);

/*                 Solve Z' * x = RHS */

		    dgetc2_(&zdim, z__, &c__8, ipiv, jpiv, &ierr);
		    if (ierr > 0) {
			*info = ierr;
		    }
		    dgesc2_(&zdim, z__, &c__8, rhs, ipiv, jpiv, &scaloc);
		    if (scaloc != 1.) {
			i__3 = *n;
			for (k = 1; k <= i__3; ++k) {
			    dscal_(m, &scaloc, &c___ref(1, k), &c__1);
			    dscal_(m, &scaloc, &f_ref(1, k), &c__1);
/* L140: */
			}
			*scale *= scaloc;
		    }

/*                 Unpack solution vector(s) */

		    c___ref(is, js) = rhs[0];
		    c___ref(is, jsp1) = rhs[1];
		    f_ref(is, js) = rhs[2];
		    f_ref(is, jsp1) = rhs[3];

/*                 Substitute R(I, J) and L(I, J) into remaining   
                   equation. */

		    if (j > p + 2) {
			i__3 = js - 1;
			daxpy_(&i__3, rhs, &b_ref(1, js), &c__1, &f_ref(is, 1)
				, ldf);
			i__3 = js - 1;
			daxpy_(&i__3, &rhs[1], &b_ref(1, jsp1), &c__1, &f_ref(
				is, 1), ldf);
			i__3 = js - 1;
			daxpy_(&i__3, &rhs[2], &e_ref(1, js), &c__1, &f_ref(
				is, 1), ldf);
			i__3 = js - 1;
			daxpy_(&i__3, &rhs[3], &e_ref(1, jsp1), &c__1, &f_ref(
				is, 1), ldf);
		    }
		    if (i__ < p) {
			i__3 = *m - ie;
			dger_(&i__3, &nb, &c_b27, &a_ref(is, ie + 1), lda, 
				rhs, &c__1, &c___ref(ie + 1, js), ldc);
			i__3 = *m - ie;
			dger_(&i__3, &nb, &c_b27, &d___ref(is, ie + 1), ldd, &
				rhs[2], &c__1, &c___ref(ie + 1, js), ldc);
		    }

		} else if (mb == 2 && nb == 1) {

/*                 Build a 4-by-4 system Z' * x = RHS */

		    z___ref(1, 1) = a_ref(is, is);
		    z___ref(2, 1) = a_ref(is, isp1);
		    z___ref(3, 1) = -b_ref(js, js);
		    z___ref(4, 1) = 0.;

		    z___ref(1, 2) = a_ref(isp1, is);
		    z___ref(2, 2) = a_ref(isp1, isp1);
		    z___ref(3, 2) = 0.;
		    z___ref(4, 2) = -b_ref(js, js);

		    z___ref(1, 3) = d___ref(is, is);
		    z___ref(2, 3) = d___ref(is, isp1);
		    z___ref(3, 3) = -e_ref(js, js);
		    z___ref(4, 3) = 0.;

		    z___ref(1, 4) = 0.;
		    z___ref(2, 4) = d___ref(isp1, isp1);
		    z___ref(3, 4) = 0.;
		    z___ref(4, 4) = -e_ref(js, js);

/*                 Set up right hand side(s) */

		    rhs[0] = c___ref(is, js);
		    rhs[1] = c___ref(isp1, js);
		    rhs[2] = f_ref(is, js);
		    rhs[3] = f_ref(isp1, js);

/*                 Solve Z' * x = RHS */

		    dgetc2_(&zdim, z__, &c__8, ipiv, jpiv, &ierr);
		    if (ierr > 0) {
			*info = ierr;
		    }

		    dgesc2_(&zdim, z__, &c__8, rhs, ipiv, jpiv, &scaloc);
		    if (scaloc != 1.) {
			i__3 = *n;
			for (k = 1; k <= i__3; ++k) {
			    dscal_(m, &scaloc, &c___ref(1, k), &c__1);
			    dscal_(m, &scaloc, &f_ref(1, k), &c__1);
/* L150: */
			}
			*scale *= scaloc;
		    }

/*                 Unpack solution vector(s) */

		    c___ref(is, js) = rhs[0];
		    c___ref(isp1, js) = rhs[1];
		    f_ref(is, js) = rhs[2];
		    f_ref(isp1, js) = rhs[3];

/*                 Substitute R(I, J) and L(I, J) into remaining   
                   equation. */

		    if (j > p + 2) {
			i__3 = js - 1;
			dger_(&mb, &i__3, &c_b42, rhs, &c__1, &b_ref(1, js), &
				c__1, &f_ref(is, 1), ldf);
			i__3 = js - 1;
			dger_(&mb, &i__3, &c_b42, &rhs[2], &c__1, &e_ref(1, 
				js), &c__1, &f_ref(is, 1), ldf);
		    }
		    if (i__ < p) {
			i__3 = *m - ie;
			dgemv_("T", &mb, &i__3, &c_b27, &a_ref(is, ie + 1), 
				lda, rhs, &c__1, &c_b42, &c___ref(ie + 1, js),
				 &c__1);
			i__3 = *m - ie;
			dgemv_("T", &mb, &i__3, &c_b27, &d___ref(is, ie + 1), 
				ldd, &rhs[2], &c__1, &c_b42, &c___ref(ie + 1, 
				js), &c__1);
		    }

		} else if (mb == 2 && nb == 2) {

/*                 Build an 8-by-8 system Z' * x = RHS */

		    dcopy_(&c__64, &c_b54, &c__0, z__, &c__1);

		    z___ref(1, 1) = a_ref(is, is);
		    z___ref(2, 1) = a_ref(is, isp1);
		    z___ref(5, 1) = -b_ref(js, js);
		    z___ref(7, 1) = -b_ref(jsp1, js);

		    z___ref(1, 2) = a_ref(isp1, is);
		    z___ref(2, 2) = a_ref(isp1, isp1);
		    z___ref(6, 2) = -b_ref(js, js);
		    z___ref(8, 2) = -b_ref(jsp1, js);

		    z___ref(3, 3) = a_ref(is, is);
		    z___ref(4, 3) = a_ref(is, isp1);
		    z___ref(5, 3) = -b_ref(js, jsp1);
		    z___ref(7, 3) = -b_ref(jsp1, jsp1);

		    z___ref(3, 4) = a_ref(isp1, is);
		    z___ref(4, 4) = a_ref(isp1, isp1);
		    z___ref(6, 4) = -b_ref(js, jsp1);
		    z___ref(8, 4) = -b_ref(jsp1, jsp1);

		    z___ref(1, 5) = d___ref(is, is);
		    z___ref(2, 5) = d___ref(is, isp1);
		    z___ref(5, 5) = -e_ref(js, js);

		    z___ref(2, 6) = d___ref(isp1, isp1);
		    z___ref(6, 6) = -e_ref(js, js);

		    z___ref(3, 7) = d___ref(is, is);
		    z___ref(4, 7) = d___ref(is, isp1);
		    z___ref(5, 7) = -e_ref(js, jsp1);
		    z___ref(7, 7) = -e_ref(jsp1, jsp1);

		    z___ref(4, 8) = d___ref(isp1, isp1);
		    z___ref(6, 8) = -e_ref(js, jsp1);
		    z___ref(8, 8) = -e_ref(jsp1, jsp1);

/*                 Set up right hand side(s) */

		    k = 1;
		    ii = mb * nb + 1;
		    i__3 = nb - 1;
		    for (jj = 0; jj <= i__3; ++jj) {
			dcopy_(&mb, &c___ref(is, js + jj), &c__1, &rhs[k - 1],
				 &c__1);
			dcopy_(&mb, &f_ref(is, js + jj), &c__1, &rhs[ii - 1], 
				&c__1);
			k += mb;
			ii += mb;
/* L160: */
		    }


/*                 Solve Z' * x = RHS */

		    dgetc2_(&zdim, z__, &c__8, ipiv, jpiv, &ierr);
		    if (ierr > 0) {
			*info = ierr;
		    }

		    dgesc2_(&zdim, z__, &c__8, rhs, ipiv, jpiv, &scaloc);
		    if (scaloc != 1.) {
			i__3 = *n;
			for (k = 1; k <= i__3; ++k) {
			    dscal_(m, &scaloc, &c___ref(1, k), &c__1);
			    dscal_(m, &scaloc, &f_ref(1, k), &c__1);
/* L170: */
			}
			*scale *= scaloc;
		    }

/*                 Unpack solution vector(s) */

		    k = 1;
		    ii = mb * nb + 1;
		    i__3 = nb - 1;
		    for (jj = 0; jj <= i__3; ++jj) {
			dcopy_(&mb, &rhs[k - 1], &c__1, &c___ref(is, js + jj),
				 &c__1);
			dcopy_(&mb, &rhs[ii - 1], &c__1, &f_ref(is, js + jj), 
				&c__1);
			k += mb;
			ii += mb;
/* L180: */
		    }

/*                 Substitute R(I, J) and L(I, J) into remaining   
                   equation. */

		    if (j > p + 2) {
			i__3 = js - 1;
			dgemm_("N", "T", &mb, &i__3, &nb, &c_b42, &c___ref(is,
				 js), ldc, &b_ref(1, js), ldb, &c_b42, &f_ref(
				is, 1), ldf);
			i__3 = js - 1;
			dgemm_("N", "T", &mb, &i__3, &nb, &c_b42, &f_ref(is, 
				js), ldf, &e_ref(1, js), lde, &c_b42, &f_ref(
				is, 1), ldf);
		    }
		    if (i__ < p) {
			i__3 = *m - ie;
			dgemm_("T", "N", &i__3, &nb, &mb, &c_b27, &a_ref(is, 
				ie + 1), lda, &c___ref(is, js), ldc, &c_b42, &
				c___ref(ie + 1, js), ldc);
			i__3 = *m - ie;
			dgemm_("T", "N", &i__3, &nb, &mb, &c_b27, &d___ref(is,
				 ie + 1), ldd, &f_ref(is, js), ldf, &c_b42, &
				c___ref(ie + 1, js), ldc);
		    }

		}

/* L190: */
	    }
/* L200: */
	}

    }
    return 0;

/*     End of DTGSY2 */

} /* dtgsy2_ */

#undef z___ref
#undef f_ref
#undef e_ref
#undef d___ref
#undef c___ref
#undef b_ref
#undef a_ref


