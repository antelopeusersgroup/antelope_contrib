#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int ztgsy2_(char *trans, integer *ijob, integer *m, integer *
	n, doublecomplex *a, integer *lda, doublecomplex *b, integer *ldb, 
	doublecomplex *c__, integer *ldc, doublecomplex *d__, integer *ldd, 
	doublecomplex *e, integer *lde, doublecomplex *f, integer *ldf, 
	doublereal *scale, doublereal *rdsum, doublereal *rdscal, integer *
	info)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    ZTGSY2 solves the generalized Sylvester equation   

                A * R - L * B = scale *   C               (1)   
                D * R - L * E = scale * F   

    using Level 1 and 2 BLAS, where R and L are unknown M-by-N matrices,   
    (A, D), (B, E) and (C, F) are given matrix pairs of size M-by-M,   
    N-by-N and M-by-N, respectively. A, B, D and E are upper triangular   
    (i.e., (A,D) and (B,E) in generalized Schur form).   

    The solution (R, L) overwrites (C, F). 0 <= SCALE <= 1 is an output   
    scaling factor chosen to avoid overflow.   

    In matrix notation solving equation (1) corresponds to solve   
    Zx = scale * b, where Z is defined as   

           Z = [ kron(In, A)  -kron(B', Im) ]             (2)   
               [ kron(In, D)  -kron(E', Im) ],   

    Ik is the identity matrix of size k and X' is the transpose of X.   
    kron(X, Y) is the Kronecker product between the matrices X and Y.   

    If TRANS = 'C', y in the conjugate transposed system Z'y = scale*b   
    is solved for, which is equivalent to solve for R and L in   

                A' * R  + D' * L   = scale *  C           (3)   
                R  * B' + L  * E'  = scale * -F   

    This case is used to compute an estimate of Dif[(A, D), (B, E)] =   
    = sigma_min(Z) using reverse communicaton with ZLACON.   

    ZTGSY2 also (IJOB >= 1) contributes to the computation in ZTGSYL   
    of an upper bound on the separation between to matrix pairs. Then   
    the input (A, D), (B, E) are sub-pencils of two matrix pairs in   
    ZTGSYL.   

    Arguments   
    =========   

    TRANS   (input) CHARACTER   
            = 'N', solve the generalized Sylvester equation (1).   
            = 'T': solve the 'transposed' system (3).   

    IJOB    (input) INTEGER   
            Specifies what kind of functionality to be performed.   
            =0: solve (1) only.   
            =1: A contribution from this subsystem to a Frobenius   
                norm-based estimate of the separation between two matrix   
                pairs is computed. (look ahead strategy is used).   
            =2: A contribution from this subsystem to a Frobenius   
                norm-based estimate of the separation between two matrix   
                pairs is computed. (DGECON on sub-systems is used.)   
            Not referenced if TRANS = 'T'.   

    M       (input) INTEGER   
            On entry, M specifies the order of A and D, and the row   
            dimension of C, F, R and L.   

    N       (input) INTEGER   
            On entry, N specifies the order of B and E, and the column   
            dimension of C, F, R and L.   

    A       (input) COMPLEX*16 array, dimension (LDA, M)   
            On entry, A contains an upper triangular matrix.   

    LDA     (input) INTEGER   
            The leading dimension of the matrix A. LDA >= max(1, M).   

    B       (input) COMPLEX*16 array, dimension (LDB, N)   
            On entry, B contains an upper triangular matrix.   

    LDB     (input) INTEGER   
            The leading dimension of the matrix B. LDB >= max(1, N).   

    C       (input/ output) COMPLEX*16 array, dimension (LDC, N)   
            On entry, C contains the right-hand-side of the first matrix   
            equation in (1).   
            On exit, if IJOB = 0, C has been overwritten by the solution   
            R.   

    LDC     (input) INTEGER   
            The leading dimension of the matrix C. LDC >= max(1, M).   

    D       (input) COMPLEX*16 array, dimension (LDD, M)   
            On entry, D contains an upper triangular matrix.   

    LDD     (input) INTEGER   
            The leading dimension of the matrix D. LDD >= max(1, M).   

    E       (input) COMPLEX*16 array, dimension (LDE, N)   
            On entry, E contains an upper triangular matrix.   

    LDE     (input) INTEGER   
            The leading dimension of the matrix E. LDE >= max(1, N).   

    F       (input/ output) COMPLEX*16 array, dimension (LDF, N)   
            On entry, F contains the right-hand-side of the second matrix   
            equation in (1).   
            On exit, if IJOB = 0, F has been overwritten by the solution   
            L.   

    LDF     (input) INTEGER   
            The leading dimension of the matrix F. LDF >= max(1, M).   

    SCALE   (output) DOUBLE PRECISION   
            On exit, 0 <= SCALE <= 1. If 0 < SCALE < 1, the solutions   
            R and L (C and F on entry) will hold the solutions to a   
            slightly perturbed system but the input matrices A, B, D and   
            E have not been changed. If SCALE = 0, R and L will hold the   
            solutions to the homogeneous system with C = F = 0.   
            Normally, SCALE = 1.   

    RDSUM   (input/output) DOUBLE PRECISION   
            On entry, the sum of squares of computed contributions to   
            the Dif-estimate under computation by ZTGSYL, where the   
            scaling factor RDSCAL (see below) has been factored out.   
            On exit, the corresponding sum of squares updated with the   
            contributions from the current sub-system.   
            If TRANS = 'T' RDSUM is not touched.   
            NOTE: RDSUM only makes sense when ZTGSY2 is called by   
            ZTGSYL.   

    RDSCAL  (input/output) DOUBLE PRECISION   
            On entry, scaling factor used to prevent overflow in RDSUM.   
            On exit, RDSCAL is updated w.r.t. the current contributions   
            in RDSUM.   
            If TRANS = 'T', RDSCAL is not touched.   
            NOTE: RDSCAL only makes sense when ZTGSY2 is called by   
            ZTGSYL.   

    INFO    (output) INTEGER   
            On exit, if INFO is set to   
              =0: Successful exit   
              <0: If INFO = -i, input argument number i is illegal.   
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
    static integer c__2 = 2;
    static integer c__1 = 1;
    
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, d_dim1, 
	    d_offset, e_dim1, e_offset, f_dim1, f_offset, i__1, i__2, i__3, 
	    i__4;
    doublecomplex z__1, z__2, z__3, z__4, z__5, z__6;
    /* Builtin functions */
    void d_cnjg(doublecomplex *, doublecomplex *);
    /* Local variables */
    static integer ierr, ipiv[2], jpiv[2], i__, j, k;
    static doublecomplex alpha, z__[4]	/* was [2][2] */;
    extern logical lsame_(char *, char *);
    extern /* Subroutine */ int zscal_(integer *, doublecomplex *, 
	    doublecomplex *, integer *), zaxpy_(integer *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, integer *), zgesc2_(
	    integer *, doublecomplex *, integer *, doublecomplex *, integer *,
	     integer *, doublereal *), zgetc2_(integer *, doublecomplex *, 
	    integer *, integer *, integer *, integer *);
    static doublereal scaloc;
    extern /* Subroutine */ int xerbla_(char *, integer *), zlatdf_(
	    integer *, integer *, doublecomplex *, integer *, doublecomplex *,
	     doublereal *, doublereal *, integer *, integer *);
    static logical notran;
    static doublecomplex rhs[2];
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
#define z___subscr(a_1,a_2) (a_2)*2 + a_1 - 3
#define z___ref(a_1,a_2) z__[z___subscr(a_1,a_2)]


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

    /* Function Body */
    *info = 0;
    ierr = 0;
    notran = lsame_(trans, "N");
    if (! notran && ! lsame_(trans, "C")) {
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
	xerbla_("ZTGSY2", &i__1);
	return 0;
    }

    if (notran) {

/*        Solve (I, J) - system   
             A(I, I) * R(I, J) - L(I, J) * B(J, J) = C(I, J)   
             D(I, I) * R(I, J) - L(I, J) * E(J, J) = F(I, J)   
          for I = M, M - 1, ..., 1; J = 1, 2, ..., N */

	*scale = 1.;
	scaloc = 1.;
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    for (i__ = *m; i__ >= 1; --i__) {

/*              Build 2 by 2 system */

		i__2 = z___subscr(1, 1);
		i__3 = a_subscr(i__, i__);
		z__[i__2].r = a[i__3].r, z__[i__2].i = a[i__3].i;
		i__2 = z___subscr(2, 1);
		i__3 = d___subscr(i__, i__);
		z__[i__2].r = d__[i__3].r, z__[i__2].i = d__[i__3].i;
		i__2 = z___subscr(1, 2);
		i__3 = b_subscr(j, j);
		z__1.r = -b[i__3].r, z__1.i = -b[i__3].i;
		z__[i__2].r = z__1.r, z__[i__2].i = z__1.i;
		i__2 = z___subscr(2, 2);
		i__3 = e_subscr(j, j);
		z__1.r = -e[i__3].r, z__1.i = -e[i__3].i;
		z__[i__2].r = z__1.r, z__[i__2].i = z__1.i;

/*              Set up right hand side(s) */

		i__2 = c___subscr(i__, j);
		rhs[0].r = c__[i__2].r, rhs[0].i = c__[i__2].i;
		i__2 = f_subscr(i__, j);
		rhs[1].r = f[i__2].r, rhs[1].i = f[i__2].i;

/*              Solve Z * x = RHS */

		zgetc2_(&c__2, z__, &c__2, ipiv, jpiv, &ierr);
		if (ierr > 0) {
		    *info = ierr;
		}
		if (*ijob == 0) {
		    zgesc2_(&c__2, z__, &c__2, rhs, ipiv, jpiv, &scaloc);
		    if (scaloc != 1.) {
			i__2 = *n;
			for (k = 1; k <= i__2; ++k) {
			    z__1.r = scaloc, z__1.i = 0.;
			    zscal_(m, &z__1, &c___ref(1, k), &c__1);
			    z__1.r = scaloc, z__1.i = 0.;
			    zscal_(m, &z__1, &f_ref(1, k), &c__1);
/* L10: */
			}
			*scale *= scaloc;
		    }
		} else {
		    zlatdf_(ijob, &c__2, z__, &c__2, rhs, rdsum, rdscal, ipiv,
			     jpiv);
		}

/*              Unpack solution vector(s) */

		i__2 = c___subscr(i__, j);
		c__[i__2].r = rhs[0].r, c__[i__2].i = rhs[0].i;
		i__2 = f_subscr(i__, j);
		f[i__2].r = rhs[1].r, f[i__2].i = rhs[1].i;

/*              Substitute R(I, J) and L(I, J) into remaining equation. */

		if (i__ > 1) {
		    z__1.r = -rhs[0].r, z__1.i = -rhs[0].i;
		    alpha.r = z__1.r, alpha.i = z__1.i;
		    i__2 = i__ - 1;
		    zaxpy_(&i__2, &alpha, &a_ref(1, i__), &c__1, &c___ref(1, 
			    j), &c__1);
		    i__2 = i__ - 1;
		    zaxpy_(&i__2, &alpha, &d___ref(1, i__), &c__1, &f_ref(1, 
			    j), &c__1);
		}
		if (j < *n) {
		    i__2 = *n - j;
		    zaxpy_(&i__2, &rhs[1], &b_ref(j, j + 1), ldb, &c___ref(
			    i__, j + 1), ldc);
		    i__2 = *n - j;
		    zaxpy_(&i__2, &rhs[1], &e_ref(j, j + 1), lde, &f_ref(i__, 
			    j + 1), ldf);
		}

/* L20: */
	    }
/* L30: */
	}
    } else {

/*        Solve transposed (I, J) - system:   
             A(I, I)' * R(I, J) + D(I, I)' * L(J, J) = C(I, J)   
             R(I, I) * B(J, J) + L(I, J) * E(J, J)   = -F(I, J)   
          for I = 1, 2, ..., M, J = N, N - 1, ..., 1 */

	*scale = 1.;
	scaloc = 1.;
	i__1 = *m;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    for (j = *n; j >= 1; --j) {

/*              Build 2 by 2 system Z' */

		i__2 = z___subscr(1, 1);
		d_cnjg(&z__1, &a_ref(i__, i__));
		z__[i__2].r = z__1.r, z__[i__2].i = z__1.i;
		i__2 = z___subscr(2, 1);
		d_cnjg(&z__2, &b_ref(j, j));
		z__1.r = -z__2.r, z__1.i = -z__2.i;
		z__[i__2].r = z__1.r, z__[i__2].i = z__1.i;
		i__2 = z___subscr(1, 2);
		d_cnjg(&z__1, &d___ref(i__, i__));
		z__[i__2].r = z__1.r, z__[i__2].i = z__1.i;
		i__2 = z___subscr(2, 2);
		d_cnjg(&z__2, &e_ref(j, j));
		z__1.r = -z__2.r, z__1.i = -z__2.i;
		z__[i__2].r = z__1.r, z__[i__2].i = z__1.i;


/*              Set up right hand side(s) */

		i__2 = c___subscr(i__, j);
		rhs[0].r = c__[i__2].r, rhs[0].i = c__[i__2].i;
		i__2 = f_subscr(i__, j);
		rhs[1].r = f[i__2].r, rhs[1].i = f[i__2].i;

/*              Solve Z' * x = RHS */

		zgetc2_(&c__2, z__, &c__2, ipiv, jpiv, &ierr);
		if (ierr > 0) {
		    *info = ierr;
		}
		zgesc2_(&c__2, z__, &c__2, rhs, ipiv, jpiv, &scaloc);
		if (scaloc != 1.) {
		    i__2 = *n;
		    for (k = 1; k <= i__2; ++k) {
			z__1.r = scaloc, z__1.i = 0.;
			zscal_(m, &z__1, &c___ref(1, k), &c__1);
			z__1.r = scaloc, z__1.i = 0.;
			zscal_(m, &z__1, &f_ref(1, k), &c__1);
/* L40: */
		    }
		    *scale *= scaloc;
		}

/*              Unpack solution vector(s) */

		i__2 = c___subscr(i__, j);
		c__[i__2].r = rhs[0].r, c__[i__2].i = rhs[0].i;
		i__2 = f_subscr(i__, j);
		f[i__2].r = rhs[1].r, f[i__2].i = rhs[1].i;

/*              Substitute R(I, J) and L(I, J) into remaining equation. */

		i__2 = j - 1;
		for (k = 1; k <= i__2; ++k) {
		    i__3 = f_subscr(i__, k);
		    i__4 = f_subscr(i__, k);
		    d_cnjg(&z__4, &b_ref(k, j));
		    z__3.r = rhs[0].r * z__4.r - rhs[0].i * z__4.i, z__3.i = 
			    rhs[0].r * z__4.i + rhs[0].i * z__4.r;
		    z__2.r = f[i__4].r + z__3.r, z__2.i = f[i__4].i + z__3.i;
		    d_cnjg(&z__6, &e_ref(k, j));
		    z__5.r = rhs[1].r * z__6.r - rhs[1].i * z__6.i, z__5.i = 
			    rhs[1].r * z__6.i + rhs[1].i * z__6.r;
		    z__1.r = z__2.r + z__5.r, z__1.i = z__2.i + z__5.i;
		    f[i__3].r = z__1.r, f[i__3].i = z__1.i;
/* L50: */
		}
		i__2 = *m;
		for (k = i__ + 1; k <= i__2; ++k) {
		    i__3 = c___subscr(k, j);
		    i__4 = c___subscr(k, j);
		    d_cnjg(&z__4, &a_ref(i__, k));
		    z__3.r = z__4.r * rhs[0].r - z__4.i * rhs[0].i, z__3.i = 
			    z__4.r * rhs[0].i + z__4.i * rhs[0].r;
		    z__2.r = c__[i__4].r - z__3.r, z__2.i = c__[i__4].i - 
			    z__3.i;
		    d_cnjg(&z__6, &d___ref(i__, k));
		    z__5.r = z__6.r * rhs[1].r - z__6.i * rhs[1].i, z__5.i = 
			    z__6.r * rhs[1].i + z__6.i * rhs[1].r;
		    z__1.r = z__2.r - z__5.r, z__1.i = z__2.i - z__5.i;
		    c__[i__3].r = z__1.r, c__[i__3].i = z__1.i;
/* L60: */
		}

/* L70: */
	    }
/* L80: */
	}
    }
    return 0;

/*     End of ZTGSY2 */

} /* ztgsy2_ */

#undef z___ref
#undef z___subscr
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


