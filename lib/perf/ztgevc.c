#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int ztgevc_(char *side, char *howmny, logical *select, 
	integer *n, doublecomplex *a, integer *lda, doublecomplex *b, integer 
	*ldb, doublecomplex *vl, integer *ldvl, doublecomplex *vr, integer *
	ldvr, integer *mm, integer *m, doublecomplex *work, doublereal *rwork,
	 integer *info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   



    Purpose   
    =======   

    ZTGEVC computes some or all of the right and/or left generalized   
    eigenvectors of a pair of complex upper triangular matrices (A,B).   

    The right generalized eigenvector x and the left generalized   
    eigenvector y of (A,B) corresponding to a generalized eigenvalue   
    w are defined by:   

            (A - wB) * x = 0  and  y**H * (A - wB) = 0   

    where y**H denotes the conjugate tranpose of y.   

    If an eigenvalue w is determined by zero diagonal elements of both A   
    and B, a unit vector is returned as the corresponding eigenvector.   

    If all eigenvectors are requested, the routine may either return   
    the matrices X and/or Y of right or left eigenvectors of (A,B), or   
    the products Z*X and/or Q*Y, where Z and Q are input unitary   
    matrices.  If (A,B) was obtained from the generalized Schur   
    factorization of an original pair of matrices   
       (A0,B0) = (Q*A*Z**H,Q*B*Z**H),   
    then Z*X and Q*Y are the matrices of right or left eigenvectors of   
    A.   

    Arguments   
    =========   

    SIDE    (input) CHARACTER*1   
            = 'R': compute right eigenvectors only;   
            = 'L': compute left eigenvectors only;   
            = 'B': compute both right and left eigenvectors.   

    HOWMNY  (input) CHARACTER*1   
            = 'A': compute all right and/or left eigenvectors;   
            = 'B': compute all right and/or left eigenvectors, and   
                   backtransform them using the input matrices supplied   
                   in VR and/or VL;   
            = 'S': compute selected right and/or left eigenvectors,   
                   specified by the logical array SELECT.   

    SELECT  (input) LOGICAL array, dimension (N)   
            If HOWMNY='S', SELECT specifies the eigenvectors to be   
            computed.   
            If HOWMNY='A' or 'B', SELECT is not referenced.   
            To select the eigenvector corresponding to the j-th   
            eigenvalue, SELECT(j) must be set to .TRUE..   

    N       (input) INTEGER   
            The order of the matrices A and B.  N >= 0.   

    A       (input) COMPLEX*16 array, dimension (LDA,N)   
            The upper triangular matrix A.   

    LDA     (input) INTEGER   
            The leading dimension of array A.  LDA >= max(1,N).   

    B       (input) COMPLEX*16 array, dimension (LDB,N)   
            The upper triangular matrix B.  B must have real diagonal   
            elements.   

    LDB     (input) INTEGER   
            The leading dimension of array B.  LDB >= max(1,N).   

    VL      (input/output) COMPLEX*16 array, dimension (LDVL,MM)   
            On entry, if SIDE = 'L' or 'B' and HOWMNY = 'B', VL must   
            contain an N-by-N matrix Q (usually the unitary matrix Q   
            of left Schur vectors returned by ZHGEQZ).   
            On exit, if SIDE = 'L' or 'B', VL contains:   
            if HOWMNY = 'A', the matrix Y of left eigenvectors of (A,B);   
            if HOWMNY = 'B', the matrix Q*Y;   
            if HOWMNY = 'S', the left eigenvectors of (A,B) specified by   
                        SELECT, stored consecutively in the columns of   
                        VL, in the same order as their eigenvalues.   
            If SIDE = 'R', VL is not referenced.   

    LDVL    (input) INTEGER   
            The leading dimension of array VL.   
            LDVL >= max(1,N) if SIDE = 'L' or 'B'; LDVL >= 1 otherwise.   

    VR      (input/output) COMPLEX*16 array, dimension (LDVR,MM)   
            On entry, if SIDE = 'R' or 'B' and HOWMNY = 'B', VR must   
            contain an N-by-N matrix Q (usually the unitary matrix Z   
            of right Schur vectors returned by ZHGEQZ).   
            On exit, if SIDE = 'R' or 'B', VR contains:   
            if HOWMNY = 'A', the matrix X of right eigenvectors of (A,B);   
            if HOWMNY = 'B', the matrix Z*X;   
            if HOWMNY = 'S', the right eigenvectors of (A,B) specified by   
                        SELECT, stored consecutively in the columns of   
                        VR, in the same order as their eigenvalues.   
            If SIDE = 'L', VR is not referenced.   

    LDVR    (input) INTEGER   
            The leading dimension of the array VR.   
            LDVR >= max(1,N) if SIDE = 'R' or 'B'; LDVR >= 1 otherwise.   

    MM      (input) INTEGER   
            The number of columns in the arrays VL and/or VR. MM >= M.   

    M       (output) INTEGER   
            The number of columns in the arrays VL and/or VR actually   
            used to store the eigenvectors.  If HOWMNY = 'A' or 'B', M   
            is set to N.  Each selected eigenvector occupies one column.   

    WORK    (workspace) COMPLEX*16 array, dimension (2*N)   

    RWORK   (workspace) DOUBLE PRECISION array, dimension (2*N)   

    INFO    (output) INTEGER   
            = 0:  successful exit.   
            < 0:  if INFO = -i, the i-th argument had an illegal value.   

    =====================================================================   


       Decode and Test the input parameters   

       Parameter adjustments */
    /* Table of constant values */
    static doublecomplex c_b1 = {0.,0.};
    static doublecomplex c_b2 = {1.,0.};
    static integer c__1 = 1;
    
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, vl_dim1, vl_offset, vr_dim1, 
	    vr_offset, i__1, i__2, i__3, i__4, i__5;
    doublereal d__1, d__2, d__3, d__4, d__5, d__6;
    doublecomplex z__1, z__2, z__3, z__4;
    /* Builtin functions */
    double d_imag(doublecomplex *);
    void d_cnjg(doublecomplex *, doublecomplex *);
    /* Local variables */
    static integer ibeg, ieig, iend;
    static doublereal dmin__;
    static integer isrc;
    static doublereal temp;
    static doublecomplex suma, sumb;
    static doublereal xmax;
    static doublecomplex d__;
    static integer i__, j;
    static doublereal scale;
    static logical ilall;
    static integer iside;
    static doublereal sbeta;
    extern logical lsame_(char *, char *);
    static doublereal small;
    static logical compl;
    static doublereal anorm, bnorm;
    static logical compr;
    extern /* Subroutine */ int zgemv_(char *, integer *, integer *, 
	    doublecomplex *, doublecomplex *, integer *, doublecomplex *, 
	    integer *, doublecomplex *, doublecomplex *, integer *);
    static doublecomplex ca, cb;
    extern /* Subroutine */ int dlabad_(doublereal *, doublereal *);
    static logical ilbbad;
    static doublereal acoefa;
    static integer je;
    static doublereal bcoefa, acoeff;
    static doublecomplex bcoeff;
    static logical ilback;
    static integer im;
    static doublereal ascale, bscale;
    extern doublereal dlamch_(char *);
    static integer jr;
    static doublecomplex salpha;
    static doublereal safmin;
    extern /* Subroutine */ int xerbla_(char *, integer *);
    static doublereal bignum;
    static logical ilcomp;
    extern /* Double Complex */ VOID zladiv_(doublecomplex *, doublecomplex *,
	     doublecomplex *);
    static integer ihwmny;
    static doublereal big;
    static logical lsa, lsb;
    static doublereal ulp;
    static doublecomplex sum;
#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]
#define b_subscr(a_1,a_2) (a_2)*b_dim1 + a_1
#define b_ref(a_1,a_2) b[b_subscr(a_1,a_2)]
#define vl_subscr(a_1,a_2) (a_2)*vl_dim1 + a_1
#define vl_ref(a_1,a_2) vl[vl_subscr(a_1,a_2)]
#define vr_subscr(a_1,a_2) (a_2)*vr_dim1 + a_1
#define vr_ref(a_1,a_2) vr[vr_subscr(a_1,a_2)]


    --select;
    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    vl_dim1 = *ldvl;
    vl_offset = 1 + vl_dim1 * 1;
    vl -= vl_offset;
    vr_dim1 = *ldvr;
    vr_offset = 1 + vr_dim1 * 1;
    vr -= vr_offset;
    --work;
    --rwork;

    /* Function Body */
    if (lsame_(howmny, "A")) {
	ihwmny = 1;
	ilall = TRUE_;
	ilback = FALSE_;
    } else if (lsame_(howmny, "S")) {
	ihwmny = 2;
	ilall = FALSE_;
	ilback = FALSE_;
    } else if (lsame_(howmny, "B") || lsame_(howmny, 
	    "T")) {
	ihwmny = 3;
	ilall = TRUE_;
	ilback = TRUE_;
    } else {
	ihwmny = -1;
    }

    if (lsame_(side, "R")) {
	iside = 1;
	compl = FALSE_;
	compr = TRUE_;
    } else if (lsame_(side, "L")) {
	iside = 2;
	compl = TRUE_;
	compr = FALSE_;
    } else if (lsame_(side, "B")) {
	iside = 3;
	compl = TRUE_;
	compr = TRUE_;
    } else {
	iside = -1;
    }

    *info = 0;
    if (iside < 0) {
	*info = -1;
    } else if (ihwmny < 0) {
	*info = -2;
    } else if (*n < 0) {
	*info = -4;
    } else if (*lda < max(1,*n)) {
	*info = -6;
    } else if (*ldb < max(1,*n)) {
	*info = -8;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("ZTGEVC", &i__1);
	return 0;
    }

/*     Count the number of eigenvectors */

    if (! ilall) {
	im = 0;
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
	    if (select[j]) {
		++im;
	    }
/* L10: */
	}
    } else {
	im = *n;
    }

/*     Check diagonal of B */

    ilbbad = FALSE_;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	if (d_imag(&b_ref(j, j)) != 0.) {
	    ilbbad = TRUE_;
	}
/* L20: */
    }

    if (ilbbad) {
	*info = -7;
    } else if (compl && *ldvl < *n || *ldvl < 1) {
	*info = -10;
    } else if (compr && *ldvr < *n || *ldvr < 1) {
	*info = -12;
    } else if (*mm < im) {
	*info = -13;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("ZTGEVC", &i__1);
	return 0;
    }

/*     Quick return if possible */

    *m = im;
    if (*n == 0) {
	return 0;
    }

/*     Machine Constants */

    safmin = dlamch_("Safe minimum");
    big = 1. / safmin;
    dlabad_(&safmin, &big);
    ulp = dlamch_("Epsilon") * dlamch_("Base");
    small = safmin * *n / ulp;
    big = 1. / small;
    bignum = 1. / (safmin * *n);

/*     Compute the 1-norm of each column of the strictly upper triangular   
       part of A and B to check for possible overflow in the triangular   
       solver. */

    i__1 = a_subscr(1, 1);
    anorm = (d__1 = a[i__1].r, abs(d__1)) + (d__2 = d_imag(&a_ref(1, 1)), abs(
	    d__2));
    i__1 = b_subscr(1, 1);
    bnorm = (d__1 = b[i__1].r, abs(d__1)) + (d__2 = d_imag(&b_ref(1, 1)), abs(
	    d__2));
    rwork[1] = 0.;
    rwork[*n + 1] = 0.;
    i__1 = *n;
    for (j = 2; j <= i__1; ++j) {
	rwork[j] = 0.;
	rwork[*n + j] = 0.;
	i__2 = j - 1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__3 = a_subscr(i__, j);
	    rwork[j] += (d__1 = a[i__3].r, abs(d__1)) + (d__2 = d_imag(&a_ref(
		    i__, j)), abs(d__2));
	    i__3 = b_subscr(i__, j);
	    rwork[*n + j] += (d__1 = b[i__3].r, abs(d__1)) + (d__2 = d_imag(&
		    b_ref(i__, j)), abs(d__2));
/* L30: */
	}
/* Computing MAX */
	i__2 = a_subscr(j, j);
	d__3 = anorm, d__4 = rwork[j] + ((d__1 = a[i__2].r, abs(d__1)) + (
		d__2 = d_imag(&a_ref(j, j)), abs(d__2)));
	anorm = max(d__3,d__4);
/* Computing MAX */
	i__2 = b_subscr(j, j);
	d__3 = bnorm, d__4 = rwork[*n + j] + ((d__1 = b[i__2].r, abs(d__1)) + 
		(d__2 = d_imag(&b_ref(j, j)), abs(d__2)));
	bnorm = max(d__3,d__4);
/* L40: */
    }

    ascale = 1. / max(anorm,safmin);
    bscale = 1. / max(bnorm,safmin);

/*     Left eigenvectors */

    if (compl) {
	ieig = 0;

/*        Main loop over eigenvalues */

	i__1 = *n;
	for (je = 1; je <= i__1; ++je) {
	    if (ilall) {
		ilcomp = TRUE_;
	    } else {
		ilcomp = select[je];
	    }
	    if (ilcomp) {
		++ieig;

		i__2 = a_subscr(je, je);
		i__3 = b_subscr(je, je);
		if ((d__2 = a[i__2].r, abs(d__2)) + (d__3 = d_imag(&a_ref(je, 
			je)), abs(d__3)) <= safmin && (d__1 = b[i__3].r, abs(
			d__1)) <= safmin) {

/*                 Singular matrix pencil -- return unit eigenvector */

		    i__2 = *n;
		    for (jr = 1; jr <= i__2; ++jr) {
			i__3 = vl_subscr(jr, ieig);
			vl[i__3].r = 0., vl[i__3].i = 0.;
/* L50: */
		    }
		    i__2 = vl_subscr(ieig, ieig);
		    vl[i__2].r = 1., vl[i__2].i = 0.;
		    goto L140;
		}

/*              Non-singular eigenvalue:   
                Compute coefficients  a  and  b  in   
                     H   
                   y  ( a A - b B ) = 0   

   Computing MAX */
		i__2 = a_subscr(je, je);
		i__3 = b_subscr(je, je);
		d__4 = ((d__2 = a[i__2].r, abs(d__2)) + (d__3 = d_imag(&a_ref(
			je, je)), abs(d__3))) * ascale, d__5 = (d__1 = b[i__3]
			.r, abs(d__1)) * bscale, d__4 = max(d__4,d__5);
		temp = 1. / max(d__4,safmin);
		i__2 = a_subscr(je, je);
		z__2.r = temp * a[i__2].r, z__2.i = temp * a[i__2].i;
		z__1.r = ascale * z__2.r, z__1.i = ascale * z__2.i;
		salpha.r = z__1.r, salpha.i = z__1.i;
		i__2 = b_subscr(je, je);
		sbeta = temp * b[i__2].r * bscale;
		acoeff = sbeta * ascale;
		z__1.r = bscale * salpha.r, z__1.i = bscale * salpha.i;
		bcoeff.r = z__1.r, bcoeff.i = z__1.i;

/*              Scale to avoid underflow */

		lsa = abs(sbeta) >= safmin && abs(acoeff) < small;
		lsb = (d__1 = salpha.r, abs(d__1)) + (d__2 = d_imag(&salpha), 
			abs(d__2)) >= safmin && (d__3 = bcoeff.r, abs(d__3)) 
			+ (d__4 = d_imag(&bcoeff), abs(d__4)) < small;

		scale = 1.;
		if (lsa) {
		    scale = small / abs(sbeta) * min(anorm,big);
		}
		if (lsb) {
/* Computing MAX */
		    d__3 = scale, d__4 = small / ((d__1 = salpha.r, abs(d__1))
			     + (d__2 = d_imag(&salpha), abs(d__2))) * min(
			    bnorm,big);
		    scale = max(d__3,d__4);
		}
		if (lsa || lsb) {
/* Computing MIN   
   Computing MAX */
		    d__5 = 1., d__6 = abs(acoeff), d__5 = max(d__5,d__6), 
			    d__6 = (d__1 = bcoeff.r, abs(d__1)) + (d__2 = 
			    d_imag(&bcoeff), abs(d__2));
		    d__3 = scale, d__4 = 1. / (safmin * max(d__5,d__6));
		    scale = min(d__3,d__4);
		    if (lsa) {
			acoeff = ascale * (scale * sbeta);
		    } else {
			acoeff = scale * acoeff;
		    }
		    if (lsb) {
			z__2.r = scale * salpha.r, z__2.i = scale * salpha.i;
			z__1.r = bscale * z__2.r, z__1.i = bscale * z__2.i;
			bcoeff.r = z__1.r, bcoeff.i = z__1.i;
		    } else {
			z__1.r = scale * bcoeff.r, z__1.i = scale * bcoeff.i;
			bcoeff.r = z__1.r, bcoeff.i = z__1.i;
		    }
		}

		acoefa = abs(acoeff);
		bcoefa = (d__1 = bcoeff.r, abs(d__1)) + (d__2 = d_imag(&
			bcoeff), abs(d__2));
		xmax = 1.;
		i__2 = *n;
		for (jr = 1; jr <= i__2; ++jr) {
		    i__3 = jr;
		    work[i__3].r = 0., work[i__3].i = 0.;
/* L60: */
		}
		i__2 = je;
		work[i__2].r = 1., work[i__2].i = 0.;
/* Computing MAX */
		d__1 = ulp * acoefa * anorm, d__2 = ulp * bcoefa * bnorm, 
			d__1 = max(d__1,d__2);
		dmin__ = max(d__1,safmin);

/*                                              H   
                Triangular solve of  (a A - b B)  y = 0   

                                        H   
                (rowwise in  (a A - b B) , or columnwise in a A - b B) */

		i__2 = *n;
		for (j = je + 1; j <= i__2; ++j) {

/*                 Compute   
                         j-1   
                   SUM = sum  conjg( a*A(k,j) - b*B(k,j) )*x(k)   
                         k=je   
                   (Scale if necessary) */

		    temp = 1. / xmax;
		    if (acoefa * rwork[j] + bcoefa * rwork[*n + j] > bignum * 
			    temp) {
			i__3 = j - 1;
			for (jr = je; jr <= i__3; ++jr) {
			    i__4 = jr;
			    i__5 = jr;
			    z__1.r = temp * work[i__5].r, z__1.i = temp * 
				    work[i__5].i;
			    work[i__4].r = z__1.r, work[i__4].i = z__1.i;
/* L70: */
			}
			xmax = 1.;
		    }
		    suma.r = 0., suma.i = 0.;
		    sumb.r = 0., sumb.i = 0.;

		    i__3 = j - 1;
		    for (jr = je; jr <= i__3; ++jr) {
			d_cnjg(&z__3, &a_ref(jr, j));
			i__4 = jr;
			z__2.r = z__3.r * work[i__4].r - z__3.i * work[i__4]
				.i, z__2.i = z__3.r * work[i__4].i + z__3.i * 
				work[i__4].r;
			z__1.r = suma.r + z__2.r, z__1.i = suma.i + z__2.i;
			suma.r = z__1.r, suma.i = z__1.i;
			d_cnjg(&z__3, &b_ref(jr, j));
			i__4 = jr;
			z__2.r = z__3.r * work[i__4].r - z__3.i * work[i__4]
				.i, z__2.i = z__3.r * work[i__4].i + z__3.i * 
				work[i__4].r;
			z__1.r = sumb.r + z__2.r, z__1.i = sumb.i + z__2.i;
			sumb.r = z__1.r, sumb.i = z__1.i;
/* L80: */
		    }
		    z__2.r = acoeff * suma.r, z__2.i = acoeff * suma.i;
		    d_cnjg(&z__4, &bcoeff);
		    z__3.r = z__4.r * sumb.r - z__4.i * sumb.i, z__3.i = 
			    z__4.r * sumb.i + z__4.i * sumb.r;
		    z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
		    sum.r = z__1.r, sum.i = z__1.i;

/*                 Form x(j) = - SUM / conjg( a*A(j,j) - b*B(j,j) )   

                   with scaling and perturbation of the denominator */

		    i__3 = a_subscr(j, j);
		    z__3.r = acoeff * a[i__3].r, z__3.i = acoeff * a[i__3].i;
		    i__4 = b_subscr(j, j);
		    z__4.r = bcoeff.r * b[i__4].r - bcoeff.i * b[i__4].i, 
			    z__4.i = bcoeff.r * b[i__4].i + bcoeff.i * b[i__4]
			    .r;
		    z__2.r = z__3.r - z__4.r, z__2.i = z__3.i - z__4.i;
		    d_cnjg(&z__1, &z__2);
		    d__.r = z__1.r, d__.i = z__1.i;
		    if ((d__1 = d__.r, abs(d__1)) + (d__2 = d_imag(&d__), abs(
			    d__2)) <= dmin__) {
			z__1.r = dmin__, z__1.i = 0.;
			d__.r = z__1.r, d__.i = z__1.i;
		    }

		    if ((d__1 = d__.r, abs(d__1)) + (d__2 = d_imag(&d__), abs(
			    d__2)) < 1.) {
			if ((d__1 = sum.r, abs(d__1)) + (d__2 = d_imag(&sum), 
				abs(d__2)) >= bignum * ((d__3 = d__.r, abs(
				d__3)) + (d__4 = d_imag(&d__), abs(d__4)))) {
			    temp = 1. / ((d__1 = sum.r, abs(d__1)) + (d__2 = 
				    d_imag(&sum), abs(d__2)));
			    i__3 = j - 1;
			    for (jr = je; jr <= i__3; ++jr) {
				i__4 = jr;
				i__5 = jr;
				z__1.r = temp * work[i__5].r, z__1.i = temp * 
					work[i__5].i;
				work[i__4].r = z__1.r, work[i__4].i = z__1.i;
/* L90: */
			    }
			    xmax = temp * xmax;
			    z__1.r = temp * sum.r, z__1.i = temp * sum.i;
			    sum.r = z__1.r, sum.i = z__1.i;
			}
		    }
		    i__3 = j;
		    z__2.r = -sum.r, z__2.i = -sum.i;
		    zladiv_(&z__1, &z__2, &d__);
		    work[i__3].r = z__1.r, work[i__3].i = z__1.i;
/* Computing MAX */
		    i__3 = j;
		    d__3 = xmax, d__4 = (d__1 = work[i__3].r, abs(d__1)) + (
			    d__2 = d_imag(&work[j]), abs(d__2));
		    xmax = max(d__3,d__4);
/* L100: */
		}

/*              Back transform eigenvector if HOWMNY='B'. */

		if (ilback) {
		    i__2 = *n + 1 - je;
		    zgemv_("N", n, &i__2, &c_b2, &vl_ref(1, je), ldvl, &work[
			    je], &c__1, &c_b1, &work[*n + 1], &c__1);
		    isrc = 2;
		    ibeg = 1;
		} else {
		    isrc = 1;
		    ibeg = je;
		}

/*              Copy and scale eigenvector into column of VL */

		xmax = 0.;
		i__2 = *n;
		for (jr = ibeg; jr <= i__2; ++jr) {
/* Computing MAX */
		    i__3 = (isrc - 1) * *n + jr;
		    d__3 = xmax, d__4 = (d__1 = work[i__3].r, abs(d__1)) + (
			    d__2 = d_imag(&work[(isrc - 1) * *n + jr]), abs(
			    d__2));
		    xmax = max(d__3,d__4);
/* L110: */
		}

		if (xmax > safmin) {
		    temp = 1. / xmax;
		    i__2 = *n;
		    for (jr = ibeg; jr <= i__2; ++jr) {
			i__3 = vl_subscr(jr, ieig);
			i__4 = (isrc - 1) * *n + jr;
			z__1.r = temp * work[i__4].r, z__1.i = temp * work[
				i__4].i;
			vl[i__3].r = z__1.r, vl[i__3].i = z__1.i;
/* L120: */
		    }
		} else {
		    ibeg = *n + 1;
		}

		i__2 = ibeg - 1;
		for (jr = 1; jr <= i__2; ++jr) {
		    i__3 = vl_subscr(jr, ieig);
		    vl[i__3].r = 0., vl[i__3].i = 0.;
/* L130: */
		}

	    }
L140:
	    ;
	}
    }

/*     Right eigenvectors */

    if (compr) {
	ieig = im + 1;

/*        Main loop over eigenvalues */

	for (je = *n; je >= 1; --je) {
	    if (ilall) {
		ilcomp = TRUE_;
	    } else {
		ilcomp = select[je];
	    }
	    if (ilcomp) {
		--ieig;

		i__1 = a_subscr(je, je);
		i__2 = b_subscr(je, je);
		if ((d__2 = a[i__1].r, abs(d__2)) + (d__3 = d_imag(&a_ref(je, 
			je)), abs(d__3)) <= safmin && (d__1 = b[i__2].r, abs(
			d__1)) <= safmin) {

/*                 Singular matrix pencil -- return unit eigenvector */

		    i__1 = *n;
		    for (jr = 1; jr <= i__1; ++jr) {
			i__2 = vr_subscr(jr, ieig);
			vr[i__2].r = 0., vr[i__2].i = 0.;
/* L150: */
		    }
		    i__1 = vr_subscr(ieig, ieig);
		    vr[i__1].r = 1., vr[i__1].i = 0.;
		    goto L250;
		}

/*              Non-singular eigenvalue:   
                Compute coefficients  a  and  b  in   

                ( a A - b B ) x  = 0   

   Computing MAX */
		i__1 = a_subscr(je, je);
		i__2 = b_subscr(je, je);
		d__4 = ((d__2 = a[i__1].r, abs(d__2)) + (d__3 = d_imag(&a_ref(
			je, je)), abs(d__3))) * ascale, d__5 = (d__1 = b[i__2]
			.r, abs(d__1)) * bscale, d__4 = max(d__4,d__5);
		temp = 1. / max(d__4,safmin);
		i__1 = a_subscr(je, je);
		z__2.r = temp * a[i__1].r, z__2.i = temp * a[i__1].i;
		z__1.r = ascale * z__2.r, z__1.i = ascale * z__2.i;
		salpha.r = z__1.r, salpha.i = z__1.i;
		i__1 = b_subscr(je, je);
		sbeta = temp * b[i__1].r * bscale;
		acoeff = sbeta * ascale;
		z__1.r = bscale * salpha.r, z__1.i = bscale * salpha.i;
		bcoeff.r = z__1.r, bcoeff.i = z__1.i;

/*              Scale to avoid underflow */

		lsa = abs(sbeta) >= safmin && abs(acoeff) < small;
		lsb = (d__1 = salpha.r, abs(d__1)) + (d__2 = d_imag(&salpha), 
			abs(d__2)) >= safmin && (d__3 = bcoeff.r, abs(d__3)) 
			+ (d__4 = d_imag(&bcoeff), abs(d__4)) < small;

		scale = 1.;
		if (lsa) {
		    scale = small / abs(sbeta) * min(anorm,big);
		}
		if (lsb) {
/* Computing MAX */
		    d__3 = scale, d__4 = small / ((d__1 = salpha.r, abs(d__1))
			     + (d__2 = d_imag(&salpha), abs(d__2))) * min(
			    bnorm,big);
		    scale = max(d__3,d__4);
		}
		if (lsa || lsb) {
/* Computing MIN   
   Computing MAX */
		    d__5 = 1., d__6 = abs(acoeff), d__5 = max(d__5,d__6), 
			    d__6 = (d__1 = bcoeff.r, abs(d__1)) + (d__2 = 
			    d_imag(&bcoeff), abs(d__2));
		    d__3 = scale, d__4 = 1. / (safmin * max(d__5,d__6));
		    scale = min(d__3,d__4);
		    if (lsa) {
			acoeff = ascale * (scale * sbeta);
		    } else {
			acoeff = scale * acoeff;
		    }
		    if (lsb) {
			z__2.r = scale * salpha.r, z__2.i = scale * salpha.i;
			z__1.r = bscale * z__2.r, z__1.i = bscale * z__2.i;
			bcoeff.r = z__1.r, bcoeff.i = z__1.i;
		    } else {
			z__1.r = scale * bcoeff.r, z__1.i = scale * bcoeff.i;
			bcoeff.r = z__1.r, bcoeff.i = z__1.i;
		    }
		}

		acoefa = abs(acoeff);
		bcoefa = (d__1 = bcoeff.r, abs(d__1)) + (d__2 = d_imag(&
			bcoeff), abs(d__2));
		xmax = 1.;
		i__1 = *n;
		for (jr = 1; jr <= i__1; ++jr) {
		    i__2 = jr;
		    work[i__2].r = 0., work[i__2].i = 0.;
/* L160: */
		}
		i__1 = je;
		work[i__1].r = 1., work[i__1].i = 0.;
/* Computing MAX */
		d__1 = ulp * acoefa * anorm, d__2 = ulp * bcoefa * bnorm, 
			d__1 = max(d__1,d__2);
		dmin__ = max(d__1,safmin);

/*              Triangular solve of  (a A - b B) x = 0  (columnwise)   

                WORK(1:j-1) contains sums w,   
                WORK(j+1:JE) contains x */

		i__1 = je - 1;
		for (jr = 1; jr <= i__1; ++jr) {
		    i__2 = jr;
		    i__3 = a_subscr(jr, je);
		    z__2.r = acoeff * a[i__3].r, z__2.i = acoeff * a[i__3].i;
		    i__4 = b_subscr(jr, je);
		    z__3.r = bcoeff.r * b[i__4].r - bcoeff.i * b[i__4].i, 
			    z__3.i = bcoeff.r * b[i__4].i + bcoeff.i * b[i__4]
			    .r;
		    z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
		    work[i__2].r = z__1.r, work[i__2].i = z__1.i;
/* L170: */
		}
		i__1 = je;
		work[i__1].r = 1., work[i__1].i = 0.;

		for (j = je - 1; j >= 1; --j) {

/*                 Form x(j) := - w(j) / d   
                   with scaling and perturbation of the denominator */

		    i__1 = a_subscr(j, j);
		    z__2.r = acoeff * a[i__1].r, z__2.i = acoeff * a[i__1].i;
		    i__2 = b_subscr(j, j);
		    z__3.r = bcoeff.r * b[i__2].r - bcoeff.i * b[i__2].i, 
			    z__3.i = bcoeff.r * b[i__2].i + bcoeff.i * b[i__2]
			    .r;
		    z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
		    d__.r = z__1.r, d__.i = z__1.i;
		    if ((d__1 = d__.r, abs(d__1)) + (d__2 = d_imag(&d__), abs(
			    d__2)) <= dmin__) {
			z__1.r = dmin__, z__1.i = 0.;
			d__.r = z__1.r, d__.i = z__1.i;
		    }

		    if ((d__1 = d__.r, abs(d__1)) + (d__2 = d_imag(&d__), abs(
			    d__2)) < 1.) {
			i__1 = j;
			if ((d__1 = work[i__1].r, abs(d__1)) + (d__2 = d_imag(
				&work[j]), abs(d__2)) >= bignum * ((d__3 = 
				d__.r, abs(d__3)) + (d__4 = d_imag(&d__), abs(
				d__4)))) {
			    i__1 = j;
			    temp = 1. / ((d__1 = work[i__1].r, abs(d__1)) + (
				    d__2 = d_imag(&work[j]), abs(d__2)));
			    i__1 = je;
			    for (jr = 1; jr <= i__1; ++jr) {
				i__2 = jr;
				i__3 = jr;
				z__1.r = temp * work[i__3].r, z__1.i = temp * 
					work[i__3].i;
				work[i__2].r = z__1.r, work[i__2].i = z__1.i;
/* L180: */
			    }
			}
		    }

		    i__1 = j;
		    i__2 = j;
		    z__2.r = -work[i__2].r, z__2.i = -work[i__2].i;
		    zladiv_(&z__1, &z__2, &d__);
		    work[i__1].r = z__1.r, work[i__1].i = z__1.i;

		    if (j > 1) {

/*                    w = w + x(j)*(a A(*,j) - b B(*,j) ) with scaling */

			i__1 = j;
			if ((d__1 = work[i__1].r, abs(d__1)) + (d__2 = d_imag(
				&work[j]), abs(d__2)) > 1.) {
			    i__1 = j;
			    temp = 1. / ((d__1 = work[i__1].r, abs(d__1)) + (
				    d__2 = d_imag(&work[j]), abs(d__2)));
			    if (acoefa * rwork[j] + bcoefa * rwork[*n + j] >= 
				    bignum * temp) {
				i__1 = je;
				for (jr = 1; jr <= i__1; ++jr) {
				    i__2 = jr;
				    i__3 = jr;
				    z__1.r = temp * work[i__3].r, z__1.i = 
					    temp * work[i__3].i;
				    work[i__2].r = z__1.r, work[i__2].i = 
					    z__1.i;
/* L190: */
				}
			    }
			}

			i__1 = j;
			z__1.r = acoeff * work[i__1].r, z__1.i = acoeff * 
				work[i__1].i;
			ca.r = z__1.r, ca.i = z__1.i;
			i__1 = j;
			z__1.r = bcoeff.r * work[i__1].r - bcoeff.i * work[
				i__1].i, z__1.i = bcoeff.r * work[i__1].i + 
				bcoeff.i * work[i__1].r;
			cb.r = z__1.r, cb.i = z__1.i;
			i__1 = j - 1;
			for (jr = 1; jr <= i__1; ++jr) {
			    i__2 = jr;
			    i__3 = jr;
			    i__4 = a_subscr(jr, j);
			    z__3.r = ca.r * a[i__4].r - ca.i * a[i__4].i, 
				    z__3.i = ca.r * a[i__4].i + ca.i * a[i__4]
				    .r;
			    z__2.r = work[i__3].r + z__3.r, z__2.i = work[
				    i__3].i + z__3.i;
			    i__5 = b_subscr(jr, j);
			    z__4.r = cb.r * b[i__5].r - cb.i * b[i__5].i, 
				    z__4.i = cb.r * b[i__5].i + cb.i * b[i__5]
				    .r;
			    z__1.r = z__2.r - z__4.r, z__1.i = z__2.i - 
				    z__4.i;
			    work[i__2].r = z__1.r, work[i__2].i = z__1.i;
/* L200: */
			}
		    }
/* L210: */
		}

/*              Back transform eigenvector if HOWMNY='B'. */

		if (ilback) {
		    zgemv_("N", n, &je, &c_b2, &vr[vr_offset], ldvr, &work[1],
			     &c__1, &c_b1, &work[*n + 1], &c__1);
		    isrc = 2;
		    iend = *n;
		} else {
		    isrc = 1;
		    iend = je;
		}

/*              Copy and scale eigenvector into column of VR */

		xmax = 0.;
		i__1 = iend;
		for (jr = 1; jr <= i__1; ++jr) {
/* Computing MAX */
		    i__2 = (isrc - 1) * *n + jr;
		    d__3 = xmax, d__4 = (d__1 = work[i__2].r, abs(d__1)) + (
			    d__2 = d_imag(&work[(isrc - 1) * *n + jr]), abs(
			    d__2));
		    xmax = max(d__3,d__4);
/* L220: */
		}

		if (xmax > safmin) {
		    temp = 1. / xmax;
		    i__1 = iend;
		    for (jr = 1; jr <= i__1; ++jr) {
			i__2 = vr_subscr(jr, ieig);
			i__3 = (isrc - 1) * *n + jr;
			z__1.r = temp * work[i__3].r, z__1.i = temp * work[
				i__3].i;
			vr[i__2].r = z__1.r, vr[i__2].i = z__1.i;
/* L230: */
		    }
		} else {
		    iend = 0;
		}

		i__1 = *n;
		for (jr = iend + 1; jr <= i__1; ++jr) {
		    i__2 = vr_subscr(jr, ieig);
		    vr[i__2].r = 0., vr[i__2].i = 0.;
/* L240: */
		}

	    }
L250:
	    ;
	}
    }

    return 0;

/*     End of ZTGEVC */

} /* ztgevc_ */

#undef vr_ref
#undef vr_subscr
#undef vl_ref
#undef vl_subscr
#undef b_ref
#undef b_subscr
#undef a_ref
#undef a_subscr


