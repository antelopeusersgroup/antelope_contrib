#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int chgeqz_(char *job, char *compq, char *compz, integer *n, 
	integer *ilo, integer *ihi, complex *a, integer *lda, complex *b, 
	integer *ldb, complex *alpha, complex *beta, complex *q, integer *ldq,
	 complex *z__, integer *ldz, complex *work, integer *lwork, real *
	rwork, integer *info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    CHGEQZ implements a single-shift version of the QZ   
    method for finding the generalized eigenvalues w(i)=ALPHA(i)/BETA(i)   
    of the equation   

         det( A - w(i) B ) = 0   

    If JOB='S', then the pair (A,B) is simultaneously   
    reduced to Schur form (i.e., A and B are both upper triangular) by   
    applying one unitary tranformation (usually called Q) on the left and   
    another (usually called Z) on the right.  The diagonal elements of   
    A are then ALPHA(1),...,ALPHA(N), and of B are BETA(1),...,BETA(N).   

    If JOB='S' and COMPQ and COMPZ are 'V' or 'I', then the unitary   
    transformations used to reduce (A,B) are accumulated into the arrays   
    Q and Z s.t.:   

         Q(in) A(in) Z(in)* = Q(out) A(out) Z(out)*   
         Q(in) B(in) Z(in)* = Q(out) B(out) Z(out)*   

    Ref: C.B. Moler & G.W. Stewart, "An Algorithm for Generalized Matrix   
         Eigenvalue Problems", SIAM J. Numer. Anal., 10(1973),   
         pp. 241--256.   

    Arguments   
    =========   

    JOB     (input) CHARACTER*1   
            = 'E': compute only ALPHA and BETA.  A and B will not   
                   necessarily be put into generalized Schur form.   
            = 'S': put A and B into generalized Schur form, as well   
                   as computing ALPHA and BETA.   

    COMPQ   (input) CHARACTER*1   
            = 'N': do not modify Q.   
            = 'V': multiply the array Q on the right by the conjugate   
                   transpose of the unitary tranformation that is   
                   applied to the left side of A and B to reduce them   
                   to Schur form.   
            = 'I': like COMPQ='V', except that Q will be initialized to   
                   the identity first.   

    COMPZ   (input) CHARACTER*1   
            = 'N': do not modify Z.   
            = 'V': multiply the array Z on the right by the unitary   
                   tranformation that is applied to the right side of   
                   A and B to reduce them to Schur form.   
            = 'I': like COMPZ='V', except that Z will be initialized to   
                   the identity first.   

    N       (input) INTEGER   
            The order of the matrices A, B, Q, and Z.  N >= 0.   

    ILO     (input) INTEGER   
    IHI     (input) INTEGER   
            It is assumed that A is already upper triangular in rows and   
            columns 1:ILO-1 and IHI+1:N.   
            1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.   

    A       (input/output) COMPLEX array, dimension (LDA, N)   
            On entry, the N-by-N upper Hessenberg matrix A.  Elements   
            below the subdiagonal must be zero.   
            If JOB='S', then on exit A and B will have been   
               simultaneously reduced to upper triangular form.   
            If JOB='E', then on exit A will have been destroyed.   

    LDA     (input) INTEGER   
            The leading dimension of the array A.  LDA >= max( 1, N ).   

    B       (input/output) COMPLEX array, dimension (LDB, N)   
            On entry, the N-by-N upper triangular matrix B.  Elements   
            below the diagonal must be zero.   
            If JOB='S', then on exit A and B will have been   
               simultaneously reduced to upper triangular form.   
            If JOB='E', then on exit B will have been destroyed.   

    LDB     (input) INTEGER   
            The leading dimension of the array B.  LDB >= max( 1, N ).   

    ALPHA   (output) COMPLEX array, dimension (N)   
            The diagonal elements of A when the pair (A,B) has been   
            reduced to Schur form.  ALPHA(i)/BETA(i) i=1,...,N   
            are the generalized eigenvalues.   

    BETA    (output) COMPLEX array, dimension (N)   
            The diagonal elements of B when the pair (A,B) has been   
            reduced to Schur form.  ALPHA(i)/BETA(i) i=1,...,N   
            are the generalized eigenvalues.  A and B are normalized   
            so that BETA(1),...,BETA(N) are non-negative real numbers.   

    Q       (input/output) COMPLEX array, dimension (LDQ, N)   
            If COMPQ='N', then Q will not be referenced.   
            If COMPQ='V' or 'I', then the conjugate transpose of the   
               unitary transformations which are applied to A and B on   
               the left will be applied to the array Q on the right.   

    LDQ     (input) INTEGER   
            The leading dimension of the array Q.  LDQ >= 1.   
            If COMPQ='V' or 'I', then LDQ >= N.   

    Z       (input/output) COMPLEX array, dimension (LDZ, N)   
            If COMPZ='N', then Z will not be referenced.   
            If COMPZ='V' or 'I', then the unitary transformations which   
               are applied to A and B on the right will be applied to the   
               array Z on the right.   

    LDZ     (input) INTEGER   
            The leading dimension of the array Z.  LDZ >= 1.   
            If COMPZ='V' or 'I', then LDZ >= N.   

    WORK    (workspace/output) COMPLEX array, dimension (LWORK)   
            On exit, if INFO >= 0, WORK(1) returns the optimal LWORK.   

    LWORK   (input) INTEGER   
            The dimension of the array WORK.  LWORK >= max(1,N).   

            If LWORK = -1, then a workspace query is assumed; the routine   
            only calculates the optimal size of the WORK array, returns   
            this value as the first entry of the WORK array, and no error   
            message related to LWORK is issued by XERBLA.   

    RWORK   (workspace) REAL array, dimension (N)   

    INFO    (output) INTEGER   
            = 0: successful exit   
            < 0: if INFO = -i, the i-th argument had an illegal value   
            = 1,...,N: the QZ iteration did not converge.  (A,B) is not   
                       in Schur form, but ALPHA(i) and BETA(i),   
                       i=INFO+1,...,N should be correct.   
            = N+1,...,2*N: the shift calculation failed.  (A,B) is not   
                       in Schur form, but ALPHA(i) and BETA(i),   
                       i=INFO-N+1,...,N should be correct.   
            > 2*N:     various "impossible" errors.   

    Further Details   
    ===============   

    We assume that complex ABS works as long as its value is less than   
    overflow.   

    =====================================================================   


       Decode JOB, COMPQ, COMPZ   

       Parameter adjustments */
    /* Table of constant values */
    static complex c_b1 = {0.f,0.f};
    static complex c_b2 = {1.f,0.f};
    static integer c__1 = 1;
    static integer c__2 = 2;
    
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, q_dim1, q_offset, z_dim1, 
	    z_offset, i__1, i__2, i__3, i__4, i__5, i__6;
    real r__1, r__2, r__3, r__4, r__5, r__6;
    complex q__1, q__2, q__3, q__4, q__5, q__6;
    /* Builtin functions */
    double c_abs(complex *);
    void r_cnjg(complex *, complex *);
    double r_imag(complex *);
    void c_div(complex *, complex *, complex *), pow_ci(complex *, complex *, 
	    integer *), c_sqrt(complex *, complex *);
    /* Local variables */
    static real absb, atol, btol, temp;
    extern /* Subroutine */ int crot_(integer *, complex *, integer *, 
	    complex *, integer *, real *, complex *);
    static real temp2, c__;
    static integer j;
    static complex s, t;
    extern /* Subroutine */ int cscal_(integer *, complex *, complex *, 
	    integer *);
    extern logical lsame_(char *, char *);
    static complex ctemp;
    static integer iiter, ilast, jiter;
    static real anorm, bnorm;
    static integer maxit;
    static complex shift;
    static real tempr;
    static complex ctemp2, ctemp3;
    static logical ilazr2;
    static integer jc, in;
    static real ascale, bscale;
    static complex u12;
    static integer jr;
    static complex signbc;
    extern doublereal slamch_(char *), clanhs_(char *, integer *, 
	    complex *, integer *, real *);
    extern /* Subroutine */ int claset_(char *, integer *, integer *, complex 
	    *, complex *, complex *, integer *), clartg_(complex *, 
	    complex *, real *, complex *, complex *);
    static real safmin;
    extern /* Subroutine */ int xerbla_(char *, integer *);
    static complex eshift;
    static logical ilschr;
    static integer icompq, ilastm;
    static complex rtdisc;
    static integer ischur;
    static logical ilazro;
    static integer icompz, ifirst, ifrstm, istart;
    static logical lquery;
    static complex ad11, ad12, ad21, ad22;
    static integer jch;
    static logical ilq, ilz;
    static real ulp;
    static complex abi22;
#define a_subscr(a_1,a_2) (a_2)*a_dim1 + a_1
#define a_ref(a_1,a_2) a[a_subscr(a_1,a_2)]
#define b_subscr(a_1,a_2) (a_2)*b_dim1 + a_1
#define b_ref(a_1,a_2) b[b_subscr(a_1,a_2)]
#define q_subscr(a_1,a_2) (a_2)*q_dim1 + a_1
#define q_ref(a_1,a_2) q[q_subscr(a_1,a_2)]
#define z___subscr(a_1,a_2) (a_2)*z_dim1 + a_1
#define z___ref(a_1,a_2) z__[z___subscr(a_1,a_2)]


    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    --alpha;
    --beta;
    q_dim1 = *ldq;
    q_offset = 1 + q_dim1 * 1;
    q -= q_offset;
    z_dim1 = *ldz;
    z_offset = 1 + z_dim1 * 1;
    z__ -= z_offset;
    --work;
    --rwork;

    /* Function Body */
    if (lsame_(job, "E")) {
	ilschr = FALSE_;
	ischur = 1;
    } else if (lsame_(job, "S")) {
	ilschr = TRUE_;
	ischur = 2;
    } else {
	ischur = 0;
    }

    if (lsame_(compq, "N")) {
	ilq = FALSE_;
	icompq = 1;
    } else if (lsame_(compq, "V")) {
	ilq = TRUE_;
	icompq = 2;
    } else if (lsame_(compq, "I")) {
	ilq = TRUE_;
	icompq = 3;
    } else {
	icompq = 0;
    }

    if (lsame_(compz, "N")) {
	ilz = FALSE_;
	icompz = 1;
    } else if (lsame_(compz, "V")) {
	ilz = TRUE_;
	icompz = 2;
    } else if (lsame_(compz, "I")) {
	ilz = TRUE_;
	icompz = 3;
    } else {
	icompz = 0;
    }

/*     Check Argument Values */

    *info = 0;
    i__1 = max(1,*n);
    work[1].r = (real) i__1, work[1].i = 0.f;
    lquery = *lwork == -1;
    if (ischur == 0) {
	*info = -1;
    } else if (icompq == 0) {
	*info = -2;
    } else if (icompz == 0) {
	*info = -3;
    } else if (*n < 0) {
	*info = -4;
    } else if (*ilo < 1) {
	*info = -5;
    } else if (*ihi > *n || *ihi < *ilo - 1) {
	*info = -6;
    } else if (*lda < *n) {
	*info = -8;
    } else if (*ldb < *n) {
	*info = -10;
    } else if (*ldq < 1 || ilq && *ldq < *n) {
	*info = -14;
    } else if (*ldz < 1 || ilz && *ldz < *n) {
	*info = -16;
    } else if (*lwork < max(1,*n) && ! lquery) {
	*info = -18;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("CHGEQZ", &i__1);
	return 0;
    } else if (lquery) {
	return 0;
    }

/*     Quick return if possible   

       WORK( 1 ) = CMPLX( 1 ) */
    if (*n <= 0) {
	work[1].r = 1.f, work[1].i = 0.f;
	return 0;
    }

/*     Initialize Q and Z */

    if (icompq == 3) {
	claset_("Full", n, n, &c_b1, &c_b2, &q[q_offset], ldq);
    }
    if (icompz == 3) {
	claset_("Full", n, n, &c_b1, &c_b2, &z__[z_offset], ldz);
    }

/*     Machine Constants */

    in = *ihi + 1 - *ilo;
    safmin = slamch_("S");
    ulp = slamch_("E") * slamch_("B");
    anorm = clanhs_("F", &in, &a_ref(*ilo, *ilo), lda, &rwork[1]);
    bnorm = clanhs_("F", &in, &b_ref(*ilo, *ilo), ldb, &rwork[1]);
/* Computing MAX */
    r__1 = safmin, r__2 = ulp * anorm;
    atol = dmax(r__1,r__2);
/* Computing MAX */
    r__1 = safmin, r__2 = ulp * bnorm;
    btol = dmax(r__1,r__2);
    ascale = 1.f / dmax(safmin,anorm);
    bscale = 1.f / dmax(safmin,bnorm);


/*     Set Eigenvalues IHI+1:N */

    i__1 = *n;
    for (j = *ihi + 1; j <= i__1; ++j) {
	absb = c_abs(&b_ref(j, j));
	if (absb > safmin) {
	    i__2 = b_subscr(j, j);
	    q__2.r = b[i__2].r / absb, q__2.i = b[i__2].i / absb;
	    r_cnjg(&q__1, &q__2);
	    signbc.r = q__1.r, signbc.i = q__1.i;
	    i__2 = b_subscr(j, j);
	    b[i__2].r = absb, b[i__2].i = 0.f;
	    if (ilschr) {
		i__2 = j - 1;
		cscal_(&i__2, &signbc, &b_ref(1, j), &c__1);
		cscal_(&j, &signbc, &a_ref(1, j), &c__1);
	    } else {
		i__2 = a_subscr(j, j);
		i__3 = a_subscr(j, j);
		q__1.r = a[i__3].r * signbc.r - a[i__3].i * signbc.i, q__1.i =
			 a[i__3].r * signbc.i + a[i__3].i * signbc.r;
		a[i__2].r = q__1.r, a[i__2].i = q__1.i;
	    }
	    if (ilz) {
		cscal_(n, &signbc, &z___ref(1, j), &c__1);
	    }
	} else {
	    i__2 = b_subscr(j, j);
	    b[i__2].r = 0.f, b[i__2].i = 0.f;
	}
	i__2 = j;
	i__3 = a_subscr(j, j);
	alpha[i__2].r = a[i__3].r, alpha[i__2].i = a[i__3].i;
	i__2 = j;
	i__3 = b_subscr(j, j);
	beta[i__2].r = b[i__3].r, beta[i__2].i = b[i__3].i;
/* L10: */
    }

/*     If IHI < ILO, skip QZ steps */

    if (*ihi < *ilo) {
	goto L190;
    }

/*     MAIN QZ ITERATION LOOP   

       Initialize dynamic indices   

       Eigenvalues ILAST+1:N have been found.   
          Column operations modify rows IFRSTM:whatever   
          Row operations modify columns whatever:ILASTM   

       If only eigenvalues are being computed, then   
          IFRSTM is the row of the last splitting row above row ILAST;   
          this is always at least ILO.   
       IITER counts iterations since the last eigenvalue was found,   
          to tell when to use an extraordinary shift.   
       MAXIT is the maximum number of QZ sweeps allowed. */

    ilast = *ihi;
    if (ilschr) {
	ifrstm = 1;
	ilastm = *n;
    } else {
	ifrstm = *ilo;
	ilastm = *ihi;
    }
    iiter = 0;
    eshift.r = 0.f, eshift.i = 0.f;
    maxit = (*ihi - *ilo + 1) * 30;

    i__1 = maxit;
    for (jiter = 1; jiter <= i__1; ++jiter) {

/*        Check for too many iterations. */

	if (jiter > maxit) {
	    goto L180;
	}

/*        Split the matrix if possible.   

          Two tests:   
             1: A(j,j-1)=0  or  j=ILO   
             2: B(j,j)=0   

          Special case: j=ILAST */

	if (ilast == *ilo) {
	    goto L60;
	} else {
	    i__2 = a_subscr(ilast, ilast - 1);
	    if ((r__1 = a[i__2].r, dabs(r__1)) + (r__2 = r_imag(&a_ref(ilast, 
		    ilast - 1)), dabs(r__2)) <= atol) {
		i__2 = a_subscr(ilast, ilast - 1);
		a[i__2].r = 0.f, a[i__2].i = 0.f;
		goto L60;
	    }
	}

	if (c_abs(&b_ref(ilast, ilast)) <= btol) {
	    i__2 = b_subscr(ilast, ilast);
	    b[i__2].r = 0.f, b[i__2].i = 0.f;
	    goto L50;
	}

/*        General case: j<ILAST */

	i__2 = *ilo;
	for (j = ilast - 1; j >= i__2; --j) {

/*           Test 1: for A(j,j-1)=0 or j=ILO */

	    if (j == *ilo) {
		ilazro = TRUE_;
	    } else {
		i__3 = a_subscr(j, j - 1);
		if ((r__1 = a[i__3].r, dabs(r__1)) + (r__2 = r_imag(&a_ref(j, 
			j - 1)), dabs(r__2)) <= atol) {
		    i__3 = a_subscr(j, j - 1);
		    a[i__3].r = 0.f, a[i__3].i = 0.f;
		    ilazro = TRUE_;
		} else {
		    ilazro = FALSE_;
		}
	    }

/*           Test 2: for B(j,j)=0 */

	    if (c_abs(&b_ref(j, j)) < btol) {
		i__3 = b_subscr(j, j);
		b[i__3].r = 0.f, b[i__3].i = 0.f;

/*              Test 1a: Check for 2 consecutive small subdiagonals in A */

		ilazr2 = FALSE_;
		if (! ilazro) {
		    i__3 = a_subscr(j, j - 1);
		    i__4 = a_subscr(j + 1, j);
		    i__5 = a_subscr(j, j);
		    if (((r__1 = a[i__3].r, dabs(r__1)) + (r__2 = r_imag(&
			    a_ref(j, j - 1)), dabs(r__2))) * (ascale * ((r__3 
			    = a[i__4].r, dabs(r__3)) + (r__4 = r_imag(&a_ref(
			    j + 1, j)), dabs(r__4)))) <= ((r__5 = a[i__5].r, 
			    dabs(r__5)) + (r__6 = r_imag(&a_ref(j, j)), dabs(
			    r__6))) * (ascale * atol)) {
			ilazr2 = TRUE_;
		    }
		}

/*              If both tests pass (1 & 2), i.e., the leading diagonal   
                element of B in the block is zero, split a 1x1 block off   
                at the top. (I.e., at the J-th row/column) The leading   
                diagonal element of the remainder can also be zero, so   
                this may have to be done repeatedly. */

		if (ilazro || ilazr2) {
		    i__3 = ilast - 1;
		    for (jch = j; jch <= i__3; ++jch) {
			i__4 = a_subscr(jch, jch);
			ctemp.r = a[i__4].r, ctemp.i = a[i__4].i;
			clartg_(&ctemp, &a_ref(jch + 1, jch), &c__, &s, &
				a_ref(jch, jch));
			i__4 = a_subscr(jch + 1, jch);
			a[i__4].r = 0.f, a[i__4].i = 0.f;
			i__4 = ilastm - jch;
			crot_(&i__4, &a_ref(jch, jch + 1), lda, &a_ref(jch + 
				1, jch + 1), lda, &c__, &s);
			i__4 = ilastm - jch;
			crot_(&i__4, &b_ref(jch, jch + 1), ldb, &b_ref(jch + 
				1, jch + 1), ldb, &c__, &s);
			if (ilq) {
			    r_cnjg(&q__1, &s);
			    crot_(n, &q_ref(1, jch), &c__1, &q_ref(1, jch + 1)
				    , &c__1, &c__, &q__1);
			}
			if (ilazr2) {
			    i__4 = a_subscr(jch, jch - 1);
			    i__5 = a_subscr(jch, jch - 1);
			    q__1.r = c__ * a[i__5].r, q__1.i = c__ * a[i__5]
				    .i;
			    a[i__4].r = q__1.r, a[i__4].i = q__1.i;
			}
			ilazr2 = FALSE_;
			i__4 = b_subscr(jch + 1, jch + 1);
			if ((r__1 = b[i__4].r, dabs(r__1)) + (r__2 = r_imag(&
				b_ref(jch + 1, jch + 1)), dabs(r__2)) >= btol)
				 {
			    if (jch + 1 >= ilast) {
				goto L60;
			    } else {
				ifirst = jch + 1;
				goto L70;
			    }
			}
			i__4 = b_subscr(jch + 1, jch + 1);
			b[i__4].r = 0.f, b[i__4].i = 0.f;
/* L20: */
		    }
		    goto L50;
		} else {

/*                 Only test 2 passed -- chase the zero to B(ILAST,ILAST)   
                   Then process as in the case B(ILAST,ILAST)=0 */

		    i__3 = ilast - 1;
		    for (jch = j; jch <= i__3; ++jch) {
			i__4 = b_subscr(jch, jch + 1);
			ctemp.r = b[i__4].r, ctemp.i = b[i__4].i;
			clartg_(&ctemp, &b_ref(jch + 1, jch + 1), &c__, &s, &
				b_ref(jch, jch + 1));
			i__4 = b_subscr(jch + 1, jch + 1);
			b[i__4].r = 0.f, b[i__4].i = 0.f;
			if (jch < ilastm - 1) {
			    i__4 = ilastm - jch - 1;
			    crot_(&i__4, &b_ref(jch, jch + 2), ldb, &b_ref(
				    jch + 1, jch + 2), ldb, &c__, &s);
			}
			i__4 = ilastm - jch + 2;
			crot_(&i__4, &a_ref(jch, jch - 1), lda, &a_ref(jch + 
				1, jch - 1), lda, &c__, &s);
			if (ilq) {
			    r_cnjg(&q__1, &s);
			    crot_(n, &q_ref(1, jch), &c__1, &q_ref(1, jch + 1)
				    , &c__1, &c__, &q__1);
			}
			i__4 = a_subscr(jch + 1, jch);
			ctemp.r = a[i__4].r, ctemp.i = a[i__4].i;
			clartg_(&ctemp, &a_ref(jch + 1, jch - 1), &c__, &s, &
				a_ref(jch + 1, jch));
			i__4 = a_subscr(jch + 1, jch - 1);
			a[i__4].r = 0.f, a[i__4].i = 0.f;
			i__4 = jch + 1 - ifrstm;
			crot_(&i__4, &a_ref(ifrstm, jch), &c__1, &a_ref(
				ifrstm, jch - 1), &c__1, &c__, &s);
			i__4 = jch - ifrstm;
			crot_(&i__4, &b_ref(ifrstm, jch), &c__1, &b_ref(
				ifrstm, jch - 1), &c__1, &c__, &s);
			if (ilz) {
			    crot_(n, &z___ref(1, jch), &c__1, &z___ref(1, jch 
				    - 1), &c__1, &c__, &s);
			}
/* L30: */
		    }
		    goto L50;
		}
	    } else if (ilazro) {

/*              Only test 1 passed -- work on J:ILAST */

		ifirst = j;
		goto L70;
	    }

/*           Neither test passed -- try next J   

   L40: */
	}

/*        (Drop-through is "impossible") */

	*info = (*n << 1) + 1;
	goto L210;

/*        B(ILAST,ILAST)=0 -- clear A(ILAST,ILAST-1) to split off a   
          1x1 block. */

L50:
	i__2 = a_subscr(ilast, ilast);
	ctemp.r = a[i__2].r, ctemp.i = a[i__2].i;
	clartg_(&ctemp, &a_ref(ilast, ilast - 1), &c__, &s, &a_ref(ilast, 
		ilast));
	i__2 = a_subscr(ilast, ilast - 1);
	a[i__2].r = 0.f, a[i__2].i = 0.f;
	i__2 = ilast - ifrstm;
	crot_(&i__2, &a_ref(ifrstm, ilast), &c__1, &a_ref(ifrstm, ilast - 1), 
		&c__1, &c__, &s);
	i__2 = ilast - ifrstm;
	crot_(&i__2, &b_ref(ifrstm, ilast), &c__1, &b_ref(ifrstm, ilast - 1), 
		&c__1, &c__, &s);
	if (ilz) {
	    crot_(n, &z___ref(1, ilast), &c__1, &z___ref(1, ilast - 1), &c__1,
		     &c__, &s);
	}

/*        A(ILAST,ILAST-1)=0 -- Standardize B, set ALPHA and BETA */

L60:
	absb = c_abs(&b_ref(ilast, ilast));
	if (absb > safmin) {
	    i__2 = b_subscr(ilast, ilast);
	    q__2.r = b[i__2].r / absb, q__2.i = b[i__2].i / absb;
	    r_cnjg(&q__1, &q__2);
	    signbc.r = q__1.r, signbc.i = q__1.i;
	    i__2 = b_subscr(ilast, ilast);
	    b[i__2].r = absb, b[i__2].i = 0.f;
	    if (ilschr) {
		i__2 = ilast - ifrstm;
		cscal_(&i__2, &signbc, &b_ref(ifrstm, ilast), &c__1);
		i__2 = ilast + 1 - ifrstm;
		cscal_(&i__2, &signbc, &a_ref(ifrstm, ilast), &c__1);
	    } else {
		i__2 = a_subscr(ilast, ilast);
		i__3 = a_subscr(ilast, ilast);
		q__1.r = a[i__3].r * signbc.r - a[i__3].i * signbc.i, q__1.i =
			 a[i__3].r * signbc.i + a[i__3].i * signbc.r;
		a[i__2].r = q__1.r, a[i__2].i = q__1.i;
	    }
	    if (ilz) {
		cscal_(n, &signbc, &z___ref(1, ilast), &c__1);
	    }
	} else {
	    i__2 = b_subscr(ilast, ilast);
	    b[i__2].r = 0.f, b[i__2].i = 0.f;
	}
	i__2 = ilast;
	i__3 = a_subscr(ilast, ilast);
	alpha[i__2].r = a[i__3].r, alpha[i__2].i = a[i__3].i;
	i__2 = ilast;
	i__3 = b_subscr(ilast, ilast);
	beta[i__2].r = b[i__3].r, beta[i__2].i = b[i__3].i;

/*        Go to next block -- exit if finished. */

	--ilast;
	if (ilast < *ilo) {
	    goto L190;
	}

/*        Reset counters */

	iiter = 0;
	eshift.r = 0.f, eshift.i = 0.f;
	if (! ilschr) {
	    ilastm = ilast;
	    if (ifrstm > ilast) {
		ifrstm = *ilo;
	    }
	}
	goto L160;

/*        QZ step   

          This iteration only involves rows/columns IFIRST:ILAST.  We   
          assume IFIRST < ILAST, and that the diagonal of B is non-zero. */

L70:
	++iiter;
	if (! ilschr) {
	    ifrstm = ifirst;
	}

/*        Compute the Shift.   

          At this point, IFIRST < ILAST, and the diagonal elements of   
          B(IFIRST:ILAST,IFIRST,ILAST) are larger than BTOL (in   
          magnitude) */

	if (iiter / 10 * 10 != iiter) {

/*           The Wilkinson shift (AEP p.512), i.e., the eigenvalue of   
             the bottom-right 2x2 block of A inv(B) which is nearest to   
             the bottom-right element.   

             We factor B as U*D, where U has unit diagonals, and   
             compute (A*inv(D))*inv(U). */

	    i__2 = b_subscr(ilast - 1, ilast);
	    q__2.r = bscale * b[i__2].r, q__2.i = bscale * b[i__2].i;
	    i__3 = b_subscr(ilast, ilast);
	    q__3.r = bscale * b[i__3].r, q__3.i = bscale * b[i__3].i;
	    c_div(&q__1, &q__2, &q__3);
	    u12.r = q__1.r, u12.i = q__1.i;
	    i__2 = a_subscr(ilast - 1, ilast - 1);
	    q__2.r = ascale * a[i__2].r, q__2.i = ascale * a[i__2].i;
	    i__3 = b_subscr(ilast - 1, ilast - 1);
	    q__3.r = bscale * b[i__3].r, q__3.i = bscale * b[i__3].i;
	    c_div(&q__1, &q__2, &q__3);
	    ad11.r = q__1.r, ad11.i = q__1.i;
	    i__2 = a_subscr(ilast, ilast - 1);
	    q__2.r = ascale * a[i__2].r, q__2.i = ascale * a[i__2].i;
	    i__3 = b_subscr(ilast - 1, ilast - 1);
	    q__3.r = bscale * b[i__3].r, q__3.i = bscale * b[i__3].i;
	    c_div(&q__1, &q__2, &q__3);
	    ad21.r = q__1.r, ad21.i = q__1.i;
	    i__2 = a_subscr(ilast - 1, ilast);
	    q__2.r = ascale * a[i__2].r, q__2.i = ascale * a[i__2].i;
	    i__3 = b_subscr(ilast, ilast);
	    q__3.r = bscale * b[i__3].r, q__3.i = bscale * b[i__3].i;
	    c_div(&q__1, &q__2, &q__3);
	    ad12.r = q__1.r, ad12.i = q__1.i;
	    i__2 = a_subscr(ilast, ilast);
	    q__2.r = ascale * a[i__2].r, q__2.i = ascale * a[i__2].i;
	    i__3 = b_subscr(ilast, ilast);
	    q__3.r = bscale * b[i__3].r, q__3.i = bscale * b[i__3].i;
	    c_div(&q__1, &q__2, &q__3);
	    ad22.r = q__1.r, ad22.i = q__1.i;
	    q__2.r = u12.r * ad21.r - u12.i * ad21.i, q__2.i = u12.r * ad21.i 
		    + u12.i * ad21.r;
	    q__1.r = ad22.r - q__2.r, q__1.i = ad22.i - q__2.i;
	    abi22.r = q__1.r, abi22.i = q__1.i;

	    q__2.r = ad11.r + abi22.r, q__2.i = ad11.i + abi22.i;
	    q__1.r = q__2.r * .5f, q__1.i = q__2.i * .5f;
	    t.r = q__1.r, t.i = q__1.i;
	    pow_ci(&q__4, &t, &c__2);
	    q__5.r = ad12.r * ad21.r - ad12.i * ad21.i, q__5.i = ad12.r * 
		    ad21.i + ad12.i * ad21.r;
	    q__3.r = q__4.r + q__5.r, q__3.i = q__4.i + q__5.i;
	    q__6.r = ad11.r * ad22.r - ad11.i * ad22.i, q__6.i = ad11.r * 
		    ad22.i + ad11.i * ad22.r;
	    q__2.r = q__3.r - q__6.r, q__2.i = q__3.i - q__6.i;
	    c_sqrt(&q__1, &q__2);
	    rtdisc.r = q__1.r, rtdisc.i = q__1.i;
	    q__1.r = t.r - abi22.r, q__1.i = t.i - abi22.i;
	    q__2.r = t.r - abi22.r, q__2.i = t.i - abi22.i;
	    temp = q__1.r * rtdisc.r + r_imag(&q__2) * r_imag(&rtdisc);
	    if (temp <= 0.f) {
		q__1.r = t.r + rtdisc.r, q__1.i = t.i + rtdisc.i;
		shift.r = q__1.r, shift.i = q__1.i;
	    } else {
		q__1.r = t.r - rtdisc.r, q__1.i = t.i - rtdisc.i;
		shift.r = q__1.r, shift.i = q__1.i;
	    }
	} else {

/*           Exceptional shift.  Chosen for no particularly good reason. */

	    i__2 = a_subscr(ilast - 1, ilast);
	    q__4.r = ascale * a[i__2].r, q__4.i = ascale * a[i__2].i;
	    i__3 = b_subscr(ilast - 1, ilast - 1);
	    q__5.r = bscale * b[i__3].r, q__5.i = bscale * b[i__3].i;
	    c_div(&q__3, &q__4, &q__5);
	    r_cnjg(&q__2, &q__3);
	    q__1.r = eshift.r + q__2.r, q__1.i = eshift.i + q__2.i;
	    eshift.r = q__1.r, eshift.i = q__1.i;
	    shift.r = eshift.r, shift.i = eshift.i;
	}

/*        Now check for two consecutive small subdiagonals. */

	i__2 = ifirst + 1;
	for (j = ilast - 1; j >= i__2; --j) {
	    istart = j;
	    i__3 = a_subscr(j, j);
	    q__2.r = ascale * a[i__3].r, q__2.i = ascale * a[i__3].i;
	    i__4 = b_subscr(j, j);
	    q__4.r = bscale * b[i__4].r, q__4.i = bscale * b[i__4].i;
	    q__3.r = shift.r * q__4.r - shift.i * q__4.i, q__3.i = shift.r * 
		    q__4.i + shift.i * q__4.r;
	    q__1.r = q__2.r - q__3.r, q__1.i = q__2.i - q__3.i;
	    ctemp.r = q__1.r, ctemp.i = q__1.i;
	    temp = (r__1 = ctemp.r, dabs(r__1)) + (r__2 = r_imag(&ctemp), 
		    dabs(r__2));
	    i__3 = a_subscr(j + 1, j);
	    temp2 = ascale * ((r__1 = a[i__3].r, dabs(r__1)) + (r__2 = r_imag(
		    &a_ref(j + 1, j)), dabs(r__2)));
	    tempr = dmax(temp,temp2);
	    if (tempr < 1.f && tempr != 0.f) {
		temp /= tempr;
		temp2 /= tempr;
	    }
	    i__3 = a_subscr(j, j - 1);
	    if (((r__1 = a[i__3].r, dabs(r__1)) + (r__2 = r_imag(&a_ref(j, j 
		    - 1)), dabs(r__2))) * temp2 <= temp * atol) {
		goto L90;
	    }
/* L80: */
	}

	istart = ifirst;
	i__2 = a_subscr(ifirst, ifirst);
	q__2.r = ascale * a[i__2].r, q__2.i = ascale * a[i__2].i;
	i__3 = b_subscr(ifirst, ifirst);
	q__4.r = bscale * b[i__3].r, q__4.i = bscale * b[i__3].i;
	q__3.r = shift.r * q__4.r - shift.i * q__4.i, q__3.i = shift.r * 
		q__4.i + shift.i * q__4.r;
	q__1.r = q__2.r - q__3.r, q__1.i = q__2.i - q__3.i;
	ctemp.r = q__1.r, ctemp.i = q__1.i;
L90:

/*        Do an implicit-shift QZ sweep.   

          Initial Q */

	i__2 = a_subscr(istart + 1, istart);
	q__1.r = ascale * a[i__2].r, q__1.i = ascale * a[i__2].i;
	ctemp2.r = q__1.r, ctemp2.i = q__1.i;
	clartg_(&ctemp, &ctemp2, &c__, &s, &ctemp3);

/*        Sweep */

	i__2 = ilast - 1;
	for (j = istart; j <= i__2; ++j) {
	    if (j > istart) {
		i__3 = a_subscr(j, j - 1);
		ctemp.r = a[i__3].r, ctemp.i = a[i__3].i;
		clartg_(&ctemp, &a_ref(j + 1, j - 1), &c__, &s, &a_ref(j, j - 
			1));
		i__3 = a_subscr(j + 1, j - 1);
		a[i__3].r = 0.f, a[i__3].i = 0.f;
	    }

	    i__3 = ilastm;
	    for (jc = j; jc <= i__3; ++jc) {
		i__4 = a_subscr(j, jc);
		q__2.r = c__ * a[i__4].r, q__2.i = c__ * a[i__4].i;
		i__5 = a_subscr(j + 1, jc);
		q__3.r = s.r * a[i__5].r - s.i * a[i__5].i, q__3.i = s.r * a[
			i__5].i + s.i * a[i__5].r;
		q__1.r = q__2.r + q__3.r, q__1.i = q__2.i + q__3.i;
		ctemp.r = q__1.r, ctemp.i = q__1.i;
		i__4 = a_subscr(j + 1, jc);
		r_cnjg(&q__4, &s);
		q__3.r = -q__4.r, q__3.i = -q__4.i;
		i__5 = a_subscr(j, jc);
		q__2.r = q__3.r * a[i__5].r - q__3.i * a[i__5].i, q__2.i = 
			q__3.r * a[i__5].i + q__3.i * a[i__5].r;
		i__6 = a_subscr(j + 1, jc);
		q__5.r = c__ * a[i__6].r, q__5.i = c__ * a[i__6].i;
		q__1.r = q__2.r + q__5.r, q__1.i = q__2.i + q__5.i;
		a[i__4].r = q__1.r, a[i__4].i = q__1.i;
		i__4 = a_subscr(j, jc);
		a[i__4].r = ctemp.r, a[i__4].i = ctemp.i;
		i__4 = b_subscr(j, jc);
		q__2.r = c__ * b[i__4].r, q__2.i = c__ * b[i__4].i;
		i__5 = b_subscr(j + 1, jc);
		q__3.r = s.r * b[i__5].r - s.i * b[i__5].i, q__3.i = s.r * b[
			i__5].i + s.i * b[i__5].r;
		q__1.r = q__2.r + q__3.r, q__1.i = q__2.i + q__3.i;
		ctemp2.r = q__1.r, ctemp2.i = q__1.i;
		i__4 = b_subscr(j + 1, jc);
		r_cnjg(&q__4, &s);
		q__3.r = -q__4.r, q__3.i = -q__4.i;
		i__5 = b_subscr(j, jc);
		q__2.r = q__3.r * b[i__5].r - q__3.i * b[i__5].i, q__2.i = 
			q__3.r * b[i__5].i + q__3.i * b[i__5].r;
		i__6 = b_subscr(j + 1, jc);
		q__5.r = c__ * b[i__6].r, q__5.i = c__ * b[i__6].i;
		q__1.r = q__2.r + q__5.r, q__1.i = q__2.i + q__5.i;
		b[i__4].r = q__1.r, b[i__4].i = q__1.i;
		i__4 = b_subscr(j, jc);
		b[i__4].r = ctemp2.r, b[i__4].i = ctemp2.i;
/* L100: */
	    }
	    if (ilq) {
		i__3 = *n;
		for (jr = 1; jr <= i__3; ++jr) {
		    i__4 = q_subscr(jr, j);
		    q__2.r = c__ * q[i__4].r, q__2.i = c__ * q[i__4].i;
		    r_cnjg(&q__4, &s);
		    i__5 = q_subscr(jr, j + 1);
		    q__3.r = q__4.r * q[i__5].r - q__4.i * q[i__5].i, q__3.i =
			     q__4.r * q[i__5].i + q__4.i * q[i__5].r;
		    q__1.r = q__2.r + q__3.r, q__1.i = q__2.i + q__3.i;
		    ctemp.r = q__1.r, ctemp.i = q__1.i;
		    i__4 = q_subscr(jr, j + 1);
		    q__3.r = -s.r, q__3.i = -s.i;
		    i__5 = q_subscr(jr, j);
		    q__2.r = q__3.r * q[i__5].r - q__3.i * q[i__5].i, q__2.i =
			     q__3.r * q[i__5].i + q__3.i * q[i__5].r;
		    i__6 = q_subscr(jr, j + 1);
		    q__4.r = c__ * q[i__6].r, q__4.i = c__ * q[i__6].i;
		    q__1.r = q__2.r + q__4.r, q__1.i = q__2.i + q__4.i;
		    q[i__4].r = q__1.r, q[i__4].i = q__1.i;
		    i__4 = q_subscr(jr, j);
		    q[i__4].r = ctemp.r, q[i__4].i = ctemp.i;
/* L110: */
		}
	    }

	    i__3 = b_subscr(j + 1, j + 1);
	    ctemp.r = b[i__3].r, ctemp.i = b[i__3].i;
	    clartg_(&ctemp, &b_ref(j + 1, j), &c__, &s, &b_ref(j + 1, j + 1));
	    i__3 = b_subscr(j + 1, j);
	    b[i__3].r = 0.f, b[i__3].i = 0.f;

/* Computing MIN */
	    i__4 = j + 2;
	    i__3 = min(i__4,ilast);
	    for (jr = ifrstm; jr <= i__3; ++jr) {
		i__4 = a_subscr(jr, j + 1);
		q__2.r = c__ * a[i__4].r, q__2.i = c__ * a[i__4].i;
		i__5 = a_subscr(jr, j);
		q__3.r = s.r * a[i__5].r - s.i * a[i__5].i, q__3.i = s.r * a[
			i__5].i + s.i * a[i__5].r;
		q__1.r = q__2.r + q__3.r, q__1.i = q__2.i + q__3.i;
		ctemp.r = q__1.r, ctemp.i = q__1.i;
		i__4 = a_subscr(jr, j);
		r_cnjg(&q__4, &s);
		q__3.r = -q__4.r, q__3.i = -q__4.i;
		i__5 = a_subscr(jr, j + 1);
		q__2.r = q__3.r * a[i__5].r - q__3.i * a[i__5].i, q__2.i = 
			q__3.r * a[i__5].i + q__3.i * a[i__5].r;
		i__6 = a_subscr(jr, j);
		q__5.r = c__ * a[i__6].r, q__5.i = c__ * a[i__6].i;
		q__1.r = q__2.r + q__5.r, q__1.i = q__2.i + q__5.i;
		a[i__4].r = q__1.r, a[i__4].i = q__1.i;
		i__4 = a_subscr(jr, j + 1);
		a[i__4].r = ctemp.r, a[i__4].i = ctemp.i;
/* L120: */
	    }
	    i__3 = j;
	    for (jr = ifrstm; jr <= i__3; ++jr) {
		i__4 = b_subscr(jr, j + 1);
		q__2.r = c__ * b[i__4].r, q__2.i = c__ * b[i__4].i;
		i__5 = b_subscr(jr, j);
		q__3.r = s.r * b[i__5].r - s.i * b[i__5].i, q__3.i = s.r * b[
			i__5].i + s.i * b[i__5].r;
		q__1.r = q__2.r + q__3.r, q__1.i = q__2.i + q__3.i;
		ctemp.r = q__1.r, ctemp.i = q__1.i;
		i__4 = b_subscr(jr, j);
		r_cnjg(&q__4, &s);
		q__3.r = -q__4.r, q__3.i = -q__4.i;
		i__5 = b_subscr(jr, j + 1);
		q__2.r = q__3.r * b[i__5].r - q__3.i * b[i__5].i, q__2.i = 
			q__3.r * b[i__5].i + q__3.i * b[i__5].r;
		i__6 = b_subscr(jr, j);
		q__5.r = c__ * b[i__6].r, q__5.i = c__ * b[i__6].i;
		q__1.r = q__2.r + q__5.r, q__1.i = q__2.i + q__5.i;
		b[i__4].r = q__1.r, b[i__4].i = q__1.i;
		i__4 = b_subscr(jr, j + 1);
		b[i__4].r = ctemp.r, b[i__4].i = ctemp.i;
/* L130: */
	    }
	    if (ilz) {
		i__3 = *n;
		for (jr = 1; jr <= i__3; ++jr) {
		    i__4 = z___subscr(jr, j + 1);
		    q__2.r = c__ * z__[i__4].r, q__2.i = c__ * z__[i__4].i;
		    i__5 = z___subscr(jr, j);
		    q__3.r = s.r * z__[i__5].r - s.i * z__[i__5].i, q__3.i = 
			    s.r * z__[i__5].i + s.i * z__[i__5].r;
		    q__1.r = q__2.r + q__3.r, q__1.i = q__2.i + q__3.i;
		    ctemp.r = q__1.r, ctemp.i = q__1.i;
		    i__4 = z___subscr(jr, j);
		    r_cnjg(&q__4, &s);
		    q__3.r = -q__4.r, q__3.i = -q__4.i;
		    i__5 = z___subscr(jr, j + 1);
		    q__2.r = q__3.r * z__[i__5].r - q__3.i * z__[i__5].i, 
			    q__2.i = q__3.r * z__[i__5].i + q__3.i * z__[i__5]
			    .r;
		    i__6 = z___subscr(jr, j);
		    q__5.r = c__ * z__[i__6].r, q__5.i = c__ * z__[i__6].i;
		    q__1.r = q__2.r + q__5.r, q__1.i = q__2.i + q__5.i;
		    z__[i__4].r = q__1.r, z__[i__4].i = q__1.i;
		    i__4 = z___subscr(jr, j + 1);
		    z__[i__4].r = ctemp.r, z__[i__4].i = ctemp.i;
/* L140: */
		}
	    }
/* L150: */
	}

L160:

/* L170: */
	;
    }

/*     Drop-through = non-convergence */

L180:
    *info = ilast;
    goto L210;

/*     Successful completion of all QZ steps */

L190:

/*     Set Eigenvalues 1:ILO-1 */

    i__1 = *ilo - 1;
    for (j = 1; j <= i__1; ++j) {
	absb = c_abs(&b_ref(j, j));
	if (absb > safmin) {
	    i__2 = b_subscr(j, j);
	    q__2.r = b[i__2].r / absb, q__2.i = b[i__2].i / absb;
	    r_cnjg(&q__1, &q__2);
	    signbc.r = q__1.r, signbc.i = q__1.i;
	    i__2 = b_subscr(j, j);
	    b[i__2].r = absb, b[i__2].i = 0.f;
	    if (ilschr) {
		i__2 = j - 1;
		cscal_(&i__2, &signbc, &b_ref(1, j), &c__1);
		cscal_(&j, &signbc, &a_ref(1, j), &c__1);
	    } else {
		i__2 = a_subscr(j, j);
		i__3 = a_subscr(j, j);
		q__1.r = a[i__3].r * signbc.r - a[i__3].i * signbc.i, q__1.i =
			 a[i__3].r * signbc.i + a[i__3].i * signbc.r;
		a[i__2].r = q__1.r, a[i__2].i = q__1.i;
	    }
	    if (ilz) {
		cscal_(n, &signbc, &z___ref(1, j), &c__1);
	    }
	} else {
	    i__2 = b_subscr(j, j);
	    b[i__2].r = 0.f, b[i__2].i = 0.f;
	}
	i__2 = j;
	i__3 = a_subscr(j, j);
	alpha[i__2].r = a[i__3].r, alpha[i__2].i = a[i__3].i;
	i__2 = j;
	i__3 = b_subscr(j, j);
	beta[i__2].r = b[i__3].r, beta[i__2].i = b[i__3].i;
/* L200: */
    }

/*     Normal Termination */

    *info = 0;

/*     Exit (other than argument error) -- return optimal workspace size */

L210:
    q__1.r = (real) (*n), q__1.i = 0.f;
    work[1].r = q__1.r, work[1].i = q__1.i;
    return 0;

/*     End of CHGEQZ */

} /* chgeqz_ */

#undef z___ref
#undef z___subscr
#undef q_ref
#undef q_subscr
#undef b_ref
#undef b_subscr
#undef a_ref
#undef a_subscr


