#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int sggbal_(char *job, integer *n, real *a, integer *lda, 
	real *b, integer *ldb, integer *ilo, integer *ihi, real *lscale, real 
	*rscale, real *work, integer *info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    SGGBAL balances a pair of general real matrices (A,B).  This   
    involves, first, permuting A and B by similarity transformations to   
    isolate eigenvalues in the first 1 to ILO$-$1 and last IHI+1 to N   
    elements on the diagonal; and second, applying a diagonal similarity   
    transformation to rows and columns ILO to IHI to make the rows   
    and columns as close in norm as possible. Both steps are optional.   

    Balancing may reduce the 1-norm of the matrices, and improve the   
    accuracy of the computed eigenvalues and/or eigenvectors in the   
    generalized eigenvalue problem A*x = lambda*B*x.   

    Arguments   
    =========   

    JOB     (input) CHARACTER*1   
            Specifies the operations to be performed on A and B:   
            = 'N':  none:  simply set ILO = 1, IHI = N, LSCALE(I) = 1.0   
                    and RSCALE(I) = 1.0 for i = 1,...,N.   
            = 'P':  permute only;   
            = 'S':  scale only;   
            = 'B':  both permute and scale.   

    N       (input) INTEGER   
            The order of the matrices A and B.  N >= 0.   

    A       (input/output) REAL array, dimension (LDA,N)   
            On entry, the input matrix A.   
            On exit,  A is overwritten by the balanced matrix.   
            If JOB = 'N', A is not referenced.   

    LDA     (input) INTEGER   
            The leading dimension of the array A. LDA >= max(1,N).   

    B       (input/output) REAL array, dimension (LDB,N)   
            On entry, the input matrix B.   
            On exit,  B is overwritten by the balanced matrix.   
            If JOB = 'N', B is not referenced.   

    LDB     (input) INTEGER   
            The leading dimension of the array B. LDB >= max(1,N).   

    ILO     (output) INTEGER   
    IHI     (output) INTEGER   
            ILO and IHI are set to integers such that on exit   
            A(i,j) = 0 and B(i,j) = 0 if i > j and   
            j = 1,...,ILO-1 or i = IHI+1,...,N.   
            If JOB = 'N' or 'S', ILO = 1 and IHI = N.   

    LSCALE  (output) REAL array, dimension (N)   
            Details of the permutations and scaling factors applied   
            to the left side of A and B.  If P(j) is the index of the   
            row interchanged with row j, and D(j)   
            is the scaling factor applied to row j, then   
              LSCALE(j) = P(j)    for J = 1,...,ILO-1   
                        = D(j)    for J = ILO,...,IHI   
                        = P(j)    for J = IHI+1,...,N.   
            The order in which the interchanges are made is N to IHI+1,   
            then 1 to ILO-1.   

    RSCALE  (output) REAL array, dimension (N)   
            Details of the permutations and scaling factors applied   
            to the right side of A and B.  If P(j) is the index of the   
            column interchanged with column j, and D(j)   
            is the scaling factor applied to column j, then   
              LSCALE(j) = P(j)    for J = 1,...,ILO-1   
                        = D(j)    for J = ILO,...,IHI   
                        = P(j)    for J = IHI+1,...,N.   
            The order in which the interchanges are made is N to IHI+1,   
            then 1 to ILO-1.   

    WORK    (workspace) REAL array, dimension (6*N)   

    INFO    (output) INTEGER   
            = 0:  successful exit   
            < 0:  if INFO = -i, the i-th argument had an illegal value.   

    Further Details   
    ===============   

    See R.C. WARD, Balancing the generalized eigenvalue problem,   
                   SIAM J. Sci. Stat. Comp. 2 (1981), 141-152.   

    =====================================================================   


       Test the input parameters   

       Parameter adjustments */
    /* Table of constant values */
    static integer c__1 = 1;
    static real c_b34 = 10.f;
    static real c_b70 = .5f;
    
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2, i__3;
    real r__1, r__2, r__3;
    /* Builtin functions */
    double r_lg10(real *), r_sign(real *, real *), pow_ri(real *, integer *);
    /* Local variables */
    static integer lcab;
    static real beta, coef;
    static integer irab, lrab;
    static real basl, cmax;
    extern doublereal sdot_(integer *, real *, integer *, real *, integer *);
    static real coef2, coef5;
    static integer i__, j, k, l, m;
    static real gamma, t, alpha;
    extern logical lsame_(char *, char *);
    extern /* Subroutine */ int sscal_(integer *, real *, real *, integer *);
    static real sfmin, sfmax;
    static integer iflow;
    extern /* Subroutine */ int sswap_(integer *, real *, integer *, real *, 
	    integer *);
    static integer kount;
    extern /* Subroutine */ int saxpy_(integer *, real *, real *, integer *, 
	    real *, integer *);
    static integer jc;
    static real ta, tb, tc;
    static integer ir, it;
    static real ew;
    static integer nr;
    static real pgamma;
    extern doublereal slamch_(char *);
    extern /* Subroutine */ int xerbla_(char *, integer *);
    extern integer isamax_(integer *, real *, integer *);
    static integer lsfmin, lsfmax, ip1, jp1, lm1;
    static real cab, rab, ewc, cor, sum;
    static integer nrp2, icab;
#define a_ref(a_1,a_2) a[(a_2)*a_dim1 + a_1]
#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]


    a_dim1 = *lda;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    --lscale;
    --rscale;
    --work;

    /* Function Body */
    *info = 0;
    if (! lsame_(job, "N") && ! lsame_(job, "P") && ! lsame_(job, "S") 
	    && ! lsame_(job, "B")) {
	*info = -1;
    } else if (*n < 0) {
	*info = -2;
    } else if (*lda < max(1,*n)) {
	*info = -4;
    } else if (*ldb < max(1,*n)) {
	*info = -5;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla_("SGGBAL", &i__1);
	return 0;
    }

    k = 1;
    l = *n;

/*     Quick return if possible */

    if (*n == 0) {
	return 0;
    }

    if (lsame_(job, "N")) {
	*ilo = 1;
	*ihi = *n;
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    lscale[i__] = 1.f;
	    rscale[i__] = 1.f;
/* L10: */
	}
	return 0;
    }

    if (k == l) {
	*ilo = 1;
	*ihi = 1;
	lscale[1] = 1.f;
	rscale[1] = 1.f;
	return 0;
    }

    if (lsame_(job, "S")) {
	goto L190;
    }

    goto L30;

/*     Permute the matrices A and B to isolate the eigenvalues.   

       Find row with one nonzero in columns 1 through L */

L20:
    l = lm1;
    if (l != 1) {
	goto L30;
    }

    rscale[1] = 1.f;
    lscale[1] = 1.f;
    goto L190;

L30:
    lm1 = l - 1;
    for (i__ = l; i__ >= 1; --i__) {
	i__1 = lm1;
	for (j = 1; j <= i__1; ++j) {
	    jp1 = j + 1;
	    if (a_ref(i__, j) != 0.f || b_ref(i__, j) != 0.f) {
		goto L50;
	    }
/* L40: */
	}
	j = l;
	goto L70;

L50:
	i__1 = l;
	for (j = jp1; j <= i__1; ++j) {
	    if (a_ref(i__, j) != 0.f || b_ref(i__, j) != 0.f) {
		goto L80;
	    }
/* L60: */
	}
	j = jp1 - 1;

L70:
	m = l;
	iflow = 1;
	goto L160;
L80:
	;
    }
    goto L100;

/*     Find column with one nonzero in rows K through N */

L90:
    ++k;

L100:
    i__1 = l;
    for (j = k; j <= i__1; ++j) {
	i__2 = lm1;
	for (i__ = k; i__ <= i__2; ++i__) {
	    ip1 = i__ + 1;
	    if (a_ref(i__, j) != 0.f || b_ref(i__, j) != 0.f) {
		goto L120;
	    }
/* L110: */
	}
	i__ = l;
	goto L140;
L120:
	i__2 = l;
	for (i__ = ip1; i__ <= i__2; ++i__) {
	    if (a_ref(i__, j) != 0.f || b_ref(i__, j) != 0.f) {
		goto L150;
	    }
/* L130: */
	}
	i__ = ip1 - 1;
L140:
	m = k;
	iflow = 2;
	goto L160;
L150:
	;
    }
    goto L190;

/*     Permute rows M and I */

L160:
    lscale[m] = (real) i__;
    if (i__ == m) {
	goto L170;
    }
    i__1 = *n - k + 1;
    sswap_(&i__1, &a_ref(i__, k), lda, &a_ref(m, k), lda);
    i__1 = *n - k + 1;
    sswap_(&i__1, &b_ref(i__, k), ldb, &b_ref(m, k), ldb);

/*     Permute columns M and J */

L170:
    rscale[m] = (real) j;
    if (j == m) {
	goto L180;
    }
    sswap_(&l, &a_ref(1, j), &c__1, &a_ref(1, m), &c__1);
    sswap_(&l, &b_ref(1, j), &c__1, &b_ref(1, m), &c__1);

L180:
    switch (iflow) {
	case 1:  goto L20;
	case 2:  goto L90;
    }

L190:
    *ilo = k;
    *ihi = l;

    if (*ilo == *ihi) {
	return 0;
    }

    if (lsame_(job, "P")) {
	return 0;
    }

/*     Balance the submatrix in rows ILO to IHI. */

    nr = *ihi - *ilo + 1;
    i__1 = *ihi;
    for (i__ = *ilo; i__ <= i__1; ++i__) {
	rscale[i__] = 0.f;
	lscale[i__] = 0.f;

	work[i__] = 0.f;
	work[i__ + *n] = 0.f;
	work[i__ + (*n << 1)] = 0.f;
	work[i__ + *n * 3] = 0.f;
	work[i__ + (*n << 2)] = 0.f;
	work[i__ + *n * 5] = 0.f;
/* L200: */
    }

/*     Compute right side vector in resulting linear equations */

    basl = r_lg10(&c_b34);
    i__1 = *ihi;
    for (i__ = *ilo; i__ <= i__1; ++i__) {
	i__2 = *ihi;
	for (j = *ilo; j <= i__2; ++j) {
	    tb = b_ref(i__, j);
	    ta = a_ref(i__, j);
	    if (ta == 0.f) {
		goto L210;
	    }
	    r__1 = dabs(ta);
	    ta = r_lg10(&r__1) / basl;
L210:
	    if (tb == 0.f) {
		goto L220;
	    }
	    r__1 = dabs(tb);
	    tb = r_lg10(&r__1) / basl;
L220:
	    work[i__ + (*n << 2)] = work[i__ + (*n << 2)] - ta - tb;
	    work[j + *n * 5] = work[j + *n * 5] - ta - tb;
/* L230: */
	}
/* L240: */
    }

    coef = 1.f / (real) (nr << 1);
    coef2 = coef * coef;
    coef5 = coef2 * .5f;
    nrp2 = nr + 2;
    beta = 0.f;
    it = 1;

/*     Start generalized conjugate gradient iteration */

L250:

    gamma = sdot_(&nr, &work[*ilo + (*n << 2)], &c__1, &work[*ilo + (*n << 2)]
	    , &c__1) + sdot_(&nr, &work[*ilo + *n * 5], &c__1, &work[*ilo + *
	    n * 5], &c__1);

    ew = 0.f;
    ewc = 0.f;
    i__1 = *ihi;
    for (i__ = *ilo; i__ <= i__1; ++i__) {
	ew += work[i__ + (*n << 2)];
	ewc += work[i__ + *n * 5];
/* L260: */
    }

/* Computing 2nd power */
    r__1 = ew;
/* Computing 2nd power */
    r__2 = ewc;
/* Computing 2nd power */
    r__3 = ew - ewc;
    gamma = coef * gamma - coef2 * (r__1 * r__1 + r__2 * r__2) - coef5 * (
	    r__3 * r__3);
    if (gamma == 0.f) {
	goto L350;
    }
    if (it != 1) {
	beta = gamma / pgamma;
    }
    t = coef5 * (ewc - ew * 3.f);
    tc = coef5 * (ew - ewc * 3.f);

    sscal_(&nr, &beta, &work[*ilo], &c__1);
    sscal_(&nr, &beta, &work[*ilo + *n], &c__1);

    saxpy_(&nr, &coef, &work[*ilo + (*n << 2)], &c__1, &work[*ilo + *n], &
	    c__1);
    saxpy_(&nr, &coef, &work[*ilo + *n * 5], &c__1, &work[*ilo], &c__1);

    i__1 = *ihi;
    for (i__ = *ilo; i__ <= i__1; ++i__) {
	work[i__] += tc;
	work[i__ + *n] += t;
/* L270: */
    }

/*     Apply matrix to vector */

    i__1 = *ihi;
    for (i__ = *ilo; i__ <= i__1; ++i__) {
	kount = 0;
	sum = 0.f;
	i__2 = *ihi;
	for (j = *ilo; j <= i__2; ++j) {
	    if (a_ref(i__, j) == 0.f) {
		goto L280;
	    }
	    ++kount;
	    sum += work[j];
L280:
	    if (b_ref(i__, j) == 0.f) {
		goto L290;
	    }
	    ++kount;
	    sum += work[j];
L290:
	    ;
	}
	work[i__ + (*n << 1)] = (real) kount * work[i__ + *n] + sum;
/* L300: */
    }

    i__1 = *ihi;
    for (j = *ilo; j <= i__1; ++j) {
	kount = 0;
	sum = 0.f;
	i__2 = *ihi;
	for (i__ = *ilo; i__ <= i__2; ++i__) {
	    if (a_ref(i__, j) == 0.f) {
		goto L310;
	    }
	    ++kount;
	    sum += work[i__ + *n];
L310:
	    if (b_ref(i__, j) == 0.f) {
		goto L320;
	    }
	    ++kount;
	    sum += work[i__ + *n];
L320:
	    ;
	}
	work[j + *n * 3] = (real) kount * work[j] + sum;
/* L330: */
    }

    sum = sdot_(&nr, &work[*ilo + *n], &c__1, &work[*ilo + (*n << 1)], &c__1) 
	    + sdot_(&nr, &work[*ilo], &c__1, &work[*ilo + *n * 3], &c__1);
    alpha = gamma / sum;

/*     Determine correction to current iteration */

    cmax = 0.f;
    i__1 = *ihi;
    for (i__ = *ilo; i__ <= i__1; ++i__) {
	cor = alpha * work[i__ + *n];
	if (dabs(cor) > cmax) {
	    cmax = dabs(cor);
	}
	lscale[i__] += cor;
	cor = alpha * work[i__];
	if (dabs(cor) > cmax) {
	    cmax = dabs(cor);
	}
	rscale[i__] += cor;
/* L340: */
    }
    if (cmax < .5f) {
	goto L350;
    }

    r__1 = -alpha;
    saxpy_(&nr, &r__1, &work[*ilo + (*n << 1)], &c__1, &work[*ilo + (*n << 2)]
	    , &c__1);
    r__1 = -alpha;
    saxpy_(&nr, &r__1, &work[*ilo + *n * 3], &c__1, &work[*ilo + *n * 5], &
	    c__1);

    pgamma = gamma;
    ++it;
    if (it <= nrp2) {
	goto L250;
    }

/*     End generalized conjugate gradient iteration */

L350:
    sfmin = slamch_("S");
    sfmax = 1.f / sfmin;
    lsfmin = (integer) (r_lg10(&sfmin) / basl + 1.f);
    lsfmax = (integer) (r_lg10(&sfmax) / basl);
    i__1 = *ihi;
    for (i__ = *ilo; i__ <= i__1; ++i__) {
	i__2 = *n - *ilo + 1;
	irab = isamax_(&i__2, &a_ref(i__, *ilo), lda);
	rab = (r__1 = a_ref(i__, irab + *ilo - 1), dabs(r__1));
	i__2 = *n - *ilo + 1;
	irab = isamax_(&i__2, &b_ref(i__, *ilo), lda);
/* Computing MAX */
	r__2 = rab, r__3 = (r__1 = b_ref(i__, irab + *ilo - 1), dabs(r__1));
	rab = dmax(r__2,r__3);
	r__1 = rab + sfmin;
	lrab = (integer) (r_lg10(&r__1) / basl + 1.f);
	ir = lscale[i__] + r_sign(&c_b70, &lscale[i__]);
/* Computing MIN */
	i__2 = max(ir,lsfmin), i__2 = min(i__2,lsfmax), i__3 = lsfmax - lrab;
	ir = min(i__2,i__3);
	lscale[i__] = pow_ri(&c_b34, &ir);
	icab = isamax_(ihi, &a_ref(1, i__), &c__1);
	cab = (r__1 = a_ref(icab, i__), dabs(r__1));
	icab = isamax_(ihi, &b_ref(1, i__), &c__1);
/* Computing MAX */
	r__2 = cab, r__3 = (r__1 = b_ref(icab, i__), dabs(r__1));
	cab = dmax(r__2,r__3);
	r__1 = cab + sfmin;
	lcab = (integer) (r_lg10(&r__1) / basl + 1.f);
	jc = rscale[i__] + r_sign(&c_b70, &rscale[i__]);
/* Computing MIN */
	i__2 = max(jc,lsfmin), i__2 = min(i__2,lsfmax), i__3 = lsfmax - lcab;
	jc = min(i__2,i__3);
	rscale[i__] = pow_ri(&c_b34, &jc);
/* L360: */
    }

/*     Row scaling of matrices A and B */

    i__1 = *ihi;
    for (i__ = *ilo; i__ <= i__1; ++i__) {
	i__2 = *n - *ilo + 1;
	sscal_(&i__2, &lscale[i__], &a_ref(i__, *ilo), lda);
	i__2 = *n - *ilo + 1;
	sscal_(&i__2, &lscale[i__], &b_ref(i__, *ilo), ldb);
/* L370: */
    }

/*     Column scaling of matrices A and B */

    i__1 = *ihi;
    for (j = *ilo; j <= i__1; ++j) {
	sscal_(ihi, &rscale[j], &a_ref(1, j), &c__1);
	sscal_(ihi, &rscale[j], &b_ref(1, j), &c__1);
/* L380: */
    }

    return 0;

/*     End of SGGBAL */

} /* sggbal_ */

#undef b_ref
#undef a_ref


