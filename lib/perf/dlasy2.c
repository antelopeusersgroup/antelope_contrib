#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int dlasy2_(logical *ltranl, logical *ltranr, integer *isgn, 
	integer *n1, integer *n2, doublereal *tl, integer *ldtl, doublereal *
	tr, integer *ldtr, doublereal *b, integer *ldb, doublereal *scale, 
	doublereal *x, integer *ldx, doublereal *xnorm, integer *info)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       October 31, 1992   


    Purpose   
    =======   

    DLASY2 solves for the N1 by N2 matrix X, 1 <= N1,N2 <= 2, in   

           op(TL)*X + ISGN*X*op(TR) = SCALE*B,   

    where TL is N1 by N1, TR is N2 by N2, B is N1 by N2, and ISGN = 1 or   
    -1.  op(T) = T or T', where T' denotes the transpose of T.   

    Arguments   
    =========   

    LTRANL  (input) LOGICAL   
            On entry, LTRANL specifies the op(TL):   
               = .FALSE., op(TL) = TL,   
               = .TRUE., op(TL) = TL'.   

    LTRANR  (input) LOGICAL   
            On entry, LTRANR specifies the op(TR):   
              = .FALSE., op(TR) = TR,   
              = .TRUE., op(TR) = TR'.   

    ISGN    (input) INTEGER   
            On entry, ISGN specifies the sign of the equation   
            as described before. ISGN may only be 1 or -1.   

    N1      (input) INTEGER   
            On entry, N1 specifies the order of matrix TL.   
            N1 may only be 0, 1 or 2.   

    N2      (input) INTEGER   
            On entry, N2 specifies the order of matrix TR.   
            N2 may only be 0, 1 or 2.   

    TL      (input) DOUBLE PRECISION array, dimension (LDTL,2)   
            On entry, TL contains an N1 by N1 matrix.   

    LDTL    (input) INTEGER   
            The leading dimension of the matrix TL. LDTL >= max(1,N1).   

    TR      (input) DOUBLE PRECISION array, dimension (LDTR,2)   
            On entry, TR contains an N2 by N2 matrix.   

    LDTR    (input) INTEGER   
            The leading dimension of the matrix TR. LDTR >= max(1,N2).   

    B       (input) DOUBLE PRECISION array, dimension (LDB,2)   
            On entry, the N1 by N2 matrix B contains the right-hand   
            side of the equation.   

    LDB     (input) INTEGER   
            The leading dimension of the matrix B. LDB >= max(1,N1).   

    SCALE   (output) DOUBLE PRECISION   
            On exit, SCALE contains the scale factor. SCALE is chosen   
            less than or equal to 1 to prevent the solution overflowing.   

    X       (output) DOUBLE PRECISION array, dimension (LDX,2)   
            On exit, X contains the N1 by N2 solution.   

    LDX     (input) INTEGER   
            The leading dimension of the matrix X. LDX >= max(1,N1).   

    XNORM   (output) DOUBLE PRECISION   
            On exit, XNORM is the infinity-norm of the solution.   

    INFO    (output) INTEGER   
            On exit, INFO is set to   
               0: successful exit.   
               1: TL and TR have too close eigenvalues, so TL or   
                  TR is perturbed to get a nonsingular equation.   
            NOTE: In the interests of speed, this routine does not   
                  check the inputs for errors.   

   =====================================================================   

       Parameter adjustments */
    /* Table of constant values */
    static integer c__4 = 4;
    static integer c__1 = 1;
    static integer c__16 = 16;
    static integer c__0 = 0;
    
    /* Initialized data */
    static integer locu12[4] = { 3,4,1,2 };
    static integer locl21[4] = { 2,1,4,3 };
    static integer locu22[4] = { 4,3,2,1 };
    static logical xswpiv[4] = { FALSE_,FALSE_,TRUE_,TRUE_ };
    static logical bswpiv[4] = { FALSE_,TRUE_,FALSE_,TRUE_ };
    /* System generated locals */
    integer b_dim1, b_offset, tl_dim1, tl_offset, tr_dim1, tr_offset, x_dim1, 
	    x_offset;
    doublereal d__1, d__2, d__3, d__4, d__5, d__6, d__7, d__8;
    /* Local variables */
    static doublereal btmp[4], smin;
    static integer ipiv;
    static doublereal temp;
    static integer jpiv[4];
    static doublereal xmax;
    static integer ipsv, jpsv, i__, j, k;
    static logical bswap;
    extern /* Subroutine */ int dcopy_(integer *, doublereal *, integer *, 
	    doublereal *, integer *), dswap_(integer *, doublereal *, integer 
	    *, doublereal *, integer *);
    static logical xswap;
    static doublereal x2[2], l21, u11, u12;
    static integer ip, jp;
    static doublereal u22, t16[16]	/* was [4][4] */;
    extern doublereal dlamch_(char *);
    extern integer idamax_(integer *, doublereal *, integer *);
    static doublereal smlnum, gam, bet, eps, sgn, tmp[4], tau1;
#define b_ref(a_1,a_2) b[(a_2)*b_dim1 + a_1]
#define x_ref(a_1,a_2) x[(a_2)*x_dim1 + a_1]
#define t16_ref(a_1,a_2) t16[(a_2)*4 + a_1 - 5]
#define tl_ref(a_1,a_2) tl[(a_2)*tl_dim1 + a_1]
#define tr_ref(a_1,a_2) tr[(a_2)*tr_dim1 + a_1]


    tl_dim1 = *ldtl;
    tl_offset = 1 + tl_dim1 * 1;
    tl -= tl_offset;
    tr_dim1 = *ldtr;
    tr_offset = 1 + tr_dim1 * 1;
    tr -= tr_offset;
    b_dim1 = *ldb;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;
    x_dim1 = *ldx;
    x_offset = 1 + x_dim1 * 1;
    x -= x_offset;

    /* Function Body   

       Do not check the input parameters for errors */

    *info = 0;

/*     Quick return if possible */

    if (*n1 == 0 || *n2 == 0) {
	return 0;
    }

/*     Set constants to control overflow */

    eps = dlamch_("P");
    smlnum = dlamch_("S") / eps;
    sgn = (doublereal) (*isgn);

    k = *n1 + *n1 + *n2 - 2;
    switch (k) {
	case 1:  goto L10;
	case 2:  goto L20;
	case 3:  goto L30;
	case 4:  goto L50;
    }

/*     1 by 1: TL11*X + SGN*X*TR11 = B11 */

L10:
    tau1 = tl_ref(1, 1) + sgn * tr_ref(1, 1);
    bet = abs(tau1);
    if (bet <= smlnum) {
	tau1 = smlnum;
	bet = smlnum;
	*info = 1;
    }

    *scale = 1.;
    gam = (d__1 = b_ref(1, 1), abs(d__1));
    if (smlnum * gam > bet) {
	*scale = 1. / gam;
    }

    x_ref(1, 1) = b_ref(1, 1) * *scale / tau1;
    *xnorm = (d__1 = x_ref(1, 1), abs(d__1));
    return 0;

/*     1 by 2:   
       TL11*[X11 X12] + ISGN*[X11 X12]*op[TR11 TR12]  = [B11 B12]   
                                         [TR21 TR22] */

L20:

/* Computing MAX   
   Computing MAX */
    d__7 = (d__1 = tl_ref(1, 1), abs(d__1)), d__8 = (d__2 = tr_ref(1, 1), abs(
	    d__2)), d__7 = max(d__7,d__8), d__8 = (d__3 = tr_ref(1, 2), abs(
	    d__3)), d__7 = max(d__7,d__8), d__8 = (d__4 = tr_ref(2, 1), abs(
	    d__4)), d__7 = max(d__7,d__8), d__8 = (d__5 = tr_ref(2, 2), abs(
	    d__5));
    d__6 = eps * max(d__7,d__8);
    smin = max(d__6,smlnum);
    tmp[0] = tl_ref(1, 1) + sgn * tr_ref(1, 1);
    tmp[3] = tl_ref(1, 1) + sgn * tr_ref(2, 2);
    if (*ltranr) {
	tmp[1] = sgn * tr_ref(2, 1);
	tmp[2] = sgn * tr_ref(1, 2);
    } else {
	tmp[1] = sgn * tr_ref(1, 2);
	tmp[2] = sgn * tr_ref(2, 1);
    }
    btmp[0] = b_ref(1, 1);
    btmp[1] = b_ref(1, 2);
    goto L40;

/*     2 by 1:   
            op[TL11 TL12]*[X11] + ISGN* [X11]*TR11  = [B11]   
              [TL21 TL22] [X21]         [X21]         [B21] */

L30:
/* Computing MAX   
   Computing MAX */
    d__7 = (d__1 = tr_ref(1, 1), abs(d__1)), d__8 = (d__2 = tl_ref(1, 1), abs(
	    d__2)), d__7 = max(d__7,d__8), d__8 = (d__3 = tl_ref(1, 2), abs(
	    d__3)), d__7 = max(d__7,d__8), d__8 = (d__4 = tl_ref(2, 1), abs(
	    d__4)), d__7 = max(d__7,d__8), d__8 = (d__5 = tl_ref(2, 2), abs(
	    d__5));
    d__6 = eps * max(d__7,d__8);
    smin = max(d__6,smlnum);
    tmp[0] = tl_ref(1, 1) + sgn * tr_ref(1, 1);
    tmp[3] = tl_ref(2, 2) + sgn * tr_ref(1, 1);
    if (*ltranl) {
	tmp[1] = tl_ref(1, 2);
	tmp[2] = tl_ref(2, 1);
    } else {
	tmp[1] = tl_ref(2, 1);
	tmp[2] = tl_ref(1, 2);
    }
    btmp[0] = b_ref(1, 1);
    btmp[1] = b_ref(2, 1);
L40:

/*     Solve 2 by 2 system using complete pivoting.   
       Set pivots less than SMIN to SMIN. */

    ipiv = idamax_(&c__4, tmp, &c__1);
    u11 = tmp[ipiv - 1];
    if (abs(u11) <= smin) {
	*info = 1;
	u11 = smin;
    }
    u12 = tmp[locu12[ipiv - 1] - 1];
    l21 = tmp[locl21[ipiv - 1] - 1] / u11;
    u22 = tmp[locu22[ipiv - 1] - 1] - u12 * l21;
    xswap = xswpiv[ipiv - 1];
    bswap = bswpiv[ipiv - 1];
    if (abs(u22) <= smin) {
	*info = 1;
	u22 = smin;
    }
    if (bswap) {
	temp = btmp[1];
	btmp[1] = btmp[0] - l21 * temp;
	btmp[0] = temp;
    } else {
	btmp[1] -= l21 * btmp[0];
    }
    *scale = 1.;
    if (smlnum * 2. * abs(btmp[1]) > abs(u22) || smlnum * 2. * abs(btmp[0]) > 
	    abs(u11)) {
/* Computing MAX */
	d__1 = abs(btmp[0]), d__2 = abs(btmp[1]);
	*scale = .5 / max(d__1,d__2);
	btmp[0] *= *scale;
	btmp[1] *= *scale;
    }
    x2[1] = btmp[1] / u22;
    x2[0] = btmp[0] / u11 - u12 / u11 * x2[1];
    if (xswap) {
	temp = x2[1];
	x2[1] = x2[0];
	x2[0] = temp;
    }
    x_ref(1, 1) = x2[0];
    if (*n1 == 1) {
	x_ref(1, 2) = x2[1];
	*xnorm = (d__1 = x_ref(1, 1), abs(d__1)) + (d__2 = x_ref(1, 2), abs(
		d__2));
    } else {
	x_ref(2, 1) = x2[1];
/* Computing MAX */
	d__3 = (d__1 = x_ref(1, 1), abs(d__1)), d__4 = (d__2 = x_ref(2, 1), 
		abs(d__2));
	*xnorm = max(d__3,d__4);
    }
    return 0;

/*     2 by 2:   
       op[TL11 TL12]*[X11 X12] +ISGN* [X11 X12]*op[TR11 TR12] = [B11 B12]   
         [TL21 TL22] [X21 X22]        [X21 X22]   [TR21 TR22]   [B21 B22]   

       Solve equivalent 4 by 4 system using complete pivoting.   
       Set pivots less than SMIN to SMIN. */

L50:
/* Computing MAX */
    d__5 = (d__1 = tr_ref(1, 1), abs(d__1)), d__6 = (d__2 = tr_ref(1, 2), abs(
	    d__2)), d__5 = max(d__5,d__6), d__6 = (d__3 = tr_ref(2, 1), abs(
	    d__3)), d__5 = max(d__5,d__6), d__6 = (d__4 = tr_ref(2, 2), abs(
	    d__4));
    smin = max(d__5,d__6);
/* Computing MAX */
    d__5 = smin, d__6 = (d__1 = tl_ref(1, 1), abs(d__1)), d__5 = max(d__5,
	    d__6), d__6 = (d__2 = tl_ref(1, 2), abs(d__2)), d__5 = max(d__5,
	    d__6), d__6 = (d__3 = tl_ref(2, 1), abs(d__3)), d__5 = max(d__5,
	    d__6), d__6 = (d__4 = tl_ref(2, 2), abs(d__4));
    smin = max(d__5,d__6);
/* Computing MAX */
    d__1 = eps * smin;
    smin = max(d__1,smlnum);
    btmp[0] = 0.;
    dcopy_(&c__16, btmp, &c__0, t16, &c__1);
    t16_ref(1, 1) = tl_ref(1, 1) + sgn * tr_ref(1, 1);
    t16_ref(2, 2) = tl_ref(2, 2) + sgn * tr_ref(1, 1);
    t16_ref(3, 3) = tl_ref(1, 1) + sgn * tr_ref(2, 2);
    t16_ref(4, 4) = tl_ref(2, 2) + sgn * tr_ref(2, 2);
    if (*ltranl) {
	t16_ref(1, 2) = tl_ref(2, 1);
	t16_ref(2, 1) = tl_ref(1, 2);
	t16_ref(3, 4) = tl_ref(2, 1);
	t16_ref(4, 3) = tl_ref(1, 2);
    } else {
	t16_ref(1, 2) = tl_ref(1, 2);
	t16_ref(2, 1) = tl_ref(2, 1);
	t16_ref(3, 4) = tl_ref(1, 2);
	t16_ref(4, 3) = tl_ref(2, 1);
    }
    if (*ltranr) {
	t16_ref(1, 3) = sgn * tr_ref(1, 2);
	t16_ref(2, 4) = sgn * tr_ref(1, 2);
	t16_ref(3, 1) = sgn * tr_ref(2, 1);
	t16_ref(4, 2) = sgn * tr_ref(2, 1);
    } else {
	t16_ref(1, 3) = sgn * tr_ref(2, 1);
	t16_ref(2, 4) = sgn * tr_ref(2, 1);
	t16_ref(3, 1) = sgn * tr_ref(1, 2);
	t16_ref(4, 2) = sgn * tr_ref(1, 2);
    }
    btmp[0] = b_ref(1, 1);
    btmp[1] = b_ref(2, 1);
    btmp[2] = b_ref(1, 2);
    btmp[3] = b_ref(2, 2);

/*     Perform elimination */

    for (i__ = 1; i__ <= 3; ++i__) {
	xmax = 0.;
	for (ip = i__; ip <= 4; ++ip) {
	    for (jp = i__; jp <= 4; ++jp) {
		if ((d__1 = t16_ref(ip, jp), abs(d__1)) >= xmax) {
		    xmax = (d__1 = t16_ref(ip, jp), abs(d__1));
		    ipsv = ip;
		    jpsv = jp;
		}
/* L60: */
	    }
/* L70: */
	}
	if (ipsv != i__) {
	    dswap_(&c__4, &t16_ref(ipsv, 1), &c__4, &t16_ref(i__, 1), &c__4);
	    temp = btmp[i__ - 1];
	    btmp[i__ - 1] = btmp[ipsv - 1];
	    btmp[ipsv - 1] = temp;
	}
	if (jpsv != i__) {
	    dswap_(&c__4, &t16_ref(1, jpsv), &c__1, &t16_ref(1, i__), &c__1);
	}
	jpiv[i__ - 1] = jpsv;
	if ((d__1 = t16_ref(i__, i__), abs(d__1)) < smin) {
	    *info = 1;
	    t16_ref(i__, i__) = smin;
	}
	for (j = i__ + 1; j <= 4; ++j) {
	    t16_ref(j, i__) = t16_ref(j, i__) / t16_ref(i__, i__);
	    btmp[j - 1] -= t16_ref(j, i__) * btmp[i__ - 1];
	    for (k = i__ + 1; k <= 4; ++k) {
		t16_ref(j, k) = t16_ref(j, k) - t16_ref(j, i__) * t16_ref(i__,
			 k);
/* L80: */
	    }
/* L90: */
	}
/* L100: */
    }
    if ((d__1 = t16_ref(4, 4), abs(d__1)) < smin) {
	t16_ref(4, 4) = smin;
    }
    *scale = 1.;
    if (smlnum * 8. * abs(btmp[0]) > (d__1 = t16_ref(1, 1), abs(d__1)) || 
	    smlnum * 8. * abs(btmp[1]) > (d__2 = t16_ref(2, 2), abs(d__2)) || 
	    smlnum * 8. * abs(btmp[2]) > (d__3 = t16_ref(3, 3), abs(d__3)) || 
	    smlnum * 8. * abs(btmp[3]) > (d__4 = t16_ref(4, 4), abs(d__4))) {
/* Computing MAX */
	d__1 = abs(btmp[0]), d__2 = abs(btmp[1]), d__1 = max(d__1,d__2), d__2 
		= abs(btmp[2]), d__1 = max(d__1,d__2), d__2 = abs(btmp[3]);
	*scale = .125 / max(d__1,d__2);
	btmp[0] *= *scale;
	btmp[1] *= *scale;
	btmp[2] *= *scale;
	btmp[3] *= *scale;
    }
    for (i__ = 1; i__ <= 4; ++i__) {
	k = 5 - i__;
	temp = 1. / t16_ref(k, k);
	tmp[k - 1] = btmp[k - 1] * temp;
	for (j = k + 1; j <= 4; ++j) {
	    tmp[k - 1] -= temp * t16_ref(k, j) * tmp[j - 1];
/* L110: */
	}
/* L120: */
    }
    for (i__ = 1; i__ <= 3; ++i__) {
	if (jpiv[4 - i__ - 1] != 4 - i__) {
	    temp = tmp[4 - i__ - 1];
	    tmp[4 - i__ - 1] = tmp[jpiv[4 - i__ - 1] - 1];
	    tmp[jpiv[4 - i__ - 1] - 1] = temp;
	}
/* L130: */
    }
    x_ref(1, 1) = tmp[0];
    x_ref(2, 1) = tmp[1];
    x_ref(1, 2) = tmp[2];
    x_ref(2, 2) = tmp[3];
/* Computing MAX */
    d__1 = abs(tmp[0]) + abs(tmp[2]), d__2 = abs(tmp[1]) + abs(tmp[3]);
    *xnorm = max(d__1,d__2);
    return 0;

/*     End of DLASY2 */

} /* dlasy2_ */

#undef tr_ref
#undef tl_ref
#undef t16_ref
#undef x_ref
#undef b_ref


