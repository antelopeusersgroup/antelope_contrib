#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int zlar1v_(integer *n, integer *b1, integer *bn, doublereal 
	*sigma, doublereal *d__, doublereal *l, doublereal *ld, doublereal *
	lld, doublereal *gersch, doublecomplex *z__, doublereal *ztz, 
	doublereal *mingma, integer *r__, integer *isuppz, doublereal *work)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    ZLAR1V computes the (scaled) r-th column of the inverse of   
    the sumbmatrix in rows B1 through BN of the tridiagonal matrix   
    L D L^T - sigma I. The following steps accomplish this computation :   
    (a) Stationary qd transform,  L D L^T - sigma I = L(+) D(+) L(+)^T,   
    (b) Progressive qd transform, L D L^T - sigma I = U(-) D(-) U(-)^T,   
    (c) Computation of the diagonal elements of the inverse of   
        L D L^T - sigma I by combining the above transforms, and choosing   
        r as the index where the diagonal of the inverse is (one of the)   
        largest in magnitude.   
    (d) Computation of the (scaled) r-th column of the inverse using the   
        twisted factorization obtained by combining the top part of the   
        the stationary and the bottom part of the progressive transform.   

    Arguments   
    =========   

    N        (input) INTEGER   
             The order of the matrix L D L^T.   

    B1       (input) INTEGER   
             First index of the submatrix of L D L^T.   

    BN       (input) INTEGER   
             Last index of the submatrix of L D L^T.   

    SIGMA    (input) DOUBLE PRECISION   
             The shift. Initially, when R = 0, SIGMA should be a good   
             approximation to an eigenvalue of L D L^T.   

    L        (input) DOUBLE PRECISION array, dimension (N-1)   
             The (n-1) subdiagonal elements of the unit bidiagonal matrix   
             L, in elements 1 to N-1.   

    D        (input) DOUBLE PRECISION array, dimension (N)   
             The n diagonal elements of the diagonal matrix D.   

    LD       (input) DOUBLE PRECISION array, dimension (N-1)   
             The n-1 elements L(i)*D(i).   

    LLD      (input) DOUBLE PRECISION array, dimension (N-1)   
             The n-1 elements L(i)*L(i)*D(i).   

    GERSCH   (input) DOUBLE PRECISION array, dimension (2*N)   
             The n Gerschgorin intervals. These are used to restrict   
             the initial search for R, when R is input as 0.   

    Z        (output) COMPLEX*16 array, dimension (N)   
             The (scaled) r-th column of the inverse. Z(R) is returned   
             to be 1.   

    ZTZ      (output) DOUBLE PRECISION   
             The square of the norm of Z.   

    MINGMA   (output) DOUBLE PRECISION   
             The reciprocal of the largest (in magnitude) diagonal   
             element of the inverse of L D L^T - sigma I.   

    R        (input/output) INTEGER   
             Initially, R should be input to be 0 and is then output as   
             the index where the diagonal element of the inverse is   
             largest in magnitude. In later iterations, this same value   
             of R should be input.   

    ISUPPZ   (output) INTEGER array, dimension (2)   
             The support of the vector in Z, i.e., the vector Z is   
             nonzero only in elements ISUPPZ(1) through ISUPPZ( 2 ).   

    WORK     (workspace) DOUBLE PRECISION array, dimension (4*N)   

    Further Details   
    ===============   

    Based on contributions by   
       Inderjit Dhillon, IBM Almaden, USA   
       Osni Marques, LBNL/NERSC, USA   
       Ken Stanley, Computer Science Division, University of   
         California at Berkeley, USA   

    =====================================================================   


       Parameter adjustments */
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublereal d__1;
    doublecomplex z__1, z__2;
    /* Builtin functions */
    double z_abs(doublecomplex *);
    /* Local variables */
    static integer indp, inds, from, i__, j;
    static doublereal s, dplus;
    static integer r1, r2;
    extern doublereal dlamch_(char *);
    static integer to;
    static logical sawnan;
    static integer indumn;
    static doublereal dminus, eps, tmp;

    --work;
    --isuppz;
    --z__;
    --gersch;
    --lld;
    --ld;
    --l;
    --d__;

    /* Function Body */
    eps = dlamch_("Precision");
    if (*r__ == 0) {

/*        Eliminate the top and bottom indices from the possible values   
          of R where the desired eigenvector is largest in magnitude. */

	r1 = *b1;
	i__1 = *bn;
	for (i__ = *b1; i__ <= i__1; ++i__) {
	    if (*sigma >= gersch[(i__ << 1) - 1] || *sigma <= gersch[i__ * 2])
		     {
		r1 = i__;
		goto L20;
	    }
/* L10: */
	}
L20:
	r2 = *bn;
	i__1 = *b1;
	for (i__ = *bn; i__ >= i__1; --i__) {
	    if (*sigma >= gersch[(i__ << 1) - 1] || *sigma <= gersch[i__ * 2])
		     {
		r2 = i__;
		goto L40;
	    }
/* L30: */
	}
L40:
	;
    } else {
	r1 = *r__;
	r2 = *r__;
    }

    indumn = *n;
    inds = (*n << 1) + 1;
    indp = *n * 3 + 1;
    sawnan = FALSE_;

/*     Compute the stationary transform (using the differential form)   
       untill the index R2 */

    if (*b1 == 1) {
	work[inds] = 0.;
    } else {
	work[inds] = lld[*b1 - 1];
    }
    s = work[inds] - *sigma;
    i__1 = r2 - 1;
    for (i__ = *b1; i__ <= i__1; ++i__) {
	dplus = d__[i__] + s;
	work[i__] = ld[i__] / dplus;
	work[inds + i__] = s * work[i__] * l[i__];
	s = work[inds + i__] - *sigma;
/* L50: */
    }

    if (! (s > 0. || s < 1.)) {

/*        Run a slower version of the above loop if a NaN is detected */

	sawnan = TRUE_;
	j = *b1 + 1;
L60:
	if (work[inds + j] > 0. || work[inds + j] < 1.) {
	    ++j;
	    goto L60;
	}
	work[inds + j] = lld[j];
	s = work[inds + j] - *sigma;
	i__1 = r2 - 1;
	for (i__ = j + 1; i__ <= i__1; ++i__) {
	    dplus = d__[i__] + s;
	    work[i__] = ld[i__] / dplus;
	    if (work[i__] == 0.) {
		work[inds + i__] = lld[i__];
	    } else {
		work[inds + i__] = s * work[i__] * l[i__];
	    }
	    s = work[inds + i__] - *sigma;
/* L70: */
	}
    }
    work[indp + *bn - 1] = d__[*bn] - *sigma;
    i__1 = r1;
    for (i__ = *bn - 1; i__ >= i__1; --i__) {
	dminus = lld[i__] + work[indp + i__];
	tmp = d__[i__] / dminus;
	work[indumn + i__] = l[i__] * tmp;
	work[indp + i__ - 1] = work[indp + i__] * tmp - *sigma;
/* L80: */
    }
    tmp = work[indp + r1 - 1];
    if (! (tmp > 0. || tmp < 1.)) {

/*        Run a slower version of the above loop if a NaN is detected */

	sawnan = TRUE_;
	j = *bn - 3;
L90:
	if (work[indp + j] > 0. || work[indp + j] < 1.) {
	    --j;
	    goto L90;
	}
	work[indp + j] = d__[j + 1] - *sigma;
	i__1 = r1;
	for (i__ = j; i__ >= i__1; --i__) {
	    dminus = lld[i__] + work[indp + i__];
	    tmp = d__[i__] / dminus;
	    work[indumn + i__] = l[i__] * tmp;
	    if (tmp == 0.) {
		work[indp + i__ - 1] = d__[i__] - *sigma;
	    } else {
		work[indp + i__ - 1] = work[indp + i__] * tmp - *sigma;
	    }
/* L100: */
	}
    }

/*     Find the index (from R1 to R2) of the largest (in magnitude)   
       diagonal element of the inverse */

    *mingma = work[inds + r1 - 1] + work[indp + r1 - 1];
    if (*mingma == 0.) {
	*mingma = eps * work[inds + r1 - 1];
    }
    *r__ = r1;
    i__1 = r2 - 1;
    for (i__ = r1; i__ <= i__1; ++i__) {
	tmp = work[inds + i__] + work[indp + i__];
	if (tmp == 0.) {
	    tmp = eps * work[inds + i__];
	}
	if (abs(tmp) < abs(*mingma)) {
	    *mingma = tmp;
	    *r__ = i__ + 1;
	}
/* L110: */
    }

/*     Compute the (scaled) r-th column of the inverse */

    isuppz[1] = *b1;
    isuppz[2] = *bn;
    i__1 = *r__;
    z__[i__1].r = 1., z__[i__1].i = 0.;
    *ztz = 1.;
    if (! sawnan) {
	from = *r__ - 1;
/* Computing MAX */
	i__1 = *r__ - 32;
	to = max(i__1,*b1);
L120:
	if (from >= *b1) {
	    i__1 = to;
	    for (i__ = from; i__ >= i__1; --i__) {
		i__2 = i__;
		i__3 = i__;
		i__4 = i__ + 1;
		z__2.r = work[i__3] * z__[i__4].r, z__2.i = work[i__3] * z__[
			i__4].i;
		z__1.r = -z__2.r, z__1.i = -z__2.i;
		z__[i__2].r = z__1.r, z__[i__2].i = z__1.i;
		i__2 = i__;
		i__3 = i__;
		z__1.r = z__[i__2].r * z__[i__3].r - z__[i__2].i * z__[i__3]
			.i, z__1.i = z__[i__2].r * z__[i__3].i + z__[i__2].i *
			 z__[i__3].r;
		*ztz += z__1.r;
/* L130: */
	    }
	    if (z_abs(&z__[to]) <= eps && z_abs(&z__[to + 1]) <= eps) {
		isuppz[1] = to + 2;
	    } else {
		from = to - 1;
/* Computing MAX */
		i__1 = to - 32;
		to = max(i__1,*b1);
		goto L120;
	    }
	}
	from = *r__ + 1;
/* Computing MIN */
	i__1 = *r__ + 32;
	to = min(i__1,*bn);
L140:
	if (from <= *bn) {
	    i__1 = to;
	    for (i__ = from; i__ <= i__1; ++i__) {
		i__2 = i__;
		i__3 = indumn + i__ - 1;
		i__4 = i__ - 1;
		z__2.r = work[i__3] * z__[i__4].r, z__2.i = work[i__3] * z__[
			i__4].i;
		z__1.r = -z__2.r, z__1.i = -z__2.i;
		z__[i__2].r = z__1.r, z__[i__2].i = z__1.i;
		i__2 = i__;
		i__3 = i__;
		z__1.r = z__[i__2].r * z__[i__3].r - z__[i__2].i * z__[i__3]
			.i, z__1.i = z__[i__2].r * z__[i__3].i + z__[i__2].i *
			 z__[i__3].r;
		*ztz += z__1.r;
/* L150: */
	    }
	    if (z_abs(&z__[to]) <= eps && z_abs(&z__[to - 1]) <= eps) {
		isuppz[2] = to - 2;
	    } else {
		from = to + 1;
/* Computing MIN */
		i__1 = to + 32;
		to = min(i__1,*bn);
		goto L140;
	    }
	}
    } else {
	i__1 = *b1;
	for (i__ = *r__ - 1; i__ >= i__1; --i__) {
	    i__2 = i__ + 1;
	    if (z__[i__2].r == 0. && z__[i__2].i == 0.) {
		i__2 = i__;
		d__1 = -(ld[i__ + 1] / ld[i__]);
		i__3 = i__ + 2;
		z__1.r = d__1 * z__[i__3].r, z__1.i = d__1 * z__[i__3].i;
		z__[i__2].r = z__1.r, z__[i__2].i = z__1.i;
	    } else if (z_abs(&z__[i__ + 1]) <= eps && z_abs(&z__[i__ + 2]) <= 
		    eps) {
		isuppz[1] = i__ + 3;
		goto L170;
	    } else {
		i__2 = i__;
		i__3 = i__;
		i__4 = i__ + 1;
		z__2.r = work[i__3] * z__[i__4].r, z__2.i = work[i__3] * z__[
			i__4].i;
		z__1.r = -z__2.r, z__1.i = -z__2.i;
		z__[i__2].r = z__1.r, z__[i__2].i = z__1.i;
	    }
	    i__2 = i__;
	    i__3 = i__;
	    z__1.r = z__[i__2].r * z__[i__3].r - z__[i__2].i * z__[i__3].i, 
		    z__1.i = z__[i__2].r * z__[i__3].i + z__[i__2].i * z__[
		    i__3].r;
	    *ztz += z__1.r;
/* L160: */
	}
L170:
	i__1 = *bn - 1;
	for (i__ = *r__; i__ <= i__1; ++i__) {
	    i__2 = i__;
	    if (z__[i__2].r == 0. && z__[i__2].i == 0.) {
		i__2 = i__ + 1;
		d__1 = -(ld[i__ - 1] / ld[i__]);
		i__3 = i__ - 1;
		z__1.r = d__1 * z__[i__3].r, z__1.i = d__1 * z__[i__3].i;
		z__[i__2].r = z__1.r, z__[i__2].i = z__1.i;
	    } else if (z_abs(&z__[i__]) <= eps && z_abs(&z__[i__ - 1]) <= eps)
		     {
		isuppz[2] = i__ - 2;
		goto L190;
	    } else {
		i__2 = i__ + 1;
		i__3 = indumn + i__;
		i__4 = i__;
		z__2.r = work[i__3] * z__[i__4].r, z__2.i = work[i__3] * z__[
			i__4].i;
		z__1.r = -z__2.r, z__1.i = -z__2.i;
		z__[i__2].r = z__1.r, z__[i__2].i = z__1.i;
	    }
	    i__2 = i__ + 1;
	    i__3 = i__ + 1;
	    z__1.r = z__[i__2].r * z__[i__3].r - z__[i__2].i * z__[i__3].i, 
		    z__1.i = z__[i__2].r * z__[i__3].i + z__[i__2].i * z__[
		    i__3].r;
	    *ztz += z__1.r;
/* L180: */
	}
L190:
	;
    }
    i__1 = isuppz[1] - 3;
    for (i__ = *b1; i__ <= i__1; ++i__) {
	i__2 = i__;
	z__[i__2].r = 0., z__[i__2].i = 0.;
/* L200: */
    }
    i__1 = *bn;
    for (i__ = isuppz[2] + 3; i__ <= i__1; ++i__) {
	i__2 = i__;
	z__[i__2].r = 0., z__[i__2].i = 0.;
/* L210: */
    }

    return 0;

/*     End of ZLAR1V */

} /* zlar1v_ */

