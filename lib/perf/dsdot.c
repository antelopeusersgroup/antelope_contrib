/* dsdot.f -- translated by f2c (version 19970219).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

doublereal dsdot_(n, sx, incx, sy, incy)
integer *n;
real *sx;
integer *incx;
real *sy;
integer *incy;
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal ret_val;

    /* Local variables */
    static integer i__, ns, kx, ky;

/* ***begin prologue  dsdot */
/* ***revision date  811015   (yymmdd) */
/* ***category no.  f1a */
/* ***keywords  blas,vector,double precision,dot product, */
/*             inner product */
/* ***date written  october 1979 */
/* ***author lawson c. (jpl),hanson r. (sla), */
/*                            kincaid d. (u texas), krogh f. (jpl) */
/* ***purpose */
/*   d.p inner product of s.p. vectors */
/* ***description */
/*                b l a s  subprogram */
/*    description of parameters */

/*     --input-- */
/*        n  number of elements in input vector(s) */
/*       sx  single precision vector with n elements */
/*     incx  storage spacing between elements of sx */
/*       sy  single precision vector with n elements */
/*     incy  storage spacing between elements of sy */

/*     --output-- */
/*    dsdot  double precision dot product (zero if n.le.0) */

/*     returns d.p. dot product accumulated in d.p., for s.p. sx and sy */
/*     dsdot = sum for i = 0 to n-1 of  sx(lx+i*incx) * sy(ly+i*incy), */
/*     where lx = 1 if incx .ge. 0, else lx = (-incx)*n, and ly is */
/*     defined in a similar way using incy. */


/* ***references */
/*  lawson c.l., hanson r.j., kincaid d.r., krogh f.t., */
/*   *basic linear algebra subprograms for fortran usage*, */
/*  algorithm no. 539, transactions on mathematical software, */
/*  volume 5, number 3, september 1979, 308-323 */
/* ***routines called  (none) */
/* ***end prologue  dsdot */

/* ***first executable statement  dsdot */
    /* Parameter adjustments */
    --sy;
    --sx;

    /* Function Body */
    ret_val = 0.;
    if (*n <= 0) {
	return ret_val;
    }
    if (*incx == *incy && *incx > 0) {
	goto L20;
    }
    kx = 1;
    ky = 1;
    if (*incx < 0) {
	kx = (1 - *n) * *incx + 1;
    }
    if (*incy < 0) {
	ky = (1 - *n) * *incy + 1;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ret_val += (doublereal) sx[kx] * (doublereal) sy[ky];
	kx += *incx;
	ky += *incy;
/* L10: */
    }
    return ret_val;
L20:
    ns = *n * *incx;
    i__1 = ns;
    i__2 = *incx;
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
	ret_val += (doublereal) sx[i__] * (doublereal) sy[i__];
/* L30: */
    }
    return ret_val;
} /* dsdot_ */

