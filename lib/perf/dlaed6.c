#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int dlaed6_(integer *kniter, logical *orgati, doublereal *
	rho, doublereal *d__, doublereal *z__, doublereal *finit, doublereal *
	tau, integer *info)
{
/*  -- LAPACK routine (version 3.0) --   
       Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,   
       Courant Institute, NAG Ltd., and Rice University   
       June 30, 1999   


    Purpose   
    =======   

    DLAED6 computes the positive or negative root (closest to the origin)   
    of   
                     z(1)        z(2)        z(3)   
    f(x) =   rho + --------- + ---------- + ---------   
                    d(1)-x      d(2)-x      d(3)-x   

    It is assumed that   

          if ORGATI = .true. the root is between d(2) and d(3);   
          otherwise it is between d(1) and d(2)   

    This routine will be called by DLAED4 when necessary. In most cases,   
    the root sought is the smallest in magnitude, though it might not be   
    in some extremely rare situations.   

    Arguments   
    =========   

    KNITER       (input) INTEGER   
                 Refer to DLAED4 for its significance.   

    ORGATI       (input) LOGICAL   
                 If ORGATI is true, the needed root is between d(2) and   
                 d(3); otherwise it is between d(1) and d(2).  See   
                 DLAED4 for further details.   

    RHO          (input) DOUBLE PRECISION   
                 Refer to the equation f(x) above.   

    D            (input) DOUBLE PRECISION array, dimension (3)   
                 D satisfies d(1) < d(2) < d(3).   

    Z            (input) DOUBLE PRECISION array, dimension (3)   
                 Each of the elements in z must be positive.   

    FINIT        (input) DOUBLE PRECISION   
                 The value of f at 0. It is more accurate than the one   
                 evaluated inside this routine (if someone wants to do   
                 so).   

    TAU          (output) DOUBLE PRECISION   
                 The root of the equation f(x).   

    INFO         (output) INTEGER   
                 = 0: successful exit   
                 > 0: if INFO = 1, failure to converge   

    Further Details   
    ===============   

    Based on contributions by   
       Ren-Cang Li, Computer Science Division, University of California   
       at Berkeley, USA   

    =====================================================================   

       Parameter adjustments */
    /* Initialized data */
    static logical first = TRUE_;
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2, d__3, d__4;
    /* Builtin functions */
    double sqrt(doublereal), log(doublereal), pow_di(doublereal *, integer *);
    /* Local variables */
    static doublereal base;
    static integer iter;
    static doublereal temp, temp1, temp2, temp3, temp4, a, b, c__, f;
    static integer i__;
    static logical scale;
    static integer niter;
    static doublereal small1, small2, fc, df, sminv1, sminv2;
    extern doublereal dlamch_(char *);
    static doublereal dscale[3], sclfac, zscale[3], erretm, sclinv, ddf, eta, 
	    eps;

    --z__;
    --d__;

    /* Function Body */

    *info = 0;

    niter = 1;
    *tau = 0.;
    if (*kniter == 2) {
	if (*orgati) {
	    temp = (d__[3] - d__[2]) / 2.;
	    c__ = *rho + z__[1] / (d__[1] - d__[2] - temp);
	    a = c__ * (d__[2] + d__[3]) + z__[2] + z__[3];
	    b = c__ * d__[2] * d__[3] + z__[2] * d__[3] + z__[3] * d__[2];
	} else {
	    temp = (d__[1] - d__[2]) / 2.;
	    c__ = *rho + z__[3] / (d__[3] - d__[2] - temp);
	    a = c__ * (d__[1] + d__[2]) + z__[1] + z__[2];
	    b = c__ * d__[1] * d__[2] + z__[1] * d__[2] + z__[2] * d__[1];
	}
/* Computing MAX */
	d__1 = abs(a), d__2 = abs(b), d__1 = max(d__1,d__2), d__2 = abs(c__);
	temp = max(d__1,d__2);
	a /= temp;
	b /= temp;
	c__ /= temp;
	if (c__ == 0.) {
	    *tau = b / a;
	} else if (a <= 0.) {
	    *tau = (a - sqrt((d__1 = a * a - b * 4. * c__, abs(d__1)))) / (
		    c__ * 2.);
	} else {
	    *tau = b * 2. / (a + sqrt((d__1 = a * a - b * 4. * c__, abs(d__1))
		    ));
	}
	temp = *rho + z__[1] / (d__[1] - *tau) + z__[2] / (d__[2] - *tau) + 
		z__[3] / (d__[3] - *tau);
	if (abs(*finit) <= abs(temp)) {
	    *tau = 0.;
	}
    }

/*     On first call to routine, get machine parameters for   
       possible scaling to avoid overflow */

    if (first) {
	eps = dlamch_("Epsilon");
	base = dlamch_("Base");
	i__1 = (integer) (log(dlamch_("SafMin")) / log(base) / 3.);
	small1 = pow_di(&base, &i__1);
	sminv1 = 1. / small1;
	small2 = small1 * small1;
	sminv2 = sminv1 * sminv1;
	first = FALSE_;
    }

/*     Determine if scaling of inputs necessary to avoid overflow   
       when computing 1/TEMP**3 */

    if (*orgati) {
/* Computing MIN */
	d__3 = (d__1 = d__[2] - *tau, abs(d__1)), d__4 = (d__2 = d__[3] - *
		tau, abs(d__2));
	temp = min(d__3,d__4);
    } else {
/* Computing MIN */
	d__3 = (d__1 = d__[1] - *tau, abs(d__1)), d__4 = (d__2 = d__[2] - *
		tau, abs(d__2));
	temp = min(d__3,d__4);
    }
    scale = FALSE_;
    if (temp <= small1) {
	scale = TRUE_;
	if (temp <= small2) {

/*        Scale up by power of radix nearest 1/SAFMIN**(2/3) */

	    sclfac = sminv2;
	    sclinv = small2;
	} else {

/*        Scale up by power of radix nearest 1/SAFMIN**(1/3) */

	    sclfac = sminv1;
	    sclinv = small1;
	}

/*        Scaling up safe because D, Z, TAU scaled elsewhere to be O(1) */

	for (i__ = 1; i__ <= 3; ++i__) {
	    dscale[i__ - 1] = d__[i__] * sclfac;
	    zscale[i__ - 1] = z__[i__] * sclfac;
/* L10: */
	}
	*tau *= sclfac;
    } else {

/*        Copy D and Z to DSCALE and ZSCALE */

	for (i__ = 1; i__ <= 3; ++i__) {
	    dscale[i__ - 1] = d__[i__];
	    zscale[i__ - 1] = z__[i__];
/* L20: */
	}
    }

    fc = 0.;
    df = 0.;
    ddf = 0.;
    for (i__ = 1; i__ <= 3; ++i__) {
	temp = 1. / (dscale[i__ - 1] - *tau);
	temp1 = zscale[i__ - 1] * temp;
	temp2 = temp1 * temp;
	temp3 = temp2 * temp;
	fc += temp1 / dscale[i__ - 1];
	df += temp2;
	ddf += temp3;
/* L30: */
    }
    f = *finit + *tau * fc;

    if (abs(f) <= 0.) {
	goto L60;
    }

/*        Iteration begins   

       It is not hard to see that   

             1) Iterations will go up monotonically   
                if FINIT < 0;   

             2) Iterations will go down monotonically   
                if FINIT > 0. */

    iter = niter + 1;

    for (niter = iter; niter <= 20; ++niter) {

	if (*orgati) {
	    temp1 = dscale[1] - *tau;
	    temp2 = dscale[2] - *tau;
	} else {
	    temp1 = dscale[0] - *tau;
	    temp2 = dscale[1] - *tau;
	}
	a = (temp1 + temp2) * f - temp1 * temp2 * df;
	b = temp1 * temp2 * f;
	c__ = f - (temp1 + temp2) * df + temp1 * temp2 * ddf;
/* Computing MAX */
	d__1 = abs(a), d__2 = abs(b), d__1 = max(d__1,d__2), d__2 = abs(c__);
	temp = max(d__1,d__2);
	a /= temp;
	b /= temp;
	c__ /= temp;
	if (c__ == 0.) {
	    eta = b / a;
	} else if (a <= 0.) {
	    eta = (a - sqrt((d__1 = a * a - b * 4. * c__, abs(d__1)))) / (c__ 
		    * 2.);
	} else {
	    eta = b * 2. / (a + sqrt((d__1 = a * a - b * 4. * c__, abs(d__1)))
		    );
	}
	if (f * eta >= 0.) {
	    eta = -f / df;
	}

	temp = eta + *tau;
	if (*orgati) {
	    if (eta > 0. && temp >= dscale[2]) {
		eta = (dscale[2] - *tau) / 2.;
	    }
	    if (eta < 0. && temp <= dscale[1]) {
		eta = (dscale[1] - *tau) / 2.;
	    }
	} else {
	    if (eta > 0. && temp >= dscale[1]) {
		eta = (dscale[1] - *tau) / 2.;
	    }
	    if (eta < 0. && temp <= dscale[0]) {
		eta = (dscale[0] - *tau) / 2.;
	    }
	}
	*tau += eta;

	fc = 0.;
	erretm = 0.;
	df = 0.;
	ddf = 0.;
	for (i__ = 1; i__ <= 3; ++i__) {
	    temp = 1. / (dscale[i__ - 1] - *tau);
	    temp1 = zscale[i__ - 1] * temp;
	    temp2 = temp1 * temp;
	    temp3 = temp2 * temp;
	    temp4 = temp1 / dscale[i__ - 1];
	    fc += temp4;
	    erretm += abs(temp4);
	    df += temp2;
	    ddf += temp3;
/* L40: */
	}
	f = *finit + *tau * fc;
	erretm = (abs(*finit) + abs(*tau) * erretm) * 8. + abs(*tau) * df;
	if (abs(f) <= eps * erretm) {
	    goto L60;
	}
/* L50: */
    }
    *info = 1;
L60:

/*     Undo scaling */

    if (scale) {
	*tau *= sclinv;
    }
    return 0;

/*     End of DLAED6 */

} /* dlaed6_ */

