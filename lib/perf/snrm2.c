#include "blaswrap.h"
#include "f2c.h"

doublereal snrm2_(integer *n, real *x, integer *incx)
{
/*        The following loop is equivalent to this call to the LAPACK   
          auxiliary routine:   
          CALL SLASSQ( N, X, INCX, SCALE, SSQ ) */
    /* System generated locals */
    integer i__1, i__2;
    real ret_val, r__1;
    /* Builtin functions */
    double sqrt(doublereal);
    /* Local variables */
    static real norm, scale, absxi;
    static integer ix;
    static real ssq;
/*  SNRM2 returns the euclidean norm of a vector via the function   
    name, so that   
       SNRM2 := sqrt( x'*x )   
    -- This version written on 25-October-1982.   
       Modified on 14-October-1993 to inline the call to SLASSQ.   
       Sven Hammarling, Nag Ltd.   
       Parameter adjustments */
    --x;
    /* Function Body */
    if (*n < 1 || *incx < 1) {
	norm = 0.f;
    } else if (*n == 1) {
	norm = dabs(x[1]);
    } else {
	scale = 0.f;
	ssq = 1.f;


	i__1 = (*n - 1) * *incx + 1;
	i__2 = *incx;
	for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
	    if (x[ix] != 0.f) {
		absxi = (r__1 = x[ix], dabs(r__1));
		if (scale < absxi) {
/* Computing 2nd power */
		    r__1 = scale / absxi;
		    ssq = ssq * (r__1 * r__1) + 1.f;
		    scale = absxi;
		} else {
/* Computing 2nd power */
		    r__1 = absxi / scale;
		    ssq += r__1 * r__1;
		}
	    }
/* L10: */
	}
	norm = scale * sqrt(ssq);
    }

    ret_val = norm;
    return ret_val;

/*     End of SNRM2. */

} /* snrm2_ */

