#include "blaswrap.h"
#include "f2c.h"

/* Subroutine */ int dlartv_(integer *n, doublereal *x, integer *incx, 
	doublereal *y, integer *incy, doublereal *c__, doublereal *s, integer 
	*incc)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       February 29, 1992   


    Purpose   
    =======   

    DLARTV applies a vector of real plane rotations to elements of the   
    real vectors x and y. For i = 1,2,...,n   

       ( x(i) ) := (  c(i)  s(i) ) ( x(i) )   
       ( y(i) )    ( -s(i)  c(i) ) ( y(i) )   

    Arguments   
    =========   

    N       (input) INTEGER   
            The number of plane rotations to be applied.   

    X       (input/output) DOUBLE PRECISION array,   
                           dimension (1+(N-1)*INCX)   
            The vector x.   

    INCX    (input) INTEGER   
            The increment between elements of X. INCX > 0.   

    Y       (input/output) DOUBLE PRECISION array,   
                           dimension (1+(N-1)*INCY)   
            The vector y.   

    INCY    (input) INTEGER   
            The increment between elements of Y. INCY > 0.   

    C       (input) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)   
            The cosines of the plane rotations.   

    S       (input) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)   
            The sines of the plane rotations.   

    INCC    (input) INTEGER   
            The increment between elements of C and S. INCC > 0.   

    =====================================================================   


       Parameter adjustments */
    /* System generated locals */
    integer i__1;
    /* Local variables */
    static integer i__, ic, ix, iy;
    static doublereal xi, yi;

    --s;
    --c__;
    --y;
    --x;

    /* Function Body */
    ix = 1;
    iy = 1;
    ic = 1;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	xi = x[ix];
	yi = y[iy];
	x[ix] = c__[ic] * xi + s[ic] * yi;
	y[iy] = c__[ic] * yi - s[ic] * xi;
	ix += *incx;
	iy += *incy;
	ic += *incc;
/* L10: */
    }
    return 0;

/*     End of DLARTV */

} /* dlartv_ */

