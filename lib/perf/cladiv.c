#include "blaswrap.h"
#include "f2c.h"

/* Complex */ VOID cladiv_(complex * ret_val, complex *x, complex *y)
{
/*  -- LAPACK auxiliary routine (version 3.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       October 31, 1992   


    Purpose   
    =======   

    CLADIV := X / Y, where X and Y are complex.  The computation of X / Y   
    will not overflow on an intermediary step unless the results   
    overflows.   

    Arguments   
    =========   

    X       (input) COMPLEX   
    Y       (input) COMPLEX   
            The complex scalars X and Y.   

    ===================================================================== */
    /* System generated locals */
    real r__1, r__2, r__3, r__4;
    complex q__1;
    /* Builtin functions */
    double r_imag(complex *);
    /* Local variables */
    static real zi, zr;
    extern /* Subroutine */ int sladiv_(real *, real *, real *, real *, real *
	    , real *);



    r__1 = x->r;
    r__2 = r_imag(x);
    r__3 = y->r;
    r__4 = r_imag(y);
    sladiv_(&r__1, &r__2, &r__3, &r__4, &zr, &zi);
    q__1.r = zr, q__1.i = zi;
     ret_val->r = q__1.r,  ret_val->i = q__1.i;

    return ;

/*     End of CLADIV */

} /* cladiv_ */

