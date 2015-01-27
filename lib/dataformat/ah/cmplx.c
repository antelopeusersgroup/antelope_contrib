#include <stdlib.h>
#include "cmplx.h"

/*   single precision complex arthimatic routines.  Routines require
     the type structure "complex" to be initilized in main.
     Most routines return pointers to the answer.
*/

complex         dummy;
// complex	r_unity = { 1.0, 0.0};
// complex	i_unity = { 0.0, 1.0};


double 
mycabs (double real, double imag)
{
    double          temp,
                    sqrt ();

    if (real < 0)
	real = -real;
    if (imag < 0)
	imag = -imag;
    if (imag > real) {
	temp = real;
	real = imag;
	imag = temp;
    }
    if ((real + imag) == real)
	return (real);

    temp = imag / real;
    temp = real * sqrt (1.0 + temp * temp);	/* overflow!! */
    return (temp);
}

/*
 *	"@(#)c_abs.c	1.1"
 */
float 
c_abs (z)
complex         z;
{
    double          mycabs ();

    return (mycabs (z.real, z.imag));
}


/*
 *	"@(#)c_cos.c	1.1"
 */
complex        *
c_cos (z)
complex         z;
{
    double          sin (),
                    cos (),
                    sinh (),
                    cosh ();

    dummy.real = cos (z.real) * cosh (z.imag);
    dummy.imag = -sin (z.real) * sinh (z.imag);
    return (&dummy);
}


/*
 *	"@(#)c_div.c	1.1"
 */
complex        *
c_div (a, b)
complex         a,
                b;
{
    double          ratio,
                    den;
    double          abr,
                    abi;

    if ((abr = b.real) < 0.)
	abr = -abr;
    if ((abi = b.imag) < 0.)
	abi = -abi;
    if (abr <= abi) {
	if (abi == 0)
	    abort ();		       /* fatal("complex division by zero"); */
	ratio = b.real / b.imag;
	den = b.imag * (1 + ratio * ratio);
	dummy.real = (a.real * ratio + a.imag) / den;
	dummy.imag = (a.imag * ratio - a.real) / den;
    } else {
	ratio = b.imag / b.real;
	den = b.real * (1 + ratio * ratio);
	dummy.real = (a.real + a.imag * ratio) / den;
	dummy.imag = (a.imag - a.real * ratio) / den;
    }
    return (&dummy);
}

/*
 *	"@(#)c_mult.c	ours"
 */
complex        *
c_mult (a, b)
complex         a,
                b;
{
    dummy.real = a.real * b.real - a.imag * b.imag;
    dummy.imag = a.imag * b.real + a.real * b.imag;
    return (&dummy);
}

/*
 *	"@(#)c_ad.c	ours"
 */
complex        *
c_ad (a, b)
complex         a,
                b;
{
    dummy.real = a.real + b.real;
    dummy.imag = a.imag + b.imag;
    return (&dummy);
}

/*
 *	"@(#)c_sub.c	ours"
 */
complex        *
c_sub (a, b)
complex         a,
                b;
{
    dummy.real = a.real - b.real;
    dummy.imag = a.imag - b.imag;
    return (&dummy);
}


/*
 *	"@(#)c_exp.c	1.1"
 */
complex        *
c_exp (z)
complex         z;
{
    double          expx;
    double          exp (),
                    cos (),
                    sin ();

    expx = exp (z.real);
    dummy.real = expx * cos (z.imag);
    dummy.imag = expx * sin (z.imag);
    return (&dummy);
}


/*
 *	"@(#)c_log.c	1.1"
 */
complex        *
c_log (z)
complex         z;
{
    double          log (),
                    mycabs (),
                    atan2 ();

    dummy.imag = atan2 (z.imag, z.real);
    dummy.real = log (mycabs (z.real, z.imag));
    return (&dummy);
}


/*
 *	"@(#)c_sin.c	1.1"
 */
complex        *
c_sin (z)
complex         z;
{
    double          sin (),
                    cos (),
                    sinh (),
                    cosh ();

    dummy.real = sin (z.real) * cosh (z.imag);
    dummy.imag = cos (z.real) * sinh (z.imag);
    return (&dummy);
}


/*
 *	"@(#)c_sqrt.c	1.1"
 */
complex        *
c_sqrt (z)
complex         z;
{
    double          mag,
                    sqrt (),
                    mycabs ();

    if ((mag = mycabs (z.real, z.imag)) == 0.)
	dummy.real = dummy.imag = 0.;
    else if (z.real > 0) {
	dummy.real = sqrt (0.5 * (mag + z.real));
	dummy.imag = z.imag / dummy.real / 2;
    } else {
	dummy.imag = sqrt (0.5 * (mag - z.real));
	if (z.imag < 0)
	    dummy.imag = -dummy.imag;
	dummy.real = z.imag / dummy.imag / 2;
    }
    return (&dummy);
}


/* $Id$ */
