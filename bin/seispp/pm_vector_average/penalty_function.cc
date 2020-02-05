#include <math.h>
/* Usage:

double huber(double x);

Returns Huber weight function for scaled residual x = r/d where
d is the scale factor (i.e. this routine assumes data are prescaled)

Author:  Gary Pavlis
Written:  June 1992
Reference:  Chave and Thompson (1989) JGR, 94, p. 14217.
*/
#define HUBER_PARAMETER_A 1.5 /* Parameter "a" in Huber weight formula */
double huber(double x)
{
	if(fabs(x)<=HUBER_PARAMETER_A) return(1.0);
	else
		return((HUBER_PARAMETER_A/fabs(x)));
}


/* $Id$ */
/* Usage:
double bisquare (double x)

Returns weight for bisquare weighting function.  Sample x is assumed to
be normalized by a robust measure of the scale of the sample distribution
spread so that residuals of order 1 are the norm, and outliers are things
with residuals significantly different from 1.  Like all residual weight
formulas, this scale factor is critical.  Here it is presumed to have been
set externally.

Author:  Gary L. Pavlis, Indiana University
Written:  Feb. 1996 as translation of old FORTRAN function
*/

#define CX 4.685
double bisquare (double x)
{
	double w,xs;
	if(fabs(x) >= CX ) return(0.0);
	xs = x/CX;
	w = 1.0 - xs*xs;
	return(w*w);
}

/* $Id$ */
/* Usage:

double thomson(double x,double beta);

Returns weight for Thomson's redescending weight function
given in equation (27) of Chave et al., 1987.
Assume x is a normalized sample value (i.e. for spectral values
x is the sample amplitude divided by the error scale factor
calculated as described by Chave et al..
This weight function is controlled by the factor "beta" passed
as argument 2.  Note:  Chave et al. recommend setting beta to
the Nth quantile of the a Rayleigh distribution.  This is assumed
calculated externally for efficiency reasons.

Author:  Gary Pavlis
Written:  June 1992
Reference:  Chave, Thomson, and Ander (1987). JGR, 92, 633-648.
*/
/* Disabled for use with this program.  Retained as possible library
routine.   Right now a float version is is libgenloc.   This is a
double version.  Could and should make these templates.
double thomson(double x,double beta)
{
	double temp;
	temp = fabs ( x);
	temp = exp( (  beta)*(temp -  (  beta)));
	return(  exp( - temp));
}
*/
