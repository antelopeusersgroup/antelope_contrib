/* Usage:

float thomson(float x,float beta);

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
#include <math.h>
float thomson(float x,float beta)
{
	double temp; /* used to make sure all calculations are done
			in double precision */
	temp = fabs ((double) x);
	temp = exp( ( (double) beta)*(temp -  ( (double) beta)));
	return( (float) exp( - temp));
	
}
	

/* $Id$ */
