/* Usage:

float huber(float x);

Returns Huber weight function for scaled residual x = r/d where
d is the scale factor (i.e. this routine assumes data are prescaled)

Author:  Gary Pavlis
Written:  June 1992
Reference:  Chave and Thompson (1989) JGR, 94, p. 14217.
*/
#define HUBER_PARAMETER_A 1.5 /* Parameter "a" in Huber weight formula */
#include <math.h>
float huber(float x)
{
	if(fabs((double)x)<=HUBER_PARAMETER_A) return(1.0);
	else 
		return((float)(HUBER_PARAMETER_A/fabs((double)x)));
}
	

/* $Id$ */
