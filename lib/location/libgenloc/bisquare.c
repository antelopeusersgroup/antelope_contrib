/* Usage:
float bisquare (float x)

Returns weight for bisquare weighting function.  Sample x is assumed to 
be normalized by a robust measure of the scale of the sample distribution
spread so that residuals of order 1 are the norm, and outliers are things
with residuals significantly different from 1.  Like all residual weight
formulas, this scale factor is critical.  Here it is presumed to have been
set externally.  

Author:  Gary L. Pavlis, Indiana University
Written:  Feb. 1996 as translation of old FORTRAN function
*/
#include <math.h>
#define CX 4.685
float bisquare (float x)
{
	float w,xs;
	if(fabs((double)x) >= CX ) return(0.0);
	xs = x/CX;
	w = 1.0 - xs*xs;
	return(w*w);
}	

/* $Id$ */
