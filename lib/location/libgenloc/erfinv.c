/* Function to calculate inverse error function.  Rational approximation 
is used to generate an initial approximation, which is then improved to 
full accuracy by two steps of Newton's method.  Code is a direct 
translation of the erfinv m file in matlab version 2.0. 

Author:  Gary L. Pavlis, Indiana University
Date:  February 1996
*/
#include <stdlib.h>
#include <math.h>
#include <values.h>

#define CENTRAL_RANGE 0.7
double erfinv( double y)
{
        double x,z,num,dem; /*working variables */
        /* coefficients in rational expansion */
        double a[4]={ 0.886226899, -1.645349621,  0.914624893, -0.140543331};
        double b[4]={-2.118377725,  1.442710462, -0.329097515,  0.012229801};
        double c[4]={-1.970840454, -1.624906493,  3.429567803,  1.641345311};
        double d[2]={ 3.543889200,  1.637067800};
        if(fabs(y) > 1.0) return (atof("NaN"));  /* This needs IEEE constant*/
        if(fabs(y) == 1.0) return((copysign(1.0,y))*MAXDOUBLE); 
        if( fabs(y) <= CENTRAL_RANGE ) 
        {
                z = y*y;
                num = (((a[3]*z + a[2])*z + a[1])*z + a[0]);
                dem = ((((b[3]*z + b[2])*z + b[1])*z +b[0])*z + 1.0);
                x = y*num/dem;
        }
        else if( (fabs(y) > CENTRAL_RANGE) && (fabs(y) < 1.0) )
        {
                z = sqrt(-log((1.0-fabs(y))/2.0));
                num = ((c[3]*z + c[2])*z + c[1])*z + c[0];
                dem = (d[1]*z + d[0])*z + 1.0;
                x = (copysign(1.0,y))*num/dem;
        }
        /* Two steps of Newton-Raphson correction */
        x = x - (erf(x) - y)/( (2.0/sqrt(M_PI))*exp(-x*x));
        x = x - (erf(x) - y)/( (2.0/sqrt(M_PI))*exp(-x*x));

        return(x);
}
/* Function to compute ith quantile of an ensemble of N normal
deviates.  Uses inverse erf function erfinv.  Returns the
ith quantile of N where we start counting from 1.     
Author:  Gary L. Pavlis, Indiana University
Written:  February 1996
*/
double normal_quantile(int i, int n)
{
        double fraction;

        fraction = ((double)i - 0.5)/((double)(n+1));
        return( sqrt(2)*erfinv(2.0*(fraction-0.5)) );
}

/* $Id$ */
