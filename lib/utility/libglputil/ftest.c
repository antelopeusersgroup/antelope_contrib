#include <math.h>
#include "elog.h"
#include "glputil.h"
double gammln(double xx)
{
        double x,y,tmp,ser;
        static double cof[6]={76.18009172947146,-86.50532032941677,
                24.01409824083091,-1.231739572450155,
                0.1208650973866179e-2,-0.5395239384953e-5};
        int j;

        y=x=xx;
        tmp=x+5.5;
        tmp -= (x+0.5)*log(tmp);
        ser=1.000000000190015;
        for (j=0;j<=5;j++) ser += cof[j]/++y;
        return -tmp+log(2.5066282746310005*ser/x);
}
#define MAXIT 100
#define EPS 3.0e-7
#define FPMIN 1.0e-30

double betacf(double a, double b, double x)
{
	int m,m2;
	double aa,c,d,del,h,qab,qam,qap;

	qab=a+b;
	qap=a+1.0;
	qam=a-1.0;
	c=1.0;
	d=1.0-qab*x/qap;
	if (fabs(d) < FPMIN) d=FPMIN;
	d=1.0/d;
	h=d;
	for (m=1;m<=MAXIT;m++) {
		m2=2*m;
		aa=m*(b-m)*x/((qam+m2)*(a+m2));
		d=1.0+aa*d;
		if (fabs(d) < FPMIN) d=FPMIN;
		c=1.0+aa/c;
		if (fabs(c) < FPMIN) c=FPMIN;
		d=1.0/d;
		h *= d*c;
		aa = -(a+m)*(qab+m)*x/((a+m2)*(qap+m2));
		d=1.0+aa*d;
		if (fabs(d) < FPMIN) d=FPMIN;
		c=1.0+aa/c;
		if (fabs(c) < FPMIN) c=FPMIN;
		d=1.0/d;
		del=d*c;
		h *= del;
		if (fabs(del-1.0) < EPS) break;
	}
	if (m > MAXIT) elog_complain(0,"a or b too big, or MAXIT too small in betacf\n");
	return h;
}
#undef MAXIT
#undef EPS
#undef FPMIN

/* Computes incomplete beta function.  Algorithm copied from
numerical recipes in C adapted for use with antelope's elog
facility.  The incomplete beta function is the integral:

1 over B(a,b) int 0 to x t sup {a-1} ( 1-t) sup (b-1) dt

thus a and b are the parameters that enter in the function
and x is the value to evalute the function for.   
*/

double betai(double a, double b, double x)
{
	double betacf(double a, double b, double x);
	double gammln(double xx);
	double bt;

	if (x < 0.0 || x > 1.0)
	{
		elog_complain(0,"Bad x value %lf passed to betai\nMust be between 0 and 1\n",
			x);
		return(-1.0);
	}
	if (x == 0.0 || x == 1.0) 
		bt=0.0;
	else
		bt=exp(gammln(a+b)-gammln(a)-gammln(b)+a*log(x)+b*log(1.0-x));
	if (x < (a+1.0)/(a+b+2.0))
		return bt*betacf(a,b,x)/a;
	else
		return 1.0-bt*betacf(b,a,1.0-x)/b;
}
/* This function implements an ensemble based F test.  A standard 
F statistic is the ratio of two independent estimates of the variance
of a set of Gaussian random numbers.  It is usually used to say if
the two variances are significantly different.  This function is a
minor variant on the usual procedure.  It is assume that var1 is 
a variance estimate with n1 degrees of freedom produced as a subset
of the estimate var2 with n2 degrees of freedom.  A sanity check
at the top make sure that n2>n1 returning an error (-1) in this
situation.  If the F statistic var1/var2 < 0 the function returns
immediately with a value 1. This is effectively a one sided F test
that says, in words, that we seek only to know when var1 is 
significantly LARGER than var2.  

critical is the confidence level against which we test the null
hypothesis that var1=var2.  

Author:  G Pavlis building on algorithm in Numerical Recipes in C.
Written:  November 2001
*/

int ftest_subset(double var1, int n1, double var2, int n2, double critical)
{
	double f;
	double var2_test;
	double testn1,testn2; /*betai wants these as doubles, not int*/
	double prob;
	double ptest;  /* set to 1 - critical */

	if(n2<=n1) 
	{
		elog_complain(0,"ftest_subset:  degrees of freedom n1 is greater than n2.  wrong since we assume var1 is a subset of var2\n");
		return(-1);
	}
	if(var1<var2) return(0);

	ptest = 1.0-critical;
	testn1=(double)n1;
	testn2=(double)(n2-n1);
	var2_test = (var2*((double)n2) - var1*testn1)/testn2;
	f = var1/var2_test;

	prob = betai(0.5*testn2,0.5*testn1,testn2/(testn2+testn1*f));
	if(prob<=ptest) 
		return(1);
	else
		return(0);
}
/* This is a more conventional, one-sided f test.  That is, it assumes
var1 and var2 are independent.  Test, as above, is one sided and only
yields true of var1 is significantly larger than var2.  Other arguments
are identical.
Author:  G pavlis
Written:  november 2001
*/

int ftest1(double var1, int n1, double var2, int n2, double critical)
{
	double f;
	double testn1,testn2; /*betai wants these as doubles, not int*/
	double prob;
	double ptest;  /* set to 1 - critical */

	if(var1<var2) return(0);

	ptest = 1.0-critical;
	testn1=(double)n1;
	testn2=(double)n2;
	f = var1/var2;

	prob = betai(0.5*testn2,0.5*testn1,testn2/(testn2+testn1*f));
	if(prob<=ptest) 
		return(1);
	else
		return(0);
}
/*Same, but this is a two-sided test as in numerical recipes.
That is, it handles either case when var1 is different from var2.
*/
int ftest2(double var1, int n1, double var2, int n2, double critical)
{
	double f;
	double testn1,testn2; /*betai wants these as doubles, not int*/
	double prob;
	double ptest;  /* set to 1 - critical */

	if(var1<var2) 
		f = var2/var1;
	else
		f=var1/var2;

	ptest = 1.0-critical;
	testn1=(double)n1;
	testn2=(double)n2;

	prob = 2.0*betai(0.5*testn2,0.5*testn1,testn2/(testn2+testn1*f));
	if(prob<=ptest) 
		return(1);
	else
		return(0);
}
