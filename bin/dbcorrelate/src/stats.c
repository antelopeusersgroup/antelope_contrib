#include <math.h>

      int stats_c(float x[], int npts, int k, float *mean, float *stdev,
                float *skew, float *kurtosis)
/*
c     Computes statistics of a data vector.  How many stats is determined by
c     the value of k.
c
c     k = 1 	mean 
c     k = 2     also standard deviation
c     k = 3     also skew
c     k = 4     also kurtosis
c
*/

{
  int i;
  float sum,sumsq,sumcu,sumqu;

  *mean=0.0;
  *stdev=0.0;
  *skew=0.0;
  *kurtosis=0.0;

  if (k == 0) return 0;

  sum=0.0;

  for (i=0;i<=npts-1;i++)
    sum=sum+x[i];

  *mean=sum/npts;

  if (k == 1) return 1;

  sumsq=0.0;

  for (i=0;i<=npts-1;i++)
    sumsq=sumsq+pow((x[i]-*mean),2);

  *stdev=sqrt(sumsq/npts);

  if (k == 2) return 1;

  sumcu=0.0;

  for (i=0;i<=npts-1;i++)
    sumcu=sumcu+pow((x[i]-*mean),3);

  *skew=(sumcu/npts)/pow(*stdev,3);

  if (k == 3) return 1;

  sumqu=0.0;

  for (i=0;i<=npts-1;i++)
    sumqu=sumqu+pow((x[i]-*mean),4);

  *kurtosis=(sumqu/npts)/pow(*stdev,4);

  return 1;
}
