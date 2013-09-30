/* ********************* FFT ************************************
                                                                                
  COMPLEX FOURIER TRANSFORM.  FORWARD FOR SIGNI = -1.0; INVERSE FOR SIGNI = 1.0.
  LX MUST BE A POWER OF TWO.

                    LX         ISIGN*2*PI*I*(J-1)*(K-1)/LX
  X(K)  =  SCALE * SUM[X(J) * E                           ]
                   J=1

  FOR K=1,2,...,LX=2**INTEGER.
 
  Modified from original Fortran code for Cooley-Tukey algorithm to C code by 
  David von Seggern, October 2008.  Verified using the driver test_fft.c.

  Note that x is now a real array of length 2*lx, replacing the former 
  complex array x of length lx.  The array x is multiplexed reals and imags.
  For instance, an input 4-point real time series of all 1's would look like:

  double x[8] = {1.,0.,1.,0.,1.,0.,1.,0.};

  and the function would be called with (for forward transform):

  fft(x,4,-1);   Note the length (lx) is 4, not 8.

  A subsequent call with isign = 1 would return x as the original data.

*/

#include <math.h> 
void fft(double *x, int lx, int isign) 
{
  int   i,j,l,m,istep;
  double scale,arg,wr,wi,tempr,tempi;

  if (isign == -1.0)
    scale = 1.0/(lx);
  else
    scale = 1.0;

  for (i=0;i<2*lx;i++) x[i] = x[i]*scale;
 
  j = 1;
  for (i=1;i<=lx;i++)
  {
    if (i > j) goto resetm;
    tempr = x[2*j-2];
    tempi = x[2*j-1];
    x[2*j-2] = x[2*i-2];
    x[2*j-1] = x[2*i-1];
    x[2*i-2] = tempr;
    x[2*i-1] = tempi;
resetm:
    m = lx/2;
checkj:
    if (j <= m) goto incj;
    j = j - m;
    m = m/2;
    if (m >= 1) goto checkj;
incj:
    j = j + m;
  }

  l = 1;

stepmore:
  istep = 2*l;
  for (m=1;m<=l;m++)
  {
    arg = (3.141592653589793*(isign)*(m-1))/l;
    wr = cos(arg);
    wi = sin(arg);
    for (i=m;i<=lx;i+=istep)
    {
      tempr = wr*x[2*i+2*l-2] - wi*x[2*i+2*l-1];
      tempi = wr*x[2*i+2*l-1] + wi*x[2*i+2*l-2];
      x[2*i+2*l-2] = x[2*i-2] - tempr;
      x[2*i+2*l-1] = x[2*i-1] - tempi;
      x[2*i-2] = x[2*i-2] + tempr;
      x[2*i-1] = x[2*i-1] + tempi;
    }
  }

  l = istep;
  if (l < lx) goto stepmore;

  return;
}
