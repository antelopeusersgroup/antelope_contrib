#include <math.h>
#define S(a,b) tempr=(a);(a)=(b);(b)=tempr

void four1(float dd[], unsigned long nn, int isign)
{
    unsigned long n,mmax,m,j,istep,i;
    double wtemp,wr,wpr,wpi,wi,theta;
    float tempr,tempi;

    n = nn << 1;
    j = 1;
    for (i=1; i<n; i+=2) {
        if (j > i) {
            S(dd[j],dd[i]);
            S(dd[j+1],dd[i+1]);
        }
        m = nn;
        while (m >= 2 && j > m) {
            j -= m;
            m >>= 1;
        }
        j += m;
    }
    mmax = 2;
    while (n > mmax) {
        istep = mmax << 1;
        theta = isign*(6.28318530717959/mmax);
        wtemp = sin(0.5*theta);
        wpr = -2.0*wtemp*wtemp;
        wpi = sin(theta);
        wr = 1.0;
        wi = 0.0;
        for (m=1; m<mmax; m+=2) {
            for (i=m; i<=n; i+=istep) {
                j = i+mmax;
                tempr = wr*dd[j]-wi*dd[j+1];
                tempi = wr*dd[j+1]+wi*dd[j];
                dd[j] = dd[i]-tempr;
                dd[j+1] = dd[i+1]-tempi;
                dd[i] += tempr;
                dd[i+1] += tempi;
            }
            wr = (wtemp=wr)*wpr-wi*wpi+wr;
            wi = wi*wpr+wtemp*wpi+wi;
        }
        mmax = istep;
    }
}
#undef S
