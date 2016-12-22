#include "c_help.h"
#include <math.h>
#define PI (3.141592653589793)
#define swap(a,b) {float ttt=(a);(a)=(b);(b)=ttt;}

void c_correlate(float d1[], float d2[], unsigned long n, float ans[])
{
    void fftreal(float data[], unsigned long n, int isign);
    void doublefft(float d1[], float d2[], float f1[], float f2[], unsigned long n);
    unsigned long no2,i;
    float tmp,*fft;

    fft = vec(1, n<<1);
    doublefft(d1, d2, fft, ans, n);
    no2 = n>>1;
    for (i=2; i<=n+2; i+=2) {
        ans[i-1] = (fft[i-1]*(tmp=ans[i-1])+fft[i]*ans[i])/no2;
        ans[i] = (fft[i]*tmp-fft[i-1]*ans[i])/no2;
    }
    ans[2] = ans[n+1];
    fftreal(ans,n,-1);
    f_vec(fft,1,n<<1);
}

void dft(float dd[], unsigned long nn, int isign)
{
    unsigned long n,mmax,m,j,istep,i;
    double wtemp,wr,wpr,wpi,wi,theta;
    float tempr,tempi;

    n = nn << 1;
    j = 1;
    for (i=1; i<n; i+=2) {
        if (j > i) {
            swap(dd[j],dd[i]);
            swap(dd[j+1],dd[i+1]);
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
        theta = isign*((2*PI)/mmax);
        wtemp = sin(0.5 * theta);
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

void fftreal(float dd[], unsigned long n, int isign)
{
    void dft(float dd[], unsigned long nn, int isign);
    unsigned long i,i1,i2,i3,i4,np3;
    float c2,h1r,h1i,h2r,h2i;
    double wr,wi,wpr,wpi,wtemp,theta;

    theta = PI/(double) (n>>1);
    if (isign == 1) {
        c2 = -0.5;
        dft(dd,n>>1,1);
    } else {
        c2 = 0.5;
        theta = -theta;
    }
    wtemp = sin(0.5 * theta);
    wpr = -2.0*wtemp*wtemp;
    wpi = sin(theta);
    wr = 1.0+wpr;
    wi = wpi;
    np3 = n+3;
    for (i=2; i<=(n>>2); i++) {
        i4 = 1+(i3=np3-(i2=1+(i1=i+i-1)));
        h1r = 0.5 * (dd[i1]+dd[i3]);
        h1i = 0.5 * (dd[i2]-dd[i4]);
        h2r = -c2 * (dd[i2]+dd[i4]);
        h2i = c2 * (dd[i1]-dd[i3]);
        dd[i1] = h1r+wr*h2r-wi*h2i;
        dd[i2] = h1i+wr*h2i+wi*h2r;
        dd[i3] = h1r-wr*h2r+wi*h2i;
        dd[i4] = -h1i+wr*h2i+wi*h2r;
        wr = (wtemp=wr)*wpr-wi*wpi+wr;
        wi = wi*wpr+wtemp*wpi+wi;
    }
    if (isign == 1) {
        dd[1] = (h1r=dd[1])+dd[2];
        dd[2] = h1r-dd[2];
    } else {
        dd[1] = 0.5 * ((h1r=dd[1])+dd[2]);
        dd[2] = 0.5 * (h1r-dd[2]);
        dft(dd,n>>1,-1);
    }
}

void doublefft(float d1[], float d2[], float f1[], float f2[],
    unsigned long n)
{
    void dft(float data[], unsigned long nn, int isign);
    unsigned long nn3,nn2,jj,j;
    float rep,rem,aip,aim;

    nn3 = 1+(nn2 = 2+n+n);
    for (j=1,jj=2; j<=n; j++,jj+=2) {
        f1[jj-1] = d1[j];
        f1[jj] = d2[j];
    }
    dft(f1,n,1);
    f2[1] = f1[2];
    f1[2] = f2[2] = 0.0;
    for (j=3; j<=n+1; j+= 2) {
        rep = 0.5 * (f1[j] + f1[nn2-j]);
        rem = 0.5 * (f1[j] - f1[nn2-j]);
        aip = 0.5 * (f1[j+1] + f1[nn3-j]);
        aim = 0.5 * (f1[j+1] - f1[nn3-j]);
        f1[j] = rep;
        f1[j+1] = aim;
        f1[nn2-j] = rep;
        f1[nn3-j]  =  -aim;
        f2[j] = aip;
        f2[j+1]  =  -rem;
        f2[nn2-j] = aip;
        f2[nn3-j] = rem;
    }
}


#undef swap
