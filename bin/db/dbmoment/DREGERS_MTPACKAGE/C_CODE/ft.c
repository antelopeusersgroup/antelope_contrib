#include <math.h>

void realft(float dd[], unsigned long n, int isign)
{
    void four1(float dd[], unsigned long nn, int isign);
    unsigned long i,i1,i2,i3,i4,np3;
    float c1=0.5,c2,h1r,h1i,h2r,h2i;
    double wr,wi,wpr,wpi,wtemp,theta;

    theta = 3.141592653589793/(double) (n>>1);
    if (isign == 1) {
        c2 = -0.5;
        four1(dd,n>>1,1);
    } else {
        c2 = 0.5;
        theta = -theta;
    }
    wtemp = sin(0.5*theta);
    wpr = -2.0*wtemp*wtemp;
    wpi = sin(theta);
    wr = 1.0+wpr;
    wi = wpi;
    np3 = n+3;
    for (i=2; i<=(n>>2); i++) {
        i4 = 1+(i3=np3-(i2=1+(i1=i+i-1)));
        h1r = c1*(dd[i1]+dd[i3]);
        h1i = c1*(dd[i2]-dd[i4]);
        h2r = -c2*(dd[i2]+dd[i4]);
        h2i = c2*(dd[i1]-dd[i3]);
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
        dd[1] = c1*((h1r=dd[1])+dd[2]);
        dd[2] = c1*(h1r-dd[2]);
        four1(dd,n>>1,-1);
    }
}
