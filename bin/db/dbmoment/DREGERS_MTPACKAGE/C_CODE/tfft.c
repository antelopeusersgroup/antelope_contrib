void twofft(float d1[], float d2[], float f1[], float f2[],
    unsigned long n)
{
    void four1(float data[], unsigned long nn, int isign);
    unsigned long nn3,nn2,jj,j;
    float rep,rem,aip,aim;

    nn3 = 1+(nn2 = 2+n+n);
    for (j=1,jj=2; j<=n; j++,jj+=2) {
        f1[jj-1] = d1[j];
        f1[jj] = d2[j];
    }
    four1(f1,n,1);
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
