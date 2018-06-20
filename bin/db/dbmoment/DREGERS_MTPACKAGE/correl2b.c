/*Copyright (c) Douglas Dreger
Berkeley Seismological Laboratory
University of California, Berkeley */
#include <math.h>
#include"tdmtinv_iso.h"

int correlate(ss,gg,i,np2)
     int i, np2;
     struct DATA   *ss;
     struct GREEN  *gg;
{
    int j, Zcor[8], zvalue;
    float cormax[8], maximum=0.0;
    float *data1, *data2, *ans;


    data1=vector(1,np2);
    data2=vector(1,np2);
    ans  =vector(1,2*np2);

    for(j=0; j < 8; j++)  /*Initialize cormax for each component*/
    {
	Zcor[j]  =0;
	cormax[j]=0.0;
    }

    for(j=0; j < np2; j++) /*Load Tangential Data*/
	if(j >= (ss[i].np))
	    data1[j+1]=0.0;
	else
	    data1[j+1]=ss[i].t[j];

    for(j=0; j < np2; j++) /*Load TSS*/
	if(j >= (gg[i].np))
	    data2[j+1]=0.0;
	else
	    data2[j+1]=gg[i].u1[j];

    correl(data1,data2,np2,ans);

    for(j=0; j < np2/2; j++)
	if(cormax[0] < fabs(ans[j+1]))
	{
	    cormax[0] = fabs(ans[j+1]);
	    Zcor[0] = j;
	}
    if(maximum < cormax[0])
    {
	zvalue=Zcor[0];
	maximum=cormax[0];
    }

    for(j=0; j < np2; j++) /*Load TDS*/
	if(j >= (gg[i].np))
	    data2[j+1]=0.0;
	else
	    data2[j+1]=gg[i].u2[j];

    correl(data1,data2,np2,ans);

    for(j=0; j < np2/2; j++)
	if(cormax[1] < fabs(ans[j+1]))
	{
	    cormax[1] = fabs(ans[j+1]);
	    Zcor[1] = j;
	}
    if(maximum < cormax[1])
    {
	zvalue=Zcor[1];
	maximum=cormax[1];
    }

    for(j=0; j < np2; j++) /*Load Radial Data*/
	if(j >= (ss[i].np))
	    data1[j+1]=0.0;
	else
	    data1[j+1]=ss[i].r[j];

    for(j=0; j < np2; j++) /*Load RSS*/
	if(j >= (gg[i].np))
	    data2[j+1]=0.0;
	else
	    data2[j+1]=gg[i].u3[j];

    correl(data1,data2,np2,ans);

    for(j=0; j < np2/2; j++)
	if(cormax[2] < fabs(ans[j+1]))
	{
	    cormax[2] = fabs(ans[j+1]);
	    Zcor[2] = j;
	}
    if(maximum < cormax[2])
    {
	zvalue=Zcor[2];
	maximum=cormax[2];
    }

    for(j=0; j < np2; j++) /*Load RDS*/
	if(j >= (gg[i].np))
	    data2[j+1]=0.0;
	else
	    data2[j+1]=gg[i].u4[j];

    correl(data1,data2,np2,ans);

    for(j=0; j < np2/2; j++)
	if(cormax[3] < fabs(ans[j+1]))
	{
	    cormax[3] = fabs(ans[j+1]);
	    Zcor[3] = j;
	}
    if(maximum < cormax[3])
    {
	zvalue=Zcor[3];
	maximum=cormax[3];
    }

    for(j=0; j < np2; j++) /*Load RDD*/
	if(j >= (gg[i].np))
	    data2[j+1]=0.0;
	else
	    data2[j+1]=gg[i].u5[j];

    correl(data1,data2,np2,ans);

    for(j=0; j < np2/2; j++)
	if(cormax[4] < fabs(ans[j+1]))
	{
	    cormax[4] = fabs(ans[j+1]);
	    Zcor[4] = j;
	}
    if(maximum < cormax[4])
    {
	zvalue=Zcor[4];
	maximum=cormax[4];
    }

    for(j=0; j < np2; j++) /*Load Vertical Data*/
	if(j >= (ss[i].np))
	    data1[j+1]=0.0;
	else
	    data1[j+1]=ss[i].z[j];

    for(j=0; j < np2; j++) /*Load ZSS*/
	if(j >= (gg[i].np))
	    data2[j+1]=0.0;
	else
	    data2[j+1]=gg[i].u6[j];

    correl(data1,data2,np2,ans);

    for(j=0; j < np2/2; j++)
	if(cormax[5] < fabs(ans[j+1]))
	{
	    cormax[5] = fabs(ans[j+1]);
	    Zcor[5] = j;
	}
    if(maximum < cormax[5])
    {
	zvalue=Zcor[5];
	maximum=cormax[5];
    }

    for(j=0; j < np2; j++) /*Load ZDS*/
	if(j >= (gg[i].np))
	    data2[j+1]=0.0;
	else
	    data2[j+1]=gg[i].u7[j];

    correl(data1,data2,np2,ans);

    for(j=0; j < np2/2; j++)
	if(cormax[6] < fabs(ans[j+1]))
	{
	    cormax[6] = fabs(ans[j+1]);
	    Zcor[6] = j;
	}
    if(maximum < cormax[6])
    {
	zvalue=Zcor[6];
	maximum=cormax[6];
    }

    for(j=0; j < np2; j++) /*Load ZDD*/
	if(j >= (gg[i].np))
	    data2[j+1]=0.0;
	else
	    data2[j+1]=gg[i].u8[j];

    correl(data1,data2,np2,ans);

    for(j=0; j < np2/2; j++)
	if(cormax[7] < fabs(ans[j+1]))
	{
	    cormax[7] = fabs(ans[j+1]);
	    Zcor[7] = j;
	}
    if(maximum < cormax[7])
    {
	zvalue=Zcor[7];
	maximum=cormax[7];
    }


    free_vector(data1,1,np2);
    free_vector(data2,1,np2);
    free_vector(ans,1,2*np2);

    return(zvalue);

}/*END CORRELATE*/





/*Collection of Numerical Recipe Routines to perform correlations*/
void correl(data1,data2,n,ans)
     float data1[],data2[],ans[];
     int n;
{
    int no2,i;
    float dum,*fft,*vector();
    void twofft(),realft(),free_vector();

    fft=vector(1,2*n);
    twofft(data1,data2,fft,ans,n);
    no2=n/2;
    for (i=2;i<=n+2;i+=2) {
	ans[i-1]=(fft[i-1]*(dum=ans[i-1])+fft[i]*ans[i])/no2;
	ans[i]=(fft[i]*dum-fft[i-1]*ans[i])/no2;
    }
    ans[2]=ans[n+1];
    realft(ans,no2,-1);
    free_vector(fft,1,2*n);
}

void realft(data,n,isign)
     float data[];
     int n,isign;
{
    int i,i1,i2,i3,i4,n2p3;
    float c1=0.5,c2,h1r,h1i,h2r,h2i;
    double wr,wi,wpr,wpi,wtemp,theta;
    void four1();

    theta=3.141592653589793/(double) n;
    if (isign == 1) {
	c2 = -0.5;
	four1(data,n,1);
    } else {
	c2=0.5;
	theta = -theta;
    }
    wtemp=sin(0.5*theta);
    wpr = -2.0*wtemp*wtemp;
    wpi=sin(theta);
    wr=1.0+wpr;
    wi=wpi;
    n2p3=2*n+3;
    for (i=2;i<=n/2;i++) {
	i4=1+(i3=n2p3-(i2=1+(i1=i+i-1)));
	h1r=c1*(data[i1]+data[i3]);
	h1i=c1*(data[i2]-data[i4]);
	h2r = -c2*(data[i2]+data[i4]);
	h2i=c2*(data[i1]-data[i3]);
	data[i1]=h1r+wr*h2r-wi*h2i;
	data[i2]=h1i+wr*h2i+wi*h2r;
	data[i3]=h1r-wr*h2r+wi*h2i;
	data[i4] = -h1i+wr*h2i+wi*h2r;
	wr=(wtemp=wr)*wpr-wi*wpi+wr;
	wi=wi*wpr+wtemp*wpi+wi;
    }
    if (isign == 1) {
	data[1] = (h1r=data[1])+data[2];
	data[2] = h1r-data[2];
    } else {
	data[1]=c1*((h1r=data[1])+data[2]);
	data[2]=c1*(h1r-data[2]);
	four1(data,n,-1);
    }
}

void twofft(data1,data2,fft1,fft2,n)
     float data1[],data2[],fft1[],fft2[];
     int n;
{
    int nn3,nn2,jj,j;
    float rep,rem,aip,aim;
    void four1();

    nn3=1+(nn2=2+n+n);
    for (j=1,jj=2;j<=n;j++,jj+=2) {
	fft1[jj-1]=data1[j];
	fft1[jj]=data2[j];
    }
    four1(fft1,n,1);
    fft2[1]=fft1[2];
    fft1[2]=fft2[2]=0.0;
    for (j=3;j<=n+1;j+=2) {
	rep=0.5*(fft1[j]+fft1[nn2-j]);
	rem=0.5*(fft1[j]-fft1[nn2-j]);
	aip=0.5*(fft1[j+1]+fft1[nn3-j]);
	aim=0.5*(fft1[j+1]-fft1[nn3-j]);
	fft1[j]=rep;
	fft1[j+1]=aim;
	fft1[nn2-j]=rep;
	fft1[nn3-j] = -aim;
	fft2[j]=aip;
	fft2[j+1] = -rem;
	fft2[nn2-j]=aip;
	fft2[nn3-j]=rem;
    }
}

#define SWAP(a,b) tempr=(a);(a)=(b);(b)=tempr

void four1(data,nn,isign)
     float data[];
     int nn,isign;
{
    int n,mmax,m,j,istep,i;
    double wtemp,wr,wpr,wpi,wi,theta;
    float tempr,tempi;

    n=nn << 1;
    j=1;
    for (i=1;i<n;i+=2) {
	if (j > i) {
	    SWAP(data[j],data[i]);
	    SWAP(data[j+1],data[i+1]);
	}
	m=n >> 1;
	while (m >= 2 && j > m) {
	    j -= m;
	    m >>= 1;
	}
	j += m;
    }
    mmax=2;
    while (n > mmax) {
	istep=2*mmax;
	theta=6.28318530717959/(isign*mmax);
	wtemp=sin(0.5*theta);
	wpr = -2.0*wtemp*wtemp;
	wpi=sin(theta);
	wr=1.0;
	wi=0.0;
	for (m=1;m<mmax;m+=2) {
	    for (i=m;i<=n;i+=istep) {
		j=i+mmax;
		tempr=wr*data[j]-wi*data[j+1];
		tempi=wr*data[j+1]+wi*data[j];
		data[j]=data[i]-tempr;
		data[j+1]=data[i+1]-tempi;
		data[i] += tempr;
		data[i+1] += tempi;
	    }
	    wr=(wtemp=wr)*wpr-wi*wpi+wr;
	    wi=wi*wpr+wtemp*wpi+wi;
	}
	mmax=istep;
    }
}

#undef SWAP
