/* routine to do receiver function processing of records */
/*  Returns pointer t a NEW array with receiver fcn	*/
#include <stdio.h>
#include <math.h>


float  *rfcn(nin_1, nin_2, datin_1, datin_2, delta, 
		phshift, wlev, gfreq, hpfreq)
int	nin_1, nin_2;
float	*datin_1, *datin_2;
float	delta;
float	phshift, wlev, gfreq, hpfreq;
{
	int length_1, length_2, i;
	float *data_1, *data_2;
	float	 *result;
	float wlev1, amp, ampmax;
	float df, phs, cphs, sphs, rrot, irot;
	float gscale, famp;
	float *f_div();

	length_1=nin_1;
	length_2=nin_2;

	/* for a circular convolution data lengths must be a power of 2 */
	
	for(i=1;((int)pow((double)2,(double)i)) < length_1;++i);
	length_1=(int)pow((double)2,(double)i);
	for(i=1;((int)pow((double)2,(double)i)) < length_2;++i);
	length_2=(int)pow((double)2,(double)i);

	/* Double lengths for zero-padding */
	length_1 *= 2;
	length_2 *= 2;

	/*  make length_1=length_2 if necessary */

	if(length_2 > length_1)length_1=length_2;
	if(length_1 > length_2)length_2=length_1;

	/* allocate array space and load first array */

	if((data_1=(float *)calloc((unsigned)(length_1+2),sizeof(float))) == NULL)
	{
		fprintf(stderr,"Error allocating space for data_1 in rfcn\n");
		exit(-2);
	}
	for (i=0; i<nin_1; i++) data_1[i]=datin_1[i];

	/* allocate array space and load second datastream */

	if((data_2=(float *)calloc((unsigned)(length_2+2),sizeof(float))) == NULL)
	{
		fprintf(stderr,"Error allocating space for stdin in rfcn\n");
		exit(-2);
	}
	for (i=0; i<nin_2; i++) data_2[i]=datin_2[i];

	/* zero-fill to 2X record length */

	if (length_1>nin_1) {
		for(i=nin_1;i<length_1; i++) data_1[i]=0.;
	}
	if (length_2>nin_2) {
		for(i=nin_2;i<length_2; i++) data_2[i]=0.;
	}


	/* linear-pad instead of zeroes -- NOT */
/*
	if (length_2>nin_2) {
		dat1=data_2[nin_2-1];
		ddat=(data_2[0]-dat1)/(float)(length_2-nin_2);
		for(i=nin_2;i<length_2; i++) data_2[i]=dat1 + ddat*(float)(i-nin_2+1);
	}
	if (length_1>nin_1) {
		dat1=data_1[nin_1-1];
		ddat=(data_1[0]-dat1)/(float)(length_1-nin_1);
		for(i=nin_1;i<length_1; i++) data_1[i]=dat1 + ddat*(float)(i-nin_1+1);
	}
*/

	/*FFT datastreams and process */
	cfftr(data_1,length_1);
	cfftr(data_2,length_2);

	if (wlev > 0.) {
		ampmax = 0.;
		for(i=0;i<=length_2;i+=2)
		{
			amp=sqrt(data_2[i]*data_2[i]+data_2[i+1]*data_2[i+1]);
			if (amp > ampmax) ampmax = amp;
		}
		wlev1 = wlev*ampmax;
		for(i=0;i<=length_2;i+=2)
		{
			amp=sqrt(data_2[i]*data_2[i]+data_2[i+1]*data_2[i+1]);
			if (amp < wlev1) {
				data_2[i]*= wlev1/amp;
				data_2[i+1] *= wlev1/amp;
			}
		}
	}

	if (phshift>0.) {
		df = 6.2831853*phshift/(2.*delta * (float)length_1);
		for(i=0;i<=length_1;i+=2)
		{
			phs = -((float)i) * df ;
			cphs = cos(phs);
			sphs = sin(phs);
			rrot=data_1[i]*cphs-data_1[i+1]*sphs;
			irot=data_1[i]*sphs+data_1[i+1]*cphs;
			data_1[i]=rrot;
			data_1[i+1]=irot;
		}
	}

	result=f_div(data_1,data_2,length_2);

	if (gfreq > 0.) {
		df = 1./(2.*delta * (float)length_2);
		gscale=1.0;		/*  Normalize gaussian NOT */
		for(i=0;i<=length_2;i+=2)
		{
			phs=((float)i)*df;
			famp=exp(-0.5*phs*phs/gfreq/gfreq)*gscale;
			result[i] *= famp;
			result[i+1] *= famp;
		}
	}

	if (hpfreq > 0.) {
		df = 1./(2.*delta * (float)length_2);
		gscale=1.0;		/*  Normalize gaussian NOT */
		for(i=0;i<=length_2;i+=2)
		{
			phs=((float)i)*df;
			famp = pow(phs*phs/(phs*phs+hpfreq*hpfreq), 3.);
			result[i] *= famp;
			result[i+1] *= famp;
		}
	}

	/* zero out nyquist frequency, just to be safe*/
	result[length_2]=0.;
	result[length_2+1]=0.;
		
	cfftri(result,length_2);
	for(i=0;i<length_2;i++)result[i]*=(2.0/length_2);    /* rescale */
	/* correct for sign convention */
	for (i=0; i<length_2; i++) result[i] *= -1.0;

	free((char *)data_2);
	free((char *)data_1);
	return (result);
}


/* routine to deconvolve two timeseries
 * routines take the fft output of two timeseries which were of the
 * same length
 *
 * Inputs:
 *	a,b=frequency components of the two timeseries
 *
 * Outputs:
 *	pointer to the resulting frequency components of length ndata
 *	Array space for the output array is created in this routine
 * 
 */
float *f_div(a,b,ndata)

	float *a,*b;
	int ndata;
{
	float *result,temp;
	int i;

	if((result=(float *)calloc((unsigned)(ndata+2),sizeof(float)))==NULL)
	{
		fprintf(stderr,"Error allocating space for frequency multiplication\n");
		exit(-2);
	}

	for(i=0;i<=ndata;i+=2)
	{
		result[i]=a[i]*b[i]+a[i+1]*b[i+1];
		result[i+1]=b[i]*a[i+1]-a[i]*b[i+1];
		temp=b[i]*b[i]+b[i+1]*b[i+1];
		if(temp == 0.0)  /* prevent division by zero */
		{
			result[i]=result[i+1]=0.0;
		}
		else
		{
			result[i]/=temp;
			result[i+1]/=temp;
		}
	}
	return(result);
}
