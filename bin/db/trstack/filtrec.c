#include <stdio.h>
#include <math.h>
/* Simple zero-phase filtering of a record */
int
filtrec(nsamp, samprate, data, hpfreq, lpfreq)
int 	nsamp;
double 	samprate;
float 	*data;
double 	hpfreq, lpfreq;
{
	int length, i, j, nyquist;
	float *result;
	float df, phs, famp, frn;
	double delta;
	/* butterworth 3-pole coefficients */
	float pr[3], pi[3];
	int npole=3;
	pr[0] = .5;
	pr[1] = .5;
	pr[2] = 1.;
	pi[0] = .8660254;
	pi[1] = -pi[0];
	pi[2] = 0.;
	fprintf(stderr, "..Using LPfilt %f and HPfilt %f\n",lpfreq, hpfreq);
	delta = 1./samprate;

	/* get next pwr of 2 */
	length = nsamp;
	for(i=1;((int)pow((double)2,(double)i)) < length;++i);
	length=(int)pow((double)2,(double)(i+1));
	
	if((result=(float *)calloc((unsigned)(length+2),sizeof(float))) == NULL)
	{
		fprintf(stderr,"Error allocating space for result in filtrec\n");
		exit(-2);
	}
	for (i=0; i<nsamp; i++) result[i]=data[i];
	for (i=nsamp; i<length+2; i++) result[i] = data[0];

	cfftr(result, length);
	df = 0.5/(delta * (float)length);
    	nyquist=1.0/(2.0*delta);
	if (hpfreq > 0.) {
		for(i=0, phs=0.;i<=length;i+=2, phs += df)
		{
			famp = 1.;
			frn = phs/hpfreq;
			for (j=0;j<npole; j++) famp *= frn*frn/(pr[j]*pr[j] + (frn + pi[j])*(frn+pi[j]));  
			/* famp = pow(phs*phs/(phs*phs+hpfreq*hpfreq), 3.); */
			result[i] *= famp;
			result[i+1] *= famp;
		}
	  	result[0] = 0.;
		result[1] = 0.;
	}
	if (lpfreq > 0.) {
		for(i=0, phs=0.;i<=length;i+=2, phs += df)
		{
			famp = 1.;
			frn = phs/lpfreq;
			for (j=0;j<npole; j++) famp *= 1./(pr[j]*pr[j] + (frn + pi[j])*(frn+pi[j])); 
			/*  famp = pow(lpfreq*lpfreq/(phs*phs+lpfreq*lpfreq), 3.); */
			result[i] *= famp;
			result[i+1] *= famp;
		}
	}
	cfftri(result, length);
	for(i=0;i<nsamp;i++)data[i]=result[i]*(2.0/length);    /* rescale */


	free(result);
}
