
#include "dbtc.h"
 
int resmpl (  int nin,
              double dtin,
	      double  tsin,
              float *data,
              int *nout,
              double dtout,
	      double tsample,
	      double *tsout,
              float **out )

{
	double fnin, fnout, tsamp, tts;
	double dtini, time, tend, tinsamp, freq, fnyq;
	double t0, y0, slope;
	int i, j, jlast, nnin;
	int ntmp;
	float *tmp;
	float dt4;
	int nt;
	float fzero = 0.0;
	int nnf;
	float ddf;

/*
 *   compute the input and output nyquist frequencies
 */
 	if (nin < 2) return (1);
	fnin =  0.5/dtin;
	fnout = 0.5/dtout;
	nnin = nin;
	tsamp = tsample - tsin;
	if (tsamp >= 0.0) {
		i = tsamp/dtout;
		tts = i*dtout;
		tts = tsamp - tts;
	} else {
		i = (-tsamp)/dtout;
		tts = (i+1)*dtout;
		tts = tts + tsamp;
	}
	*tsout = tsin + tts;
/*
 *   set up output array
 */
 	if (*nout == 0) {
 		tend = (nin-1)*dtin;
 		tend -= tts;
 		*nout = tend/dtout;
 		(*nout)++;
 	}
 	if (*nout < 1) return (1);
 	*out = (float *) malloc ((*nout)*sizeof(float));
 	if (*out == NULL) {
 		fprintf (stderr, "resamp: malloc() error.\n");
 		return (0);
 	}
/*
 *   if the output nyquist is less then the input nyquist - apply anti-aliasing
 */
 	if (fnout < fnin) {
		ntmp = 4*nin;
		tmp = (float *) malloc (ntmp*sizeof(float));
		if (tmp == NULL) {
			fprintf (stderr, "resamp: malloc() error.\n");
			free (*out);
			*out = NULL;
			return (0);
		}
		for (i=0; i<nin; i++) tmp[i] = data[i];
		for (i=0; i<nin; i++) tmp[i+nin] = 0.0;
		nt = ntmp/2;
		dt4 = dtin;
		t2f_ (&nnin, &nt, &fzero, &dt4, tmp, &tmp[nt], &nnf, &ddf);
		for (i=0; i<nnin/2 + 1; i++) {
			freq = ddf*i;
			if (freq > fnout) {
				tmp[i] = 0.0;
				tmp[nt+i] = 0.0;
			}
		}
		f2t_ (&nnf, tmp, &tmp[nt], &fzero, &dt4);
/*
 *   if the output nyquist is greater then the input nyquist - zero pad spectrum
 */
 	} else if (fnout > fnin) {
		ntmp = 4*(*nout);
		tmp = (float *) malloc (ntmp*sizeof(float));
		if (tmp == NULL) {
			fprintf (stderr, "resamp: malloc() error.\n");
			free (*out);
			*out = NULL;
			return (0);
		}
		for (i=0; i<nin; i++) tmp[i] = data[i];
		for (i=0; i<nin; i++) tmp[i+nin] = 0.0;
		nt = ntmp/2;
		dt4 = dtin;
		t2f_ (&nnin, &nt, &fzero, &dt4, tmp, &tmp[nt], &nnf, &ddf);
		while ((fnyq = ddf*nnin/2) < fnout) {
			nnf++;
			nnin *= 2;
			dtin *= 0.5;
		}
		for (i=0; i<nnin/2 + 1; i++) {
			freq = ddf*i;
			if (freq >= fnin) {
				tmp[i] = 0.0;
				tmp[nt+i] = 0.0;
			}
		}
		f2t_ (&nnf, tmp, &tmp[nt], &fzero, &dt4);
	} else if (tts == 0.0) {
		if (*nout > nin) {
			*nout = nin;
 			free(*out);
 			*out = (float *) malloc ((*nout)*sizeof(float));
 			if (*out == NULL) {
 				fprintf (stderr, "resamp: malloc() error.\n");
 				return (0);
 			}
		}
		for (i=0; i<*nout; i++) (*out)[i] = data[i];
		return (1);
	} else {
		tmp = data;
		nnin = nin;
	}
/*
 *   start resampling
 */
 	dtini = 1.0/dtin;
 	jlast = -1000;
 	for (i=0; i<*nout; i++) {
 		time = tts+dtout*i;
 		tinsamp = time*dtini;
 		j = tinsamp;
 		if (j >= nnin-1 && tinsamp > 1.e-6) {
 			*nout = i;
 			*out = (float *) realloc (*out, (*nout)*sizeof(float));
 			if (*out == NULL) {
 				fprintf (stderr, "resamp: realloc() error.\n");
 				if (tmp != data) free (tmp);
 				return (0);
 			}
 			break;
		}
		if (j != jlast) {
			jlast = j;
			t0 = dtin*j;
			y0 = tmp[j];
			slope = (tmp[j+1] - y0)*dtini;
		}
		(*out)[i] = y0 + (time-t0)*slope;
	}
/*
 *   normal exit
 */
	if (tmp != data) free (tmp);
 	return (1);
}

/* $Id$ */
