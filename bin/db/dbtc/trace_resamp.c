
#include <stdio.h>
#include <math.h>


#include "scv2.h"

int
trace_resamp (trace, samprate, tstart)

Trace *trace;
double               samprate, tstart;

/*
 *	trace_resamp will resample a time trace.
 *
 *	Inputs -	trace	= Input data trace.
 *			samprate= Output sample rate.
 *			tstart	= Time for synchronizing output samples.
 *
 *	Outputs -	trace	= Output resampled trace.
 */

{
	Trace *tr;
	double dtout;
	float *out;
	int nout, i, n;

	for (tr=trace; tr!=NULL; tr=tr->next) {
		if (tr->nsamps < 2) continue;
		dtout = 1.0/samprate;
		nout = 0;
		if (!resample (tr->nsamps, tr->dt, tr->tstart, tr->data, &nout, dtout, tstart, &tr->tstart, &out)) {
			fprintf (stderr, "trace_resamp: resample() error.\n");
			return (0);
		}
		tr->nsamps = nout;
		tr->dt = dtout;
		if (tr->data_free) my_free (tr->data);
		tr->data_malloc = nout*sizeof(float);
		tr->data = out;
		tr->data_free = out;
	}

	/* Normal exit */

	return (1);
}

/*
 * NAME
 *	resample - resample data
 *
 * SYNOPSIS
 *	int
 *	resample (nin, dtin, tsin, data, nout, dtout, tsample, tsout, out)
 *
 *	int       nin;
 *	double         dtin;
 *	double               tsin;
 *	float *                    data;
 *	int *                            nout;
 *	double                                 dtout;
 *	double                                        tsample;
 *	double *                                               tsout;
 *	float **                                                      out;
 *
 * DESCRIPTION
 *	resample() will resample a constant time increment sampled
 *	function at some other constant time increment. This routine will
 *	apply a simple anti-aliasing operation to insure that the resampled
 *	function is not aliased. It is assumed that the input time function
 *	is unwrapped and that the entire input function is contained within 
 *	the input time window so that it is OK to zero pad the input time
 *	function at the end of the input time window. The output resampled
 *	function is always dynamically allocated in this routine. Note that
 *	since a FFT is used to insure anti-aliasing, it is not possible to
 *	enforce an exact output sampling increment without time-domain
 *	interpolation.
 *
 * ARGUMENTS
 *	int		nin	= (i) Number of input samples.
 *	double		dtin	= (i) Input data sampling increment.
 *	double		tsin	= (i) Start time of first input data sample.
 *	float *		data	= (i) Input data sample values.
 *	int *		nout	= (i/o) Number of output samples. If this is
 *				  set to zero on input, then it is computed
 *				  automatically.
 *	double		dtout	= (i) Output data sampling increment.
 *	double		tsample	= (i) A time at which there will be an output
 *				  data sample. This provides a means for aligning
 *				  output data samples.
 *	double *	tsout	= (o) Start time of first output data sample.
 *	float **	out	= (o) Output data sample values.
 *
 * RETURNS
 *	1 if OK or 0 if ERROR.
 */

int
resample (nin, dtin, tsin, data, nout, dtout, tsample, tsout, out)

int       nin;
double         dtin;
double               tsin;
float *                    data;
int *                            nout;
double                                 dtout;
double                                        tsample;
double *                                               tsout;
float **                                                      out;

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
