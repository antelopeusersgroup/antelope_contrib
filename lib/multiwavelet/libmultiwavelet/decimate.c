#include <stdlib.h>
#include <stdio.h>
#include <perf.h>

#include "stock.h"
#include "response.h"
#include "elog.h"
#include "multiwavelet.h"
/* This routine parses the parameter object pf to produce the decimator
definitions used by the general decimation routine found below.  
It returns a vector of Tbl objects in t containing a sequence of
decimation definitions as the argument t.  The value returned 
is the number of elements in the vector t.

Author:  Gary L. Pavlis
Written:  July 21, 1998
*/
Tbl **define_decimation(Pf *pf, int *nbands)
{
	Tbl *list;
	Tbl **t;
	char *white=" \t";
	int i;
	char *line;
	char *s;
	int ntest;
	
	list = pfget_tbl(pf, "bands");
	*nbands = maxtbl(list);
	if(*nbands <= 0)
		elog_die(0,"define_decimation:  wavelet bands not defined\n");
	ntest=pfget_int(pf,"number_frequency_bands");
	if(ntest>*nbands)
		elog_die(0,"define_decimation:  decimation band definition not consistent with number_frequency_bands\nband tbl is of length %d while number_frequency_bands parameter is %d\n",
			*nbands,ntest);
	/* note we silently truncate if the Tbl is larger than requested.  We could
	log and error, but why be so verbose */
	else if(ntest<*nbands) 
		*nbands=ntest;

	t = (Tbl **)calloc(*nbands,sizeof(Tbl *));
	for(i=0;i<*nbands;i++)
	{
		t[i] = newtbl(0);
		line = (char *)gettbl(list,i);
		s = strtok(line,white);
		do
		{
			pushtbl(t[i],s);
		}
		while((s=strtok(NULL,white)) != NULL);
	}
	return(t);		
}

/* This routine looks at a sequence of css response files passed
through the dec_stages list of file names, reads these response
files, and stores the description of them in a series of output 
Tbls.  
arguments:
	decdef = tbl of strings defining css response files to use
		to define decimation.  
	resp - returned response structure for this sequence of filters 
	dec_fac = total decimation factor defined by Tbl *decdef (output).
	decimators = returned tbl of pointers to FIR_decimation structures 

Normal return is 1,  any failure returns 0 and results should not
be used.  Return of 2 may be needed to handled seperately.  This
occurs when "none" is specified for decimation and the returned
Tbl has a single null entry.  

This routine is a drastically modified form of a routine by the same name 
written by Danny Harvey in dbdec.. G Pavlis

*/

int read_dec_files (Tbl *decdef, int *dec_fac, Tbl **decimators)

{
	int i, j, n;
	Response *rsp;
	char string[512];
	FILE *file;
	char **dec_stages;

	FIR_decimation *decptr;
 	Response *resp;

	resp = (Response *) new_response ();

	if(*decimators == NULL) *decimators = newtbl(0);
	if (resp == NULL) {
		elog_die(0, "read_dec_files: Malloc error on decimation response structure.\n");
		return (0);
	}
	for (i=0,(*dec_fac)=1; i<maxtbl(decdef); i++) {
		int ok;
		char *decfile;
		decfile = gettbl(decdef,i);
		if(!strcmp(decfile,"none") )
		{
			*dec_fac = 1.0;
			decptr = (FIR_decimation *) malloc(sizeof(FIR_decimation));
			if(decptr == NULL)
			{
				elog_notify(0,"Cannot malloc decimation structure for stage %d\n",
					i);
				return(0);
			}
			decptr->decfac = 1.0;
			decptr->ncoefs = 0;
			decptr->coefs=NULL;
			pushtbl(*decimators,decptr);
			return(2);
		}

		file = fopen(decfile, "r");
		if (file == NULL) {
			elog_notify(0, "read_dec_files: Unable to open response stage file '%s'.\n",
							decfile);
			return (0);
		}
		if (read_response (file, &rsp)) {
			elog_clear_register(1);
			elog_notify(0, "read_dec_files: read_response() error on stage file '%s'.\n",
							decfile);
			return (0);
		}
		fclose (file);
		get_response_nstages (rsp, &n);
		for (j=0,ok=0; j<n; j++) {
			int dec_factor, nnum, nden;
			double srate;

			get_response_stage_type (rsp, j, string);
			if (strcmp(string, "fir")) continue;
			get_response_stage_fir_ncoefs (rsp, j, &srate, &dec_factor, &nnum, &nden);
			if (nden > 1) {
				elog_notify(0, "read_dec_files: Dont know how to do IIR filters (%s).\n",
										dec_stages[i]);
				return (0);
			}
			if (nnum < 1) {
				elog_notify(0, "read_dec_files: No numerator terms (%s).\n",
										dec_stages[i]);
				return (0);
			}
			ok=1;
			(*dec_fac) *= dec_factor;
		}
		if (!ok) {
			elog_notify(0, "read_dec_files: no fir stage on file '%s'.\n",
							dec_stages[i]);
			return (0);
		}
		for (j=0; j<n; j++) {
			Response_group *gpi;

			get_response_stage_type (rsp, j, string);
			if (strcmp(string, "fir")) continue;
			gpi = rsp->groups + j;
			if (copy_response_group (gpi, (resp), -1) < 0) {
				elog_notify(0, "read_dec_files: copy_response_group() error.\n");
				return (0);
			}
		}
		free_response (rsp);
	}
	get_response_nstages ((resp), &n);

	for (i=0; i<n; i++) {
		Response_group *gpi;
		int dec_factor, nnum, nden, n2;
		double srate;
		double *coefsi, *coefs_err;
		double *coefdi, *coefd_err;
		float *cfs;
		int *numb, *decf;

		decptr = (FIR_decimation *) malloc(sizeof(FIR_decimation));
		if(decptr == NULL)
		{
			elog_notify(0,"Cannot malloc decimation structure for stage %d\n",
					i);
			return(0);
		}

		gpi = (resp)->groups + i;
		get_response_stage_fir_ncoefs ((resp), i, &srate, &dec_factor, &nnum, &nden);
		get_response_stage_fir_coefs ((resp), i, &nnum, &coefsi, &coefs_err,
						&nden, &coefdi, &coefd_err);
		for (j=0; j<nnum/2; j++) if (coefsi[j] != coefsi[nnum-j-1]) break;
		if (j < nnum/2) {
			elog_notify(0, "read_dec_files: Can only do symetrical FIR filters.\n");
			return (0);
		}

		decptr->ncoefs = nnum;
		decptr->decfac = dec_factor;
		decptr->coefs = calloc(nnum,sizeof(float));
		if((decptr->coefs) == NULL)
			elog_die(0,"read_dec_files:  can't alloc filter coef array of length %d\n",nnum);
		for(j=0; j<nnum;j++) decptr->coefs[j] = coefsi[j];
		pushtbl(*decimators,decptr);
	}
	return (1);
}

/*decimate routine based on response file definitions 
read by read_dec_files (see below).  That is multiple stage
decimators of arbitrary complexity are implemented.  

arguments:  
		-----INPUTS -----
	dectbl - tbl of pointers to FIR_decimation structures
		to be applied to trace 
	in[nin] - input  vector of length nin of input trace samples 
	dt0 - sample interval of in
	t0 - start time of in
		-----OUTPUTS -----
	out - output vector of decimated trace.  IMPORTANT:  this
		vector is created by a call to calloc within this
		function.  DO NOT pass a pointer to prevously alloced
		space. 
	nout - actual length of out
	dt - sample interval of out
	t0out - start time of decimated trace, out.

Note this routine does not do partial convolutions at edges so the
returned start time t0out is never the same as t0 but will always be
somewhat later in time.  The same happens at the end time, but the 
truncation can be more complex depending on how multiple decimators
interact with number of samples.  In general, more generous padding 
will be needed on the endtime than for t0 because of rounding interaction
with the edges.  

Normal return for this function is the decimation factor (a positive number)
	negative numbers indicate an error.

	-999 = malloc error no output (calling program probably usually die.)

	-1, -2, ... (anything but -999) indicates a failure in the decimation
	at stage abs(return value). This easily occurs if the the requested
	decimation reaches a point where the output vector is less than one
	sample.  No valid data will be returned in this condition so the
	calling routine should trap this condition.


Author:  Gary Pavlis stealing generously from dbdec code by Danny Harvey
Written: July 21, 1998
*/
	 
	
int decimate_trace(Tbl *dectbl,float *in, int nin, double dt0, double t0,
		float **out, int *nout, double *dt, double *t0out)

{

	int i,nstages;
	float *buf, *buf2;
	FIR_decimation *d;
	int decfac;  /* current decimation factor */
	double deltat0=0.0;  /* accumulation of time offsets for edge 
			condition of FIR filter (i.e. we don't 
			do partial convolutions on edges) */
	double dt_this_stage;
	double si;  /* current sample interval (staged to larger values) */
	int n;  /* current number of samples */
	int ret_code;

	nstages = maxtbl(dectbl);
	buf = (float *) calloc (nin,sizeof(float));
	buf2 = (float *) calloc (nin,sizeof(float));

	if( (buf == NULL) || (buf2 == NULL) )
	{
		elog_notify(0,"decimate_trace:  cannot malloc buffers of length %d\n",
				nin);
		return(-999);
	}
	/* Note this routine works if nstages = 0 (empty tbl) */
	scopy(nin,in,1,buf,1);
	decfac = 1;
	si=dt0;
	n=nin;
	for (i=0; i<nstages; i++)
	{
		d = (FIR_decimation *) gettbl(dectbl,i);

		/* this computation of the time offset to the first
		decimated output sample assumes the FIR filter is 
		zero phase and symmetric.  The -1 may surprise
		you, but is correct and was verified by tests.
		It is the interval versus points problem. */

		decfac *= d->decfac;
		dt_this_stage = si*((double)((d->ncoefs)-1))/2.0;
		deltat0 += dt_this_stage;
		si *= (double)(d->decfac);

		if((d->decfac == 1) && (d->ncoefs == 0) )
		{
			/* Fall in this block for no decimation with
			zero length filter (the "none" case ) */
			scopy(n,buf,1,buf2,1);
			*nout = n;
		}
		else
		{
			if((d->ncoefs)>n)
			{
				free(buf);
				free(buf2);
				out = NULL;
				*nout = 0;
				return(-1);
			}	
			ret_code = sconv(buf,n,d->coefs,d->ncoefs,0,d->decfac,
					buf2,nout);

		/* This fragment should never really be executed, but better
		safe than sorry.  We don't issue a warning because the
		correction should be appropriate. */
			if(ret_code > 0)
			{
				double time_correction;
				time_correction = si*((double)ret_code);
				deltat0 += time_correction;
			}
		}
		/* this always works because nout <= nin */
		scopy(*nout,buf2,1,buf,1);
		n = *nout;
	}

	/* finally the output vector is created here */
	*out = (float *)calloc(*nout,sizeof(float));
	if(*out == NULL)
		elog_die(0,"decimate_trace:  cannot malloc output vector of length %d\n",
			*nout);
	scopy(*nout,buf2,1,*out,1);
	*t0out = t0 + deltat0;
	*dt = si;
	free(buf);
	free(buf2);
	return(decfac);
}
	
/* This routine takes the result or define_decimation and 
calls read_dec_files to build an array of Tbl pointers that
define the decimation stages for the multiwavelet transform
using the FIR_decimation objects. 

The routine takes the set of decimation file definitions
parsed with define_decimation, calls read_dec_files and 
builds an output array of Tbl pointers parallel to 
the input Tbl array (filelists).  This is assumed to 
be a setup routine and it will die if any problems are
encountered.  nbands is passed as the size of the 
filelist input Tbl array which will equal the length
of the output Tbl array.

Decimation factors in each band are returned in the 
int array decfac.  Routine blindly assumes decfac is long
enough to hold this information.  

Author:  Gary pavlis
Written:  July 24, 1998
*/ 


Tbl **build_decimation_objects(Tbl **filelists, int nbands, int *decfac)
{
	int i;
	Tbl **fir;
	int ret_code;
	int dec_this_stage,dec_previous;

	fir = (Tbl **)calloc(nbands,sizeof(Tbl *));
	if(fir == NULL) elog_die(0,"build_decimation_objects:  cannot alloc pointer array of length %d\n",nbands);

	for(i=0,dec_previous=1;i<nbands;i++)
	{
		fir[i] = newtbl(0);
		ret_code = read_dec_files(filelists[i],&dec_this_stage,
				(fir+i));
		if(ret_code == 0) 
			elog_die(0,"fatal: read_dec_files returned error code %d\n",
				ret_code);	
		decfac[i] = dec_this_stage*dec_previous;
		dec_previous = decfac[i];
	}
	return(fir);		
}
/* Free routine for FIR_decimation strutures */
void free_decimation(FIR_decimation *d)
{
	free(d->coefs);
	free(d);
}
