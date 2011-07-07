#include <stdlib.h>
#include <stdio.h>
#include <perf.h>

#include "stock.h"
#include "pf.h"
#include "response.h"
#include "multiwavelet.h"
/* Straightforward parameter file input of wavelet data defining
a set of transforms.  Returns number of complex wavelets
defined.  Functions themselves are returned throught the mwbasis
argument*/

MWbasis  *load_multiwavelets_pf(Pf *pf,int *nwavelets)
{
	int i,j,k;
	char *line;
	double f0,fw;
	int nsamples;
	Tbl *w;
	MWbasis *mw;
	int nrows;

	nsamples = pfget_int(pf,"nsamples");
	*nwavelets =  pfget_int(pf,"nwavelets");
	f0 = pfget_double(pf,"f0");
	fw = pfget_double(pf,"fw");
	w = pfget_tbl(pf,"wavelets");
	nrows = maxtbl(w);
	if(nrows != (nsamples*(*nwavelets)))
		elog_die(0,"Size mismatch in wavelet parameter file\nExpected %d from %d wavelets each %d long\nFound %d instead\n",
			nsamples*(*nwavelets),*nwavelets,nsamples,nrows);
	mw = (MWbasis *)calloc(*nwavelets, sizeof(MWbasis));
	if(mw == NULL)
		elog_die(0,"Cannot alloc %d multiwavelet structures\n",nwavelets);
	for(i=0,k=0;i<(*nwavelets);++i)
	{
		mw[i].r = calloc(nsamples,sizeof(float));
		mw[i].i = calloc(nsamples,sizeof(float));
		if((mw[i].r == NULL) || (mw[i].i == NULL) )
			elog_die(0,"Cannot alloc %d samples for complex wavelet %d\n",
				nsamples,i);
		mw[i].n = nsamples;
		mw[i].f0 = f0;
		mw[i].fw = fw;
		for(j=0;j<nsamples;j++,k++)
		{
			line = gettbl(w,k);
			if(sscanf(line,"%g%g",&(mw[i].r[j]),&(mw[i].i[j])) != 2)
				elog_die(0,"Error reading wavelets from parameter file on line %d of wavelet tbl\n",
					k);
		}
	}
	return(mw);
}
/* This is an alloc routine to hold matrix of MWtrace objects.
This is a simple generalization of the matrix alloc routines 
in numerical recipes. */
MWtrace **MWmatrix(int nrl, int nrh, int ncl,int nch)
{
        int i;
	MWtrace **t;
	t = (MWtrace **)calloc(nrh-nrl+1,sizeof(MWtrace *));
        if(t == NULL) 
        {
                elog_log(1,"MWmatrix:  Cannot alloc MWmatrix vector of pointers\n");
                return (NULL);
        }
        t-=nrl;
        for(i=nrl;i<=nrh;i++)
        {
                t[i] = (MWtrace *) calloc(nch-ncl+1,sizeof(MWtrace));
                if(t[i] == NULL)
                {
                        elog_log(1,"MWmatrix:  Cannot alloc row %d of matrix of length %d\n",
                                i,nch-ncl+1);
                        /* cleanup in case you want to continue*/
                        if(i!=nrl)
                        {
                                int j;
                                for(j=nrl;j<i;++j) free(t[j]);
                        }
                        return (NULL);
                }
                t[i]-=ncl;
        }
        return (t);
}
/* Free routine companion to MWmatrix */
void free_MWtrace_matrix(MWtrace **t,int nrl, int nrh, int ncl,int nch)
{
	int i,j;
	for(i=nrh;i>=nrl;i--)
	{
		/* The nz test avoids trying to clear null
		entries.  the transform routine leaves nz 0 
		to signal no contents */
		for(j=nch;j>=ncl;j--)  
			if((t[i][j].nz)>0) free(t[i][j].z);
		free((char *)(t[i]+ncl));
	}
	free((char *)(t+nrl));
}

/* This is the main multiwavelet transform routine.  It processes
multiple bands by applying specified decimation fir filters to
the input from one stage to the next.  The same basis 
function are applied in each band, but successive bands always
merge to successive lower frequencies because of decimation.  
Actual center frequencies and bandwidths are computing from
the nondimensional numbers stored with the basis functions using
a Nyquist scaling.    

Arguments 
	trace - actual samples of trace to be processed.
	dt - sample interval of input
	starttime - epoch time of start of trace
	nsamples - length of trace
	MWbasis - vector of MWbasis objects used to define
		multiple wavelets
	nbasis - number of elements in MWbasis
	decimators - this is assumed to be a vector of Tbl object pointers
		to decimation definitions.  That is, decimators[i] is
		a Tbl of css response files defining decimations to 
		be applied to this band from the previous band
		(i.e. decimators[i-1]) to produce this working band.  
		Normally one expects the first band to have an 
		an empty Tbl structure, but it does not have to be.  
	nbands  - length of decimators vector of pointers.

The function returns a matrix of MWtrace objects.  Individual wavelets
results are in the columns and individual bands are in the rows of this
matrix.  

The only fatal errors in this routine are malloc errors.

It can easily happen that the transform may not be fully completed 
according to the specifications passed to it.  Both the decimation
and the multiwavelets are fir filters implemented by convolution.
I choose to not allow incomplete convolutions so the input trace 
become progressively trimmed on the left and right because the use
of zero phase fir filters reduces the trace length by nfilter_samples
for each convolution (half on the left and half on the right).  
This function traps this condition and sets the variable nz
(length of the output trace) to zero when this happens.  The
user should not attempt to free the variable variable "z" in
the MWtrace structure when nz=0 as no space will have been alloced
under this condition.  The rule is don't look at any variable in
the MWtrace structure until first testing the value of nz.  When it
is zero, do not process that object in any way.

Author:  Gary Pavlis
Written:  July 22, 1998
*/



MWtrace **MWtransform(float *trace, double dt, double starttime, int nsamples,
		 MWbasis *basis, int nbasis, Tbl **decimators, int nbands)  
{
	complex *z;  /* working complex vector */
	float *newtrace,*work,*re_work, *im_work;  /* trace data work spaces */
	MWtrace **allmw;  /* working matrix variable */
	int i,j;
	int ii,k;
	double stime,etime;  /* start end times of working trace */
	int n;  /* samples in current trace */
	double dtnew;  /* sample interval of current band */
	double dtprevious,stprevious;
	int decfac=1, dec_this_stage=1;  /* total and current decimation
					factor respectively */
	int nout,n_this_band;

	/* Create the workspace matrix */
	allmw = MWmatrix(0,nbands-1,0,nbasis-1);

	work = (float *)calloc(nsamples,sizeof(float));
	if(work == NULL)
		elog_die(0,"MWtranform:  cannot alloc work array of length %d\n",
			nsamples);


	/* We copy trace to a work space where it will be successively
	decimated */
	scopy(nsamples,trace,1,work,1);

	/* Now work through adjacent bands */
	for(i=0,dtprevious=dt,stprevious=starttime,n_this_band=nsamples;i<nbands;++i)
	{
		/*An empty tbl defines no decimation, so returning
		0 will cause this block to be skipped */
		if(maxtbl(decimators[i]))
		{
			/* newtrace is alloced in decimate_trace and needs to
			be freed later.  See free logic below */
			dec_this_stage = decimate_trace(decimators[i],
					work,n_this_band,dtprevious,stprevious,
					&newtrace,&n,&dtnew,&stime);
			if((dec_this_stage < 0) || (n< basis[i].n))
			{
			/* either of these conditions indicate we cannot
			continue the transform to lower frequency bands.
			We signal this in the return by setting nz 
			to zero, so calling programs must trap this 
			condition.  The second would probably normally
			occur before the first, but either is possible*/
				elog_notify(0,"MWtransform:  transform truncated in band %d\nInsufficient trace length for specified transform\nLower frequency bands will not be computed\n",i);
				for(ii=i;ii<nbands;++ii)
					for(j=0;j<nbasis;++j)
					{
						allmw[ii][j].nz = 0;
						allmw[ii][j].z = NULL;
					}
				free(newtrace);
				free(work);
				return(allmw);
			}
			decfac *= dec_this_stage;
		}
			
		/* Now we convolve each wavelet basis function
		with the trace data held in the newtrace vector and
		build the complete set of traces for this band */
		for(j=0;j<nbasis;++j)
		{
			allmw[i][j].dt = dtnew;
			allmw[i][j].dt0 = dt;
			allmw[i][j].decimation_factor = decfac;
			allmw[i][j].basis = (basis+j);
			/* Basis function f0 and fw are nondimensional
			and become physical units by using the 
			Nyquist for this sample rate */
			allmw[i][j].f0 = (basis[j].f0)/(2.0*dtnew);
			allmw[i][j].fw = (basis[j].fw)/(2.0*dtnew);

			/* We assume the wavelets are zero phase
			filters so we skew the start time by the 
			time of the middle of the basis function.
			The -1 is the infamous interval versus points
			problem. It is correct, believe me.   */
			allmw[i][j].starttime = stime
				+ ((double)((basis[j].n)-1)*dtnew/2.0);
				
			re_work = (float *)calloc(n,sizeof(float));
			im_work = (float *)calloc(n,sizeof(float));
			if( (re_work == NULL) || (im_work == NULL))
				elog_die(0,"MWtransform:  malloc fails for two work spaces of length %d\n",n);
			/* both real and imag parts of basis must the same length so 
			we overwrite nout the second call below and don't trap this 
			condition */
			if((sconv(newtrace,n,basis[j].r,basis[j].n,0,1,re_work,&nout) < 0)
				|| (sconv(newtrace,n,basis[j].i,basis[j].n,0,1,im_work,&nout)<0))
			{
				elog_log(0,"Multiwavelet transform for wavelet %d in band %d has zero length\n",
						j,i);
				allmw[i][j].nz = 0;
			}
			else
			{
				allot(complex *,z,nout);
				for(k=0;k<nout;++k)
				{
					z[k].r = re_work[k];
					z[k].i = im_work[k];
				}
				allmw[i][j].z = z;
				allmw[i][j].endtime = allmw[i][j].starttime
					+ dtnew*((double)nout);
				allmw[i][j].nz = nout;
			}
			free(re_work);
			free(im_work);
		}
		dtprevious = dtnew;
		stprevious = stime;
		/* This is weird, but it basically clears the workspace, and the
		defines the contents of newtrace to be the same as work.  This is
		how we do the staged decimation as newtrace contains the decimated
		data from the previous stage. */
		free(work);
		work = newtrace;
		n_this_band = n;
	}
	free(newtrace);
	return(allmw);
}
