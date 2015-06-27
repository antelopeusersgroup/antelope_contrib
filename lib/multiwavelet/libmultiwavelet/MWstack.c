#include "multiwavelet.h"
#include "perf.h"
/* constructor and destructor for MWstack object */
MWstack *create_MWstack(int nwavelets,int nchan,int nt)
{
	MWstack *s;
	int i;

	allot(MWstack *,s,1);

	s->nwavelets = nwavelets;   s->nchan = nchan;  s->nt = nt;

	allot(complex **,s->amp,nwavelets);
	for(i=0;i<nwavelets;++i) allot(complex *,s->amp[i],nchan);

	allot(double *,s->weights,nchan);
	allot(double *,s->timeweight,nt);
	allot(complex **,s->z,nwavelets);
	allot(double *,s->coherence,nt);
	for(i=0;i<nwavelets;++i) allot(complex *,s->z[i],nt);

	s->timeweight_applied = 0;  /* default is off */
	s->stack_is_valid = 0; /* have to turn this on to make it valid */
	return(s);
}
void destroy_MWstack(MWstack *s)
{
	int i;
	for(i=0;i<s->nwavelets;++i)
	{
		free(s->amp[i]);
		free(s->z[i]);
	}
	free(s->amp);
	free(s->coherence);
	free(s->z);
	free(s->weights);
	free(s->timeweight);
	free(s);
}
/* Normalizes a complex vector to have an L2 norm of 1.0 */
void normalize_stack(complex *z,int nz)
{
	complex cmag;
	cmag = cdotc(nz,z,1,z,1);
	cmag.r = (float)(1.0/sqrt((double)cmag.r));
#ifdef SUNPERF6
	cscal(nz,cmag,z,1);
#else
	cscal(nz,&cmag,z,1);
#endif
}
/* This function extracts a time subset of a MWstack object defined
by the Time_Window function passed to it.  It copies the subset of
the stack requested and returns a new MWstack object with the 
ancillary parameters that define it correctly set.  REturns a
NULL pointer if the time window requested does not overlap the
input stack data at all.  If the input data span a smaller range
than the requested window a diagnostic is issue and the object
will be trimmed to match what is possible.  

Arguments:
	instack - input MWstack object
	win - Time_Window defining region to be extractged 
		(only the start and end parameters are used)

Author:  Gary Pavlis
Written:  December 2001
*/
MWstack *MWextract_stack_window(MWstack *instack,Time_Window *win)
{
	int i,j,k,kk,its,ite;
	int tstart, tend;  /* start and end values(in samples) actually used */
	int nused;
	int lag;
	char *message="MWextract_stack_window:  requested window is inconsistent with the stack window";

	MWstack *outstack;
	int nwavelets,nchan;

	nwavelets = instack->nwavelets;
	nchan = instack->nchan;

	its = nint((instack->tstart)/(instack->dt));
	ite = nint((instack->tend)/(instack->dt));
	if(((win->tstart)>=ite) || ((win->tend)<=its) )
	{
		elog_complain(0,"%s\nStack window is %d to %d samples;  Requested %d to %d\n",
			message,its, ite, win->tstart, win->tend);
		return(NULL);
	}
	else if( ((win->tstart)<its) && ((win->tend)<=ite))
	{
		tstart = its;
		tend = win->tend;
		elog_complain(0,"%s\nStack window is %d to %d samples;  Requested %d to %d\nReset to %d to %d\n",
			message,its, ite, win->tstart, win->tend,tstart,tend);
		
	}
	else if( ((win->tstart)>=its) && ((win->tend)>ite))
	{
		tstart = win->tstart;
		tend = ite;
		elog_complain(0,"%s\nStack window is %d to %d samples;  Requested %d to %d\nReset to %d to %d\n",
			message,its, ite, win->tstart, win->tend,tstart,tend);
		
	}
	else
	{
		tstart = win->tstart;
		tend = win->tend;
	}
	lag = tstart - its;
	nused = tend - tstart + 1;
	outstack = create_MWstack(nwavelets,nchan,nused);
	outstack->dt = instack->dt;
	outstack->tstart = instack->tstart + ((double)lag)*instack->dt;
	outstack->tend = outstack->tstart + ((double)(nused-1))*instack->dt;
	outstack->dt = instack->dt;
	for(i=0;i<nwavelets;++i)
		for(j=0;j<nchan;++j)
		{
			outstack->amp[i][j].r = instack->amp[i][j].r;
			outstack->amp[i][j].i = instack->amp[i][j].i;
		}
	for(j=0;j<nchan;++j) outstack->weights[j]=instack->weights[j];
	if(instack->timeweight_applied)
	{
		elog_notify(0,"MWextract_subset_window:  existing stack has a time weight function already applied\nResults may be biased\n");
		for(k=0,kk=lag;k<nused;++k,++kk) 
			outstack->timeweight[k]=instack->timeweight[kk];
	}
	else
		for(k=0;k<nused;++k) outstack->timeweight[k]=1.0;
	outstack->stack_is_valid = 1;
	for(i=0;i<nwavelets;++i)
		ccopy(nused,instack->z[i]+lag,1,outstack->z[i],1);
	return(outstack);
}
/* this function sets the timeweight vector of an MWstack object to 
a trapezoidal weight function defined by the time points ts0, ts1,
te1, te0.  These are supposed to be mnemonic for 
ts0 = start time at 0 weight
ts1 = start time for full (1.0) weight
te1 = time of end of full weight 
te0 = time of absolute end (zero weight from then on.

A linear ramp is applied between ts0 and ts1 and from te1 to te0.
The actual algorithm uses a general 1d interpolator to define
the weight function based on this suite of points.  

Note this function does not apply the time window, it only computes it.
The apply function below does that.

Author:  Gary Pavlis
Written:  December 2001
*/
double *MWstack_set_trapezoidal_window(double t0,double dt,int nt,
	double ts0, double ts1, double te1, double te0)
{
	double x[4],y[4];
	int i,ierr;
	double *timeweight;

	allot(double *,timeweight,nt);
	x[0]=ts0;
	y[0]=0.0;
	x[1]=ts1;
	y[1]=1.0;
	x[2]=te1;
	y[2]=1.0;
	x[3]=te0;
	y[3]=0.0;

	ierr = irregular_to_regular_interpolate(x,y,4,
		timeweight,t0,dt,nt);

	if(ierr<0)
	{
		elog_complain(0,"MWstack_set_trapezoidal_window:  window setup failed\nReverting to uniform time weights\n");
		for(i=0;i<nt;++i) timeweight[i]=1.0;
	}
	return(timeweight);
}
/* Applies a timeweight function to an MWstack object.  Warning, this cannot
normally be undone.  The only argument is a pointer to the MWstack object.
*/
void MWstack_apply_timeweight(MWstack *s)
{
	int i,j;

	if(s->timeweight_applied) 
		elog_notify(0,"MWstack_apply_timeweight:  warning applying time weights to a MWstack object with the apply bit already set\nResult may be distorted\n");
	for(i=0;i<s->nwavelets;++i)
		for(j=0;j<s->nt;++j)
		{
			s->z[i][j].r *= (float)(s->timeweight[j]);
			s->z[i][j].i *= (float)(s->timeweight[j]);
		}
	/* We also have to normalized the new stack */
	for(i=0;i<s->nwavelets;++i)
		normalize_stack(s->z[i],s->nt);
}
	
	
/* special function to 2D slice of 3D matrix z for "wavelet" element of
the work space.  Result is copied to a fortran like output vector, zwork,
with the individual channel results in the rows of the output matrix.
An offset of ts samples from the beginning is given and nt_used samples
are blindly copied to the output matrix.  No error checking is made in
this function as it just assumes this was done earlier.  Uses sunperf
C interface to BLAS functions.  Note z[][][] is assumed to be indexed
left to right as:  wavelet number, channel, timelag.

This function is a little silly because it is so short, but it is repeatedly
and has some pretty obscure indexing.  I hope this clarifies rather than
confuses the algorithm.

Author:  Gary Pavlis
Written: December 2001
*/
void MWcopy_one_wavelet_matrix(complex ***z,int wavelet,complex *zwork,
			int nchan, int ts, int nt_used)
{
	int i;

	for(i=0;i<nchan;++i)
		ccopy(nt_used,(z[wavelet][i])+ts,1,zwork+i,nchan);
}
/* This function applies a time weight function across the matrix built by
the function immediately above.  That is, it scales the ith column of z by
w[i].  
*/
void MWapply_time_window(complex *z,
		int n1z,
			int nrows, 
				int ncol,
					double *w)
{
	complex cweight;
	int i;
	
	cweight.i = 0.0;
	for(i=0;i<ncol;++i)
	{
		cweight.r = w[i];
#ifdef SUNPERF6
		cscal(nrows,cweight,z+i*n1z,1);
#else
		cscal(nrows,&cweight,z+i*n1z,1);
#endif
	}
}
		

/* calculated median stack of the columns of the complex matrix z.
That is, for each column of the input matrix we compute the median of
each of the nchan samples.  The results are returned in the nt length
output vector o.  Note that z is a fortran style array with "leading
dimension of n1z".  

Author:  G Pavlis
Written:  December 2001
*/
void MWmedian_stack_complex(complex *z,int n1z,int nchan,int nt,complex *o)
{
	int i,j;
	float *work;
	MW_scalar_statistics stats;

	allot(float *,work,nchan);

	for(i=0;i<nt;++i)
	{
		for(j=0;j<nchan;++j)work[j]=z[i*n1z+j].r;
		stats = MW_calc_statistics_float(work,nchan);
		o[i].r = stats.median;
		for(j=0;j<nchan;++j)work[j]=z[i*n1z+j].i;
		stats = MW_calc_statistics_float(work,nchan);
		o[i].i = stats.median;
	}		
	free(work);
}
#define CONVERGENCE_RATIO 0.0001
#define MAXIT 30

/* This function produces a robust stack of an ensemble of multiwavelet
transformed signals stored in the input array z.  The algorithm used here
is novel, or perhaps more appropriately called experimental.  It was
written to replace the principal component method used in the Bear and
Pavlis paper for computing phased array stacks.  That method repeatly 
showed instability problems that hard to track.  Entirely possible 
this was only a buggy program, but the very concept of the pc method
seemed inherently unstable to me.  Furthermore, it clearly would not
work right in an extension to correlation of data from clusters of
events (source arrays) because the size of signals in that situation
can vary by orders of magnitude.  

This algorithm uses a different approach that is like M-estimators
but with the loss function that does not require a statistical model.
I don't know the proper term for this approach, but I'm sure there
is one in the mathematical literature.  i.e. I'm fairly sure is a
reinvention of a concept used elsewhere.  Haven't found it yet is all.
Key elements of the algorithm are:
1.  We form the solution as a weighted stack of an ensemble of complex
valued traces passed through the 3D z array of complex numbers.
2.  The initial solution is a median stack.
3.  The parent loss function used to compute the weights has not
been derived, but the solution is based on weights computed as follows:
             |  1/(residual amp)   if residual amp > noise level
w_channel =  |
             |  1/noise            otherwise

This is something like weighting each element by coherence, but it
is normalized differently and made more aggressive by the reciprocal
relationship.  Because the traces are not normalized prior to the
stack this weighting is effectively an approximate weighting by signal
to noise ratio if the underlying signals to be stacked are perfectly coherent.
However, when there is a large misfit due to any reason the 1/residual 
form will dominate and a signal gets downweighted quickly.  
4.  The MWstack object returns a set of complex amplitudes coefficients.
The modulus of these numbers are the relative amplitude numbers for
each channel and the phase can be used to estimate a time shift as
described in the Bear and Pavlis papers.  These factors are computed
as a complex dot product between the final stack and each of the parent,
multiwavelet transformed signals.  Nonparametric statistics can be applied
to these estimates to estimate uncertainties in the relative amplitudes
and timing.  
5.  The algorith allows one to pass in channel weights.  This can be used
to produce variable array apertures with frequency to compensate for 
coherence loss with the spatial separation of receivers.  This vector
should be all ones if one does not wish to use this feature.  

Arguments:
	z - 3D C array of multiwavelet transform data to be stacked. 
		z[i][j][k] is assumed to be the kth time sample from 
		channel j of wavelet number i.  
	nwavelet - number of multiwavelets in transform
	nchan - number of channels to be stacked
	nt - number of time samples to stack 
	NOTE:  z is thus assumed of size z[nwavelet][nchan][nt]
	t0 - relative time of sample 0 of input matrices.
	dt - dample interval 
	chanweight - base channel weight.  Always used to scale 
		each channel in the stack, but ignored for statistical
		estimates.
	timeweight - vector of length nt of time window weights
		to be applied (input) in computing stack
	noise - nwavelet by nchan C matrix of estimate of single sample
		noise level for each wavelet and each channel.  
Author:  Gary L. Pavlis
Written:  December 2001
*/
MWstack *MWrobuststack(complex ***z,
		int nwavelets,
		int nchan,
		int nt,
		double t0,
		double dt,
		double *chanweight,
		double *timeweight,
		double **noise)
{
	MWstack *s;
	int ts,te;  /* index of first and last nonzero element of timeweight */
	int nt_used;
	int i,j,k,l;
	/* These are matrix work spaces.  All are created as 1d vectors with
	virtual indexing ala fortran.  Sizes are as noted*/
	complex *zwork, *residuals;  /* per wavelet workspace:  nwavelet X nt_used*/
	double **amps;  /*scalar amplitude factors nwavelets x nchan */
	complex *window_stacks;  /* accumulation space for stack with time weights applied 
				size is nt_used by nwavelets (note transpose from others) */
	complex *wstack_last;  /* window_stacks from previous iteration */
	/* vector work spaces */
	double *resnorms;  /* Used to accumulate residual norms across all wavelets */
	complex *stack;  /* Used to accumulate full stack without windowing -- copied to
				MWstack object */
	double *reswt;  /* working vector of robust weights derived from residuals */
	complex cscale;
	double sum_column_weights,sum_chan_weights;  /* required normalizations */
	double weight;
	complex cwork;
	double cmag;
	int iteration;
	double dzmod,ctest;

	/* We scan for the first and last number in timeweight that are nonzero.
	We need to use a reduced workspace to build stack to avoid biasing statistics
	with 0 weight samples */
	for(ts=0;ts<nt;++ts) if(timeweight[ts]!= 0.0) break;
	if(ts>=nt)
	{
		elog_complain(0,"Invalid time weight function--all zeros\n");
		return(NULL);
	}
	/* we don't need to test from the reverse direction, although we could
	end up with as few as 1 nonzero weight */
	for(te=nt-1;te>=0;--te) if(timeweight[te]!=0.0) break;
	nt_used = te-ts+1;
	for(i=ts,sum_column_weights=0.0;i<=te;++i)
			sum_column_weights += fabs(timeweight[i]);

	s = create_MWstack(nwavelets,nchan,nt);
	s->tstart = t0;
	s->dt = dt;
	s->tend = t0 + dt*((double)(nt-1));
	for(i=0;i<nt;++i)s->timeweight[i]=timeweight[i];
	/* work spaces */
	allot(complex *,zwork,nchan*nt_used);
	allot(complex *,residuals,nchan*nt_used);
	allot(double **,amps,nwavelets);
	for(i=0;i<nwavelets;++i) allot(double *,amps[i],nchan);
	allot(double *,resnorms,nchan);
	allot(complex *,window_stacks,nchan*nt_used);
	allot(complex *,wstack_last,nchan*nt_used);
	allot(complex *,stack,nt);
	allot(double *,reswt,nchan);

	/* Use a median stack as a starting point.  We don't apply channel
	weights here as it will bias the results */
	for(i=0;i<nwavelets;++i)
	{
		MWcopy_one_wavelet_matrix(z,i,zwork,nchan,ts,nt_used);
		MWapply_time_window(zwork,nchan,nchan,nt_used,timeweight+ts);
		MWmedian_stack_complex(zwork,nchan,nchan,nt_used,window_stacks+i*nt_used);
		/* cdotc of a complex vector with itself gives the L2 norm squared.
		We use this as a measure of raw signal strength.  It only needs
		to be computed once. Note sqrt and normalization by window weights*/
		for(j=0;j<nchan;++j)
		{
		    	cwork = cdotc(nt_used,zwork+j,nchan,zwork+j,nchan);
				/* cwork will only have a nonzero real part that is the sume
				of the moduli of all the components of the zwork vector */
				amps[i][j]=sqrt((double)cwork.r);
				amps[i][j] /= sum_column_weights;
		}
		/* We normalize the stacks to unit L2 norm because otherwise computing
		relative amplitudes gets problematic. */
		normalize_stack(window_stacks+i*nt_used,nt_used);
	}
	iteration = 0;
	do
	{
		/* We can do this because we packed this matrix into continguous block.
		It saves the last stack value for comparison at the bottom of the loop. */
		ccopy(nt_used*nwavelets,window_stacks,1,wstack_last,1);

		/* We first have to accumulate residuals for all channels across all 
		wavelets for the current stack.  */
		for(j=0;j<nchan;++j)
		{
			resnorms[j]=0.0;
			reswt[j]=0.0;
		}
		for(i=0;i<nwavelets;++i)
		{
			MWcopy_one_wavelet_matrix(z,i,zwork,nchan,ts,nt_used);
			MWapply_time_window(zwork,nchan,nchan,nt_used,timeweight+ts);
			for(j=0;j<nchan;++j)
			{
				ccopy(nt_used,zwork+j,nchan,residuals+j,nchan);
				if(chanweight[j]!=1.0)
				{
					cscale.r = chanweight[j];
					cscale.i = 0.0;
#ifdef SUNPERF6
					cscal(nt_used,cscale,residuals+j,nchan);
#else
					cscal(nt_used,&cscale,residuals+j,nchan);
#endif
				}
				/* This complex pointer stuff happens because of the use of
				implicit fortran type matrices in contiguous blocks. The
				cdotc call computes the dot product between the current 
				stack and data.  caxpy call computes the residual derived by
				computing difference between data and this scaled version of 
				the stack. */
				s->amp[i][j] = cdotc(nt_used,zwork+j,nchan,window_stacks+i*nt_used,1);
				cwork.r =  -(s->amp[i][j].r);
				cwork.i =  -(s->amp[i][j].i);
				cmag=hypot((double)cwork.r,(double)cwork.i);
#ifdef SUNPERF6
				caxpy(nt_used,cwork,window_stacks+i*nt_used,1,
						residuals+j,nchan);
#else
				caxpy(nt_used,&cwork,window_stacks+i*nt_used,1,
						residuals+j,nchan);
#endif
		   		if(chanweight[j]>0.0)
		   		{
					cwork = cdotc(nt_used,residuals+j,nchan,
							residuals+j,nchan);
					resnorms[i] += sqrt((double)(cwork.r));
				}
			}
		}
		for(i=0;i<nwavelets;++i)
		{
		    resnorms[i] /= sum_column_weights;
		  /* When chanweight is 0 the associated row is 0.0 so computing residuals
		  is not only pointless, but could cause inf errors */
		    for(j=0;j<nchan;++j)
		    {
		   	if(chanweight[j]==0.0)
				reswt[j] = 0.0;
			else
			{
				if(resnorms[i]>noise[i][j])
					reswt[j] += amps[i][j]/resnorms[i];
				else
					reswt[j] += amps[i][j]/noise[i][j];
			}
			reswt[j] /= ((double)nwavelets);
		    }
		}
		/* we now form weighted stacks */
		cwork.i = 0.0;
		for(i=0;i<nwavelets;++i)
		{
			for(k=0;k<nt;++k) 
			{
				s->z[i][k].r = 0.0;
				s->z[i][k].i = 0.0;
			}
			for(j=0,sum_chan_weights=0.0;j<nchan;++j)
			{
				weight = reswt[j]*chanweight[j];
				s->weights[j]=weight;
				sum_chan_weights += weight;
				cwork.r = weight;
#ifdef SUNPERF6
				caxpy(nt,cwork,z[i][j],1,s->z[i],1);
#else
				caxpy(nt,&cwork,z[i][j],1,s->z[i],1);
#endif
			}
			cwork.r = 1.0/sum_chan_weights;
#ifdef SUNPERF6
			cscal(nt,cwork,s->z[i],1);
#else
			cscal(nt,&cwork,s->z[i],1);
#endif
			/* That is the full window stack, when we loop back we
			use the windowed version so we have to form it.  I 
			use explicit indexing here to make this clearer rather
			than use the the BLAS for something this simple to compute.
			The indexing is ugly enough this way.*/
			for(k=ts,l=0;k<=te;++k,++l)
			{
				window_stacks[i*nt_used + l].r = (s->z[i][k].r)*timeweight[k];
				window_stacks[i*nt_used + l].i = (s->z[i][k].i)*timeweight[k];
			}
			normalize_stack(window_stacks+i*nt_used,nt_used);
		}
		/* convergence is defined by ratio of correction vector to norm of stack
		across all wavelets.  The stacks are normalized in each pass to unit norm
		so this becomes simply the norm of the correction vector.  This 
		crude loop works because we assume the window_stack matrices are in a 
		packed storage arrangement and can be dealt with as one long vector  */
		for(l=0,ctest=0.0;l<nt_used*nwavelets;++l)
		{
			dzmod = hypot((double)(window_stacks[l].r - wstack_last[l].r),
						(double)(window_stacks[l].i - wstack_last[l].i));
			ctest += dzmod*dzmod;
		}
		ctest /= ((double)((cwork.r)*(cwork.r)));
		++iteration;
	} while ( (ctest>CONVERGENCE_RATIO) && (iteration<=MAXIT));
	/* We perhaps should recompute the amp factors before exiting, but if the
	algorithm converges properly this should not be an issue.  */
	if(iteration>MAXIT)
	{
		elog_complain(0,"MWrobuststack:  iterative loop did not converge\n");
		s->stack_is_valid=0;
	}
	else
		s->stack_is_valid=1;
	free(zwork);
	free(residuals);
	for(i=0;i<nwavelets;++i)free(amps[i]);
	free(amps);
	free(resnorms);
	free(window_stacks);
	free(wstack_last);
	free(stack);
	free(reswt);
	return(s);
}
