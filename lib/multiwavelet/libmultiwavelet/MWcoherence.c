#include "multiwavelet.h"
/* This function computes a matrix of coherence of an ensemble
of multiwavelet transformed traces stored in the 3D array z 
relative to the stack stored in the MWstack object stack. 
It also computes an average beam coherence as a weighted sum
(weights from the MWstack object are used) of the single 
station elements of the coherence (not a simple average). 

The stack is taken from the nwavelet by nt matrix stored under
stack->z.  The matrix z is assumed to be nwavelet by nchan by nt
and represent the aligned, multitransformed data.  The stack is 
scaled by the amplitude factors stored in the stack object before
being applied to compute residuals.  Note this include the phase
factor.

The function computes and writes to a nchan by nt matrix of coherence values.
Memory for this matrix is allocated here and needs to be released
late to avoid a memory leak.  Since the matrix is created in 
proper C fashion note a loop of frees will be required to release
the space.

Arguments:
	stack - MWstack object used as a basis for computing coherence
	z - 3D matrix described above of multiwavelet transformed data.
	coherence - nchan by nt C matrix to hold coherence estimates.
		Note this MUST have been allocated as a 2d array prior
		to calling this function or a seg fault is guaranteed.
	cohb - nt vector to hold beam coherence estimates.  cohb is
		formed as a weighted sum of station and residual powers
		with the weighting on a per channel basis.

Normal return is 0.  If results cannot be computed the function returns -1.
In that case an error message is left on the elog
stack and the calling program must trap this error condition.  The 
contents of coherence and cohb will be unaltered in these error conditions. 
*/
int MWcoherence(MWstack *stack,complex ***z,double **coherence,double *cohb)
{
        int i,j,k;
        complex **residuals;
	complex cwork;
	double z2,r2;
	double *z2b,*r2b;  /* accumulated sums for beam */
	/* These are copies of size variables from stack used
	to make this more readable. */
	int nt, nwavelets, nchan;
	double *relative_amp,sumamp,sumwt,avgamp;  /* These are needed to normalize amplitudes. */

	if(!stack->stack_is_valid) 
	{
		elog_notify(0,"MWcoherence:  stack is flagged as invalid\nCannot compute coherence\n");
		return(-1);
	}
	if(stack->timeweight_applied)
	{
		elog_notify(0,"MWcoherence:  improper usage\nTime weight function has already been applied to the stack--This makes computing coherence impossible\n");
		return(-1);
	}

	nt = stack->nt;   
	nwavelets=stack->nwavelets;
	nchan=stack->nchan;
	/* We work our way through the ensemble station by 
	station.  We store the residuals for wavelet in the
	nwavelet by nt matrix residuals.  So, create it.*/
	allot(complex **,residuals,nwavelets);
	for(i=0;i<nwavelets;++i) allot(complex *,residuals[i],nt);
	allot(double *,z2b,nt);
	allot(double *,r2b,nt);
	allot(double *,relative_amp,nchan);

	/* We need to compute relative amplitude scale factors to scale the beam consistently with
	the stack.  We use a weighted stack with the weights fetched from those stored with
	the stack.  This should be consistent with the method used the MWrobust_stack algorithm.
	It should also work correctly for both receiver and source array stacks. */
	for(j=0,sumwt=0.0,sumamp=0.0;j<nchan;++j)
	{
		for(i=0;i<nwavelets;++i)
		{
			sumamp += (stack->weights[j])
				   *hypot((double)(stack->amp[i][j].r),
					(double)(stack->amp[i][j].i));
			sumwt += (stack->weights[j]);
		}
	}
	avgamp = sumamp/sumwt;
	for(j=0;j<nchan;++j) 
	{
		for(i=0,relative_amp[j]=0.0;i<nwavelets;++i)
		{
			relative_amp[j] += hypot((double)(stack->amp[i][j].r),
                                        (double)(stack->amp[i][j].i));
		}
		relative_amp[j] /= ((double)nwavelets);
		relative_amp[j] /= avgamp;
	}

	/* We need to initialize these buffers */
	for(k=0;k<nt;++k) 
	{
		z2b[k]=0.0;
		r2b[k]=0.0;
	}

	/* top of main loop */
	cwork.i=0.0;
	for(j=0;j<nchan;++j)
	{
		for(i=0;i<nwavelets;++i)
		{
			/* note imaginary part is set to zero above */
			cwork.r = -relative_amp[j];
			ccopy(nt,z[i][j],1,residuals[i],1);
#ifdef SUNPERF6
			caxpy(nt,cwork,stack->z[i],1,residuals[i],1);
#else
			caxpy(nt,&cwork,stack->z[i],1,residuals[i],1);
#endif
		}
		for(k=0;k<nt;++k)
		{
			for(i=0,z2=0.0,r2=0.0;i<nwavelets;++i)
			{
				z2 += ((double)z[i][j][k].r)*((double)z[i][j][k].r);
				z2 += ((double)z[i][j][k].i)*((double)z[i][j][k].i);
				r2 += ((double)residuals[i][k].r)
					* ((double)residuals[i][k].r);
				r2 += ((double)residuals[i][k].i)
					* ((double)residuals[i][k].i);
			}
			if((z2<=0.0) || (r2>z2) )
				coherence[j][k]=0.0;
			else
				coherence[j][k]=(z2-r2)/z2;
			/* This is for accumulating beam coherence */
			z2b[k] += z2*(stack->weights[j]);
			r2b[k] += r2*(stack->weights[j]);
		}
	}
	/* Now compute the beam average coherence.  Note we don't need to 
	normalize by the sum of the weights because 1/sum_weights is a common
	factor in the numerator and denominator of the coherence formula.*/
	for(k=0;k<nt;++k)
	{
		if(z2b[k]<=0.0) 
			cohb[k] = 0.0;
		else 
			cohb[k] = (z2b[k]-r2b[k])/z2b[k];
		if(cohb[k]<0.0) cohb[k]=0.0;
	}

	for(i=0;i<nwavelets;++i)free(residuals[i]);
	free(residuals);
	free(z2b);
	free(r2b);
	free(relative_amp);
	return(0);
}			

