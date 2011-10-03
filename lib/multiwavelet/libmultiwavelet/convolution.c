#include <stdio.h>
#include <perf.h>
#include "elog.h"

/* sconv is a general purpose convolution routine using sunperf BLAS function
sdot for speed.  

arguments:

	in - input vector of function to be convolved with
	nin  - length of in
	filter - vector of filter coefficients to convolve into in
	nfilter - length of filter
	ioff - number of samples to skip into trace before starting
		convolution operation (i.e. left side of integral of
		first sample will start here. )
	decfac - decimation factor.  For use in decimation can make
		this > 1 and routine will only hit every decfac values.
		This is much more efficient than decimation after the fact.
		For normal convolution, set to 1.
	out - output vector  (WARNING:  this vector must be alloced large
		enough to hold complete output.  No check is made to 
		verify if it's length is sufficiently large to hold output vec.
	nout - actual number of samples in out (output)

Returns:

	normal return is 0.  -1 for illegal inputs with no output.  This
condition must be trapped as the routine produces null results.
A positive result is returned if ioff is passed as a negative value.
The result can still be used when this happens, but the time of the
first sample may not be what the calling program believes.  This condition
should be trapped and the time of first output sample in out increased
by ret_code*dt where dt is the sample interval of the in trace. 
 

Author:  Gary L. Pavlis
Written:  July 21, 1998
*/
int sconv(float *in, int nin, float *filter, int nfilter,
		int ioff, int decfac,
		float *out, int *nout)
{
	int i,j,iend;
	int ret_code=0;

	/* First check for bad parameters and correct if possible */
	if(nfilter > nin)
	{
		elog_log(0,"sconv:  filter length (%d) longer than input time series (%d)\n",nfilter,nin);
		return(-1);
	}
	if(ioff < 0)
	{
		elog_log(0,"sconv:  illegal offset value = %d set to 0\n",
			ioff);
		ret_code = -ioff;
		ioff = 0;
	}
	/* elegance of blas shows here */
	for(i=ioff,j=0,iend=ioff+nfilter,*nout=0;
		iend<nin;
		i += decfac,++j,iend += decfac)
	{
		out[j] = (float)sdot(nfilter,in+i,1,filter,1);
	}
	*nout = j;
	return(ret_code);
}
