/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using tr_time2samp\n\n\
Usage: N_INDEXED_BY_1 = TR_TIME2SAMP ( TIME0, SAMPRATE, TIME1 )\n"

#include "antelope_mex.h"
#include "tr.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	double time0;
	double samprate;
	double time1;
	int sampnum_indexed_by_0;
	int sampnum_indexed_by_1;

	if( nrhs != 3  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_scalar( prhs[0], &time0 ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! get_scalar( prhs[1], &samprate ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! get_scalar( prhs[2], &time1 ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	sampnum_indexed_by_0 = TIME2SAMP( time0, samprate, time1 );

	/* WAKE UP */
	sampnum_indexed_by_1 = sampnum_indexed_by_0 + 1;

	plhs[0] = CreateDouble( (double) sampnum_indexed_by_1 );
}
