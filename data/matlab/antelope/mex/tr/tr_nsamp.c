/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using tr_nsamp\n\n\
Usage: N = TR_NSAMP ( TIME, SAMPRATE, ENDTIME )\n"

#include "antelope_mex.h"
#include "tr.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	double time;
	double samprate;
	double endtime;

	if( nrhs != 3  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_scalar( prhs[0], &time ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! get_scalar( prhs[1], &samprate ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! get_scalar( prhs[2], &endtime ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	plhs[0] = CreateDouble( (double) NSAMP( time, samprate, endtime ) );
}
