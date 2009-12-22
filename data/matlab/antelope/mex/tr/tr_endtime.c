/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using tr_endtime\n\n\
Usage: N = TR_ENDTIME ( TIME, SAMPRATE, NSAMP )\n"

#include "antelope_mex.h"
#include "tr.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	double nsamp_double;
	double time;
	double samprate;

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
        else if( ! get_scalar( prhs[2], &nsamp_double ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	plhs[0] = CreateDouble( ENDTIME( time, samprate, (long) nsamp_double ) );
}
