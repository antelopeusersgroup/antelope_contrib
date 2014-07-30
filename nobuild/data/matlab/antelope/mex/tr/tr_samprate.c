/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using tr_samprate\n\n\
Usage: N = TR_SAMPRATE ( TIME, NSAMP, ENDTIME )\n"

#include "antelope_mex.h"
#include "tr.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	double nsamp_double;
	double time;
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
        else if( ! get_scalar( prhs[1], &nsamp_double ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! get_scalar( prhs[2], &endtime ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	plhs[0] = CreateDouble( SAMPRATE( time, (long) nsamp_double, endtime ) );
}
