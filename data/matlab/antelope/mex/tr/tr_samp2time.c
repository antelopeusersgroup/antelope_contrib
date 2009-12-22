/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using tr_samp2time\n\n\
Usage: TIME1 = TR_SAMP2TIME ( TIME0, SAMPRATE, N_INDEXED_BY_1 )\n"

#include "antelope_mex.h"
#include "tr.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	double	time;
	double	samprate;
	double	n_indexed_by_1_double;
	int	n_indexed_by_0;

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
        else if( ! get_scalar( prhs[2], &n_indexed_by_1_double ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	/* WAKE UP: index shifting */
	n_indexed_by_0 = (long) n_indexed_by_1_double - 1;

	plhs[0] = CreateDouble( SAMP2TIME( time, samprate, n_indexed_by_0 ) );
}
