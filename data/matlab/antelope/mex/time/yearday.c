/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> yearday\n\n\
Usage: YEARDAY = YEARDAY ( EPOCH )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	double	*epoch;
	double	*yday;
	int	M, N;
	int	nsubs = 2;
	int	subs[2];
	int	cell_index;
	int	i, j;

	if( nrhs != 1 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( mxGetClassID( prhs[0] ) != mxDOUBLE_CLASS )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	epoch = mxGetPr( prhs[0] );

	M = mxGetM( prhs[0] );
	N = mxGetN( prhs[0] );

	plhs[0] = mxCreateDoubleMatrix( M, N, mxREAL );

	yday = (double *) mxGetPr( plhs[0] );

	for( i = 0; i < M; i++ ) 
	{
	   for( j = 0; j < N; j++ )
	   {
		*(yday + i*N + j) = (double) yearday( *(epoch + i*N + j) );
		antelope_mex_clear_register( 1 );
	   }
	}
}
