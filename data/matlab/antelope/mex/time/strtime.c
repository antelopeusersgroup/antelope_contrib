/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> strtime\n\n\
Usage: STRTIME = STRTIME ( EPOCH )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	char	*timestring;
	mxArray	*timestr;
	double	*epoch;
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

	if( M == 1 && N == 1 )
	{
		timestring = strtime( *epoch );
		antelope_mex_clear_register( 1 );
		plhs[0] = mxCreateString( timestring );
		free( timestring );
	} 
	else 
	{
		plhs[0] = mxCreateCellMatrix( M, N );

		for( i = 0; i < M; i++ ) 
		{
		   for( j = 0; j < N; j++ )
		   {
			timestring = strtime( *(epoch + i*N + j) );
			antelope_mex_clear_register( 1 );
			timestr = mxCreateString( timestring );
			free( timestring );

			subs[0] = i;
			subs[1] = j;

			cell_index = mxCalcSingleSubscript( plhs[0],
							    nsubs, subs );

			mxSetCell( plhs[0], cell_index, timestr );
		   }
		}
	}
}
