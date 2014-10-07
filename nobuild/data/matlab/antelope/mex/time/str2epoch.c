/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> str2epoch\n\n\
Usage: EPOCH = STR2EPOCH ( TIMESTRING )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	mxArray	*mycell;
	char	*timestring;
	char	*errmsg;
	double	*epoch;
	int	single_string;
	int	M, N;
	int	cell_index;
	int	nsubs;
	int	subs[2];
	int	i, j;

	if( nrhs != 1 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( mxGetClassID( prhs[0] ) == mxCHAR_CLASS )
        {
		single_string = 1;
	} 
	else if( mxGetClassID( prhs[0] ) == mxCELL_CLASS )
	{
		single_string = 0;
	}
	else
	{
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	if( single_string ) 
	{
		if( mxGetM( prhs[0] ) != 1 && mxGetN( prhs[0] ) != 1 )
		{
			errmsg = mxCalloc( STRSZ, sizeof( char ) );
			sprintf( errmsg,
			  "str2epoch: Character array must be Nx1 or" );
			sprintf( errmsg,
			  "%s 1xN.\nFor multiple strings use a", errmsg );
			sprintf( errmsg, "%s cell array.\n", errmsg );

			mexErrMsgTxt( errmsg );
		}

		get_malloced_string( prhs[0], &timestring );

		plhs[0] = mxCreateDoubleMatrix( 1, 1, mxREAL );
		epoch = (double *) mxGetPr( plhs[0] );

		*epoch = str2epoch( timestring );
		antelope_mex_clear_register( 1 );

		mxFree( timestring );
	}
	else
	{
		M = mxGetM( prhs[0] );
		N = mxGetN( prhs[0] );

		plhs[0] = mxCreateDoubleMatrix( M, N, mxREAL );
		epoch = (double *) mxGetPr( plhs[0] );

		nsubs = 2;
		
		for( i = 0; i < M; i++ )
		{
		   for( j = 0; j < N; j++ )
		   {
			subs[0] = i;
			subs[1] = j;

			cell_index = mxCalcSingleSubscript( prhs[0],
							    nsubs,
							    subs );
			mycell = mxGetCell( prhs[0], cell_index );

			if( ! mxIsChar( mycell ) ) 
			{
				mxDestroyArray( plhs[0] );
				antelope_mexUsageMsgTxt ( USAGE );
				return;
			}
			if( mxGetM( mycell ) != 1 && mxGetN( mycell ) != 1 )
			{
				mxDestroyArray( plhs[0] );
				antelope_mexUsageMsgTxt ( USAGE );
				return;
			}

			get_malloced_string( mycell, &timestring );
			
			*(epoch + i*N + j) = str2epoch( timestring ); 
			antelope_mex_clear_register( 1 );

			mxFree( timestring );
		   }
		}
	}

}
