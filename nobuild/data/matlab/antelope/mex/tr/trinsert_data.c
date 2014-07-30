/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> trinsert_data\n\n\
Usage: TRINSERT_DATA ( TRPTR, DATA )\n"

#include "antelope_mex.h"
#include "tr.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	tr;
	float	*data;
	double	*pr;
	int	M, N;
	int	i;

	if( nrhs != 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &tr ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( ! mxIsDouble( prhs[1] ) ||
		 mxIsComplex( prhs[1] ) || 
		 mxIsEmpty( prhs[1] ) ||
		 ! ( mxGetM( prhs[1] ) == 1 || mxGetN( prhs[1] ) == 1 ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( tr.record < 0 )
	{
		mexErrMsgTxt( "trinsert_data: TRPTR must specify a single row" );
	}

	M = mxGetM( prhs[1] );
	N = mxGetN( prhs[1] );
	pr = mxGetPr( prhs[1] );

	/* trace library mallocs outside of Matlab context, so we will too: */
	allot( float *, data, M*N );
	
	for( i = 0; i < M*N; i++ )
	{
		data[i] = (float) pr[i];
	}

	dbputv( tr, 0, "nsamp", M*N, "data", data, NULL );
	antelope_mex_clear_register( 1 );
}
