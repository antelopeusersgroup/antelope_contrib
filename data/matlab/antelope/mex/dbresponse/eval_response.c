/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> eval_response\n\n\
Usage: RESPONSE = EVAL_RESPONSE ( DBRESPONSE, OMEGA )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Response *response;
	double 	*real_ptr, *imag_ptr; 
	double	*omega_ptr;
	int	M, N;
	int	nelements;
	int	rc;
	int	i;

	if( nlhs > 1 ) 
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( nrhs != 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! mtlb_get_response( prhs[0], &response ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( mxGetClassID( prhs[1] ) != mxDOUBLE_CLASS )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( mxGetNumberOfDimensions( prhs[1] ) != 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	
	M = mxGetM( prhs[1] );
	N = mxGetN( prhs[1] );

	plhs[0] = mxCreateDoubleMatrix( M, N, mxCOMPLEX );

	nelements = M * N;

	omega_ptr = (double *) mxGetPr( prhs[1] );
	real_ptr = (double *) mxGetPr( plhs[0] );
	imag_ptr = (double *) mxGetPi( plhs[0] );

	for( i = 0; i < nelements; i++ ) 
	{
		rc = eval_response( *omega_ptr++, response,
					real_ptr++, imag_ptr++ );
		
		if( rc ) {
			mxDestroyArray( plhs[0] );
			antelope_mex_clear_register( 1 );
			mexErrMsgTxt( "eval_response failed" );

		}
	}
}
 
 

