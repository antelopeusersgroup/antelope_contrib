/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> clear\n\n\
Usage: CLEAR ( DBRESPONSE )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Response *response;
	mxArray	*address;
	mxArray	*varname[1];
	mxArray	*output_array[1];
	Tbl	*errors;
	char	*error;
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

	if( nrhs != 1 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_response( prhs[0], &response ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	
	free_response( response );
	antelope_mex_clear_register( 1 );

	varname[0] = mxCreateString( mxGetName( prhs[0] ) );
	if( varname[0] == 0 )
	{
		mexErrMsgTxt ( "Couldn't allocate name string for callback" );
	}

	mexCallMATLAB( 0, output_array, 1, varname, "clear" );
}
