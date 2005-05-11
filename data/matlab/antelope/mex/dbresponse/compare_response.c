/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> compare_response\n\n\
Usage: COMPARE_RESPONSE ( DBRESPONSE1, DBRESPONSE2 )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Response *response1;
	Response *response2;
	int	rc;

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
        else if( ! mtlb_get_response( prhs[0], &response1 ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;

        }
	else if( ! mtlb_get_response( prhs[1], &response2 ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	
	rc = compare_response( response1, response2 );
	antelope_mex_clear_register( 1 );

	plhs[0] = CreateDouble( (double) rc );

	if( plhs[0] == NULL )
	{
		mexErrMsgTxt( "compare_response: failed to create return value" );
	}

}
