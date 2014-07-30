/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> free_response\n\n\
Usage: FREE_RESPONSE ( DBRESPONSE )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Response *response;

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
        else if( ! mtlb_get_response( prhs[0], &response ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	
	free_response( response );
	antelope_mex_clear_register( 1 );
}
