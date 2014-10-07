/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2003
 */

#define USAGE "Error using ==> cggrid_getmesh\n\n\
Usage: [X, Y, Z] = CGGRID_GETMESH ( CGGRID )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
#ifndef HAVE_CGEOM
	mexErrMsgTxt( "No cggrid support in your version of Antelope" );
#else
	FILE	*fp;
	CGGrid	*cgg;
	int	rc;

	if( nlhs != 3 ) 
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( nrhs != 1 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_cggrid( prhs[0], &cgg ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	rc = CGGrid2plaid_mxArrays( cgg, &plhs[0], &plhs[1], &plhs[2] );

	if( ! rc ) 
	{
		mexErrMsgTxt( "Failed to create return values");
	} 
#endif
}
