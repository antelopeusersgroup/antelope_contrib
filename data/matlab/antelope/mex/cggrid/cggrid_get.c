/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2003
 */

#define USAGE "Error using ==> cggrid_get\n\n\
Usage: [TRIPLETS, NX, NY] = CGGRID_GET ( CGGRID )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
#ifndef HAVE_CGEOM
	mexErrMsgTxt( "No cggrid support in your version of Antelope" );
#else
	FILE	*fp;
	CGGrid	*cgg;

	if( nlhs > 3 ) 
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

	plhs[0] = CGGrid2linear_mxArray( cgg );

	if( ! plhs[0] ) 
	{
		mexErrMsgTxt( "Failed to create cggrid return value");
	} 

	if( nlhs >= 2 )
	{
		plhs[1] = CreateDouble( (double) cgg->nx );
	}
	if( nlhs == 3 )
	{
		plhs[2] = CreateDouble( (double) cgg->ny );
	}
#endif
}
