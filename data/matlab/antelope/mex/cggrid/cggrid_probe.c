/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2003
 */

#define USAGE "Error using ==> cggrid_probe\n\n\
Usage: VAL = CGGRID_PROBE ( CGGRID, X, Y )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
#ifndef HAVE_CGEOM
	mexErrMsgTxt( "No cggrid support in your version of Antelope" );
#else
	CGGrid	*cgg;
	double	x;
	double	y;
	double	val;

	if( nlhs > 1 ) 
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( nrhs != 3 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_cggrid( prhs[0], &cgg ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! get_scalar( prhs[1], &x ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! get_scalar( prhs[2], &y ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	val = cggrid_probe( cgg, x, y );

	plhs[0] = CreateDouble( val );

	if( ! plhs[0] ) 
	{
		mexErrMsgTxt( "Failed to create return value");
	} 
#endif
}
