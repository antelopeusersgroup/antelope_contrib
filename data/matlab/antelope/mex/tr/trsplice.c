/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> trsplice\n\n\
Usage: TRSPLICE ( TRPTR, TOLERANCE )\n"

#include "antelope_mex.h"
#include "tr.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	tr;
	double	tolerance;

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
	else if( ! get_scalar( prhs[1], &tolerance ) )
	{
                antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	trsplice( tr, tolerance, 0, 0 ); 
	antelope_mex_clear_register( 1 );
}
