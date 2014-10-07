/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 1997-2003
 */

#define USAGE "Error using ==> trfilter\n\n\
Usage: TRFILTER ( TRPTR, FILTER_STRING )\n"

#include "antelope_mex.h"
#include "tr.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	tr;
	char	*filter_string;

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
	else if( ! mtlb_get_string( prhs[1], &filter_string ) )
	{
                antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	trfilter( tr, filter_string );
	antelope_mex_clear_register( 1 );
}
