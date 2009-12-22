/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2002
 */

#define USAGE "Error using abspath\n\n\
Usage: ABS = ABSPATH ( REL )\n"

#include "antelope_mex.h"
#include "tttaup.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	char	*rel;
	char	abs[FILENAME_MAX] = "";

	if( nrhs != 1  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! mtlb_get_string( prhs[0], &rel ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	abspath( rel, abs );

	plhs[0] = mxCreateString( abs );
}
