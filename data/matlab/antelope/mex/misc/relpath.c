/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2002
 */

#define USAGE "Error using relpath\n\n\
Usage: REL = RELPATH ( FROM, TO )\n"

#include "antelope_mex.h"
#include "tttaup.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	char	*from;
	char	*to;
	char	rel[FILENAME_MAX] = "";

	if( nrhs != 2  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! mtlb_get_string( prhs[0], &from ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! mtlb_get_string( prhs[1], &to ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	relpath( from, to, rel );

	plhs[0] = mxCreateString( rel );
}
