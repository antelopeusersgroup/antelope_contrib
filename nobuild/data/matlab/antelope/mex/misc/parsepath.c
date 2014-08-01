/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2002
 */

#define USAGE "Error using parsepath\n\n\
Usage: [DIR, BASE [, SUFFIX]] = PARSEPATH ( PATH )\n"

#include "antelope_mex.h"
#include "tttaup.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	char	*path;
	char	dir[FILENAME_MAX] = "";
	char	base[FILENAME_MAX] = "";
	char	suffix[FILENAME_MAX] = "";

	if( nrhs != 1  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! mtlb_get_string( prhs[0], &path ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	if( nlhs > 2 ) {

		parsepath( path, dir, base, suffix );

	} else {

		parsepath( path, dir, base, 0 );
	}		

	plhs[0] = mxCreateString( dir );
	plhs[1] = mxCreateString( base );

	if( nlhs > 2 ) {

		plhs[2] = mxCreateString( suffix );
	}
}
