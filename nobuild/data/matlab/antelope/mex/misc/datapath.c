/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting, Inc.
 * 2011
 */

#define USAGE "Error using datapath\n\n\
Usage: FNAME = DATAPATH ( ENVNAME, DIRNAME, FILENAME, SUFFIX )\n"

#include "antelope_mex.h"
#include "tttaup.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	char	*envname = NULL;
	char	*dirname = NULL;
	char	*filename = NULL;
	char	*suffix = NULL;
	char	*fname = NULL;

	if( nrhs != 4  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! mtlb_get_string( prhs[0], &envname ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! mtlb_get_string( prhs[1], &dirname ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! mtlb_get_string( prhs[2], &filename ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! mtlb_get_string( prhs[3], &suffix ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	fname = datapath( envname, dirname, filename, suffix );

	plhs[0] = mxCreateString( fname );

	if( fname != (char *) NULL ) {

		free( fname );
	}
}
