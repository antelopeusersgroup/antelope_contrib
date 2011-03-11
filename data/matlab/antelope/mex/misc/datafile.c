/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting, Inc.
 * 2011
 */

#define USAGE "Error using datafile\n\n\
Usage: FNAME = DATAFILE ( ENVNAME, FILENAME )\n"

#include "antelope_mex.h"
#include "tttaup.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	char	*envname = NULL;
	char	*filename = NULL;
	char	*fname = NULL;

	if( nrhs != 2  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! mtlb_get_string( prhs[0], &envname ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! mtlb_get_string( prhs[1], &filename ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	fname = datafile( envname, filename );

	plhs[0] = mxCreateString( fname );

	if( fname != (char *) NULL ) {

		free( fname );
	}
}
