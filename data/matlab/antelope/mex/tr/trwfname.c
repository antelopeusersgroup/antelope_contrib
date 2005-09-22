/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2002
 */

#define USAGE "Error using ==> trwfname\n\n\
Usage: PATH = TRWFNAME ( DBPTR [, PATTERN]  )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	char	*pattern = 0;
	char	*path = 0;
	int	rc;

	if( nrhs > 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	if( nrhs == 2 && ( ! mtlb_get_string( prhs[1], &pattern ) ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
		
	rc = trwfname( db, pattern, &path );
	antelope_mex_clear_register( 1 );

	if( pattern ) {

		mxFree( pattern );
	}

	if( rc < 0 )
	{
		mexErrMsgTxt( "trwfname failed\n" );
	}

	plhs[0] = mxCreateString( path );
}
