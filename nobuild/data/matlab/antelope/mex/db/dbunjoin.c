/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbunjoin\n\n\
Usage: DBUNJOIN ( DBPTR, OUTPUT )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	char	*output;
	int	result;

	if( nrhs != 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! mtlb_get_string( prhs[1], &output ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	result = dbunjoin( db, output, 0 );
	antelope_mex_clear_register( 1 );
	mxFree( output );

	if( result != 0 ) mexErrMsgTxt( "dbunjoin failed\n" );
}
