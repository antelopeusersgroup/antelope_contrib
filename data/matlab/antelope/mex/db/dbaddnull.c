/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbaddnull\n\n\
Usage: RECORD_NUMBER = DBADDNULL ( DBPTR )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	int	rc;

	if( nrhs != 1  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! get_dbptr( prhs[0], &db ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	rc = dbaddnull( db );
	antelope_mex_clear_register( 1 );

	plhs[0] = CreateDouble( (double) rc );
	if( plhs[0] == NULL )
	{
		mexErrMsgTxt( "dbaddnull: failed to create return value" );
	}
}
