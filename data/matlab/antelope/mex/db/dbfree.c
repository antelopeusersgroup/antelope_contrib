/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using dbfree\n\n\
Usage: DBFREE ( DBPTR )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	mxArray	*DbptrStruct;
	mxArray *varname[1];
	mxArray *output_array[1];

	if( nrhs != 1 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! get_dbptr( prhs[0], &db ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( dbfree( db ) == dbINVALID )
	{
		antelope_mex_clear_register( 1 );
		mexErrMsgTxt ( "dbfree failed\n" );
	}
}
