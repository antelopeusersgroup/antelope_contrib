/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2002
 */

#define USAGE "Error using dbsave_view\n\n\
Usage: DBSAVE_VIEW ( DBPTR )\n"

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

	if( dbsave_view( db ) != 0 )
	{
		antelope_mex_clear_register( 1 );
		mexErrMsgTxt ( "dbsave_view failed\n" );
	}
}
