/* 
 * Matlab interface to Datascope package
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using dbclose\n\n\
Usage: DBCLOSE ( DBPTR )\n"

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

	if( dbclose( db ) == dbINVALID )
	{
		antelope_mex_clear_register( 1 );
		mexErrMsgTxt ( "dbclose failed\n" );
	}

	varname[0] = mxCreateString( mxGetName( prhs[0] ) );
	if( varname[0] == 0 )
	{
		mexErrMsgTxt ( "Couldn't allocate name string for callback" );
	}

	mexCallMATLAB( 0, output_array, 1, varname, "clear" );
}
