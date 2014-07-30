/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2002
 */

#define USAGE "Error using dbwrite_view\n\n\
Usage: DBWRITE_VIEW ( DBPTR, FILENAME )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	mxArray	*DbptrStruct;
	mxArray *varname[1];
	mxArray *output_array[1];
	char	errmsg[STRSZ];
	char	*filename;
	FILE	*fp;

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
	else if( ! get_trimmed_string( prhs[1], &filename ) ) 
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( ( fp = fopen( filename, "w" ) ) == NULL ) {
		
		sprintf( errmsg, 
			 "dbwrite_view: Error opening file '%s' for writing\n", 
			 filename );
		mxFree( filename );
		mexErrMsgTxt( errmsg );
	} 

	if( dbwrite_view( db, fp ) != 0 )
	{
		fclose( fp );
		mxFree( filename );
		antelope_mex_clear_register( 1 );
		mexErrMsgTxt ( "dbwrite_view failed\n" );
	}
	
	fclose( fp );
	mxFree( filename );
}
