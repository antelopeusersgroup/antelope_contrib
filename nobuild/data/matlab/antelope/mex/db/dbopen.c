/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbopen\n\n\
Usage: DBPTR = DBOPEN ( FILENAME, OPENTYPE )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	char	*filename;
	char	*permission;
	char	errmsg[STRSZ];
	Dbptr	db;

	mexLock();

	if( nrhs != 2 )
	{
		antelope_mexUsageMsgTxt( USAGE );
		return;
	}
	else if( ! get_trimmed_string( prhs[0], &filename ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! mtlb_get_string( prhs[1], &permission ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( dbopen( filename, permission, &db ) == dbINVALID )
	{
		antelope_mex_clear_register( 1 );
		sprintf( errmsg, "dbopen: error opening %s\n", filename );
		mxFree( filename );
		mxFree( permission );
		mexErrMsgTxt ( errmsg );
	}
	else
	{
		mxFree( filename );
		mxFree( permission );
	}

	plhs[0] = CreateDbptrStructFromDbptr( db );

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "dbopen: failed to create database-pointer " );
		strcat( errmsg, "structure for result" );
		mexErrMsgTxt( errmsg );
	}
}
