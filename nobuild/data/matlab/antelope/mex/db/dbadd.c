/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbadd\n\n\
Usage: RECORD_NUMBER = DBADD ( DBPTR, RECORD )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	char	*record;
	int	use_dbscratch = 0;
	int	rc;

	if( nrhs != 2  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! get_dbptr( prhs[0], &db ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! mtlb_get_string( prhs[1], &record ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( STREQ( record, "dbSCRATCH" ) )
	{
		mxFree( record );
		record = 0;
	}

	rc = dbadd( db, record );
	antelope_mex_clear_register( 1 );

	if( record ) mxFree( record );

	if( rc < 0 )
	{
		mexErrMsgTxt( "dbadd: failed to add record" );
	}
	else
	{
		plhs[0] = CreateDouble( (double) rc );
		if( plhs[0] == NULL )
		{
			mexErrMsgTxt( "dbadd: failed to create return value" );
		}
	}
}
