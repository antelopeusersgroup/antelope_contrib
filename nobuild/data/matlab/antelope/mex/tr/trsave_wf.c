/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> trsave_wf\n\n\
Usage: TRSAVE_WF ( TRPTR, DBPTR, DATATYPE, WFNAME, ['overwrite' | 'append'] )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	tr;
	Dbptr	db;
	char	*datatype;
	char	*wfname;
	int	rc;
	int	flags = 0;
	char 	*flag_string;

	if( nrhs < 4 || nrhs > 5 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &tr ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! get_dbptr( prhs[1], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! mtlb_get_string( prhs[2], &datatype ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! mtlb_get_string( prhs[3], &wfname ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	if( nrhs == 5 )
	{
		if( ! mtlb_get_string( prhs[4], &flag_string ) ) 
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}

		if( STREQ( flag_string, "overwrite" ) )
		{
			flags |= trOVERWRITE;
		}
		else if( STREQ( flag_string, "append" ) )
		{
			flags |= trAPPEND;
		}
		else
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
	}

	rc = trsave_wf( tr, db, datatype, wfname, flags );
	antelope_mex_clear_register( 1 );

	mxFree( datatype );
	mxFree( wfname );

	if( rc & TR_CLIPPED || rc & TR_TRUNCATED )
	{
		mexWarnMsgTxt( 
			"trsave_wf: clipping occurred during conversion\n" );
	}
	if( rc < 0 ) mexErrMsgTxt( "trsave_wf failed\n" );
}
