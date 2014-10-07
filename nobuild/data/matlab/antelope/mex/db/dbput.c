/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbput\n\n\
Usage: DBPUT ( DBPTR, VALUE )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	char	*value;
	int	type;
	char	*format;
	int	rc;

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
	else if( ! mtlb_get_string( prhs[1], &value ) )
	{
		mexWarnMsgTxt ( "dbput: value must be of string type\n" );
                antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( STREQ( value, "dbSCRATCH" ) )
	{
		rc = dbput( db, 0 );
	}
	else 
	{
		rc = dbput( db, value );
	}

	antelope_mex_clear_register( 1 );

	mxFree( value );

	if( rc ) 
	{
		mexErrMsgTxt( "dbput failed" );
	}
}
