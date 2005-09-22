/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbtheta\n\n\
Usage: DBPTR = DBTHETA ( DBPTR1, DBPTR2 [, EXPRESSION] [, 'outer'] )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db, db1, db2;
	char	*expression = 0;
	char	errmsg[STRSZ];
	char 	*outer;
	int	outer_join = 0;

	if( nrhs < 2 || nrhs > 4  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db1 ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! get_dbptr( prhs[1], &db2 ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( nrhs >= 3 && ! mtlb_get_string( prhs[2], &expression ) )
	{
                antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( nrhs == 4 ) 
	{
		if( !mtlb_get_string( prhs[3], &outer ) )
		{
                	antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else if( ! STREQ( outer, "outer" ) )
		{
			mxFree( outer );
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else
		{
			mxFree( outer );
			outer_join = 1;
		}
	}

	if( nrhs == 3 && STREQ( expression, "outer" ) )
	{
		mxFree( expression );
		expression = NULL;
		outer_join = 1;
	}

	db = dbtheta( db1, db2, expression, outer_join, 0 );
	antelope_mex_clear_register( 1 );

	if( expression ) mxFree( expression );

	if( db.table == dbINVALID ) 
	{
		mexErrMsgTxt( "dbtheta: join failed" );
	}

	plhs[0] = CreateDbptrStructFromDbptr( db );

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "dbtheta: failed to create " );
		strcat( errmsg, "database-pointer structure for result" );
		mexErrMsgTxt( errmsg );
	}
}
