/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbget\n\n\
Usage: RECORD = DBGET( DBPTR, ['dbSCRATCH'] )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	char	*result = NULL;
	char	*dbscratch_string;
	int	use_dbscratch = 0;
	long	n;
	int	rc;
	double	*doublep;
	int	*intp;

	if( nrhs < 1 || nrhs > 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( nrhs == 2 ) 
        {
		if( ! mtlb_get_string( prhs[1], &dbscratch_string ) )
		{
                	antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else if( ! STREQ( dbscratch_string, "dbSCRATCH" ) )
		{
			mxFree( dbscratch_string );
                	antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else
		{
			mxFree( dbscratch_string );
			use_dbscratch = 1;
		}
        }

	if( use_dbscratch )
	{
		if( dbget( db, NULL ) < 0 )
		{
			antelope_mex_clear_register( 1 );
			mexErrMsgTxt( "dbget into scratch record failed" );
		}
	}
	else
	{
		rc = dbquery(db, dbSIZE, &n);
		antelope_mex_clear_register( 1 );
		if( rc == dbINVALID )
		{
			mexErrMsgTxt( "dbget: dbquery failed" );
		}

		result = (char *) mxCalloc( n+3, sizeof( char ) );

		if( dbget( db, result ) < 0 )
		{

			antelope_mex_clear_register( 1 );

			mxFree( result );
			mexErrMsgTxt( "dbget failed\n" );

		}
		else
		{
			if ( db.field != dbALL )
			{
				copystrip( result, result, n );
			}
			plhs[0] = mxCreateString( result );
			mxFree( result );
		}
	}

	antelope_mex_clear_register( 1 );
}
