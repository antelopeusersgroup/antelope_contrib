/* 
 * Matlab interface to Datascope package
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbeval\n\n\
Usage: DBEVAL ( DBPTR, EXPRESSION [,TYPE] )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	int	code = 0;
	int	rc = 0;
	char	*expression;
	char	*codestr;
	Expression *expr;
	Dbvalue	exresult;

	if( nrhs != 2 && nrhs != 3 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! get_string( prhs[1], &expression ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( nrhs == 3 && ! get_string( prhs[2], &codestr ) )
        {
		mxFree( expression );
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( nrhs == 3 )
	{
		code = xlatname( codestr, Dbxlat, NDbxlat );
		mxFree( codestr );
	}

	if( dbex_compile( db, expression, &expr, code ) < 0 )
	{
		antelope_mex_clear_register( 1 );
		mxFree( expression );
		mexErrMsgTxt( "dbeval: expression failed to compile\n" );
	}
	else
	{
		antelope_mex_clear_register( 1 );
		mxFree( expression );
	}

	rc = dbex_eval ( db, expr, 1, &exresult ) ;
	antelope_mex_clear_register( 1 );

	if( rc >= 0 )
	{
		switch( expr->type )
		{
		case dbBOOLEAN:
		case dbINTEGER:
		case dbYEARDAY:
			plhs[0] = CreateDouble( (double)  exresult.i );
			if( plhs[0] == NULL )
			{
				dbex_free( expr );
				mexErrMsgTxt( 
				   "dbeval: failed to create return value" );
			}
			break;
		case dbREAL:
		case dbTIME:
			plhs[0] = CreateDouble( exresult.d );
			if( plhs[0] == NULL )
			{
				dbex_free( expr );
				mexErrMsgTxt( 
				   "dbeval: failed to create return value" );
			}
			break;
		case dbSTRING:
			plhs[0] = mxCreateString( exresult.t );
			break;
		default:
			dbex_free( expr );
			mexErrMsgTxt( "Unknown type in dbeval\n" );
			break;
		}
		dbex_free( expr );
	}
	else
	{
		dbex_free( expr );
		mexErrMsgTxt( "dbeval: eval failed\n" );
	}
}
