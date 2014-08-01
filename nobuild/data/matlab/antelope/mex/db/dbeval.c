/* 
 * Antelope Toolbox for Matlab
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
	int	single_row;
	long	nrows;
	int	code = 0;
	int	rc = 0;
	char	*expression;
	char	*codestr;
	Expression *expr;
	Dbvalue	exresult;
	double	*pr;

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
        else if( ! mtlb_get_string( prhs[1], &expression ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( nrhs == 3 && ! mtlb_get_string( prhs[2], &codestr ) )
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

	rc = dbquery( db, dbRECORD_COUNT, &nrows );
	antelope_mex_clear_register( 1 );
	if( rc == dbINVALID ) 
	{
		mexErrMsgTxt( "dbgetv: query for number of records failed" );
	}

	if( db.record == dbALL ) {
		if( nrows == 1 ) {
			single_row = 1;
			db.record = 0;
		} else {
			single_row = 0;
		}
	} else {
		single_row = 1;
	}

	if( single_row ) 
	{
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
				free( exresult.t );
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
	} else {

		db.record = 0;

		rc = dbex_eval ( db, expr, 1, &exresult ) ;
		antelope_mex_clear_register( 1 );

		if( rc >= 0 )
		{
		   switch( expr->type )
		   {
			case dbBOOLEAN:
			case dbINTEGER:
			case dbYEARDAY:
				plhs[0] = mxCreateDoubleMatrix( nrows, 1, mxREAL );
				if( plhs[0] == NULL )
				{
					dbex_free( expr );
					mexErrMsgTxt( 
				   	"dbeval: failed to create return value" );
				}

				pr = mxGetPr( plhs[0] );
				pr[db.record] = (double) exresult.i;

				for( db.record = 1; db.record < nrows; db.record++ )
				{
					if( dbex_eval ( db, expr, 1, &exresult ) < 0 ) 
					{
						antelope_mex_clear_register( 1 );
						dbex_free( expr );
						mxDestroyArray( plhs[0] );
						mexErrMsgTxt( "dbeval: eval failed\n" );
					} else {
						pr[db.record] = (double) exresult.i;
					}
				}

				break;
			case dbREAL:
			case dbTIME:
				plhs[0] = mxCreateDoubleMatrix( nrows, 1, mxREAL );
				if( plhs[0] == NULL )
				{
					dbex_free( expr );
					mexErrMsgTxt( 
				   	"dbeval: failed to create return value" );
				}

				pr = mxGetPr( plhs[0] );
				pr[db.record] = exresult.d;

				for( db.record = 1; db.record < nrows; db.record++ )
				{
					if( dbex_eval ( db, expr, 1, &exresult ) < 0 ) 
					{
						antelope_mex_clear_register( 1 );
						dbex_free( expr );
						mxDestroyArray( plhs[0] );
						mexErrMsgTxt( "dbeval: eval failed\n" );
					} else {
						pr[db.record] = (double) exresult.d;
					}
				}

				break;
			case dbSTRING:
				plhs[0] = mxCreateCellMatrix( nrows, 1 );
				if( plhs[0] == NULL )
				{
					dbex_free( expr );
					mexErrMsgTxt( 
				   	"dbeval: failed to create return value" );
				}

				mxSetCell( plhs[0], db.record, mxCreateString( exresult.t ) );
				free( exresult.t );

				for( db.record = 1; db.record < nrows; db.record++ )
				{
					if( dbex_eval ( db, expr, 1, &exresult ) < 0 ) 
					{
						antelope_mex_clear_register( 1 );
						dbex_free( expr );
						mxDestroyArray( plhs[0] );
						mexErrMsgTxt( "dbeval: eval failed\n" );
					} else {
						mxSetCell( plhs[0], db.record, mxCreateString( exresult.t ) );
						free( exresult.t );
					}
				}

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
}
