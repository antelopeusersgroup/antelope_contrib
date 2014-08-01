/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using dbsubset\n\n\
Usage: DBPTR = DBSUBSET ( DBPTR, EXPRESSION )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	char	*expression;	
	char	errmsg[STRSZ];

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
        else if( ! mtlb_get_string( prhs[1], &expression ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	db = dbsubset( db, expression, 0 );
	antelope_mex_clear_register( 1 );

	mxFree( expression );

	if( db.table == dbINVALID )
	{
		mexErrMsgTxt( "dbsubset failed" );
	}

	plhs[0] = CreateDbptrStructFromDbptr( db );

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "dbsubset: failed to create " );
		strcat( errmsg, "database-pointer structure for result" );
		mexErrMsgTxt( errmsg );
	}
}
