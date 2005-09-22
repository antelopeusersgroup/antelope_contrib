/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2002
 * 
 */

#define USAGE "Error using ==> dbsever\n\n\
Usage: DBPTR = DBSEVER ( DBPTR, TABLE )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	char	*table;
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
	else if( ! mtlb_get_string( prhs[1], &table ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	db = dbsever( db, table, 0 );
	antelope_mex_clear_register( 1 );
	if( db.table == dbINVALID ) 
	{
		mexErrMsgTxt( "dbsever: returned invalid database pointer" );
	}

	mxFree( table );

	plhs[0] = CreateDbptrStructFromDbptr( db );

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "dbsever: failed to create " );
		strcat( errmsg, "database-pointer structure for result" );
		mexErrMsgTxt( errmsg );
	}
}
