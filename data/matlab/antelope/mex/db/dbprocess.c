/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using dbprocess\n\n\
Usage: DBPTR = DBPROCESS ( DBPTR, STATEMENT_LIST )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	Tbl	*list;	
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
        else if( ! get_stringtbl( prhs[1], &list ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	db = dbprocess( db, list, dbprocess_error );
	antelope_mex_clear_register( 1 );

	freetbl( list, 0 );

	if( db.table == dbINVALID )
	{
		mexErrMsgTxt( "dbprocess failed" );
	}

	plhs[0] = CreateDbptrStructFromDbptr( db );

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "dbprocess: failed to create " );
		strcat( errmsg, "database-pointer structure for result" );
		mexErrMsgTxt( errmsg );
	}
}
