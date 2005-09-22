/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dblookup_table\n\n\
Usage: DBPTR = DBLOOKUP_TABLE ( DBPTR, TABLE )\n"

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

	db = dblookup( db, 0, table, 0, 0 );
	antelope_mex_clear_register( 1 );
	if( db.table == dbINVALID ) 
	{
		mexErrMsgTxt( "dblookup_table: table lookup failed" );
	}

	mxFree( table );

	plhs[0] = CreateDbptrStructFromDbptr( db );

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "dblookup_table: failed to create " );
		strcat( errmsg, "database-pointer structure for result" );
		mexErrMsgTxt( errmsg );
	}
}
