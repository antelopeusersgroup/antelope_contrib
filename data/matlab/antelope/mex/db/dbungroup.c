/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting 
 * 1997-2002
 */

#define USAGE "Error using ==> dbungroup\n\n\
Usage: DBPTR = DBUNGROUP ( DBPTR )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	Tbl	*groupfields = 0;
	char	errmsg[STRSZ];
	int	rhs_index;
	int	type = 1;
	double 	type_fp = 1;

	if( nrhs != 1 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	db = dbungroup( db, 0 );
	antelope_mex_clear_register( 1 );

	if( db.table == dbINVALID )
	{
		mexErrMsgTxt( "dbungroup: ungroup failed" );
	}

	plhs[0] = CreateDbptrStructFromDbptr( db );

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "dbungroup: failed to create database-pointer " );
		strcat( errmsg, "structure for result" );
		mexErrMsgTxt( errmsg );
	}
}
