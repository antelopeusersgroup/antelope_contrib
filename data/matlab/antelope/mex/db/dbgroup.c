/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting 
 * 1997-2002
 */

#define USAGE "Error using ==> dbgroup\n\n\
Usage: DBPTR = DBGROUP ( DBPTR, GROUPFIELDS, [TYPE] )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	Tbl	*groupfields = 0;
	char	errmsg[STRSZ];
	int	rhs_index;
	long	type = 1;
	double 	type_fp = 1;

	if( nrhs < 2 || nrhs > 3  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( ! get_stringtbl( prhs[1], &groupfields ) ) 
	{
                antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( nrhs == 3 ) {

		if( ! get_scalar( prhs[2], &type_fp ) ) {

                	antelope_mexUsageMsgTxt ( USAGE );
			return;

		} else {

			type = (long) type_fp;
		} 
	}

	db = dbgroup( db, groupfields, NULL, type );
	antelope_mex_clear_register( 1 );

	freetbl( groupfields, 0 );

	if( db.table == dbINVALID )
	{
		mexErrMsgTxt( "dbgroup: group failed" );
	}

	plhs[0] = CreateDbptrStructFromDbptr( db );

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "dbgroup: failed to create database-pointer " );
		strcat( errmsg, "structure for result" );
		mexErrMsgTxt( errmsg );
	}
}
