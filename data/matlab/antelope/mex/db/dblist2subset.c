/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting 
 * 1997-2003
 */

#define USAGE "Error using ==> dblist2subset\n\n\
Usage: DBPTR = DBLIST2SUBSET ( DBPTR [, LIST] )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	Tbl	*list = 0;
	char	errmsg[STRSZ];
	int	rhs_index;
	int	type = 1;
	double 	type_fp = 1;

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
	else if( nrhs == 2 && ! get_inttbl( prhs[1], &list ) )
	{
                antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	db = dblist2subset( db, list );
	antelope_mex_clear_register( 1 );

	if( db.table == dbINVALID )
	{
		mexErrMsgTxt( "dblist2subset: subset creation failed" );
	}

	plhs[0] = CreateDbptrStructFromDbptr( db );

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "dblist2subset: failed to create database-pointer " );
		strcat( errmsg, "structure for result" );
		mexErrMsgTxt( errmsg );
	}
}
