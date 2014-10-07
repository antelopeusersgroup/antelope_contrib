/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 1997-2002
 */

#define USAGE "Error using ==> dbnrecs\n\n\
Usage: NRECS = DBNRECS ( DBPTR )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	long	nrecs;

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

	dbquery( db, dbRECORD_COUNT, &nrecs );
	antelope_mex_clear_register( 1 );

	plhs[0] = CreateDouble( (double) nrecs );
	if( plhs[0] == NULL )
	{
		mexErrMsgTxt( "dbnrecs: failed to create return value" );
	}
}
