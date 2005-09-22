/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbadd_remark\n\n\
Usage: DBADD_REMARK ( DBPTR, REMARK )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	char	*remark;
	int	result;

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
        else if( ! mtlb_get_string( prhs[1], &remark ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	result = dbadd_remark( db, remark );
	antelope_mex_clear_register( 1 );

	if( result < 0 )
	{
		mxFree( remark );
		mexErrMsgTxt( "dbadd_remark failed\n" );
	}

	mxFree( remark );
}
