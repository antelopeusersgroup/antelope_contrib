/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbnextid\n\n\
Usage: ID = DBNEXTID ( DBPTR, IDNAME )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	char	*idname;
	long	id;

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
        else if( ! mtlb_get_string( prhs[1], &idname ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	id = dbnextid( db, idname );
	antelope_mex_clear_register( 1 );

	mxFree( idname );

	plhs[0] = CreateDouble( (double) id );
	if( plhs[0] == NULL )
	{
		mexErrMsgTxt( "dbnextid: failed to create return value" );
	}
}
