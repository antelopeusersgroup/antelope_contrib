/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> trfree\n\n\
Usage: TRFREE ( TRPTR )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	tr;
	mxArray *varname[1];
	mxArray *output_array[1];
	int	destroy = 0;

	if( nrhs != 1 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &tr ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	if( tr.table == dbALL ) destroy = 1;

	trfree( tr );
	antelope_mex_clear_register( 1 );

	if( destroy )
	{
		varname[0] = mxCreateString( mxGetName( prhs[0] ) );
		if( varname[0] == 0 )
		{
			mexErrMsgTxt (
				"Couldn't allocate name string for callback" );
		}

		mexCallMATLAB( 0, output_array, 1, varname, "clear" );
	}
}
