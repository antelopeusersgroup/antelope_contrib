/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> pffree\n\n\
Usage: PFFREE ( DBPF )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Pf *pf;
	mxArray	*varname[1];
	mxArray *output_array[1];

	if( nlhs > 0 ) 
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( nrhs != 1 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_pf( prhs[0], &pf ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	if( pf->type == PFFILE )
	{
		pffree( pf );
		antelope_mex_clear_register( 1 );
	}

	varname[0] = mxCreateString( mxGetName( prhs[0] ) );
	if( varname[0] == 0 )
	{
		mexErrMsgTxt ( "Couldn't allocate name string for callback" );
	}

	mexCallMATLAB( 0, output_array, 1, varname, "clear" );
}
