/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> pfkeys\n\n\
Usage: KEYS = PFKEYS ( DBPF )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Pf 	*pf;
	Tbl	*keys;

	if( nlhs > 1 ) 
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

	keys = pfkeys( pf );	
	antelope_mex_clear_register( 1 );

	if( keys == NULL )
	{
		mexErrMsgTxt( "pfkeys: Empty list of keys" );
	}
	else
	{
		plhs[0] = stringtbl2cellstr( keys );	
	}
}
