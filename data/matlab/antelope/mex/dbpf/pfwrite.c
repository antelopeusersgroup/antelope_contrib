/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> pfwrite\n\n\
Usage: PFWRITE ( DBPF, FILENAME )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Pf 	*pf;
	char	*filename;

	if( nlhs > 0 ) 
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( nrhs != 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_pf( prhs[0], &pf ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( ! get_trimmed_string( prhs[1], &filename ) )
	{
                antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( pfwrite( filename, pf ) )
	{
		mxFree( filename );
		antelope_mex_clear_register( 1 );
		mexErrMsgTxt( "pfwrite: failed to write file" );
	}

	mxFree( filename );	
}
