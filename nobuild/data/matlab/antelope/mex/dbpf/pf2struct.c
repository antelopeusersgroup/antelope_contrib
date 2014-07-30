/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> pf2struct\n\n\
Usage: STRUCT = PF2STRUCT ( DBPF [, 'recursive' ] )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Pf 	*pf;
	char	*recstring;
	int	recursive = 0;

	if( nrhs == 0 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( nrhs > 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_pf( prhs[0], &pf ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( nrhs == 2 && ! mtlb_get_string( prhs[1], &recstring ) )
	{
                antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( nrhs == 2 )
	{
		if( ! STREQ( recstring, "recursive" ) )
		{
                	antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else
		{
			mxFree( recstring );
			recursive = 1;
		}
	}

	plhs[0] = pfarr2struct( pf, recursive );

	if( plhs[0] == NULL )
	{
		mexErrMsgTxt( "pf2struct: failed to convert DBPF to struct" );
	}
}
