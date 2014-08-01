/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> pffiles\n\n\
Usage: FILENAMES = PFFILES ( PFNAME [, 'all' ] )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	char	*pfname;
	char	*optall;
	Tbl	*files;
	int	all = 0;

	if( nlhs > 1 ) 
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( nrhs == 0 || nrhs > 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! mtlb_get_string( prhs[0], &pfname ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( nrhs == 2 && ! mtlb_get_string( prhs[1], &optall ) )
        {
		mxFree( pfname );
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( nrhs == 2 )
	{
		if( ! STREQ( optall, "all" ) )
		{
			mxFree( pfname );
			mxFree( optall );
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		} 
		else
		{
			mxFree( optall );
			all = 1;
		}
	}

	files = pffiles( pfname, all );
	antelope_mex_clear_register( 1 );

	mxFree( pfname );

	if( files == NULL )
	{
		mexErrMsgTxt( "pffiles: No relevant files found" );
	} 
	else
	{
		plhs[0] = stringtbl2cellstr( files );

		freetbl( files, free );
	}
}
