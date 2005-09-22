/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> pfget_boolean\n\n\
Usage: VALUE = PFGET_BOOLEAN ( DBPF, NAME )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Pf 	*pf;
	Pf	*promptpf;
	char	*name;
	int	boolean;
	mxArray	*mxboolean;

	if( nlhs > 1 ) 
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
	else if( ! mtlb_get_string( prhs[1], &name ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	pfconfig( "ask", (void *) matlabPfprompt );

	boolean = pfget_boolean( pf, name );
	antelope_mex_clear_register( 1 );

	mxFree( name );

	plhs[0] = CreateDouble( (double) boolean );
}
