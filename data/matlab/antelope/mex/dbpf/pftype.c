/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> pftype\n\n\
Usage: TYPESTRING = PFTYPE ( DBPF )\n"

#include <stdio.h>
#include "antelope_mex.h"
#include "mypfxlat.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Pf 	*pf;
	char	*type;

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

	type = xlatnum( pf->type, myPfxlat, myNPfxlat );

	if( type == NULL )
	{
		mexErrMsgTxt( "pftype: type not understood" );
	}
	else
	{
		plhs[0] = mxCreateString( type );
	}
}
