/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> pfupdate\n\n\
Usage: [DBPF, MODIFIED] = PFUPDATE ( DBPF )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Pf 	*pf;
	char	*pfname;
	int	modified;

	if( nlhs > 2 ) 
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

	if( pf->type != PFFILE )
	{
		mexErrMsgTxt( "pfupdate: not of PFFILE type" );
	}
	else
	{
		if( !  mtlb_get_string( mxGetField( prhs[0], 0, "pfname" ), &pfname ) )
		{
			mexErrMsgTxt( "pfupdate: could not retrieve name" );
		}
	}

	modified = pfupdate( pfname, &pf );
	antelope_mex_clear_register( 1 );

	plhs[0] = Pf2mxArray( pf, pfname );

	mxFree( pfname );

	if( plhs[0] == NULL )
	{
		mexErrMsgTxt( "pfupdate: failed to create return value" );
	}

	if( nlhs == 2 )
	{
		plhs[1] = CreateDouble( (double) modified );
	}
}
