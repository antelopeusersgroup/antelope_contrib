/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> orbclose\n\n\
Usage: ORBCLOSE ( ORBFD )\n"

#include "antelope_mex.h"
#include "mex_orb.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	double	orbfd;

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
	if( ! get_scalar( prhs[0], &orbfd ) )
	{
		mexErrMsgTxt( "orbclose: bad orb file descriptor\n" );
	}

	if( mex_orbclose( (int) orbfd ) ) {
		antelope_mex_clear_register( 1 );
		mexErrMsgTxt ( "orbclose: failed\n" );
	}
}
