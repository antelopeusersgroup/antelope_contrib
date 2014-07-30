/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> orbping\n\n\
Usage: VERSION = ORBPING ( ORBFD )\n"

#include "antelope_mex.h"
#include "mex_orb.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	double	orbfd;
	int	version;

	if( nrhs != 1 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	if( ! get_scalar( prhs[0], &orbfd ) )
	{
		mexErrMsgTxt( "orbping: bad orb file descriptor\n" );
	}

	if( mex_orbping( (int) orbfd, &version ) ) {
		antelope_mex_clear_register( 1 );
		mexErrMsgTxt ( "orbping: failed\n" );
	}

	plhs[0] = CreateDouble( (double) version );
	if( plhs[0] == NULL )
	{
		mexErrMsgTxt( "orbping: failed to create return value" );
	}
}
