/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> orbafter\n\n\
Usage: PKTID = ORBAFTER ( ORBFD, TIME )\n"

#include "antelope_mex.h"
#include "mex_orb.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	double	orbfd;
	double	time;
	int	nselected;
	int	pktid;

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
	if( ! get_scalar( prhs[0], &orbfd ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		mexErrMsgTxt( "orbafter: bad orb file descriptor\n" );
	}
	if( ! get_scalar( prhs[1], &time ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
	}

	pktid = mex_orbafter( (int) orbfd, time );
	antelope_mex_clear_register ( 1 );

	plhs[0] = CreateDouble( (double) pktid );
	if( plhs[0] == NULL )
	{
		mexErrMsgTxt( "orbafter: failed to create return value" );
	}
}
