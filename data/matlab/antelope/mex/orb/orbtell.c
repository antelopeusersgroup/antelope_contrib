/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> orbtell\n\n\
Usage: PKTID = ORBTELL ( ORBFD )\n"

#include "antelope_mex.h"
#include "mex_orb.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	double	orbfd;
	int	pktid;

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
	if( ! get_scalar( prhs[0], &orbfd ) )
	{
		mexErrMsgTxt( "orbtell: bad orb file descriptor\n" );
	}

	pktid = mex_orbtell( (int) orbfd );
	antelope_mex_clear_register( 1 );

	plhs[0] = CreateDouble( (double) pktid );
}
