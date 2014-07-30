/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 1997-2002
 */

#define USAGE "Error using ==> getpid\n\n\
Usage: PID = GETPID ( )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	pid_t 	pid;

	if( nrhs != 0 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	pid = getpid();

	plhs[0] = CreateDouble( (double) pid );
	if( plhs[0] == NULL )
	{
		mexErrMsgTxt( "getpid: failed to create return value" );
	}
}
