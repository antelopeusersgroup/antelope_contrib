/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2003
 */

#define USAGE "Error using ==> cggrid_supported\n\n\
Usage: SUPPORTED = CGGRID_SUPPORTED ( )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{

#ifndef HAVE_CGEOM

	plhs[0] = CreateDouble( 0 );

#else

	plhs[0] = CreateDouble( 1 );

#endif

}
