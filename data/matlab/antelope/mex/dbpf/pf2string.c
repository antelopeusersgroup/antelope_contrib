/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> pf2string\n\n\
Usage: STRING = PF2STRING ( DBPF )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Pf 	*pf;
	char	*s;

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

	s = pf2string( pf );
	antelope_mex_clear_register( 1 );

	if( s == NULL ) 
	{
		mexErrMsgTxt( "pf2string: input parameter space is empty" );
	}
	else
	{
		plhs[0] = mxCreateString( s );
		free( s );
	}
}
