/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2002
 */

#define USAGE "Error using concatpaths\n\n\
Usage: RES = CONCATPATHS ( A, B [, ...] )\n"

#include "antelope_mex.h"
#include "tttaup.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	char	*a;
	char	*b;
	char	*res;
	char	*hold;
	char	*another;
	int	nleft;

	if( nrhs < 2  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! mtlb_get_string( prhs[0], &a ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! mtlb_get_string( prhs[1], &b ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	res = concatpaths( a, b, NULL );

	nleft = nrhs - 2;

	while( nleft > 0 ) {

		nleft--;

		hold = res;

        	if( ! mtlb_get_string( prhs[nrhs-1-nleft], &another ) )
        	{
               		antelope_mexUsageMsgTxt ( USAGE );
			return;
        	}

		res = concatpaths( hold, another, NULL );

		free( hold );
	}

	plhs[0] = mxCreateString( res );

	if( res != (char *) NULL ) {

		free( res );
	}
}
