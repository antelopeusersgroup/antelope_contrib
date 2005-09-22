/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> clear_register\n\n\
Usage: CLEAR_REGISTER ( [ 'print' ] )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	int	type;
	char	*string;
	int	printflag;
	double	*doublep;
	int	*intp;
	int	i;
	char	**messages;
	int	*count;
	int	NMAX;
	int	next, wrapflag;
	char	s[STRSZ];

	if( nrhs > 1 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( nrhs == 1 )
	{
		if( ! mtlb_get_string( prhs[0], &string ) )
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		} 
		else if( ! STREQ( string, "print" ) )
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		} 
		else
		{
			mxFree( string );
			printflag = 1;
		}

	}
	else
	{
		printflag = 0;
	}

	antelope_mex_clear_register ( printflag );
}
