/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> pfget_arr\n\n\
Usage: PFARR = PFGET_ARR ( DBPF, NAME [, 'recursive' ] )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Pf 	*pf;
	char	*name;
	char	*recstring;
	Pf	*pfarr;
	char	errstr[STRSZ];
	int	recursive = 0;

	if( nrhs < 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( nrhs > 3 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_pf( prhs[0], &pf ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( ! mtlb_get_string( prhs[1], &name ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( nrhs == 3 )
	{
		if( !mtlb_get_string( prhs[2], &recstring ) )
		{
                	antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else if( ! STREQ( recstring, "recursive" ) )
		{
                	antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else
		{
			mxFree( recstring );
			recursive = 1;
		}
	}

	pfget( pf, name, (void **) &pfarr );
	antelope_mex_clear_register( 1 );
	if( pfarr == NULL )
	{
		mxFree( name );
		mexErrMsgTxt( "pfget_arr: Couldn't find specified array" );
	}
	else if( pfarr->type != PFARR )
	{
		mxFree( name );
		mexErrMsgTxt( "pfget_arr: Specified parameter is not an array");
	}
	else 
	{
		if( recursive )
		{
			plhs[0] = pfarr2struct( pfarr, recursive );

			if( plhs[0] == NULL )
			{
				strcpy( errstr,
				 "pfget_arr: failed to recursively " );
				strcat( errstr,
				 "descend parameter-file object" );
				mexWarnMsgTxt( errstr );

				plhs[0] = Pf2mxArray( pfarr, name );
			}
		} 
		else
		{
			plhs[0] = Pf2mxArray( pfarr, name );
		}

		mxFree( name );
	}
}
