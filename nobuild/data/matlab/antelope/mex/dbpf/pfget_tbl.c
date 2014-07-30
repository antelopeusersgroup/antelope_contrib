/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> pfget_tbl\n\n\
Usage: TBL = PFGET_TBL ( DBPF, NAME [, 'recursive' ] )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Pf 	*pf;
	char	*name;
	char	*recstring;
	Tbl	*tbl;
	Pf	*pftbl;
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
		if( ! mtlb_get_string( prhs[2], &recstring ) )
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

	pfget( pf, name, (void **) &pftbl );
	antelope_mex_clear_register( 1 );
	if( pftbl == NULL )
	{
		mxFree( name );
		mexErrMsgTxt( "pfget_tbl: Couldn't find specified list" );
	}
	else if( pftbl->type != PFTBL )
	{
		mxFree( name );
		mexErrMsgTxt( "pfget_tbl: Specified parameter is not a table" );
	}
	else 
	{
		mxFree( name );
	
		plhs[0] = pftbl2cellarr( pftbl, recursive );	
	}
}
