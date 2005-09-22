/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> dbpf\n\n\
Usage: PF = DBPF ()\n       PF = DBPF ( PFNAME )\n       PF = DBPF ( PF, STRING )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	char	*pfname;
	char	*string;
	Pf 	*pf;
	int	recursive = 0;

	if( nlhs > 1 ) 
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( nrhs > 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( nrhs == 0 )
	{
		pf = pfnew( PFFILE );

		plhs[0] = Pf2mxArray( pf, "" );
	}
        else if( nrhs == 1 )
        {
		if( ! mtlb_get_string( prhs[0], &pfname ) )
		{
                	antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else
		{
			if( pfread( pfname, &pf ) != 0 ) {
				mxFree( pfname );
				antelope_mex_clear_register( 1 );
				mexErrMsgTxt( "Failed to read parameter file." );
			} 
		
			plhs[0] = Pf2mxArray( pf, pfname );
		
			mxFree( pfname );
		}
	}
	else if( nrhs == 2 )
	{
		if( ! get_pf( prhs[0], &pf ) )
		{
                	antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else if( ! mtlb_get_string( prhs[1], &string ) )
		{
                	antelope_mexUsageMsgTxt ( USAGE );
			return;
		}

	mtlb_get_string( mxGetField( prhs[0], 0, "pfname" ), &pfname );

		pfcompile( string, &pf );

		plhs[0] = Pf2mxArray( pf, pfname  );

		mxFree( pfname );
		mxFree( string );
	}

	if( ! plhs[0] )
	{
		mexErrMsgTxt( "Failed to create an object of class dbpf");
	}
}
