/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2003
 */

#define USAGE "Error using ==> cggrid\n\n\
Usage: CGG = CGGRID ( FILE )\n       CGG = CGGRID ( X, Y, Z )\n"

#include <stdio.h>
#include <errno.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
#ifndef HAVE_CGEOM
	mexErrMsgTxt( "No cggrid support in your version of Antelope" );
#else
	char	*filename;
	FILE	*fp;
	CGGrid	*cgg;
	mxArray	*linear;
	char	errormsg[STRSZ];

	if( nlhs > 1 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( nrhs == 1 )
	{
		if( ! get_trimmed_string( prhs[0], &filename ) )
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}

		if( ( fp = fopen( filename, "r" ) ) == NULL ) 
		{
			mxFree( filename );
			sprintf( errormsg, "Failed to open cggrid file: %s.",
				 strerror( errno ) );
			mexErrMsgTxt( errormsg );
		}
		else 
		{
			mxFree( filename );
		}

		if( ( cgg = cggrid_read( fp ) ) == 0 ) 
		{
			fclose( fp );
			antelope_mex_clear_register( 1 );
			mexErrMsgTxt( "Failed to read cggrid file." );
		}
		else 
		{
			antelope_mex_clear_register( 1 );
			fclose( fp );
		}
	} 
	else if( nrhs == 3 ) 
	{
		cgg = plaid_mxArrays2CGGrid( prhs[0], prhs[1], prhs[2] );

		if( cgg == (CGGrid *) NULL ) 
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
	}
	else 
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	plhs[0] = CGGrid2mxArray( cgg );

	if( ! plhs[0] ) 
	{
		cggrid_free( &cgg );
		antelope_mex_clear_register( 1 );
		mexErrMsgTxt( "Failed to create cggrid return value");
	} 

	ATM_cggrid_register( cgg );
#endif
}
