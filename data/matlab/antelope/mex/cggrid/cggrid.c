/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2003
 */

#define USAGE "Error using ==> cggrid\n\n\
Usage: [CGGRID] = CGGRID ( FILE )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	char	*filename;
	FILE	*fp;
	CGGrid	*cgg;
	mxArray	*linear;

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
        else if( ! get_string( prhs[0], &filename ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	if( ( fp = fopen( filename, "r" ) ) == NULL ) 
	{
		perror( "cggrid" );
		mxFree( filename );
		mexErrMsgTxt( "Failed to open cggrid file." );
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

	plhs[0] = CGGrid2mxArray( cgg );

	if( ! plhs[0] ) 
	{
		antelope_mex_clear_register( 1 );
		mexErrMsgTxt( "Failed to create cggrid return value");
	} 
}
