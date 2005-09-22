/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2003
 */

#define USAGE "Error using ==> cggrid_write\n\n\
Usage: CGGRID_WRITE ( CGG, FORMAT, FILE )\n"

#include <stdio.h>
#include <errno.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
#ifndef HAVE_CGEOM
	mexErrMsgTxt( "No cggrid support in your version of Antelope" );
#else
	char	*filename;
	char	*format;
	FILE	*fp;
	CGGrid	*cgg;
	char	errormsg[STRSZ];

	if( nlhs >= 1 ) 
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( nrhs != 3 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_cggrid( prhs[0], &cgg ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! mtlb_get_string( prhs[1], &format ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! get_trimmed_string( prhs[2], &filename ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	if( ( fp = fopen( filename, "w" ) ) == NULL ) 
	{
		mxFree( format );
		mxFree( filename );
		sprintf( errormsg,
			"Failed to open cggrid file for writing: %s.",
			strerror( errno ) );
		mexErrMsgTxt( errormsg );
	}

	if( cggrid_write( cgg, format, fp ) < 0 ) 
	{
		fclose( fp );
		mxFree( format );
		mxFree( filename );
		antelope_mex_clear_register( 1 );
		mexErrMsgTxt( "Failed to write cggrid file." );
	}
	else 
	{
		antelope_mex_clear_register( 1 );
		fclose( fp );
		mxFree( format );
		mxFree( filename );
	}
#endif
}
