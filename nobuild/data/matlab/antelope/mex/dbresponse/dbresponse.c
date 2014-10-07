/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> dbresponse\n\n\
Usage: DBRESPONSE = DBRESPONSE ( FILE )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	char	*filename;
	FILE	*fp;
	Response *response;

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
        else if( ! get_trimmed_string( prhs[0], &filename ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	if( ( fp = fopen( filename, "r" ) ) == NULL ) {
		perror( "dbresponse" );
		mxFree( filename );
		mexErrMsgTxt( "Failed to open response file." );
	} else {
		mxFree( filename );
	}

	if( read_response( fp, &response ) != 0 ) {
		fclose( fp );
		antelope_mex_clear_register( 1 );
		mexErrMsgTxt( "Failed to read response file." );
	} else {
		antelope_mex_clear_register( 1 );
		fclose( fp );
	}

	plhs[0] = Response2mxArray( response );

	if( ! plhs[0] )
	{
		mexErrMsgTxt( "Failed to create an object of class dbresponse");
	}
}
 
 

