/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbfilename\n\n\
Usage: [FILENAME STATUS] = DBFILENAME ( DBPTR )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	char	filename[FILENAME_MAX];
	char	status[STRSZ];
	int	rc;

	if( nrhs != 1 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	rc = dbfilename( db, filename );
	antelope_mex_clear_register( 1 );
	switch( rc ) {
	case 2:
		sprintf( status, "file exists, is compressed, and is readable" );
		break;
	case 1:
		sprintf( status, "file exists and is readable" );
		break;
	case 0:
		sprintf( status, "file does not exist, but directory is writeable" );
		break;
	case -1:
		sprintf( status, "file does not exist; directory is not writeable" );
		break;
	default:
		sprintf( status, "file status unknown" );
		break;
	}

	plhs[0] = mxCreateString( filename );
	plhs[1] = mxCreateString( status );
}
