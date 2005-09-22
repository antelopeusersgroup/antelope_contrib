/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbextfile\n\n\
Usage: [FILENAME STATUS] = DBEXTFILE ( DBPTR, TABLENAME )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	char	*tablename = 0;
	char	filename[FILENAME_MAX];
	char	status[STRSZ];
	int	rc;

	if( nrhs != 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( ! mtlb_get_string( prhs[1], &tablename ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	rc = dbextfile( db, tablename, filename );
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

	mxFree( tablename );

	plhs[0] = mxCreateString( filename );
	plhs[1] = mxCreateString( status );
}
