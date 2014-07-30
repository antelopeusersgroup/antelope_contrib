/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2002
 */

#define USAGE "Error using ==> dbread_view\n\n\
Usage: DBPTR = DBREAD_VIEW ( FILENAME [, VIEWNAME] )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	char	*filename = 0;
	char	*viewname = 0;
	char	errmsg[STRSZ];
	FILE	*fp;
	int	rc;

	if( nrhs < 1 || nrhs > 2  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_trimmed_string( prhs[0], &filename ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( nrhs == 2 && ! mtlb_get_string( prhs[1], &viewname ) )
	{
                antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( ( fp = fopen( filename, "r" ) ) == NULL ) {
		
		sprintf( errmsg, 
			 "dbread_view: error opening file '%s'", 
			 filename );

		mxFree( filename );

		if( viewname ) { 
			mxFree( viewname ); 
		}

		mexErrMsgTxt( errmsg );

	}

	rc = dbread_view( fp, &db, viewname );
	antelope_mex_clear_register( 1 );

	mxFree( filename );

	if( viewname ) { 
		mxFree( viewname ); 
	}

	if( rc != 0 ) 
	{
		mexErrMsgTxt( "dbread_view: failed" );
	}

	plhs[0] = CreateDbptrStructFromDbptr( db );

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "dbread_view: failed to create " );
		strcat( errmsg, "database-pointer structure for result" );
		mexErrMsgTxt( errmsg );
	}
}
