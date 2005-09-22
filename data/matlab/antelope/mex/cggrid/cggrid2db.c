/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2003
 */

#define USAGE "Error using ==> cggrid2db\n\n\
Usage: CGGRID2DB ( CGG, DBPTR, RECIPE_NAME, GRID_NAME, OUTPUT_FILE, FMT, UNITS, QGRIDTYPE, AUTH, ['overwrite'] )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
#ifndef HAVE_CGEOM
	mexErrMsgTxt( "No cggrid support in your version of Antelope" );
#else
	CGGrid	*cgg;
	char	*recipe_name;
	char	*grid_name;
	char	*output_file;
	char	*format;
	char	*units;
	char	*qgridtype;
	char	*auth;
	char	*overwrite;
	char	errmsg[STRSZ];
	Dbptr	db;
	int	flags;
	int	rc;

	if( nrhs != 9 && nrhs != 10 )
	{
		antelope_mexUsageMsgTxt( USAGE );
		return;
	}
	else if( ! get_cggrid( prhs[0], &cgg ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! get_dbptr( prhs[1], &db ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! mtlb_get_string( prhs[2], &recipe_name ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! mtlb_get_string( prhs[3], &grid_name ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! get_trimmed_string( prhs[4], &output_file ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! mtlb_get_string( prhs[5], &format ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! mtlb_get_string( prhs[6], &units ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! mtlb_get_string( prhs[7], &qgridtype ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! mtlb_get_string( prhs[8], &auth ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( nrhs == 10 ) 
	{
		if( ! mtlb_get_string( prhs[9], &overwrite ) ) 
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else if( ! STREQ( overwrite, "overwrite" ) )
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else
		{
			mxFree( overwrite );
			flags |= CG_OVERWRITE;
		}
	}

	rc = cggrid2db( db, cgg, recipe_name, grid_name,
		        output_file, format, units, qgridtype, 
			auth, flags );
	antelope_mex_clear_register( 1 );

	mxFree( recipe_name );
	mxFree( grid_name );
	mxFree( output_file );
	mxFree( format );
	mxFree( units );
	mxFree( qgridtype );
	mxFree( auth );

	if( rc != 0 ) 
	{
		antelope_mex_clear_register( 1 );
		mexErrMsgTxt( "Failed to save cggrid to database");
	}
#endif
}
