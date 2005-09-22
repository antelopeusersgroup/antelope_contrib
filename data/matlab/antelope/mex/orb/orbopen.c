/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using orbopen\n\n\
Usage: ORBFD = ORBOPEN ( ORBNAME, OPENTYPE )\n"

#include "antelope_mex.h"
#include "mex_orb.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	char	*filename;
	char	*permission;
	char	errmsg[STRSZ];
	int	orbfd;

	if( nrhs != 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! get_trimmed_string( prhs[0], &filename ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! mtlb_get_string( prhs[1], &permission ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( ( orbfd = mex_orbopen( filename, permission ) ) < 0 )
	{
		antelope_mex_clear_register( 1 );
		sprintf( errmsg, "orbopen: error opening %s\n", filename );
		mxFree( filename );
		mxFree( permission );
		mexErrMsgTxt ( errmsg );
	}

	mxFree( filename );
	mxFree( permission );

	plhs[0] = CreateDouble( (double) orbfd );
	if( plhs[0] == NULL )
	{
		mexErrMsgTxt( "orbopen: failed to create return value" );
	}
}
