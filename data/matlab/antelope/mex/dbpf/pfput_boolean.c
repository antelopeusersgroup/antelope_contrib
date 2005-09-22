/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> pfput_boolean\n\n\
Usage: PFPUT_BOOLEAN ( DBPF, NAME, VALUE )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Pf 	*pf;
	char	*name;
	double	boolean;
	mxArray	*mxboolean;

	if( nlhs > 0 ) 
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( nrhs != 3 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_pf( prhs[0], &pf ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( ! mtlb_get_string( prhs[1], &name ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ( mxboolean = mxTranslate_Boolean( prhs[2] ) ) == (mxArray *) NULL )
	{

		mxFree( name );
		antelope_mex_clear_register( 1 );
		return;
	}

	get_scalar( mxboolean, &boolean );

	pfput_boolean( pf, name, (int) boolean );
	antelope_mex_clear_register( 1 );

	mxFree( name );
	mxDestroyArray( mxboolean );
}
