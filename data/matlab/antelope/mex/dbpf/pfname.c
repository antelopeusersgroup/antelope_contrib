/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> pfname\n\n\
Usage: NAME = PFNAME ( DBPF )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Pf 	*pf;
	mxArray	*pfname;

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
        else if( ! get_pf( prhs[0], &pf ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	if( pf->type != PFFILE )
	{
		mexErrMsgTxt( "pfname: not of PFFILE type" );
	}
	else
	{
		pfname = mxGetField( prhs[0], 0, "pfname" );
		if( pfname == (mxArray *) NULL )
		{
			mexErrMsgTxt( "pfname: could not retrieve name" );
		}
		else
		{
			plhs[0] = mxDuplicateArray( pfname );
		}
	}
}
