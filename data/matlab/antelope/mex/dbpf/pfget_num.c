/* 
 * Matlab interface to Datascope package
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> pfget_num\n\n\
Usage: NUM = PFGET_NUM ( DBPF, NAME )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Pf 	*pf;
	Pf	*promptpf;
	char	*name;
	char	*mystring;
	mxArray	*input[1];

	if( nlhs > 1 ) 
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( nrhs != 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_pf( prhs[0], &pf ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( ! get_string( prhs[1], &name ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	switch( pfpeek( pf, name, &promptpf ) )
	{
	case PFINVALID:
		mxFree( name );
		mexErrMsgTxt( "pfget_num: Specified parameter not found" );
	case PFPROMPT:
		mxFree( name );
		plhs[0] = mxPfprompt( promptpf->value.s );
		return;
	case PFSTRING:
		break;
	case PFARR:
	case PFTBL:
	default:
		mxFree( name );
		mexErrMsgTxt( 
		    "pfget_num: Specified parameter is not a scalar value" );
	}

	mystring = pfget_string( pf, name );	
	antelope_mex_clear_register( 1 );

	mxFree( name );

	if( mystring == NULL )
	{
		mexErrMsgTxt( "pfget_num: Couldn't get specified value" );
	}
	else
	{
		input[0] = mxCreateString( mystring );
		mexCallMATLAB( 1, plhs, 1, input, "str2num" );
		mxDestroyArray( input[0] );
	}
}
