/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> pfresolve\n\n\
Usage: RESULT = PFRESOLVE ( DBPF, NAME [, 'recursive' ] )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Pf 	*pf;
	char	*name;
	char	*recstring;
	Pf	*value;
	int	recursive = 0;
	int	rc;

	if( nrhs < 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( nrhs > 3 )
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

	if( nrhs == 3 )
	{
		if( ! mtlb_get_string( prhs[2], &recstring ) )
		{
                	antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else if( ! STREQ( recstring, "recursive" ) )
		{
                	antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else
		{
			mxFree( recstring );
			recursive = 1;
		}
	}

	pfconfig( "ask", (void *) matlabPfprompt );

	rc = pfresolve( pf, name, 0, &value );
	if( rc == PFINVALID || rc < 0 )
	{
		mxFree( name );
		antelope_mex_clear_register( 1 );
		mexErrMsgTxt( "pfresolve: parameter extraction failed" );	
	}
	else
	{
		antelope_mex_clear_register( 1 );
	}

	if( value == NULL )
	{
		mxFree( name );
		mexErrMsgTxt( "pfresolve: Couldn't find specified entry" );
	}

	switch( value->type )
	{
	case PFSTRING:
		plhs[0] = pfstring2mxArray( value->value.s );
		break;
	case PFFILE:
	case PFARR:
		if( recursive )
		{
			plhs[0] = pfarr2struct( value, recursive );
		}
		else
		{
			plhs[0] = Pf2mxArray( value, name );
		}
		break;
	case PFTBL:
		plhs[0] = pftbl2cellarr( value, recursive );
		break;
#ifdef PFPROMPT
	case PFPROMPT:
		plhs[0] = mxPfprompt( value->value.s );
		break;
#endif
	case PFINVALID:
	default:
		plhs[0] = NULL;
		break;
	}

	mxFree( name ); 

	if( plhs[0] == NULL )
	{
		mexErrMsgTxt( "pfresolve: failed to create return value" );
	}
}
