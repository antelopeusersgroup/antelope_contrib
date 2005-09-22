/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using ==> pfget\n\n\
Usage: RESULT = PFGET ( DBPF, NAME [, 'recursive' ] )\n"

#include <stdio.h>
#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Pf 	*pf;
	char	*name;
	char	*recstring;
	void	*value;
	int	type;
	int	recursive = 0;

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
	if( ! mtlb_get_string( prhs[1], &name ) )
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

#ifdef PFPROMPT
	type = pfpeek( pf, name, &value );
	if( type == PFINVALID )
	{
		mxFree( name );
		mexErrMsgTxt( "pfget: Couldn't find specified entry" );
	}
	else if( type == PFPROMPT )
	{
		mxFree( name );
		plhs[0] = mxPfprompt( ((Pf *) value)->value.s );
		return;
	}
#else
	pfconfig( "ask", (void *) matlabPfprompt );
#endif

	type = pfget( pf, name, &value );
	antelope_mex_clear_register( 1 );

	switch( type )
	{
	case PFSTRING:
		plhs[0] = pfstring2mxArray( (char *) value );
		break;
	case PFFILE:
	case PFARR:
		if( recursive )
		{
			plhs[0] = pfarr2struct( (Pf *) value, recursive );
		}
		else
		{
			plhs[0] = Pf2mxArray( (Pf *) value, name );
		}
		break;
	case PFTBL:
		plhs[0] = pftbl2cellarr( (Pf *) value, recursive );
		break;
#ifdef PFPROMPT
	case PFPROMPT:
#endif
	case PFINVALID:
	default:
		plhs[0] = NULL;
		break;
	}

	mxFree( name ); 

	if( plhs[0] == NULL )
	{
		mexErrMsgTxt( "pfget: failed to create return value" );
	}
}
