/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using orbseek\n\n\
Usage: PKTID = ORBSEEK ( ORBFD, CODE **or** PKTID )\n"

#include "antelope_mex.h"
#include "mex_orb.h"
#include "myorbxlat.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	double	orbfd;
	char	*orbstring_code;
	char	errmsg[STRSZ];
	int	which;
	double	which_dbl;
	int	pktid;

	if( nrhs != 2  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_scalar( prhs[0], &orbfd ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	if( mxIsChar( prhs[1] ) )
	{
		if( ! mtlb_get_string( prhs[1], &orbstring_code ) ) 
       		{
               		antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else
		{
			which =	 xlatname( orbstring_code, myOrbxlat, myNOrbxlat );
			if( which == -1 )
			{
				sprintf( errmsg,
					"orbseek: bad packet code %s", 
					orbstring_code );
				mxFree( orbstring_code );
				mexErrMsgTxt( errmsg );
			}
			mxFree( orbstring_code );
		}
	}
	else if( mxIsDouble( prhs[1] ) )
	{
		get_scalar( prhs[1], &which_dbl );
		which = (int) which_dbl;
	}
	else
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	pktid = mex_orbseek( (int) orbfd, which );
	antelope_mex_clear_register( 1 );

	plhs[0] = CreateDouble( (double) pktid );
}
