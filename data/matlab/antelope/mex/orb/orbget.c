/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using orbget\n\n\
Usage: [RESULT, TIME, SRCNAME, PKTID, TYPE] = ORBGET ( ORBFD [, CODE **or** PKTID] )\n"

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
	char	srcname[STRSZ];
	char	pkttype[STRSZ];
	double	time;
	char	*packet = 0;
	int	nbytes = 0;
	int	bufsize = 0;
	int	rc;		

	if( nrhs < 1 || nrhs > 2  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_scalar( prhs[0], &orbfd ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	which = xlatname( "ORBCURRENT", myOrbxlat, myNOrbxlat );

        if( nrhs == 2 )
	{
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
						"orbget: bad packet code %s", 
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
        }

	rc = mex_orbget( (int) orbfd, which, &pktid, &srcname[0], &time,
			      &packet, &nbytes, &bufsize );
	antelope_mex_clear_register( 1 );
	if( rc < 0 )
	{
		return;
	}

	plhs[0] = orbpkt2mxArray( srcname, time, packet, nbytes, pkttype );
	plhs[1] = CreateDouble( time );
	plhs[2] = mxCreateString( srcname );
	plhs[3] = CreateDouble( (double) pktid );
	plhs[4] = mxCreateString( pkttype );

	free( packet );
}
