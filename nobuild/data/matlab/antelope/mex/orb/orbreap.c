/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using orbreap\n\n\
Usage: [RESULT, TIME, SRCNAME, PKTID, TYPE] = ORBREAP ( ORBFD [, 'nodelay'] )\n"

#include "antelope_mex.h"
#include "mex_orb.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	double	orbfd;
	char	*orbstring_code;
	char	*nodelay_string;
	int	nodelay = 0;
	char	errmsg[STRSZ];
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

        if( nrhs == 2 )
	{
		if( ! mtlb_get_string( prhs[1], &nodelay_string ) || ! STREQ( nodelay_string, "nodelay" ) )
		{
                	antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
		else
		{
			mxFree( nodelay_string );
			nodelay = 1;
		}
        }

	if( nodelay )
	{
		rc = mex_orbreap_nd( (int) orbfd, &pktid, &srcname[0], &time,
			      &packet, &nbytes, &bufsize );
	} 
	else
	{
		rc = mex_orbreap( (int) orbfd, &pktid, &srcname[0], &time,
			      &packet, &nbytes, &bufsize );
	}
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
