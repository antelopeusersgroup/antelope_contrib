/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 1997-2003
 */

#define USAGE "Error using ==> trrotate\n\n\
Usage: TRROTATE ( TRPTR, PHI, THETA, NEWCHANS )\n"

#include "antelope_mex.h"
#include "tr.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	tr;
	double	phi;
	double	theta;
	char	*newchan[3];
	Tbl	*newchan_tbl;
	int	rc;
	int	i;

	if( nrhs != 4 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &tr ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( ! get_scalar( prhs[1], &phi ) )
	{
                antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! get_scalar( prhs[2], &theta ) )
	{
                antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ! get_stringtbl( prhs[3], &newchan_tbl ) ) 
	{
                antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	if( maxtbl( newchan_tbl ) != 3 ) 
	{
		freetbl( newchan_tbl, 0 );
		mexErrMsgTxt( "NEWCHANS array must have three channel names" );
	} 
	else 
	{
		for( i = 0; i < 3; i++ ) 
		{
			newchan[i] = gettbl( newchan_tbl, i );
		}
	}

	rc = trrotate( tr, phi, theta, newchan );

	antelope_mex_clear_register( 1 );

	freetbl( newchan_tbl, 0 );

	plhs[0] = CreateDouble( (double) rc );
	if( plhs[0] == NULL )
	{
		mexErrMsgTxt( "trrotate: failed to create return value" );
	}
}
