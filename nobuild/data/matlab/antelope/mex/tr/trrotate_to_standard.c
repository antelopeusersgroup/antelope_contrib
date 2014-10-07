/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 1997-2003
 */

#define USAGE "Error using ==> trrotate_to_standard\n\n\
Usage: TRROTATE_TO_STANDARD ( TRPTR, NEWCHANS )\n"

#include "antelope_mex.h"
#include "tr.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	tr;
	char	*newchan[3];
	Tbl	*newchan_tbl;
	int	rc;
	int	i;

	if( nrhs != 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &tr ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( ! get_stringtbl( prhs[1], &newchan_tbl ) ) 
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

	rc = rotate_to_standard( tr, newchan );

	antelope_mex_clear_register( 1 );

	freetbl( newchan_tbl, 0 );

	plhs[0] = CreateDouble( (double) rc );
	if( plhs[0] == NULL )
	{
		mexErrMsgTxt( "trrotate_to_standard: failed to create return value" );
	}
}
