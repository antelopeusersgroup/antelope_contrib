/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#define USAGE "Error using dbtemplate\n\n\
Usage: DBPTR = DBTEMPLATE ( ,,, )\n"
#define USAGE "Error using dbtemplate\n\n\
Usage: DBTEMPLATE ( ,,,, )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{

	if( nrhs !=  )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( ! mtlb_get_string( prhs[1], &
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	mxFree(

	plhs[0] =
}
