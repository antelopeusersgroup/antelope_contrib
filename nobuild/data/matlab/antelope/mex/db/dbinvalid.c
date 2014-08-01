/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbinvalid\n\n\
Usage: DBPTR = DBINVALID\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	char	errmsg[STRSZ];

	if( nrhs != 0 )
	{
		antelope_mexUsageMsgTxt( USAGE );
		return;
	}

	plhs[0] = CreateNullDbptrStruct();

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "dbinvalid: failed to create invalid " );
		sprintf( errmsg, "database-pointer " );
		strcat( errmsg, "structure for result" );
		mexErrMsgTxt( errmsg );
	}
}
