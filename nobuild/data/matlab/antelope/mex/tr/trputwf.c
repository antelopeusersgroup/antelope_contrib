/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> trputwf\n\n\
Usage: TRPUTWF ( DBPTR, DATA )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	float	*data = NULL;
	double	*doublep;
	mwSize	*dims;
	int	nsamp;
	int	rc;
	int	i;

	mexWarnMsgTxt ( "\ntrputwf is DEPRECATED; switch to trsave_wf\n" );

	if( nrhs != 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( mxGetClassID( prhs[1] ) != mxDOUBLE_CLASS )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
        else if( mxGetNumberOfDimensions( prhs[1] ) != 2 )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	dims = (mwSize *) mxGetDimensions( prhs[1] );
	if( dims[1] != 1 && dims[0] != 1 ) 
		mexErrMsgTxt ( "trputwf: DATA must have dimensions Nx1 or 1xN\n" );
	if( dims[1] == 1 ) nsamp = dims[0];
	else nsamp = dims[1];

	doublep = mxGetPr( prhs[1] );

	data = mxCalloc( nsamp, sizeof( float ) );
	for( i=0; i<nsamp; i++ )
	{
		data[i] = (float) doublep[i];
	}

	rc = trputwf( db, data );
	antelope_mex_clear_register( 1 );

	mxFree( data );

	if( rc == dbINVALID )
	{
		mexErrMsgTxt( "trputwf: failed\n" );
	}
}
