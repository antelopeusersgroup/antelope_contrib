/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 1997-2004
 */

#define USAGE "Error using ==> trextract_data\n\n\
Usage: DATA = TREXTRACT_DATA ( TRPTR )\n"

#include "antelope_mex.h"
#include "tr.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	tr;
	long	nrecs;
	int 	single_row;
	long	nsamp = 0;
	float	*data = NULL;
	double	*doublep;
	int	rc;
	int	i;

	if( nrhs != 1 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &tr ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( nlhs > 1 ) 
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	rc = dbquery( tr, dbRECORD_COUNT, &nrecs );
	antelope_mex_clear_register( 1 );
	if( rc == dbINVALID )
	{
		mexErrMsgTxt( 
			"trextract_data: query for number of records failed" );
	}

	if( tr.record == dbALL ) {
		if( nrecs == 1 ) {
			single_row = 1;
			tr.record = 0;
		} else {
			single_row = 0;
		}
	} else {
		single_row = 1;
	}

	if( ! single_row )
	{
		mexErrMsgTxt( "trextract_data: TRPTR  must have one row only" );
	}

	dbgetv( tr, 0, "nsamp", &nsamp, "data", &data, NULL );
	antelope_mex_clear_register( 1 );

	if( nsamp == 0 || data == NULL ) 
	{
		mexErrMsgTxt( "trextract_data: No data" );
	}

	plhs[0] = mxCreateDoubleMatrix( (mwSize) nsamp, 1, mxREAL );
	if( plhs[0] == NULL ) 
	{
		mexErrMsgTxt( "trextract_data: failed to allocate memory" );
	}

	doublep = (double *) mxGetPr( plhs[0] );

	for( i=0; i<nsamp; i++ )
	{
		doublep[i] = (double) data[i];
	}
}
