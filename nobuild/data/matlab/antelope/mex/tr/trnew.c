/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> trnew\n\n\
Usage: TRNEW ( [PATH [, SCHEMA_NAME]] )\n"

#include "antelope_mex.h"
#include "tr.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	tr;
	char	*tr_path = 0;
	char 	*schema_name = 0;
	char	errmsg[STRSZ];

	if( nrhs > 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( nrhs >= 1 && ! get_trimmed_string( prhs[0], &tr_path ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }
	else if( nrhs == 2 && ! mtlb_get_string( prhs[1], &schema_name ) )
        {
		mxFree( tr_path );
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	tr = trnew( tr_path, schema_name ); 
	antelope_mex_clear_register( 1 );

	if( tr_path ) mxFree( tr_path );
	if( schema_name ) mxFree( schema_name );

	if( tr.database == dbINVALID ) 
	{
		mexErrMsgTxt( "trnew: failed to create new trace database" );
	}

	plhs[0] = CreateDbptrStructFromDbptr( tr );

	if( plhs[0] == NULL )
	{
		sprintf( errmsg, "trnew: failed to create database-pointer " );
		strcat( errmsg, "structure for result" );
		mexErrMsgTxt( errmsg );
	}
}
