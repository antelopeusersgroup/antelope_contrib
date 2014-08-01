/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbgetv\n\n\
Usage: [VALUE, VALUE, ...] = DBGETV ( DBPTR, FIELD [,FIELD, ...] )\n"

#include <stdlib.h>
#include "antelope_mex.h"

void
cleanup_and_bail( mxArray *plhs[], char *field_name, int i )
{
	int	iclean;
	char    errmsg[STRSZ];

	for( iclean = 0; iclean < i; iclean++ )
	{
		mxDestroyArray( plhs[iclean] );
	}

	sprintf( errmsg, "dbgetv failed on field %s\n", field_name );
	mxFree( field_name );

	mexErrMsgTxt ( errmsg );
}

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	int	single_row;
	long	nrows;
	int	nfields;
	char	*field_name;
	int	arg_index;
	int	rc;
	int	i;

	if( nrhs < 2 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	nfields = nrhs - 1;
	for( i=0; i< nfields; i++ )
	{
		arg_index = i + 1;
		if( mxGetClassID( prhs[arg_index] ) != mxCHAR_CLASS )
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
	}

	rc = dbquery( db, dbRECORD_COUNT, &nrows );
	antelope_mex_clear_register( 1 );
	if( rc == dbINVALID ) 
	{
		mexErrMsgTxt( "dbgetv: query for number of records failed" );
	}

	if( nrows <= 0 ) 
	{
		mexErrMsgTxt( "dbgetv: no rows in database view" );
	}

	if( db.record == dbALL ) {
		if( nrows == 1 ) {
			single_row = 1;
			db.record = 0;
		} else {
			single_row = 0;
		}
	} else {
		single_row = 1;
	}

	for( i = 0; i < nfields; i++ )
	{
		arg_index = i + 1;

		get_malloced_string( prhs[arg_index], &field_name );

		db = dblookup ( db, 0, 0, field_name, 0 );
		antelope_mex_clear_register( 1 );

		if( db.field < 0 ) 
		{
			cleanup_and_bail( plhs, field_name, i );
		}

		if( single_row )
		{
			plhs[i] = dbfield2mxArray( db );
		}
		else
		{
			plhs[i] = dbcolumn2mxArray( db );
		}

		if( plhs[i] == (mxArray *) NULL )
		{
			cleanup_and_bail( plhs, field_name, i );
		}

		mxFree( field_name );
	}
}
