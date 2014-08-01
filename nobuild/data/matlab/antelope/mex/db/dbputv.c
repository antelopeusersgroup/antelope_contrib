/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> dbputv\n\n\
Usage: DBPUTV ( DBPTR, FIELD, VALUE [,FIELD, VALUE ... ] )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Dbptr	db;
	Dbvalue	*value;
	char	errmsg[STRSZ];
	char	*field_name;
	int	nfields;
	int	retcode = 0;
	int	rc;
	long	type;
	int	fieldname_index;
	int	fieldval_index;
	int	i;

	if( nrhs < 3 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
	else if( ( nrhs - 1 ) % 2 != 0 )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}
        else if( ! get_dbptr( prhs[0], &db ) )
        {
                antelope_mexUsageMsgTxt ( USAGE );
		return;
        }

	nfields = ( nrhs - 1 ) / 2;

	for( i = 0; i < nfields; i++ )
	{
		fieldname_index = i * 2 + 1;
		if( mxGetClassID( prhs[fieldname_index] ) != mxCHAR_CLASS )
		{
			antelope_mexUsageMsgTxt ( USAGE );
			return;
		}
	}

	for( i=0; i<nfields; i++ )
	{
		fieldname_index = i * 2 + 1;
		fieldval_index = fieldname_index + 1;

		get_malloced_string( prhs[fieldname_index], &field_name );

		db = dblookup ( db, 0, 0, field_name, 0 );
		antelope_mex_clear_register( 1 );

		rc = dbquery ( db, dbFIELD_TYPE, &type );
		antelope_mex_clear_register( 1 );
		if( rc == dbINVALID )
		{
			sprintf( errmsg,
				 "dbputv: dbquery failed for field %s",
				 field_name );
			mxFree( field_name );
			mexErrMsgTxt( errmsg );
		}

		value = mxArray2dbvalue( prhs[fieldval_index], type );
		if( value == (Dbvalue *) NULL )
		{
			sprintf( errmsg,
				 "dbputv: failed to convert field %s",
				 field_name );
			mxFree( field_name );
			mexErrMsgTxt( errmsg );
		}

		switch (type ) {
		case dbDBPTR:
			retcode |= dbputv( db, 0, field_name, value->db, NULL );
			break;
		case dbSTRING:
			retcode |= dbputv( db, 0, field_name, value->s, NULL );
			break;
		case dbBOOLEAN:
		case dbYEARDAY:
		case dbINTEGER:
			retcode |= dbputv( db, 0, field_name, value->i, NULL );
			break;
		case dbREAL:
		case dbTIME:
			retcode |= dbputv( db, 0, field_name, value->d, NULL );
			break;
		default:
			retcode = -1;
			break;
		}
		antelope_mex_clear_register( 1 );

		mxFree( value );
		mxFree( field_name );
	}

	if( retcode != 0 )
	{
		mexErrMsgTxt( "dbputv failed" );
	}
}
