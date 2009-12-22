/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#define USAGE "Error using ==> db2struct\n\n\
Usage: S = DB2STRUCT ( DBPTR [, FIELD [, FIELD, ... ]] )\n"

#include "antelope_mex.h"

void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] )
{
	Tbl	*tables;
	Tbl	*available_fields;
	Tbl	*use_fields;
	Arr	*wantfields;
	char	**field_names;
	char	*field_name;
	char	errmsg[STRSZ];
	mxArray	*mxfield;
	Dbptr	db;
	int	nonnull = 1;
	long	is_view;
	int	Nfields;
	long	Nrecords;
	int	single_row;
	int	first_record;
	int	last_record;
	int	ifield;
	int	irhs;
	int	rc;

	if( nrhs < 1 )
	{
		antelope_mexUsageMsgTxt( USAGE );
		return;
	}
	else if( ! get_dbptr( prhs[0], &db ) )
	{
		antelope_mexUsageMsgTxt ( USAGE );
		return;
	}

	rc = dbquery( db, dbRECORD_COUNT, &Nrecords );
	antelope_mex_clear_register( 1 );
	if( rc == dbINVALID )
	{
		mexErrMsgTxt( "db2struct: dbquery failed" );
	}
	if( Nrecords < 1 )
	{
		mexErrMsgTxt( "db2struct: no records in view" );
	}

	if( db.record == dbALL ) {
		if( Nrecords == 1 ) {
			single_row = 1;
			db.record = 0;
		} else {
			single_row = 0;
		}
	} else {
		single_row = 1;
		Nrecords = 1;
	}

	rc = dbquery( db, dbTABLE_IS_VIEW, &is_view );
	antelope_mex_clear_register( 1 );
	if( rc == dbINVALID )
	{
		mexErrMsgTxt( "db2struct: dbquery failed" );
	}

	if( is_view )
	{
		rc = dbquery( db, dbVIEW_TABLES, &tables );
		antelope_mex_clear_register( 1 );
		if( rc == dbINVALID )
		{
			mexErrMsgTxt( "db2struct: dbquery failed" );
		}

		if( ! tables )
		{
			mexErrMsgTxt( "db2struct: failed to get view_tables" );
		}
		else if( maxtbl( tables ) != 1 )
		{
			mexErrMsgTxt( "db2struct: multiple tables in view" );
		}
		
	}

	rc = dbquery( db, dbTABLE_FIELDS, &available_fields );
	antelope_mex_clear_register( 1 );
	if( rc == dbINVALID )
	{
		mexErrMsgTxt( "db2struct: dbquery failed" );
	}

	if( available_fields == NULL )
	{
		mexErrMsgTxt( "db2struct: failed to find any fields in view" );
	}

	Nfields = maxtbl( available_fields );

	if( Nfields < 1 )
	{
		mexErrMsgTxt( "db2struct: failed to find any fields in view" );
	}

	if( nrhs > 1 )
	{
		wantfields = newarr( 0 );

		for( irhs = 1; irhs < nrhs; irhs++ )
		{
			if( ! mtlb_get_string( prhs[irhs], &field_name ) )
			{
				freearr( wantfields, 0 );
				antelope_mexUsageMsgTxt ( USAGE );
				return;
			} 
			else 
			{
				setarr( wantfields, 
					field_name,
					(void *) &nonnull );
			}

			mxFree( field_name );
		}

		use_fields = newtbl( 0 );

		for( ifield = 0; ifield < Nfields; ifield++ )
		{
			field_name = gettbl( available_fields, ifield );
			if( getarr( wantfields, field_name ) )
			{
				pushtbl( use_fields, strdup( field_name ) );
			}
		}

		freearr( wantfields, 0 );

		Nfields = maxtbl( use_fields );

		if( Nfields < 1 )
		{
			if( use_fields ) freetbl( use_fields, free );
			mexErrMsgTxt( 
				"db2struct: no requested fields available" );
		}
	}
	else
	{
		use_fields = available_fields;
	}

	field_names = (char **) mxCalloc( Nfields, sizeof( char * ) );
	for( ifield = 0; ifield < Nfields; ifield++ )
	{
		field_names[ifield] = (char *) mxCalloc( STRSZ, sizeof( char ));
		strcpy( field_names[ifield],
			(char *) gettbl( use_fields, ifield ));
	}

	plhs[0] = mxCreateStructMatrix( 1, Nrecords, Nfields,
					(const char **) field_names );

	for( ifield = 0; ifield < Nfields; ifield++ )
	{
		mxFree( field_names[ifield] );
	}
	mxFree( field_names );

	if( plhs[0] == NULL )
	{
		if( nrhs > 1 && use_fields ) freetbl( use_fields, free );
		sprintf( errmsg, "db2struct: failed to create structure " );
		strcat( errmsg, "for result" );
		mexErrMsgTxt( errmsg );
	}

	if( single_row )
	{
		first_record = db.record;
		last_record = db.record;
	}
	else
	{
		first_record = 0;
		last_record = Nrecords - 1;
	}

	for( ifield = 0; ifield < Nfields; ifield++ )
	{
		db = dblookup( db, 0, 0, gettbl( use_fields, ifield ), 0 );
		antelope_mex_clear_register( 1 );

		for( db.record = first_record;
		       db.record <= last_record;
			 db.record++ )
		{
			mxfield = dbfield2mxArray( db );

			if( ! mxfield )
			{
				if( nrhs > 1 && use_fields ) 
				{
					freetbl( use_fields, free );
				}
				mxDestroyArray( plhs[0] );
				mexErrMsgTxt( "db2struct: conversion failed" );
			}

			mxSetField( plhs[0], 
				    db.record - first_record, 
				    gettbl( use_fields, ifield ),
				    mxfield );
		}
	}
	if( nrhs > 1 && use_fields ) freetbl( use_fields, free );
}
