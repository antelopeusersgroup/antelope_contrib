/*
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#include "antelope_mex.h"

static int dbptr_struct_nrows = 1;
static int dbptr_struct_ncols = 1;
static int dbptr_struct_nfields = 4;
static const char *dbptr_struct_fieldnames[] = {"database",
						"table",
						"field",
						"record"};

mxArray *
CreateNullDbptrStruct( void )
{
	return CreateDbptrStructFromDbptr( dbinvalid() );
}

mxArray *
CreateDbptrStructFromDbptr( Dbptr db )
{
	mxArray	*struct_array_ptr;
	mxArray	*dbcode;

	struct_array_ptr = mxCreateStructMatrix(dbptr_struct_nrows,
						dbptr_struct_ncols,
						dbptr_struct_nfields,
						dbptr_struct_fieldnames);

	if (struct_array_ptr == NULL)
	{
		return (mxArray *) NULL;
	}

	dbcode = CreateDouble( (double) db.database );
	if( dbcode == NULL )
	{
		mxDestroyArray( struct_array_ptr );
		return (mxArray *) NULL;
	}
	else
	{
		mxSetField( struct_array_ptr, 0, "database", dbcode );
	}

	dbcode = CreateDouble( (double) db.table );
	if( dbcode == NULL )
	{
		mxDestroyArray( struct_array_ptr );
		return (mxArray *) NULL;
	}
	else
	{
		mxSetField( struct_array_ptr, 0, "table", dbcode );
	}

	dbcode = CreateDouble( (double) db.field );
	if( dbcode == NULL )
	{
		mxDestroyArray( struct_array_ptr );
		return (mxArray *) NULL;
	}
	else
	{
		mxSetField( struct_array_ptr, 0, "field", dbcode );
	}

	dbcode = CreateDouble( (double) db.record );
	if( dbcode == NULL )
	{
		mxDestroyArray( struct_array_ptr );
		return (mxArray *) NULL;
	}
	else
	{
		mxSetField( struct_array_ptr, 0, "record", dbcode );
	}

	return struct_array_ptr;
}

void
SetDbptrStructToDbptr( mxArray *DbptrStruct, Dbptr db )
{
	double	*pr;
	mxArray	*field;

	if( ! AssertIsDbptrStruct( DbptrStruct ) )
	{
		mexErrMsgTxt( "SetDbptrStructToDbptr: Needs dbptr struct\n" );
	}

	field = mxGetField( DbptrStruct, 0, "database" );
	pr = mxGetPr( field );
	*pr = (double) db.database;

	field = mxGetField( DbptrStruct, 0, "table" );
	pr = mxGetPr( field );
	*pr = (double) db.table;

	field = mxGetField( DbptrStruct, 0, "field" );
	pr = mxGetPr( field );
	*pr = (double) db.field;

	field = mxGetField( DbptrStruct, 0, "record" );
	pr = mxGetPr( field );
	*pr = (double) db.record;
}

Dbptr
CastDbptrStructToDbptr( const mxArray *DbptrStruct )
{
	Dbptr	db;
	mxArray	*field;
	int	*pr;

	if( ! AssertIsDbptrStruct( DbptrStruct ) )
	{
		return dbinvalid();
	}

	field = mxGetField( DbptrStruct, 0, "database" );
	db.database = mxArrayToLong( field );

	field = mxGetField( DbptrStruct, 0, "table" );
	db.table = mxArrayToLong( field );

	field = mxGetField( DbptrStruct, 0, "field" );
	db.field = mxArrayToLong( field );

	field = mxGetField( DbptrStruct, 0, "record" );
	db.record = mxArrayToLong( field );

	return db;
}

int
AssertIsDbptrStruct( const mxArray *teststruct )
{
	int	i;

	if( mxGetClassID( teststruct ) != mxSTRUCT_CLASS )
	{
		return 0;
	}

	if( mxGetNumberOfFields( teststruct ) != dbptr_struct_nfields )
	{
		return 0;
	}

	for( i=0; i<dbptr_struct_nfields; i++ )
	{
		if( ! STREQ( dbptr_struct_fieldnames[i],
			     mxGetFieldNameByNumber( teststruct, i ) ) )
		{
			return 0;
		}
	}

	return 1;
}
