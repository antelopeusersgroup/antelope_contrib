/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#include <stdio.h>
#include "antelope_mex.h"

static int dbpf_struct_nrows = 1;
static int dbpf_struct_ncols = 1;
static int dbpf_struct_nfields = 2;
static const char *dbpf_struct_fieldnames[] = {"address","pfname"};

Pf *mxArray2Pf( mxArray *array )
{
	Pf	*pf;
	mxArray *address;

	address = mxGetField( array, 0, "address" );

	if( address == (mxArray *) NULL ) 
	{
		return (Pf *) NULL;
	}

	pf = (Pf *) mxArrayToUlong( address );

	if( pf == (Pf *) NULL )
	{
		return (Pf *) NULL;
	}
	else {
		return pf;
	}
}

mxArray *Pf2mxArray( Pf *pf, char *name )
{
	mxArray	*address;
	mxArray	*pfname;
	mxArray *input[2];
	mxArray	*array[1];

	input[0] = mxCreateStructMatrix(dbpf_struct_nrows,
                                        dbpf_struct_ncols,
                                        dbpf_struct_nfields,
                                        dbpf_struct_fieldnames);

	input[1] = mxCreateString("dbpf");

	address = UlongToMxArray( (unsigned long) pf );
	if( address == NULL )
	{
		mxDestroyArray( input[0] );
		mxDestroyArray( input[1] );
		return (mxArray *) NULL;
	}
	else
	{
		mxSetField( input[0], 0, "address", address );
	}

	if( name == 0 )
	{
		pfname = mxCreateString( "" );
	}
	else
	{
		pfname = mxCreateString( name );
	}

	mxSetField( input[0], 0, "pfname", pfname );

	mexCallMATLAB( 1, array, 2, input, "class" );

	mxDestroyArray( input[0] );
	mxDestroyArray( input[1] ); 

	if( ! mxIsClass( array[0], "dbpf" ) )
	{
		mxDestroyArray( array[0] );
		return (mxArray *) NULL;
	}
	else
	{
		return array[0];
	}
}
