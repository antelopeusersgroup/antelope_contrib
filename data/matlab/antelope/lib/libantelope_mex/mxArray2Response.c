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

static int dbresponse_struct_nrows = 1;
static int dbresponse_struct_ncols = 1;
static int dbresponse_struct_nfields = 1;
static const char *dbresponse_struct_fieldnames[] = {"address"};

Response *mxArray2Response( mxArray *array )
{
	Response *response;
	mxArray *address;

	address = mxGetField( array, 0, "address" );

	if( address == (mxArray *) NULL ) 
	{
		return (Response *) NULL;
	}

	response = (Response *) mxArrayToUint32( address );

	if( response == (Response *) NULL )
	{
		return (Response *) NULL;
	}
	else {
		return response;
	}
}

mxArray *Response2mxArray( Response *response )
{
	mxArray	*address;
	mxArray *input[2];
	mxArray	*array[1];

	input[0] = mxCreateStructMatrix(dbresponse_struct_nrows,
                                        dbresponse_struct_ncols,
                                        dbresponse_struct_nfields,
                                        dbresponse_struct_fieldnames);

	input[1] = mxCreateString("dbresponse");

	address = Uint32ToMxArray( (unsigned int) response );
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

	mexCallMATLAB( 1, array, 2, input, "class" );

	mxDestroyArray( input[0] );
	mxDestroyArray( input[1] ); 

	if( ! mxIsClass( array[0], "dbresponse" ) )
	{
		mxDestroyArray( array[0] );
		return (mxArray *) NULL;
	}
	else
	{
		return array[0];
	}
}
