/*
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#include "antelope_mex.h"

mxArray *
Int32ToMxArray( int input_int )
{
	mxArray	*struct_array_ptr;
	int     ndims = 1, dims[1] = {1};
	int	*pr;

	mxAssert( (8*sizeof(int) == 32 ),
		"Int32ToMxArray expects 4-byte ints\n");

        struct_array_ptr = mxCreateNumericArray( ndims, dims,
						 mxINT32_CLASS, mxREAL );
        if (struct_array_ptr == NULL)
	{
		return (mxArray *) NULL;
	}

	pr = (int *) mxGetData( struct_array_ptr );
	if( pr == NULL )
	{
		mxDestroyArray( struct_array_ptr );
		return (mxArray *) NULL;
	}
	else
	{
		*pr = input_int;
	}

	return struct_array_ptr;
}

int
mxArrayToInt32( mxArray *array_ptr )
{
	int	type;
	int	*intp;
	double	*doublep;
	float 	*floatp;
	char	*s;
	int	returnval = 0;

	type = mxGetClassID( array_ptr );
	switch( type )
	{
	case mxCHAR_CLASS:
		get_malloced_string( array_ptr, &s );
		returnval = atoi( s );		
		mxFree( s );
		break;
	case mxDOUBLE_CLASS:
		doublep = mxGetPr( array_ptr );
		returnval = (int) *doublep;
		break;
	case mxSINGLE_CLASS:
		floatp = (float *) mxGetData( array_ptr );
		returnval = (int) *floatp;
		break;
	case mxINT32_CLASS:
		intp = (int *) mxGetData( array_ptr );
		returnval = *intp;
		break;
	default:
		mexErrMsgTxt( "Wrong class passed to mxArrayToInt32\n" );
		break;
	}

	return returnval;

}

mxArray *
Uint32ToMxArray( unsigned int input_uint )
{
	mxArray	*struct_array_ptr;
	int     ndims = 1, dims[1] = {1};
	unsigned int	*pr;

	mxAssert( (8*sizeof(unsigned int) == 32 ),
		"Uint32ToMxArray expects 4-byte uints\n");

        struct_array_ptr = mxCreateNumericArray( ndims, dims,
						 mxUINT32_CLASS, mxREAL );
        if (struct_array_ptr == NULL)
	{
                return (mxArray *) NULL;
	}

	pr = (unsigned int *) mxGetData( struct_array_ptr );
	if( pr == NULL )
	{
		mxDestroyArray( struct_array_ptr );
		return (mxArray *) NULL;
	}
	else
	{
		*pr = input_uint;
	}

	return struct_array_ptr;
}

unsigned int
mxArrayToUint32( mxArray *array_ptr )
{
	int	type;
	int	*intp;
	unsigned int *uintp;
	double	*doublep;
	float 	*floatp;
	char	*s;
	unsigned int returnval = 0;

	type = mxGetClassID( array_ptr );
	switch( type )
	{
	case mxCHAR_CLASS:
		get_malloced_string( array_ptr, &s );
		returnval = (unsigned int) atoi( s );		
		mxFree( s );
		break;
	case mxDOUBLE_CLASS:
		doublep = mxGetPr( array_ptr );
		returnval = (unsigned int) *doublep;
		break;
	case mxSINGLE_CLASS:
		floatp = (float *) mxGetData( array_ptr );
		returnval = (unsigned int) *floatp;
		break;
	case mxINT32_CLASS:
		intp = (int *) mxGetData( array_ptr );
		returnval = (unsigned int) *intp;
		break;
	case mxUINT32_CLASS:
		uintp = (unsigned int *) mxGetData( array_ptr );
		returnval = *uintp;
		break;
	default:
		mexErrMsgTxt( "Wrong class passed to mxArrayToUint32\n" );
		break;
	}

	return returnval;

}

mxArray *
CreateDouble( double input_double )
{
	mxArray	*array_ptr;
	double	*doublep;

	array_ptr = mxCreateDoubleMatrix( 1, 1, mxREAL );
        if (array_ptr == NULL)
	{
                return (mxArray *) NULL;
	}

	doublep = (double *) mxGetPr( array_ptr );

	if( doublep == NULL )
	{
		mxDestroyArray( array_ptr );
		return (mxArray *) NULL;
	}
	else
	{
		*doublep = input_double;
	}

	return array_ptr;
}
