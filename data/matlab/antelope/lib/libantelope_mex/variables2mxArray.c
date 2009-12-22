/*
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1997
 */

#include "antelope_mex.h"

long
mxArrayToLong( mxArray *array_ptr )
{
	int	type;
	int	*intp;
	long	*longp;
	double	*doublep;
	float 	*floatp;
	char	*s;
	long	returnval = 0L;

	type = mxGetClassID( array_ptr );
	switch( type )
	{
	case mxCHAR_CLASS:
		get_malloced_string( array_ptr, &s );
		returnval = atol( s );		
		mxFree( s );
		break;
	case mxDOUBLE_CLASS:
		doublep = mxGetPr( array_ptr );
		returnval = (long) *doublep;
		break;
	case mxSINGLE_CLASS:
		floatp = (float *) mxGetData( array_ptr );
		returnval = (long) *floatp;
		break;
	case mxINT32_CLASS:
		intp = (int *) mxGetData( array_ptr );
		returnval = (long) *intp;
		break;
	case mxINT64_CLASS:
		longp = (long *) mxGetData( array_ptr );
		returnval =  *longp;
		break;
	default:
		mexErrMsgTxt( "Wrong class passed to mxArrayToLong\n" );
		break;
	}

	return returnval;

}

mxArray *
UlongToMxArray( unsigned long input_uint )
{
	mxArray	*struct_array_ptr;
	mwSize   ndims = 1, dims[1] = {1};
	unsigned long	*pr;

        struct_array_ptr = mxCreateNumericArray( ndims, dims,
						 mxUINT64_CLASS, mxREAL );
        if (struct_array_ptr == NULL)
	{
                return (mxArray *) NULL;
	}

	pr = (unsigned long *) mxGetData( struct_array_ptr );
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

unsigned long
mxArrayToUlong( mxArray *array_ptr )
{
	mxClassID type;
	int	*intp;
	unsigned int *uintp;
	unsigned long *ulongp;
	double	*doublep;
	float 	*floatp;
	char	*s;
	unsigned long returnval = 0;

	type = mxGetClassID( array_ptr );
	switch( type )
	{
	case mxCHAR_CLASS:
		get_malloced_string( array_ptr, &s );
		returnval = (unsigned long) atol( s );		
		mxFree( s );
		break;
	case mxDOUBLE_CLASS:
		doublep = mxGetPr( array_ptr );
		returnval = (unsigned long) *doublep;
		break;
	case mxSINGLE_CLASS:
		floatp = (float *) mxGetData( array_ptr );
		returnval = (unsigned long) *floatp;
		break;
	case mxINT32_CLASS:
		intp = (int *) mxGetData( array_ptr );
		returnval = (unsigned long) *intp;
		break;
	case mxUINT32_CLASS:
		uintp = (unsigned int *) mxGetData( array_ptr );
		returnval = (unsigned long) *uintp;
		break;
	case mxUINT64_CLASS:
		ulongp = (unsigned long *) mxGetData( array_ptr );
		returnval = *ulongp;
		break;
	default:
		mexErrMsgTxt( "Wrong class passed to mxArrayToUlong\n" );
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

mxArray *
Complex_tToMxArray( Complex_t *cx, int nentries )
{
	mxArray	*array_ptr;
	double	*real;
	double	*imag;
	int	i;

	array_ptr = mxCreateDoubleMatrix( nentries, 1, mxCOMPLEX );

        if (array_ptr == NULL)
	{
		mxDestroyArray( array_ptr );
                return (mxArray *) NULL;
	}

	if( nentries <= 0 ) {

		mxDestroyArray( array_ptr );
                return (mxArray *) NULL;
	}

	real = mxGetPr( array_ptr );
	imag = mxGetPi( array_ptr );

	for( i = 0; i < nentries; i++ ) {

		real[i] = cx[i].real;
		imag[i] = cx[i].imag;
	}

	return array_ptr;
}

mxArray *
DoubleArrToMxArray( double *vals, int nentries )
{
	mxArray	*array_ptr;
	double	*real;
	int	i;

	array_ptr = mxCreateDoubleMatrix( nentries, 1, mxREAL );

        if (array_ptr == NULL)
	{
		mxDestroyArray( array_ptr );
                return (mxArray *) NULL;
	}

	if( nentries <= 0 ) {

		mxDestroyArray( array_ptr );
                return (mxArray *) NULL;
	}

	real = mxGetPr( array_ptr );

	for( i = 0; i < nentries; i++ ) {

		real[i] = vals[i];
	}

	return array_ptr;
}
