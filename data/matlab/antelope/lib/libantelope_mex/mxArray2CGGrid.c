/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2003
 */

#include <stdio.h>
#include "antelope_mex.h"

static int cggrid_struct_nrows = 1;
static int cggrid_struct_ncols = 1;
static int cggrid_struct_nfields = 1;
static const char *cggrid_struct_fieldnames[] = {"address"};

CGGrid *mxArray2CGGrid( mxArray *array )
{
	CGGrid *cgg;
	mxArray *address;

        if( mxGetClassID( array ) != mxOBJECT_CLASS )
        {
		mexWarnMsgTxt( "Input must be a cggrid object" );
		return 0;
        }
	else if( ! mxIsClass( array, "cggrid" ) ) 
	{
		mexWarnMsgTxt( "Input must be a cggrid object" );
		return 0;
	}

	address = mxGetField( array, 0, "address" );

	if( address == (mxArray *) NULL ) 
	{
		return (CGGrid *) NULL;
	}

	cgg = (CGGrid *) mxArrayToUint32( address );

	if( cgg == (CGGrid *) NULL )
	{
		return (CGGrid *) NULL;
	}
	else {
		return cgg;
	}
}

mxArray *CGGrid2mxArray( CGGrid *cgg )
{
	mxArray	*address;
	mxArray *input[2];
	mxArray	*array[1];

	input[0] = mxCreateStructMatrix(cggrid_struct_nrows,
                                        cggrid_struct_ncols,
                                        cggrid_struct_nfields,
                                        cggrid_struct_fieldnames);

	input[1] = mxCreateString("cggrid");

	address = Uint32ToMxArray( (unsigned int) cgg );
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

	if( ! mxIsClass( array[0], "cggrid" ) )
	{
		mxDestroyArray( array[0] );
		return (mxArray *) NULL;
	}
	else
	{
		return array[0];
	}
}

double cggrid_set_mx_triplets( CGGrid *cgg, int ix, int iy, void *private )
{
	mxArray *result = (mxArray *) private;
	double	*data;
	int	triplet_row_number;
	int	mxindex_ix;
	int	mxindex_iy;
	int	mxindex_iz;

	data = mxGetPr( result );

	triplet_row_number = ix * cgg->ny + iy;

	mxindex_ix = 0 * cgg->nx * cgg->ny + triplet_row_number;
	mxindex_iy = 1 * cgg->nx * cgg->ny + triplet_row_number;
	mxindex_iz = 2 * cgg->nx * cgg->ny + triplet_row_number;

	data[mxindex_ix] = CGGRID_X( cgg, ix );
	data[mxindex_iy] = CGGRID_Y( cgg, iy );
	data[mxindex_iz] = CGGRID_DATA( cgg, ix, iy );

	return CGGRID_DATA( cgg, ix , iy );
}

double cggrid_set_mx_x( CGGrid *cgg, int ix, int iy, void *private )
{
	mxArray *result = (mxArray *) private;
	double	*data;

	data = mxGetPr( result );

	data[ix * cgg->ny + iy] = CGGRID_X( cgg, ix );

	return CGGRID_DATA( cgg, ix , iy );
}

double cggrid_set_mx_y( CGGrid *cgg, int ix, int iy, void *private )
{
	mxArray *result = (mxArray *) private;
	double	*data;

	data = mxGetPr( result );

	data[ix * cgg->ny + iy] = CGGRID_Y( cgg, iy );

	return CGGRID_DATA( cgg, ix , iy );
}

double cggrid_set_mx_z( CGGrid *cgg, int ix, int iy, void *private )
{
	mxArray *result = (mxArray *) private;
	double	*data;

	data = mxGetPr( result );

	data[ix * cgg->ny + iy] = CGGRID_DATA( cgg, ix, iy );

	return CGGRID_DATA( cgg, ix , iy );
}

mxArray *CGGrid2linear_mxArray( CGGrid *cgg )
{
	mxArray	*result;

	if( cgg == (CGGrid *) NULL ) 
	{
		mexWarnMsgTxt( "Null Input CGGrid" );
		return (mxArray *) NULL;
	}

	result = mxCreateDoubleMatrix( cgg->nx * cgg->ny, 3, mxREAL );

	if( result == NULL )
	{
		mexWarnMsgTxt( "Failed to create output array for cggrid" );
		return (mxArray *) NULL;
	}

	cggrid_apply( &cgg, cggrid_set_mx_triplets, (void *) result );

	return result;
}

int CGGrid2plaid_mxArrays( CGGrid *cgg, mxArray **x, mxArray **y, mxArray **z ) 
{
	*x = mxCreateDoubleMatrix( cgg->nx, cgg->ny, mxREAL );
	*y = mxCreateDoubleMatrix( cgg->nx, cgg->ny, mxREAL );
	*z = mxCreateDoubleMatrix( cgg->nx, cgg->ny, mxREAL );

	cggrid_apply( &cgg, cggrid_set_mx_x, (void *) *x );
	cggrid_apply( &cgg, cggrid_set_mx_y, (void *) *y );
	cggrid_apply( &cgg, cggrid_set_mx_z, (void *) *z );

	return 1;
}
