/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 2003
 */

#include <stdio.h>
#include "antelope_mex.h"

#ifdef HAVE_CGEOM

static int cggrid_struct_nrows = 1;
static int cggrid_struct_ncols = 1;
static int cggrid_struct_nfields = 1;
static const char *cggrid_struct_fieldnames[] = {"address"};

Arr *ATM_CGGrid_Registry = 0;

int ATM_cggrid_register( CGGrid *cgg )
{
	char	*key;

	allot( char *, key, STRSZ );
	sprintf( key, "%x", (unsigned long) cgg );

	if( ATM_CGGrid_Registry == (Arr *) NULL ) 
	{
		ATM_CGGrid_Registry = newarr( 0 );
	}

	if( getarr( ATM_CGGrid_Registry, key ) != NULL ) 
	{
		mexWarnMsgTxt( "cggrid is already registered" );
		free( key );
		return 1;
	}

	setarr( ATM_CGGrid_Registry, key, (void *) 0x1 );

	return 1;
}

int ATM_cggrid_unregister( CGGrid *cgg )
{
	char	*key;

	if( ATM_CGGrid_Registry == (Arr *) NULL ) 
	{
		mexWarnMsgTxt( "internal error, cggrid registry "
			       "not initialized" );
		return 0;
	}

	allot( char *, key, STRSZ );
	sprintf( key, "%x", (unsigned long) cgg );

	if( getarr( ATM_CGGrid_Registry, key ) == NULL ) 
	{
		free( key );
		mexWarnMsgTxt( "cggrid is already unregistered" );
		return 1;
	} 

	delarr( ATM_CGGrid_Registry, key );

	free( key );

	return 1;
}

int ATM_cggrid_is_registered( CGGrid *cgg )
{
	char	*key;
	int	retcode;

	if( ATM_CGGrid_Registry == (Arr *) NULL ) 
	{
		mexWarnMsgTxt( "internal error, cggrid registry "
			       "not initialized" );
		return 0;
	}

	allot( char *, key, STRSZ );
	sprintf( key, "%x", (unsigned long) cgg );

	if( getarr( ATM_CGGrid_Registry, key ) != NULL ) 
	{
		retcode = 1;
	}
	else 
	{
		retcode = 0;
	}

	free( key );

	return retcode;
}

CGGrid *mxArray2CGGrid( const mxArray *array )
{
	CGGrid *cgg;
	mxArray *address;

	if( ! mxIsClass( array, "cggrid" ) ) 
	{
		mexWarnMsgTxt( "Input must be a cggrid object" );
		return 0;
	}

	address = mxGetField( array, 0, "address" );

	if( address == (mxArray *) NULL ) 
	{
		return (CGGrid *) NULL;
	}

	cgg = (CGGrid *) mxArrayToUlong( address );

	if( ! ATM_cggrid_is_registered( cgg ) ) 
	{
		mexWarnMsgTxt( "cggrid object no longer registered" );
		return (CGGrid *) NULL;
	}

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

	address = UlongToMxArray( (unsigned long) cgg );
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

double cggrid_set_mx_triplets( CGGrid *cgg, long ix, long iy, void *private )
{
	mxArray *result = (mxArray *) private;
	double	*data;
	long	triplet_row_number;
	long	mxindex_ix;
	long	mxindex_iy;
	long	mxindex_iz;

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

double cggrid_set_mx_x( CGGrid *cgg, long ix, long iy, void *private )
{
	mxArray *result = (mxArray *) private;
	double	*data;

	data = mxGetPr( result );

	data[ix * cgg->ny + iy] = CGGRID_X( cgg, ix );

	return CGGRID_DATA( cgg, ix , iy );
}

double cggrid_set_mx_y( CGGrid *cgg, long ix, long iy, void *private )
{
	mxArray *result = (mxArray *) private;
	double	*data;

	data = mxGetPr( result );

	data[ix * cgg->ny + iy] = CGGRID_Y( cgg, iy );

	return CGGRID_DATA( cgg, ix , iy );
}

double cggrid_set_mx_z( CGGrid *cgg, long ix, long iy, void *private )
{
	mxArray *result = (mxArray *) private;
	double	*data;

	data = mxGetPr( result );

	data[ix * cgg->ny + iy] = CGGRID_DATA( cgg, ix, iy );

	return CGGRID_DATA( cgg, ix , iy );
}

double cggrid_get_mx_z( CGGrid *cgg, long ix, long iy, void *private )
{
	mxArray *source = (mxArray *) private;
	double *data;

	data = mxGetPr( source );

	return data[ix * cgg->ny + iy];
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

static int
is_2d_mxArray( const mxArray *in )
{
	if( ! mxIsNumeric( in ) ||
	    ! mxIsDouble( in ) ||
	    mxIsEmpty( in ) ||
	    mxIsComplex( in ) ||
	    ( mxGetM( in ) <= 1 ) ||
	    ( mxGetN( in ) <= 1 ) )
	{
		return 0;
	}
	else 
	{
		return 1;
	}
}

static mxArray *unique_row( const mxArray *in )
{
	mxArray	*input[2];
	mxArray	*output[1];

	input[0] = (mxArray *) in;
	input[1] = mxCreateString("rows");
	mexCallMATLAB( 1, output, 2, input, "unique" );
	mxDestroyArray( input[1] );

	return output[0];
}

static double
get_spacing( mxArray *in )
{
	double	*data;
	double	spacing;
	long	i;

	data = mxGetPr( in );

	spacing = data[1] - data[0];

	for( i = 1; i < mxGetM( in ) * mxGetN( in ) - 1; i++ ) 
	{
		if( data[i+1] - data[i] - spacing > 2 * mxGetEps() ) 
		{
			return 0;
		}
	}

	return spacing;
}

static int
get_plaid_params( const mxArray *x,
		  const mxArray *y, 
		  const mxArray *z, 
		  double  *minx, 
		  double  *maxx,
		  double  *miny, 
		  double  *maxy,
		  double  *dx, 
		  double  *dy, 
		  mxArray **sorted_z )
{
	mxArray	*unique_xrow;
	mxArray	*unique_transpose_yrow;
	mxArray	*transpose_y;
	mxArray	*input[1];
	mxArray	*output[1];
	int	flip_x = 0;
	int	flip_y = 0;

	unique_xrow = unique_row( x );

	if( mxGetM( unique_xrow ) != 1 ) 
	{
		mxDestroyArray( unique_xrow );
		return 0;
	}
	else if( ( *dx = get_spacing( unique_xrow ) ) == 0 )
	{
		mxDestroyArray( unique_xrow );
		return 0;
	}
	else if( *dx < 0 ) 
	{
		*dx *= -1;
		flip_x = 1;
	}

	input[0] = unique_xrow;

	mexCallMATLAB( 1, output, 1, input, "min" );
	*minx = mxGetScalar( output[0] );
	mxDestroyArray( output[0] );

	mexCallMATLAB( 1, output, 1, input, "max" );
	*maxx = mxGetScalar( output[0] );
	mxDestroyArray( output[0] );

	mxDestroyArray( unique_xrow );

	input[0] = (mxArray *) y;
	mexCallMATLAB( 1, output, 1, input, "transpose" );
	transpose_y = output[0];

	unique_transpose_yrow = unique_row( transpose_y );

	if( mxGetM( unique_transpose_yrow ) != 1 ) 
	{
		mxDestroyArray( unique_transpose_yrow );
		return 0;
	}
	else if( ( *dy = get_spacing( unique_transpose_yrow ) ) == 0 )
	{
		mxDestroyArray( unique_transpose_yrow );
		return 0;
	}
	else if( *dy < 0 ) 
	{
		*dy *= -1;
		flip_y = 1;
	}

	input[0] = unique_transpose_yrow;

	mexCallMATLAB( 1, output, 1, input, "min" );
	*miny = mxGetScalar( output[0] );
	mxDestroyArray( output[0] );

	mexCallMATLAB( 1, output, 1, input, "max" );
	*maxy = mxGetScalar( output[0] );
	mxDestroyArray( output[0] );

	mxDestroyArray( unique_transpose_yrow );

	*sorted_z = mxDuplicateArray( z );

	if( flip_x )
	{
		input[0] = *sorted_z;
		mexCallMATLAB( 1, output, 1, input, "fliplr" );
		mxDestroyArray( input[0] );
		*sorted_z = output[0];
	}

	if( flip_y )
	{
		input[0] = *sorted_z;
		mexCallMATLAB( 1, output, 1, input, "flipud" );
		mxDestroyArray( input[0] );
		*sorted_z = output[0];
	}

	return 1;
}

CGGrid *plaid_mxArrays2CGGrid( const mxArray *x, const mxArray *y, const mxArray *z )
{
	CGGrid *cgg;
	double	minx;
	double	maxx;
	double	miny;
	double	maxy;
	double	dx;
	double	dy;
	mxArray	*sorted_z = 0;
	
	if( ! is_2d_mxArray( x ) ||
	    ! is_2d_mxArray( y ) ||
	    ! is_2d_mxArray( z ) )
	{
		return (CGGrid *) NULL;
	}

	if( mxGetM( x ) != mxGetM( y ) ||
	    mxGetM( x ) != mxGetM( z ) || 
	    mxGetN( x ) != mxGetN( y ) ||
	    mxGetN( x ) != mxGetN( z ) )
	{
		mexWarnMsgTxt( "Array dimensions must match" );
		return (CGGrid *) NULL;
	}

	if( ! get_plaid_params( x, y, z,
				&minx, &maxx, 
				&miny, &maxy, 
				&dx, &dy, 
				&sorted_z ) ) 
	{
		mexWarnMsgTxt( "Input arrays must represent a regular mesh" );
		return (CGGrid *) NULL;
	}

	cgg = cggrid_new( minx, maxx, miny, maxy, dx, dy );

	cggrid_apply( &cgg, cggrid_get_mx_z, (void *) sorted_z );

	return cgg;
}
#endif
