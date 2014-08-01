/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 1999
 */

#include <stdio.h>
#include <stdarg.h>
#include "antelope_mex.h"

static int dbresponse_struct_nrows = 1;
static int dbresponse_struct_ncols = 1;
static int dbresponse_struct_nfields = 1;
static const char *dbresponse_struct_fieldnames[] = {"address"};

Response *mxArray2Response( mxArray *array )
{
	Response *response;
	mxArray *address;

	if( ! mxIsClass( array, "dbresponse" ) ) 
	{
		mexWarnMsgTxt( "Input must be a dbresponse object" );
		return 0;
	}

	address = mxGetField( array, 0, "address" );

	if( address == (mxArray *) NULL ) 
	{
		return (Response *) NULL;
	}

	response = (Response *) mxArrayToUlong( address );

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

	address = UlongToMxArray( (unsigned long) response );
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

static mxArray *mxResponseGroupInit( int n_ptrs, ... ) 
{
	mxArray *result;
	va_list	ap;
	char	**fieldnames = 0;
	int	nargs = 0;
	char	*name;
	int	i;

	va_start( ap, n_ptrs );

	while( nargs < n_ptrs ) 
	{
		nargs++;

		name = va_arg( ap, char * );

		reallot( char **, fieldnames, nargs );

		fieldnames[nargs-1] = strdup( name );
	}

	va_end( ap );

	result = mxCreateStructMatrix( 1, 1, nargs, (const char **) fieldnames );

	for( i = 0; i < nargs; i++ ) 
	{
		free( fieldnames[i] );
	}

	free( fieldnames );

	return result;
}

mxArray *Response2mxArray_parse( Response *response )
{
	mxArray *result;
	mxArray *cell;
	mxArray *numbers;
	Response_group *group;
	Paz	*paz;
	Fap	*fap;
	Fir	*fir;
	Iir	*iir;
	Fap2	*fap2;
	int	igroup;
	double	*real;
	double	*imag;

	if( response == NULL || response->ngroups <= 0 ) {

		mexWarnMsgTxt( "Empty response object" );
		return 0;
	}

	result = mxCreateCellMatrix( response->ngroups, 1 );

	for( igroup = 0; igroup < response->ngroups; igroup++ )
	{
		group = &(response->groups[igroup]);

		switch( group->id ) {
		case PAZ:
			paz = (Paz *) group->pvt;

			/*
			typedef struct Paz {
			    int npoles, nzeros ; 
			    double normalization, frequency ;
			    Complex_t *poles, *pole_errors ; 
			    Complex_t *zeros, *zero_errors ; 
			    } Paz ; 
			*/
 
			cell = mxResponseGroupInit( 9, "type", 
						    "npoles", 
						    "nzeros",
						    "normalization",
						    "frequency",
						    "poles",
						    "pole_errors",
						    "zeros",
						    "zero_errors");

			mxSetField( cell, 0, "type", mxCreateString( "paz" ) );
			mxSetField( cell, 0, "npoles", 
				CreateDouble( (double) paz->npoles  ) );
			mxSetField( cell, 0, "nzeros", 
				CreateDouble( (double) paz->nzeros  ) );
			mxSetField( cell, 0, "normalization", 
				CreateDouble( paz->normalization  ) );
			mxSetField( cell, 0, "frequency", 
				CreateDouble( paz->frequency  ) );
			mxSetField( cell, 0, "poles", 
				Complex_tToMxArray( paz->poles, paz->npoles ) );
			mxSetField( cell, 0, "pole_errors", 
				Complex_tToMxArray( paz->pole_errors, paz->npoles ) );
			mxSetField( cell, 0, "zeros", 
				Complex_tToMxArray( paz->zeros, paz->nzeros ) );
			mxSetField( cell, 0, "zero_errors", 
				Complex_tToMxArray( paz->zero_errors, paz->nzeros ) );

			break;

		case FAP:

			fap = (Fap *) group->pvt;

			/*
			typedef struct Fap {
			    int ntriplets;
			    double *freqs;
			    double *amps, *amp_errors ;
			    double *phases, *phase_errors ;
			    } Fap ; 
			*/

			cell = mxResponseGroupInit( 7, "type", 
						    "ntriplets",
						    "freqs",
						    "amps",
						    "amp_errors",
						    "phases",
						    "phase_errors" );

			mxSetField( cell, 0, "type", mxCreateString( "fap" ) );

			mxSetField( cell, 0, "ntriplets", 
				CreateDouble( (double) fap->ntriplets  ) );
			mxSetField( cell, 0, "freqs", 
				DoubleArrToMxArray( fap->freqs, fap->ntriplets  ) );
			mxSetField( cell, 0, "amps", 
				DoubleArrToMxArray( fap->amps, fap->ntriplets  ) );
			mxSetField( cell, 0, "amp_errors", 
				DoubleArrToMxArray( fap->amp_errors, fap->ntriplets  ) );
			mxSetField( cell, 0, "phases", 
				DoubleArrToMxArray( fap->phases, fap->ntriplets  ) );
			mxSetField( cell, 0, "phase_errors", 
				DoubleArrToMxArray( fap->phase_errors, fap->ntriplets  ) );

			break;

		case FIR:

			fir = (Fir *) group->pvt;

			/*
			typedef struct Fir {
			    int nnum, nden ;
			    double srate ; 
			    int dec_factor ;
			    int seed_dec_offset ;
			    double midpoint ; 
			    double *num_coefs, *num_coef_errors ;
			    double *den_coefs, *den_coef_errors ;
			    } Fir ;
			*/

			cell = mxResponseGroupInit( 11, "type",
						    "nnum",
						    "nden",
						    "srate",
						    "dec_factor",
						    "seed_dec_offset",
						    "midpoint",
						    "num_coefs",
						    "num_coef_errors",
						    "den_coefs",
						    "den_coef_errors" );
			
			mxSetField( cell, 0, "type", mxCreateString( "fir" ) );

			mxSetField( cell, 0, "nnum", 
				CreateDouble( (double) fir->nnum  ) );
			mxSetField( cell, 0, "nden", 
				CreateDouble( (double) fir->nden  ) );
			mxSetField( cell, 0, "srate", 
				CreateDouble( fir->srate  ) );
			mxSetField( cell, 0, "dec_factor", 
				CreateDouble( (double) fir->dec_factor  ) );
			mxSetField( cell, 0, "seed_dec_offset", 
				CreateDouble( (double) fir->seed_dec_offset  ) );
			mxSetField( cell, 0, "midpoint", 
				CreateDouble( fir->midpoint  ) );
			mxSetField( cell, 0, "num_coefs", 
				DoubleArrToMxArray( fir->num_coefs, fir->nnum  ) );
			mxSetField( cell, 0, "num_coef_errors", 
				DoubleArrToMxArray( fir->num_coef_errors, fir->nnum  ) );
			mxSetField( cell, 0, "den_coefs", 
				DoubleArrToMxArray( fir->den_coefs, fir->nden  ) );
			mxSetField( cell, 0, "den_coef_errors", 
				DoubleArrToMxArray( fir->den_coef_errors, fir->nden  ) );

			break;

		case IIR:

			iir = (Iir *) group->pvt;

			/*
			typedef struct Iir {
			    int npoles, nzeros ; 
			    double normalization, frequency ;
			    Complex_t *poles, *pole_errors ; 
			    Complex_t *zeros, *zero_errors ; 
			    } Iir ; 
			*/
			
			cell = mxResponseGroupInit( 9, "type", 
						    "npoles", 
						    "nzeros",
						    "normalization",
						    "frequency",
						    "poles",
						    "pole_errors",
						    "zeros",
						    "zero_errors" );

			mxSetField( cell, 0, "type", mxCreateString( "iir" ) );
			mxSetField( cell, 0, "npoles", 
				CreateDouble( (double) iir->npoles  ) );
			mxSetField( cell, 0, "nzeros", 
				CreateDouble( (double) iir->nzeros  ) );
			mxSetField( cell, 0, "normalization", 
				CreateDouble( iir->normalization  ) );
			mxSetField( cell, 0, "frequency", 
				CreateDouble( iir->frequency  ) );
			mxSetField( cell, 0, "poles", 
				Complex_tToMxArray( iir->poles, iir->npoles ) );
			mxSetField( cell, 0, "pole_errors", 
				Complex_tToMxArray( iir->pole_errors, iir->npoles ) );
			mxSetField( cell, 0, "zeros", 
				Complex_tToMxArray( iir->zeros, iir->nzeros ) );
			mxSetField( cell, 0, "zero_errors", 
				Complex_tToMxArray( iir->zero_errors, iir->nzeros ) );

			break;

		case FAP2:

			fap2 = (Fap2 *) group->pvt;

			/*
			typedef struct Fap2 {
			    int ntriplets;
			    double *freqs;
			    double *amps, *amp_errors_high, *amp_errors_low ;
			    double *phases, *phase_errors_high, *phase_errors_low ;
			    } Fap2 ; 
			*/

			cell = mxResponseGroupInit( 9, "type",
						    "ntriplets",
						    "freqs",
						    "amps",
						    "amp_errors_high",
						    "amp_errors_low",
						    "phases",
						    "phase_errors_high",
						    "phase_errors_low" );

			mxSetField( cell, 0, "type", mxCreateString( "fap2" ) );

			mxSetField( cell, 0, "ntriplets", 
				CreateDouble( (double) fap2->ntriplets  ) );
			mxSetField( cell, 0, "freqs", 
				DoubleArrToMxArray( fap2->freqs, fap2->ntriplets  ) );
			mxSetField( cell, 0, "amps", 
				DoubleArrToMxArray( fap2->amps, fap2->ntriplets  ) );
			mxSetField( cell, 0, "amp_errors_high", 
				DoubleArrToMxArray( fap2->amp_errors_high, fap2->ntriplets  ) );
			mxSetField( cell, 0, "amp_errors_low", 
				DoubleArrToMxArray( fap2->amp_errors_low, fap2->ntriplets  ) );
			mxSetField( cell, 0, "phases", 
				DoubleArrToMxArray( fap2->phases, fap2->ntriplets  ) );
			mxSetField( cell, 0, "phase_errors_high", 
				DoubleArrToMxArray( fap2->phase_errors_high, fap2->ntriplets  ) );
			mxSetField( cell, 0, "phase_errors_low", 
				DoubleArrToMxArray( fap2->phase_errors_low, fap2->ntriplets  ) );
			break;
		}

		mxSetCell( result, igroup, cell );
	}

	return result;
}
