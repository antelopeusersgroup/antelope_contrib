/*
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Lindquist Consulting
 * 1997-2004
 */

#include "antelope_mex.h"

/* Convert a string to a number if possible, a string otherwise */

mxArray *
pfstring2mxArray( char *in )
{
	mxArray *result;
	mxArray *input[1];
	mxArray *output[1];

	input[0] = mxCreateString( in );

	mexCallMATLAB( 1, output, 1, input, "str2double" );
	
	if( mxIsNaN( mxGetScalar( output[0] ) ) ) {
		
		result = input[0];

		mxDestroyArray( output[0] );

	} else {
		
		result = output[0];

		mxDestroyArray( input[0] );
	}

	return result;
}
