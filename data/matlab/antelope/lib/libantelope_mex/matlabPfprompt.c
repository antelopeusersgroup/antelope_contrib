/* 
 * Antelope Toolbox for Matlab
 *
 * Kent Lindquist
 * Geophysical Institute
 * University of Alaska, Fairbanks
 * 2000
 */

#include <stdio.h>
#include "antelope_mex.h"

char *
matlabPfprompt( char *prompt )
{
	Pf	*pf;
	mxArray *input_array[2];
	mxArray *output_array[1];
	mxArray *answer;
	char	*full_prompt;
	char	*result;
	static char prompt_tag[] = " : ";
	double	is_integer;

	full_prompt = (char *) mxCalloc( strlen( prompt ) +
					 strlen( prompt_tag ) + 1,
					 sizeof( char ) );

	sprintf( full_prompt, "%s%s", prompt, prompt_tag );
	input_array[0] = mxCreateString( full_prompt );
	mxFree( full_prompt );

	if( mexCallMATLAB( 1, output_array, 1, input_array, "input" ) != 0 )
	{
		mxDestroyArray( input_array[0] );
		return matlabPfprompt( prompt );
	}
	
	mxDestroyArray( input_array[0] );

	answer = output_array[0];

	if( mxIsChar( answer ) )
	{
		get_malloced_string( answer, &result );
		mxDestroyArray( answer );
		return result;
	} 
	else if( mxIsNumeric( answer ) )
	{

		if( mxGetM( answer ) == 0 && mxGetN( answer ) == 0 )
		{
			mxDestroyArray( answer );
			return matlabPfprompt( prompt );
		}
		else if( mxGetM( answer ) != 1 || mxGetN( answer ) != 1 )
		{
			mexWarnMsgTxt( "multidimensional responses not supported\n" );
			mxDestroyArray( answer );
			return matlabPfprompt( prompt );
		}

		input_array[0] = answer;
		if( mexCallMATLAB( 1, output_array, 1, input_array, "floor" ) != 0 )
		{
			mxDestroyArray( input_array[0] );
			return matlabPfprompt( prompt );
		}

		input_array[1] = output_array[0];
		if( mexCallMATLAB( 1, output_array, 2, input_array, "eq" ) != 0 )
		{
			mxDestroyArray( input_array[0] );
			mxDestroyArray( input_array[1] );
			return matlabPfprompt( prompt );
		}

		if( ! get_scalar( output_array[0], &is_integer ) )
		{
			mxDestroyArray( input_array[0] );
			mxDestroyArray( input_array[1] );
			mxDestroyArray( output_array[0] );
			return matlabPfprompt( prompt );
		}
		mxDestroyArray( input_array[1] );
		mxDestroyArray( output_array[0] );

		if( is_integer ) 
		{
			/* Suppress expression of superfluous decimal 
			   to avoid interpretation errors on boolean */

			input_array[0] = mxCreateString( "%.0f" );
		}
		else
		{
			/* Use %f to prevent loss of precision */

			input_array[0] = mxCreateString( "%f" );
		}
		input_array[1] = answer;

		if( mexCallMATLAB( 1, output_array, 2, input_array, "sprintf" ) != 0 )
		{
			mxDestroyArray( input_array[0] );
			mxDestroyArray( input_array[1] );
			return matlabPfprompt( prompt );
		}

		get_malloced_string( output_array[0], &result );

		mxDestroyArray( input_array[0] );
		mxDestroyArray( input_array[1] );
		mxDestroyArray( output_array[0] );

		return result;
	}
	else 
	{
		mexWarnMsgTxt( "output type not supported\n" );
		mxDestroyArray( output_array[0] );
		return matlabPfprompt( prompt );
	}
}
